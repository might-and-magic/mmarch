#!/usr/bin/env python3
"""Rebuild the tiny regression fixtures in ../data_general from real archives.

Only needed if a fixture has to change. It takes a handful of entries out of
real game archives, keeps the 288-byte header byte for byte (so Version,
LodType and Description stay exactly what the game ships) and copies the entry
blobs unchanged — then lays the data out in a different order from the entry
table, which is what real MM6/7/8 bitmaps.lod, icons.lod, sprites.lod and MM8
English*.lod do and what the v6.0.1 "size = gap to the next entry" bug could
not survive.

    python3 make_fixtures.py /path/to/mm678_installed [--h3 DIR] [outdir]

`--h3` points at a directory holding `H3ab_bmp.lod` and `H3ab_spr.lod` from a
Shadow of Death release (the Restoration of Erathia + Armageddon's Blade discs
ship a different `Lcdesc.txt` that does not have the property being tested);
without it the Heroes fixture is left alone. After regenerating, refresh the assertions with expected.py and re-run
audit_test_crcs.py.

Every fixture is rebuilt byte for byte, so `git status` staying clean after a
run is itself the check that these recipes still describe what is checked in.
"""

import os
import struct
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import rspak                                                    # noqa: E402


def _embedded_name_size(archive):
    """Bitmaps/icons/MM8 blobs start with the name padded to Options.NameSize,
    sprites with a fixed 12 bytes (RSLod.pas TRSLod.UnpackSprite)."""
    return 12 if archive.version == 'sprites' else archive.name_size


def build_lod(picks, out_path, order, header_from=None):
    """picks: (src_path, want[, rename]) — `want` is an entry name, or an index
    when the name appears more than once in the source. `rename` stores the
    entry under a different name; the copy of the name inside the blob is
    rewritten to match, which is what the game and MMArchive both do."""
    cache = {}

    def archive(path):
        if path not in cache:
            cache[path] = rspak.open_archive(path)
        return cache[path]

    names, blobs = [], []
    for pick in picks:
        src, want, rename = (pick + (None,))[:3] if len(pick) < 3 else pick
        a = archive(src)
        if isinstance(want, int):
            i = want
        else:
            hit = [k for k, e in enumerate(a.entries) if e.name.lower() == want.lower()]
            if not hit:
                raise SystemExit('no entry %r in %s' % (want, src))
            i = hit[0]
        name = rename or a.entries[i].name
        blob = bytearray(a.raw(i))
        ens = _embedded_name_size(a)
        if rename and len(blob) >= ens and blob[:ens].split(b'\0')[0]:
            blob[:ens] = name.encode('latin1').ljust(ens, b'\0')
        names.append(name)
        blobs.append(bytes(blob))

    head = archive(header_from or picks[0][0])
    ns = head.name_size
    item = 0x4C if ns == 0x40 else 0x20
    table_size = len(names) * item

    addr, data = {}, bytearray()
    for pos in order:
        addr[pos] = table_size + len(data)
        data += blobs[pos]

    header = bytearray(head.data[:288])
    struct.pack_into('<I', header, 272, 288)                      # ArchiveStart
    struct.pack_into('<I', header, 276, table_size + len(data))   # ArchiveSize
    struct.pack_into('<H', header, 284, len(names))               # Count

    table = bytearray(table_size)
    for k, name in enumerate(names):
        raw = name.encode('latin1')
        table[k * item:k * item + len(raw)] = raw
        struct.pack_into('<III', table, k * item + ns, addr[k], len(blobs[k]), 0)

    _write(out_path, bytes(header) + bytes(table) + bytes(data), len(names))


def build_h3_lod(picks, out_path, order):
    """A Heroes LOD is laid out nothing like an MM one: a 92-byte header, then
    32 bytes per entry (name[16], address, unpacked size, unknown, packed
    size), and the packed size being non-zero is what marks an entry
    compressed (RSLod.pas, TRSLodBase.InitOptions for the Heroes branch)."""
    cache = {}
    picked = []
    for src, want in picks:
        if src not in cache:
            cache[src] = rspak.open_archive(src)
        a = cache[src]
        hit = [k for k, e in enumerate(a.entries) if e.name.lower() == want.lower()]
        if not hit:
            raise SystemExit('no entry %r in %s' % (want, src))
        i = hit[0]
        picked.append((a.entries[i], a.raw(i)))

    head = cache[picks[0][0]]
    table_size = 92 + len(picked) * 32
    addr, data = {}, bytearray()
    for pos in order:
        addr[pos] = table_size + len(data)
        data += picked[pos][1]

    out = bytearray(head.data[:92])
    struct.pack_into('<I', out, 8, len(picked))
    table = bytearray(len(picked) * 32)
    for k, (e, blob) in enumerate(picked):
        raw = e.name.encode('latin1')
        table[k * 32:k * 32 + len(raw)] = raw
        struct.pack_into('<IIII', table, k * 32 + 16,
                         addr[k], e.unpacked, 0, e.size if e.packed else 0)
    _write(out_path, bytes(out) + bytes(table) + bytes(data), len(picked))


def build_vid(out_path, names, payloads, order):
    """A .vid entry has no size field and payload contents are opaque to
    mmarch, so a synthetic one is enough — what matters is that the addresses
    run out of table order. (Every shipped .vid happens to be in order.)"""
    data_start = 4 + len(names) * 0x2C
    addr, data = {}, bytearray()
    for pos in order:
        addr[pos] = data_start + len(data)
        data += payloads[pos]
    out = bytearray(struct.pack('<I', len(names)))
    for k, name in enumerate(names):
        entry = bytearray(0x2C)
        entry[:len(name)] = name.encode('latin1')
        struct.pack_into('<I', entry, 40, addr[k])
        out += entry
    out += data
    _write(out_path, bytes(out), len(names))


def _write(out_path, blob, count):
    old = None
    if os.path.exists(out_path):
        with open(out_path, 'rb') as f:
            old = f.read()
    with open(out_path, 'wb') as f:
        f.write(blob)
    state = 'new' if old is None else ('unchanged' if old == blob else 'CHANGED')
    print('%-32s %2d entries %7d bytes  %s'
          % (os.path.basename(out_path), count, len(blob), state))


def main(argv):
    argv = list(argv)
    h3 = None
    if '--h3' in argv:
        i = argv.index('--h3')
        h3 = argv[i + 1].rstrip('/') + '/'
        del argv[i:i + 2]
    if len(argv) < 2:
        print(__doc__.strip())
        return 2
    g = argv[1].rstrip('/') + '/'
    out = argv[2] if len(argv) > 2 else os.path.join(
        os.path.dirname(os.path.abspath(__file__)), '..', 'data_general')
    j = lambda n: os.path.join(out, n)                            # noqa: E731

    mm6b = g + 'mm6en/data/BITMAPS.LOD'
    mm7b = g + 'mm7en/DATA/BITMAPS.LOD'
    mm8b = g + 'mm8en/Data/bitmaps.lod'

    # MM8 localisation LOD: 64-byte names, unsorted addresses, a .str entry.
    build_lod([(g + 'mm8en/Data/EnglishT.lod', n) for n in
               ['Awards.txt', 'Out15.STR', 'fontpal.pcx', 'D07.STR']],
              j('real_mm8loc_unsorted.T.lod'), order=[2, 0, 3, 1])

    # MM6/7 icons LOD: 16-byte names, same unsorted layout.
    build_lod([(g + 'mm7en/DATA/events.lod', n) for n in
               ['dchest.bin', 'NWC.STR', 'D28.EVT', 'D13.STR']],
              j('real_icons_unsorted.lod'), order=[3, 1, 0, 2])

    # MM6 chapter LOD: OutB3.ddm's header DataSize counts its own 8 bytes, and
    # header.bin is in there twice (indices 24 and 73 of the source archive).
    build_lod([(g + 'mm6en/data/new.lod', n) for n in
               ['clock.bin', 'OutB3.ddm', 24, 'outa1.ddm', 73]],
              j('real_chapter_ddm.lod'), order=[1, 4, 0, 3, 2])

    # Textures. Every shipped bitmap resource is mipmapped, so the two kinds
    # that exist are bits=18 (stored) and bits=19 (zlib); both are here, with a
    # 64x16 / 16x64 pair so a swapped width and height cannot pass, the two
    # palettes they name (spelt PAL007 and pal008 — the case really does vary
    # inside one archive), and a plain data entry that lives in a picture LOD.
    build_lod([(mm7b, 'sgSTARS'), (mm7b, 'solid01'),
               (mm7b, 'Trim9_16a'), (mm7b, 'Trim9_16'),
               (mm7b, 'PAL007'), (mm7b, 'pal008'),
               (g + 'mm7en/DATA/ICONS.LOD', 'errorlog.txt')],
              j('real_bitmaps_unsorted.lod'), order=[4, 2, 6, 0, 5, 3, 1])

    # Palettes only, named in every casing MM8 actually ships (93 of its 292
    # are `Pal`), because the palette lookup used to be case-sensitive and lost
    # 450 MM8 sprites.
    build_lod([(mm6b, 'pal156', 'pal156'), (mm6b, 'pal002', 'Pal002'),
               (mm7b, 'pal120', 'PAL120'), (mm8b, 'pal338', 'pal338'),
               (mm6b, 'pal422', 'PaL422'), (mm8b, 'pal862', 'pal862'),
               (mm8b, 'pal857', 'Pal857')],
              j('real_pictures.bitmaps.lod'), order=[6, 2, 0, 5, 3, 1, 4],
              header_from=mm6b)

    # Sprites, including the two whose palette the games get wrong: MM6
    # BATATA0 (422 -> 156) and MM7 Swptree1 (940 -> 120).
    build_lod([(g + 'mm6en/data/SPRITES.LOD', 'BATATA0'),
               (g + 'mm6en/data/SPRITES.LOD', 'C3_HASTE'),
               (g + 'mm7en/DATA/SPRITES.LOD', 'Swptree1'),
               (g + 'mm8en/Data/sprites.lod', 'ARROWA0')],
              j('real_sprite_pal.lod'), order=[3, 1, 0, 2],
              header_from=g + 'mm6en/data/SPRITES.LOD')

    # ...and the counter-example: the fixup table is keyed on the name AND the
    # 20 header bytes, so C3_HASTE wearing the name BATATA0 keeps its own
    # palette 2 rather than being corrected to 156.
    build_lod([(g + 'mm6en/data/SPRITES.LOD', 'C3_HASTE', 'BATATA0')],
              j('real_sprite_nofix.lod'), order=[0])

    build_vid(j('real_vid_unsorted.vid'), ['Alpha', 'Bravo', 'Charlie'],
              [bytes([0xA0 + i]) * (200 + 37 * i) for i in range(3)],
              order=[2, 0, 1])

    # Heroes III: Lcdesc.txt and SkillLev.txt compress to exactly their own
    # length, which is what the "packed if the two sizes differ" guess got
    # wrong; AH16_.msk is under MinFileSize; Camp1DB2.pcx is a real one.
    if h3:
        build_h3_lod([(h3 + H3_SRC[n], n) for n in
                      ['Lcdesc.txt', 'SkillLev.txt', 'AH16_.msk',
                       'HPSyyy.pcx', 'Camp1DB2.pcx']],
                     j('real_h3_sizes.lod'), order=[2, 4, 0, 3, 1])
    else:
        print('%-32s skipped (pass --h3 DIR to rebuild)' % 'real_h3_sizes.lod')
    return 0


# Which archive each Heroes entry comes from; see the note in ../../DEVNOTES.md
# about the releases these were taken from.
H3_SRC = {
    'Lcdesc.txt': 'H3ab_bmp.lod',
    'SkillLev.txt': 'H3ab_bmp.lod',
    'AH16_.msk': 'H3ab_spr.lod',
    'HPSyyy.pcx': 'H3ab_bmp.lod',
    'Camp1DB2.pcx': 'H3ab_bmp.lod',
}


if __name__ == '__main__':
    sys.exit(main(sys.argv))

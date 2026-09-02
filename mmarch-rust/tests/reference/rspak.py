"""A Python model of how GrayFace's RSPak reads MM/Heroes archives.

This exists so the Rust test suite can assert what an entry *should* decode to
instead of what mmarch currently decodes it to. Every rule below is traced to a
place in `mmarch-delphi/RSPak/Extra/RSLod.pas`; nothing here was derived by
running mmarch and writing down the answer.

That distinction is not academic. `real_mm8loc.T.lod` shipped with
`D07.EVT = 382A0F77` as its expected CRC for two releases. That was the CRC of
768 bytes of raw archive data, produced by a bug: extraction read the
TMMLodFile header at offset $10 when MM8 puts it at Options.NameSize = $40. The
test passed for exactly as long as the bug lived, and failed the moment it was
fixed. A golden value taken from the implementation asserts only that today's
behaviour equals today's behaviour.

Read-only. `extract()` returns `(bytes, tag)`; the tag says what kind of
resource it was, and a `-raw` suffix means the picture could not be built and
the stored payload is being handed back instead — a sprite whose palette is not
in any neighbouring `bitmaps.lod`, for instance.

Usage:
    python3 rspak.py <archive> [...]        # list what RSPak would extract
"""

import os
import struct
import sys
import zlib

GAMES7_SIG1, GAMES7_SIG2 = 0x16741, 0x6969766D
TMMLODFILE_HEADER_SIZE = 32
COMPRESSED_MAP_EXTS = ('.blv', '.dlv', '.odm', '.ddm')

# LodTypes[] in RSLod.pas: (header Version, header LodType) -> our name for it.
LOD_TYPES = (
    ('MMVI',     'bitmaps',   'bitmaps'),
    ('MMVI',     'icons',     'icons'),
    ('MMVI',     'sprites08', 'sprites'),
    ('GameMMVI', 'maps',      'games'),
    ('MMVI',     'chapter',   'chapter'),
    ('MMVII',    'chapter',   'chapter7'),
    ('MMVIII',   'language',  'mm8'),
)


def unzip(data, expected):
    """TRSLod.Unzip / TRSMMFiles.RawExtract: copy exactly `expected` bytes out
    of a TDecompressionStream wrapped around the entry, and stop.

    Two consequences that a one-shot `zlib.decompress` would get wrong:

    * Whatever sits behind the deflate stream inside the entry is never read —
      a bitmap's palette, padding, a DataSize field that counts its own header,
      or the leftovers of a longer entry that a patcher overwrote in place.
    * The trailing Adler-32 is never reached, so it is not checked. GOG's MM6
      `Games.lod` ships `cd3.blv` and `d08.blv` with a stale checksum behind an
      otherwise intact stream, and the game reads them fine.

    The unpacked size the entry declares is the integrity check instead.
    """
    if expected == 0:
        return b''
    # Skip the 2-byte zlib header and raw-inflate, so the trailer never matters.
    out = zlib.decompressobj(-15).decompress(data[2:], expected)
    if len(out) < expected:
        raise ValueError('stream ended after %d of the %d bytes the entry '
                         'declares' % (len(out), expected))
    return out


def ext_of(name):
    return os.path.splitext(name)[1].lower()


def write_bmp8(width, height, rows, palette768):
    """The 8-bit BMP Delphi's TBitmap.SaveToStream writes, which is what
    TRSLod.DoExtract saves for a texture, icon, sprite or 8-bit PCX."""
    stride = ((width * 8 + 31) // 32) * 4
    pad = b'\0' * (stride - width)
    pixels = b''.join(rows[y] + pad for y in range(height - 1, -1, -1))   # bottom-up
    table = b''.join(bytes((palette768[i * 3 + 2], palette768[i * 3 + 1],
                            palette768[i * 3], 0)) for i in range(256))
    info = struct.pack('<IiiHHIIiiII', 40, width, height, 1, 8, 0, len(pixels), 0, 0, 256, 0)
    head = struct.pack('<2sIHHI', b'BM', 14 + 40 + 1024 + len(pixels), 0, 0, 14 + 40 + 1024)
    return head + info + table + pixels


def write_bmp24(width, height, rows_bgr):
    stride = ((width * 24 + 31) // 32) * 4
    pad = b'\0' * (stride - width * 3)
    pixels = b''.join(rows_bgr[y] + pad for y in range(height - 1, -1, -1))
    info = struct.pack('<IiiHHIIiiII', 40, width, height, 1, 24, 0, len(pixels), 0, 0, 0, 0)
    head = struct.pack('<2sIHHI', b'BM', 14 + 40 + len(pixels), 0, 0, 14 + 40)
    return head + info + pixels


def _load_palette_fixups():
    """RSLodEdt.pas PalFix: sprites naming a palette that is wrong (MM6 bats
    point at 422) or missing (MM7 swamp trees point at 940). Keyed by name plus
    the 20 header bytes, so a modified sprite of the same name is left alone."""
    path = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'palette_fixups.txt')
    out = {}
    if not os.path.exists(path):
        return out
    for line in open(path):
        line = line.strip()
        if not line or line.startswith('#'):
            continue
        name, pal, hexed = line.split()
        out[(name.lower(), bytes.fromhex(hexed))] = int(pal)
    return out


PALETTE_FIXUPS = _load_palette_fixups()
_palette_cache = {}


def sibling_palettes(archive_path):
    """RSLod.pas TRSLod.LoadBitmapsLods: bitmaps.lod next to the archive plus
    every *.bitmaps.lod, later ones winning."""
    d = os.path.dirname(os.path.abspath(archive_path))
    if d in _palette_cache:
        return _palette_cache[d]
    pals = {}
    try:
        names = os.listdir(d)
    except OSError:
        names = []
    base = [n for n in names if n.lower() == 'bitmaps.lod']
    patches = sorted(n for n in names if n.lower().endswith('.bitmaps.lod'))
    for n in base + patches:
        try:
            lod = open_archive(os.path.join(d, n))
        except Exception:
            continue
        for i, e in enumerate(lod.entries):
            low = e.name.lower()
            if not low.startswith('pal') or not low[3:].isdigit():
                continue
            try:
                data, _ = lod.extract(i)
            except Exception:
                continue
            if len(data) >= 768:
                pals[int(low[3:])] = data[:768]
    _palette_cache[d] = pals
    return pals


def _cstr(raw):
    return raw.split(b'\0')[0].decode('latin1')


class Entry:
    def __init__(self, name, addr, size, unpacked=0, packed=False):
        self.name = name
        self.addr = addr          # absolute offset in the file
        self.size = size          # stored size, i.e. RSPak's Size[i]
        self.unpacked = unpacked
        self.packed = packed      # RSPak's IsPacked[i]

    def __repr__(self):
        return 'Entry(%r, addr=%d, size=%d)' % (self.name, self.addr, self.size)


class Lod:
    """LOD archives: Heroes 3, and MM6/7/8 in all their flavours."""

    def __init__(self, path):
        self.path = path
        self.data = open(path, 'rb').read()
        if self.data[:4] != b'LOD\0':
            raise ValueError('not a LOD file')
        if struct.unpack_from('<I', self.data, 4)[0] <= 0xFFFF:
            self._load_heroes()
        else:
            self._load_mm()

    def _load_heroes(self):
        self.version = 'heroes'
        self.name_size = 0x10
        self.archive_start = None
        count = struct.unpack_from('<I', self.data, 8)[0]
        self.entries = []
        for i in range(count):
            eo = 92 + i * 32
            name = _cstr(self.data[eo:eo + 16])
            addr, unpacked, _unk, packed_size = struct.unpack_from('<IIII', self.data, eo + 16)
            # GetSize: PackedSizeOffset >= 0, so use it when non-zero and fall
            # back to UnpackedSize. GetIsPacked: PackedSize <> 0.
            size = packed_size if packed_size else unpacked
            self.entries.append(Entry(name, addr, size, unpacked, packed_size != 0))

    def _load_mm(self):
        version = _cstr(self.data[4:84])
        lod_type = _cstr(self.data[256:272])
        self.version = None
        for want_version, want_type, key in LOD_TYPES:
            if version == want_version and lod_type == want_type:
                self.version = key
                break
        if self.version is None:
            raise ValueError('unknown LOD version %r / type %r' % (version, lod_type))

        self.archive_start = struct.unpack_from('<I', self.data, 272)[0]
        count = struct.unpack_from('<H', self.data, 284)[0]
        # TRSLodBase.InitOptions
        self.name_size, item_size = (0x40, 0x4C) if self.version == 'mm8' else (0x10, 0x20)

        self.entries = []
        for i in range(count):
            eo = self.archive_start + i * item_size
            if eo + item_size > len(self.data):
                break
            name = _cstr(self.data[eo:eo + self.name_size])
            addr, size, _unused = struct.unpack_from('<III', self.data, eo + self.name_size)
            # The field at NameSize+4 is what RSPak calls UnpackedSizeOffset,
            # but with SizeOffset left at -1 GetSize falls through to it, so it
            # IS the stored size. The third field is 0 in every real archive.
            # IsPacked is always false here: compression is described by the
            # per-entry data header, not by the entry table.
            self.entries.append(Entry(name, self.archive_start + addr, size))

        # TRSLodBase.Load: a games LOD is promoted to the MM7 layout when the
        # first compressed map carries the signature.
        if self.version == 'games':
            for e in self.entries:
                if ext_of(e.name) in COMPRESSED_MAP_EXTS:
                    if e.size >= 16:
                        sig = struct.unpack('<II', self.data[e.addr:e.addr + 8])
                        if sig == (GAMES7_SIG1, GAMES7_SIG2):
                            self.version = 'games7'
                    break

    def raw(self, i):
        e = self.entries[i]
        if e.addr + e.size > len(self.data):
            raise ValueError('entry runs past the end of the file')
        return self.data[e.addr:e.addr + e.size]

    def extract(self, i):
        """Return (bytes, tag).

        tag is 'bmp' or 'sprite' or 'pcx' where RSPak would build a Windows
        bitmap and the Rust port returns the payload instead; 'act' for a
        palette; 'data' otherwise.
        """
        e = self.entries[i]
        blob = self.raw(i)
        v = self.version

        if v == 'heroes':
            out = unzip(blob, e.unpacked) if e.packed else blob
            if ext_of(e.name) != '.pcx':
                return out, 'data'
            # TRSLod.UnpackPcx: 12-byte header, raw pixels, palette when 8-bit.
            image_size, pw, ph = struct.unpack_from('<iii', out, 0)
            if pw > 0 and ph > 0 and image_size > 0 and image_size % (pw * ph) == 0:
                bpp = image_size // (pw * ph)
                px = out[12:12 + image_size]
                if bpp == 1 and len(out) >= 12 + image_size + 768:
                    rows = [px[y * pw:(y + 1) * pw] for y in range(ph)]
                    return write_bmp8(pw, ph, rows,
                                      out[12 + image_size:12 + image_size + 768]), 'pcx'
                if bpp == 3:
                    rows = [px[y * pw * 3:(y + 1) * pw * 3] for y in range(ph)]
                    return write_bmp24(pw, ph, rows), 'pcx'
            return out, 'pcx-raw'

        if v == 'sprites':
            # TRSLod.UnpackSprite: 12-byte name, TSprite, one line record per
            # row, then the pixel spans. Anything outside a line's span is the
            # transparent index 0.
            at = 16 - 4
            _sz, w, h, pal, _u1, _ys, _u2, unp = struct.unpack_from('<IhhhhhhI', blob, at)
            lines_at = at + 20
            pixels_at = lines_at + h * 8
            pixels = unzip(blob[pixels_at:], unp) if unp else blob[pixels_at:]
            rows = []
            for y in range(h):
                a1, a2, pos = struct.unpack_from('<hhi', blob, lines_at + y * 8)
                row = bytearray(w)
                if a1 >= 0 and a2 >= a1 and pos >= 0:
                    n = min(a2 - a1 + 1, w - a1)
                    row[a1:a1 + n] = pixels[pos:pos + n]
                rows.append(bytes(row))
            pal = PALETTE_FIXUPS.get((e.name.lower(), blob[at:at + 20]), pal)
            table = sibling_palettes(self.path).get(pal)
            if table is None:
                return blob, 'sprite-raw'      # no palette to colour it with
            return write_bmp8(w, h, rows, table), 'sprite'

        if v in ('bitmaps', 'icons', 'mm8'):
            ns = self.name_size
            bmp_size, data_size = struct.unpack_from('<ii', blob, ns)
            unp_size = struct.unpack_from('<i', blob, ns + 24)[0]
            # TMMLodFile stores these signed. A negative one is nonsense; say so
            # rather than trying to honour it.
            if data_size < 0 or unp_size < 0:
                raise ValueError('TMMLodFile header has a negative size field')
            payload = blob[ns + TMMLODFILE_HEADER_SIZE:]
            if bmp_size != 0:
                # TRSLod.UnpackBitmap: BmpWidth x BmpHeight indices, then the
                # 768-byte palette behind the compressed data. Mipmapped
                # textures decompress to more than w*h; the rest is not part of
                # the picture.
                bw, bh = struct.unpack_from('<hh', blob, ns + 8)
                pixels = unzip(payload, unp_size) if unp_size else payload[:data_size]
                palette = payload[data_size:data_size + 768]
                if (bw <= 0 or bh <= 0 or bmp_size != bw * bh
                        or len(pixels) < bw * bh or len(palette) != 768):
                    return (pixels + palette if len(palette) == 768 else pixels), 'bmp-raw'
                rows = [pixels[y * bw:(y + 1) * bw] for y in range(bh)]
                return write_bmp8(bw, bh, rows, palette), 'bmp'
            if data_size == 0 and e.size >= 768 + TMMLODFILE_HEADER_SIZE + ns:
                return payload[:768], 'act'
            out = unzip(payload, unp_size) if unp_size else payload[:data_size]
            if ext_of(e.name) == '.str':
                # TRSLod.UnpackStr: the NUL separators become line breaks.
                out = out.replace(b'\x00', b'\r\n')
            return out, 'data'

        if v in ('games', 'games7', 'chapter', 'chapter7'):
            if ext_of(e.name) not in COMPRESSED_MAP_EXTS:
                return blob, 'data'
            # TRSLod.DoExtract reads the header only for UnpackedSize and hands
            # Unzip `Size[i] - headerSize` as the compressed length. It cannot
            # use the header's own DataSize: MM6 chapter LODs ship .ddm entries
            # whose DataSize counts the 8-byte header as well.
            header = 16 if v in ('games7', 'chapter7') else 8
            unpacked = struct.unpack_from('<I', blob, 12 if header == 16 else 4)[0]
            body = blob[header:]
            return (unzip(body, unpacked) if unpacked else body), 'data'

        raise ValueError('unhandled version %r' % v)

    def extract_name(self, i):
        e = self.entries[i]
        v = self.version
        if v == 'heroes':
            if ext_of(e.name) == '.pcx':
                return os.path.splitext(e.name)[0] + '.bmp'
            return e.name
        if v == 'sprites':
            return e.name + '.bmp' if not ext_of(e.name) else e.name
        if v in ('bitmaps', 'icons', 'mm8'):
            _, tag = self.extract(i)
            if tag in ('bmp', 'bmp-raw'):
                return e.name + '.bmp'
            if tag == 'act':
                return e.name + '.act'
            # RSPak leaves the rest of the names alone. mmarch keeps its
            # documented "no extension -> .bmp / .act" mapping instead, because
            # the Rust port stores added .bmp files as generic data and they
            # have to come back under the name they went in with. The two only
            # differ on entries no shipped archive contains: a few MM6 fan
            # patches store palettes as DataSize = 768 rather than 0. Model
            # mmarch, since mmarch is what this file is used to check.
            if not ext_of(e.name):
                data, _ = self.extract(i)
                return e.name + ('.act' if len(data) == 768 else '.bmp')
            return e.name
        return e.name


class Snd:
    """SND archives (Heroes 3 and MM). TRSSnd in RSLod.pas."""

    def __init__(self, path):
        self.path = path
        self.data = open(path, 'rb').read()
        self.version = 'snd'
        count = struct.unpack_from('<I', self.data, 0)[0]

        # TRSSnd.ReadHeader: read the first entry as if it were the MM layout,
        # then look at what its data starts with.
        if count > 0 and len(self.data) >= 56:
            addr, size, unpacked = struct.unpack_from('<III', self.data, 44)
            sig = struct.unpack_from('<H', self.data, addr)[0] if addr + 2 <= len(self.data) else 0
            self.mm = (sig == 0x9C78) or (unpacked == size)
        else:
            self.mm = count == 0 and not os.path.exists(
                os.path.join(os.path.dirname(path), 'H3sprite.lod'))

        item_size = 0x34 if self.mm else 0x30
        self.entries = []
        for i in range(count):
            eo = 4 + i * item_size
            if eo + item_size > len(self.data):
                break
            name = _cstr(self.data[eo:eo + 40])
            addr, size = struct.unpack_from('<II', self.data, eo + 40)
            unpacked = struct.unpack_from('<I', self.data, eo + 48)[0] if self.mm else 0
            # GetIsPacked: both offsets are set for MM, so Size <> UnpackedSize.
            self.entries.append(Entry(name, addr, size, unpacked,
                                      self.mm and size != unpacked))

    def extract(self, i):
        e = self.entries[i]
        if e.addr + e.size > len(self.data):
            raise ValueError('resource runs past the end of the file')
        blob = self.data[e.addr:e.addr + e.size]
        return (unzip(blob, e.unpacked) if e.packed else blob), 'data'

    def extract_name(self, i):
        # TRSSnd.GetExtractName appends .wav unconditionally, so an entry
        # stored as `death.old` comes out as `death.old.wav`.
        return self.entries[i].name + '.wav'


# Signatures MMArchive may append to a .vid (RSLod.pas).
VID_SIG_OLD = bytes.fromhex('3EB9C5C5794748BD913AACEB28EBE015')
VID_SIG_START = bytes.fromhex('8703C24E26CF4CC697DDE2ECAEBECDB4')
VID_SIG_END = bytes.fromhex('0B74524676094D9FAFE53F7E9B23780E')
VID_SIG_NOEXT = bytes.fromhex('3F78DE47E92E40659AF174BBAE9D77D7')


class Vid:
    """VID archives. TRSVid in RSLod.pas."""

    def __init__(self, path):
        self.path = path
        self.data = open(path, 'rb').read()
        self.version = 'vid'
        count = struct.unpack_from('<I', self.data, 0)[0]

        self.entries = []
        for i in range(count):
            eo = 4 + i * 0x2C
            if eo + 0x2C > len(self.data):
                break
            self.entries.append(Entry(_cstr(self.data[eo:eo + 40]),
                                      struct.unpack_from('<I', self.data, eo + 40)[0], 0))

        self.size_table = self._read_size_table(len(self.entries))

        # TRSVid.GetFileSize: there is no size field, so an entry runs until
        # the nearest following address of ANY other entry — not the next one
        # in the table — bounded by the size table when there is one.
        for i, e in enumerate(self.entries):
            end = e.addr + self.size_table[i] if self.size_table else len(self.data)
            for j, other in enumerate(self.entries):
                if j != i and e.addr <= other.addr < end:
                    end = other.addr
            e.size = end - e.addr

    def _read_size_table(self, count):
        d = self.data
        if len(d) < 16 or count == 0:
            return None
        tail = d[-16:]
        if tail == VID_SIG_OLD:
            off = len(d) - 16 - count * 4
            if off >= 0:
                return list(struct.unpack_from('<%dI' % count, d, off))
        elif tail == VID_SIG_END:
            off = len(d) - 32 - count * 4
            if off >= 0 and d[off:off + 16] == VID_SIG_START:
                return list(struct.unpack_from('<%dI' % count, d, off + 16))
        return None

    def extract(self, i):
        e = self.entries[i]
        if e.addr + e.size > len(self.data):
            raise ValueError('resource runs past the end of the file')
        return self.data[e.addr:e.addr + e.size], 'data'

    def extract_name(self, i):
        name = self.entries[i].name
        return name if ext_of(name) else name + '.smk'


def open_archive(path):
    """Dispatch on extension, the way RSLoadMMArchive does."""
    e = ext_of(path)
    if e == '.snd':
        return Snd(path)
    if e == '.vid':
        return Vid(path)
    return Lod(path)      # .lod, .lwd, .pac, .mm6, .mm7, .dod


def main(argv):
    if len(argv) < 2:
        print(__doc__.strip())
        return 2
    for path in argv[1:]:
        a = open_archive(path)
        print('%s  [%s]  %d entries' % (os.path.basename(path), a.version, len(a.entries)))
        for i in range(len(a.entries)):
            try:
                data, tag = a.extract(i)
                print('  %-32s %-7s %8d  %08X'
                      % (a.extract_name(i), tag, len(data), zlib.crc32(data) & 0xFFFFFFFF))
            except Exception as exc:                       # noqa: BLE001
                print('  %-32s ERROR   %s' % (a.entries[i].name, exc))
    return 0


if __name__ == '__main__':
    sys.exit(main(sys.argv))

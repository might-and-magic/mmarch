#!/usr/bin/env python3
"""Take an archive apart with mmarch, put it back together, and compare.

The differential checker only exercises reading. This covers the other half:
extract an archive, build a new one of the same type out of what came out, read
*that* back with the RSPak model — which is what the game and MMArchive do —
and check every resource survived byte for byte.

    python3 roundtrip.py <mmarch-binary> <archive> [...]

Getting this wrong is quiet: an archive with a malformed header round-trips
through mmarch quite happily and is unreadable everywhere else. That is exactly
what the `mm8loclod` name-size bug did for two releases.
"""

import os
import shutil
import subprocess
import sys
import tempfile

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import rspak                                                    # noqa: E402

# RSPak version -> the type name `mmarch create` takes, and the extension the
# new archive needs (MMArchMain.pas `new`).
TYPE_OF = {
    'bitmaps':  ('mmbitmapslod', '.bitmaps.lod'),
    'icons':    ('mmiconslod',   '.lod'),
    'sprites':  ('mmspriteslod', '.lod'),
    'mm8':      ('mm8loclod',    '.T.lod'),
    'games':    ('mm6gameslod',  '.lod'),
    'games7':   ('mm78gameslod', '.lod'),
    'chapter':  ('mm6save',      '.mm6'),
    'chapter7': ('mm78save',     '.mm7'),
    'heroes':   ('h3lod',        '.lod'),
}


def roundtrip(mmarch, path):
    a = rspak.open_archive(path)
    if isinstance(a, rspak.Snd):
        kind, ext = ('mmsnd' if a.mm else 'h3snd'), '.snd'
    elif isinstance(a, rspak.Vid):
        no_ext = not any(rspak.ext_of(e.name) for e in a.entries)
        kind, ext = ('mm6vid' if no_ext else 'h3mm78vid'), '.vid'
    elif a.version in TYPE_OF:
        kind, ext = TYPE_OF[a.version]
    else:
        return 'SKIP unsupported %s' % a.version

    tmp = tempfile.mkdtemp(prefix='mmroundtrip_')
    try:
        shutil.copy(path, tmp)
        base = os.path.basename(path)
        # sprites and textures are coloured from a bitmaps.lod next door, both
        # coming out and going back in
        src_dir = os.path.dirname(os.path.abspath(path))
        for n in os.listdir(src_dir):
            low = n.lower()
            if (low == 'bitmaps.lod' or low.endswith('.bitmaps.lod')) and n != base:
                try:
                    shutil.copy(os.path.join(src_dir, n), tmp)
                except OSError:
                    pass

        def run(args, cwd):
            return subprocess.run([mmarch] + args, cwd=cwd, capture_output=True,
                                  text=True, timeout=3600)

        run(['extract', base, 'out'], tmp)
        out = os.path.join(tmp, 'out')
        if not os.path.isdir(out) or not os.listdir(out):
            return 'EXTRACT_EMPTY'

        rebuilt = 'rebuilt' + ext
        # <FOLDER> is where the new archive goes; the files to add are resolved
        # against the working directory, so build from inside out/
        r = run(['create', rebuilt, kind, '..', '*'], out)
        made = os.path.join(tmp, rebuilt)
        if r.returncode != 0 or not os.path.exists(made):
            return 'CREATE_FAIL %s' % (r.stderr or r.stdout).strip()[:120]

        notes = []
        try:
            b = rspak.open_archive(made)
            if isinstance(b, rspak.Lod) and getattr(b, 'archive_start', None) is not None:
                tail = max((e.addr + e.size for e in b.entries), default=0)
                if tail != os.path.getsize(made):
                    notes.append('TAIL_MISMATCH %d vs %d' % (tail, os.path.getsize(made)))
                if b.version != a.version:
                    notes.append('VERSION %s -> %s' % (a.version, b.version))
            unreadable = 0
            for i in range(len(b.entries)):
                try:
                    b.extract(i)
                except Exception:                               # noqa: BLE001
                    unreadable += 1
            if unreadable:
                notes.append('MODEL_CANNOT_READ=%d/%d' % (unreadable, len(b.entries)))
        except Exception as exc:                                # noqa: BLE001
            notes.append('MODEL_OPEN_FAIL ' + str(exc)[:70])

        run(['extract', rebuilt, 'back'], tmp)
        first, again = out, os.path.join(tmp, 'back')
        f1 = {n.lower(): n for n in os.listdir(first)}
        f2 = {n.lower(): n for n in os.listdir(again)} if os.path.isdir(again) else {}
        lost = sorted(set(f1) - set(f2))
        extra = sorted(set(f2) - set(f1))
        changed = [k for k in set(f1) & set(f2)
                   if open(os.path.join(first, f1[k]), 'rb').read()
                   != open(os.path.join(again, f2[k]), 'rb').read()]
        if lost:
            notes.append('LOST=%d %s' % (len(lost), lost[:3]))
        if extra:
            notes.append('EXTRA=%d %s' % (len(extra), extra[:3]))
        if changed:
            notes.append('CHANGED=%d %s' % (len(changed), changed[:3]))
        return ('%s n=%d %s' % (kind, len(f1), ' '.join(notes))) if notes \
            else ('OK %s n=%d' % (kind, len(f1)))
    finally:
        shutil.rmtree(tmp, ignore_errors=True)


def main(argv):
    if len(argv) < 3:
        print(__doc__.strip())
        return 2
    bad = 0
    for path in argv[2:]:
        try:
            result = roundtrip(argv[1], path)
        except Exception as exc:                                # noqa: BLE001
            result = 'ERROR %s' % str(exc)[:100]
        print('%-46s %s' % (os.path.basename(path), result), flush=True)
        if not result.startswith(('OK', 'SKIP')):
            bad += 1
    print('\n%d archive(s) did not round-trip' % bad)
    return 1 if bad else 0


if __name__ == '__main__':
    sys.exit(main(sys.argv))

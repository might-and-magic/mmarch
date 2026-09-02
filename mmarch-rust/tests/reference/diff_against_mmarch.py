#!/usr/bin/env python3
"""Extract archives with mmarch and byte-compare against the RSPak model.

Point it at a game install (or any list of archives) and it will, one archive
at a time, extract into a temporary directory, compare every file against what
rspak.py says the entry decodes to, and delete the output again. Peak disk use
is one archive's worth.

    python3 diff_against_mmarch.py <mmarch-binary> <root-dir-or-file> [...]

Output is one line per archive. Anything other than OK is worth reading:

    OK                  every file matches the model
    DIFF=n              n files decoded to different bytes  -> a real bug
    MISSING=n           the model produced a file mmarch did not
    EXTRA=n             mmarch produced a file the model did not
    CORRUPT_SRC=n       both sides refused the same entry   -> damaged source file
    MODEL_ONLY_FAIL=n   only the model refused it           -> the model is wrong
"""

import os
import shutil
import subprocess
import sys
import tempfile
import traceback

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import rspak                                                    # noqa: E402

ARCHIVE_EXTS = ('.lod', '.pac', '.snd', '.vid', '.lwd', '.mm6', '.mm7', '.dod')


def check(mmarch, path):
    notes, details = [], []
    try:
        a = rspak.open_archive(path)
    except Exception as exc:                                    # noqa: BLE001
        return 'MODEL_OPEN_FAIL', ['%s' % str(exc)[:120]], '?', 0

    tmp = tempfile.mkdtemp(prefix='mmdiff_')
    try:
        r = subprocess.run([mmarch, 'extract', path, os.path.join(tmp, 'out')],
                           capture_output=True, text=True, timeout=1800)
        mmarch_failed = set()
        for line in r.stderr.splitlines():
            if line.startswith('File `') and '` in archive' in line:
                mmarch_failed.add(line[6:line.index('` in archive')].lower())

        out = os.path.join(tmp, 'out')
        got = {}
        if os.path.isdir(out):
            for n in os.listdir(out):
                p = os.path.join(out, n)
                if os.path.isfile(p):
                    got[n.lower()] = open(p, 'rb').read()

        # Duplicate extracted names resolve to the last entry.
        winner = {}
        for i in range(len(a.entries)):
            try:
                winner[a.extract_name(i).lower()] = i
            except Exception:                                   # noqa: BLE001
                pass

        missing = different = corrupt = model_only = 0
        expected = set()
        for i in range(len(a.entries)):
            try:
                data, tag = a.extract(i)
                name = a.extract_name(i)
            except Exception as exc:                            # noqa: BLE001
                if a.entries[i].name.lower() in mmarch_failed:
                    corrupt += 1
                else:
                    model_only += 1
                    if len(details) < 8:
                        details.append('MODEL_ONLY_FAIL %s: %s'
                                       % (a.entries[i].name, str(exc)[:70]))
                continue
            expected.add(name.lower())
            if winner.get(name.lower()) != i:
                continue
            have = got.get(name.lower())
            if have is None:
                missing += 1
                if len(details) < 8:
                    details.append('MISSING %s (%s)' % (name, tag))
            elif have != data:
                different += 1
                if len(details) < 8:
                    details.append('DIFF %s %s model=%d mmarch=%d'
                                   % (name, tag, len(data), len(have)))

        extra = sorted(set(got) - expected)
        if corrupt:
            notes.append('CORRUPT_SRC=%d' % corrupt)
        if model_only:
            notes.append('MODEL_ONLY_FAIL=%d' % model_only)
        if missing:
            notes.append('MISSING=%d' % missing)
        if different:
            notes.append('DIFF=%d' % different)
        if extra:
            notes.append('EXTRA=%d' % len(extra))
            details.append('EXTRA ' + ', '.join(extra[:4]))
        if r.returncode != 0:
            notes.append('EXIT=%d' % r.returncode)
    except subprocess.TimeoutExpired:
        notes.append('TIMEOUT')
    except Exception:                                           # noqa: BLE001
        notes.append('CHECKER_ERROR')
        details.append(traceback.format_exc().splitlines()[-1][:120])
    finally:
        shutil.rmtree(tmp, ignore_errors=True)

    return (' '.join(notes) or 'OK'), details, a.version, len(a.entries)


def collect(targets):
    for t in targets:
        if os.path.isfile(t):
            yield t
        else:
            for root, _dirs, files in os.walk(t):
                for f in sorted(files):
                    if f.lower().endswith(ARCHIVE_EXTS):
                        yield os.path.join(root, f)


def main(argv):
    if len(argv) < 3:
        print(__doc__.strip())
        return 2
    mmarch, targets = argv[1], argv[2:]
    base = targets[0] if os.path.isdir(targets[0]) else os.path.dirname(targets[0])
    bad = 0
    for path in collect(targets):
        status, details, version, count = check(mmarch, path)
        print('%-64s %-9s n=%-6d %s'
              % (os.path.relpath(path, base)[:64], version, count, status), flush=True)
        for d in details:
            print('      ' + d)
        if status != 'OK':
            bad += 1
    print('\n%d archive(s) not clean' % bad)
    return 1 if bad else 0


if __name__ == '__main__':
    sys.exit(main(sys.argv))

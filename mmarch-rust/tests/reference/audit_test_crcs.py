#!/usr/bin/env python3
"""Check the expected values in tests/integration.rs against ground truth.

Two passes:

* every CRC literal is checked against the RSPak model (rspak.py);
* the two sprites whose palette the game gets wrong are checked against the
  .bmp files MMArchive itself produced, kept in `mmarchive/` next to this
  script. That turns "byte-identical to the reference tool" from a comment
  into something CI can fail on.

The point of the exercise is to make it impossible to quietly re-introduce an
expected value copied out of mmarch's own output. Run it after touching a
fixture or an assertion:

    python3 audit_test_crcs.py

Exits non-zero if any literal disagrees with what the format says the entry
decodes to. A resource the model could not turn into a picture — a sprite whose
palette is in no neighbouring bitmaps.lod, say — is counted separately: its CRC
is of the stored payload both mmarch and the Delphi CLI fall back to writing.
"""

import os
import re
import sys
import zlib

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import rspak                                                    # noqa: E402

TESTS = os.path.join(HERE, '..', 'integration.rs')
FIXTURES = os.path.join(HERE, '..', 'data_general')

FIXTURE_RE = re.compile(r'"(real_[A-Za-z0-9_.]+\.(?:lod|snd|vid|mm6|mm7))"')
# ("name", "CRC") and ("name", <n>, "CRC")
PAIR_RE = re.compile(r'\("([^"]+)"\s*,\s*(?:[0-9_]+usize\s*,\s*)?"([0-9A-F]{8})"\)')
# bare "CRC" strings inside an array, e.g. for crc in ["A17FA155", ...]
BARE_RE = re.compile(r'"([0-9A-F]{8})"')


def model_of(fixture):
    a = rspak.open_archive(os.path.join(FIXTURES, fixture))
    winner = {}
    for i in range(len(a.entries)):
        winner[a.extract_name(i).lower()] = i
    out = {}
    for i in range(len(a.entries)):
        name = a.extract_name(i)
        if winner[name.lower()] != i:
            continue
        data, tag = a.extract(i)
        out[name.lower()] = ('%08X' % (zlib.crc32(data) & 0xFFFFFFFF), tag)
    return out


def check_against_mmarch_output():
    """Compare our decode of the two corrected sprites with MMArchive's own
    output. `mmarchive/` holds the .bmp files MMArchive wrote for them; the
    sprites themselves and the palettes they need are both in
    tests/data_general, so this needs nothing installed.

    The correction these prove is the one thing here that no format can be
    read off: RSLodEdt.pas simply hardcodes that MM6's BATATA0 says palette
    422 and means 156, and MM7's Swptree1 says 940 and means 120."""
    ref_dir = os.path.join(HERE, 'mmarchive')
    if not os.path.isdir(ref_dir):
        print('mmarchive/ not found, skipping the MMArchive comparison')
        return 0
    names = {n.lower(): n for n in os.listdir(ref_dir) if n.lower().endswith('.bmp')}
    a = rspak.open_archive(os.path.join(FIXTURES, 'real_sprite_pal.lod'))
    bad = 0
    for i in range(len(a.entries)):
        want = names.get(a.entries[i].name.lower() + '.bmp')
        if want is None:
            continue
        with open(os.path.join(ref_dir, want), 'rb') as f:
            theirs = f.read()
        ours, _tag = a.extract(i)
        if ours == theirs:
            print("%-26s matches MMArchive's own mmarchive/%s (%d bytes)"
                  % (a.entries[i].name, want, len(theirs)))
        else:
            print("%-26s DIFFERS from MMArchive's mmarchive/%s: %d vs %d bytes"
                  % (a.entries[i].name, want, len(ours), len(theirs)))
            bad += 1
    return bad


def main():
    lines = open(TESTS, encoding='utf-8').read().splitlines()
    models, current = {}, None
    checked = snapshots = bad = 0

    for lineno, line in enumerate(lines, 1):
        if line.lstrip().startswith('//'):
            continue
        m = FIXTURE_RE.search(line)
        if m:
            current = m.group(1)
        named = PAIR_RE.findall(line)
        bare = [c for c in BARE_RE.findall(line) if c not in {p[1] for p in named}]
        if not named and not bare:
            continue
        if current is None:
            print('L%-5d cannot tell which fixture these CRCs belong to' % lineno)
            bad += 1
            continue
        if current not in models:
            path = os.path.join(FIXTURES, current)
            if not os.path.exists(path):
                print('L%-5d fixture %s does not exist' % (lineno, current))
                bad += 1
                continue
            models[current] = model_of(current)
        model = models[current]

        for name, crc in named:
            entry = model.get(name.lower())
            if entry is None:
                print('L%-5d %-26s %-24s no such entry in the model' % (lineno, current, name))
                bad += 1
            elif entry[0] != crc:
                print('L%-5d %-26s %-24s asserts %s, model says %s'
                      % (lineno, current, name, crc, entry[0]))
                bad += 1
            else:
                checked += 1
                if entry[1].endswith('-raw'):
                    snapshots += 1
        for crc in bare:
            if crc in {v[0] for v in model.values()}:
                checked += 1
            else:
                print('L%-5d %-26s bare CRC %s matches no entry' % (lineno, current, crc))
                bad += 1

    print('%d CRC literal(s) verified against the format model '
          '(%d of a resource that fell back to its stored payload), %d disagree'
          % (checked, snapshots, bad))
    bad += check_against_mmarch_output()
    return 1 if bad else 0


if __name__ == '__main__':
    sys.exit(main())

#!/usr/bin/env python3
"""Print the values a test should assert for an archive.

Run this to get the expected extracted names, sizes and CRC-32s for a fixture,
derived from the archive format (see rspak.py) rather than from mmarch's
output, then paste them into tests/integration.rs.

    python3 expected.py ../data_general/real_mm8loc_unsorted.T.lod

Use --rust to get the assertion literal directly.
"""

import os
import sys
import zlib

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import rspak                                                    # noqa: E402


def main(argv):
    args = [a for a in argv[1:] if not a.startswith('--')]
    as_rust = '--rust' in argv
    if not args:
        print(__doc__.strip())
        return 2

    for path in args:
        a = rspak.open_archive(path)
        rows = []
        # Duplicate names resolve to the last entry, the way Delphi's
        # sequential extraction loop leaves them on disk.
        winner = {}
        for i in range(len(a.entries)):
            winner[a.extract_name(i).lower()] = i
        for i in range(len(a.entries)):
            name = a.extract_name(i)
            if winner[name.lower()] != i:
                continue
            data, tag = a.extract(i)
            rows.append((a.entries[i].name, name, tag, len(data),
                         '%08X' % (zlib.crc32(data) & 0xFFFFFFFF)))

        if as_rust:
            print('        real_archive_test(bin, label, "%s",' % os.path.basename(path))
            print('            &[%s],' % ', '.join('"%s"' % r[0] for r in rows))
            print('            &[%s],' % ', '.join('"%s"' % r[1] for r in rows))
            print('            &[%s],' % ', '.join('("%s", "%s")' % (r[1], r[4]) for r in rows))
            print('        );')
        else:
            print('%s  [%s]' % (os.path.basename(path), a.version))
            for in_name, out_name, tag, size, crc in rows:
                note = ''
                if tag.endswith('-raw'):
                    note = '   <- could not be decoded; this is the stored payload'
                print('  %-28s -> %-28s %-7s %8d  %s%s'
                      % (in_name, out_name, tag, size, crc, note))
        print()
    return 0


if __name__ == '__main__':
    sys.exit(main(sys.argv))

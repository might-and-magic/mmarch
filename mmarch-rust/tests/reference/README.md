# RSPak reference model

A read-only Python model of how GrayFace's RSPak parses MM and Heroes archives,
plus the tools built on it. Nothing here is compiled or run by `cargo test`;
it exists so a human can check *where the numbers in the tests came from*.

## Why

Expected values in `tests/integration.rs` must be derived from the archive
format, never recorded from mmarch's output. A golden value taken from the
implementation asserts only that today's behaviour equals today's behaviour.

`real_mm8loc.T.lod` shipped with `D07.EVT = 382A0F77` as its expected CRC for
two releases. That was the CRC of 768 bytes of raw archive data, produced by a
bug: extraction read the `TMMLodFile` header at offset `$10` when MM8 puts it
at `Options.NameSize = $40`. The test passed for exactly as long as the bug
lived, and failed the moment it was fixed.

## Files

| | |
|---|---|
| `rspak.py` | the model. Every rule cites the place in `mmarch-delphi/RSPak/Extra/RSLod.pas` it comes from |
| `expected.py` | prints the names / sizes / CRCs a test should assert, `--rust` emits the literal |
| `make_fixtures.py` | rebuilds every `data_general/real_*` fixture out of real game archives; a run that reports them all `unchanged` is the proof the recipes still describe what is checked in |
| `diff_against_mmarch.py` | extracts archives with a built mmarch and byte-compares every file against the model |
| `roundtrip.py` | extracts an archive, rebuilds one of the same type from the result, and checks the model can read it back unchanged — the write path the differential checker cannot see |
| `audit_test_crcs.py` | checks every CRC literal in `integration.rs` against the model, and the two palette-corrected sprites against MMArchive's own output |
| `mmarchive/` | two `.bmp` files MMArchive itself wrote, the only ground truth here that was not read off a format |

## Use

```sh
# what should this fixture decode to?
python3 expected.py ../data_general/real_mm8loc_unsorted.T.lod
python3 expected.py --rust ../data_general/real_icons_unsorted.lod   # paste into integration.rs

# check a whole game install, one archive at a time, cleaning up as it goes
python3 diff_against_mmarch.py ../../target/release/mmarch /path/to/mm678_installed

# check the write path: extract, rebuild, read the rebuilt archive back
python3 roundtrip.py ../../target/release/mmarch /path/to/bitmaps.lod

# make sure no expected value in the tests was copied out of mmarch's output
python3 audit_test_crcs.py

# rebuild the fixtures (only if one has to change; re-run expected.py after).
# --h3 wants H3ab_bmp.lod and H3ab_spr.lod from a Shadow of Death release.
python3 make_fixtures.py /path/to/mm678_installed --h3 /path/to/h3/Data
```

`mmarchive/` is the one piece of ground truth in here that did not come from
reading a format spec: `BATATA0.bmp` and `Swptree1.bmp` as MMArchive itself
wrote them. `audit_test_crcs.py` decodes the same two sprites out of
`real_sprite_pal.lod` and compares, so "byte-identical to the reference tool"
is something CI can fail on rather than a claim in a comment. They are here
because the palette correction is the one rule in mmarch that no format
describes — RSLodEdt.pas hardcodes it — so a transcription slip would sail
past every other test in the suite.

`diff_against_mmarch.py` prints one line per archive. `DIFF` / `MISSING` /
`EXTRA` mean mmarch and the model disagree — a bug in one of them.
`CORRUPT_SRC` means both refused the same entry, i.e. the game file is damaged;
`../../../DEVNOTES.md` lists the ones already known.

## What the model does not cover

`rspak.py` decodes what RSPak decodes — bitmaps, sprites and Heroes "PCX"
images all come back as Windows `.bmp` files, palettes as bare 768-byte
`.act`, `.str` entries with their NUL separators turned into line breaks — so
the expected values for those really are derived from the format and not from
mmarch. Three resources in the tests are the exception, tagged `bmp-raw`:
their `TMMLodFile` header does not describe a picture (`BmpSize` disagrees
with `BmpWidth * BmpHeight`, or the 768 bytes of palette are not there), and
both RSPak and mmarch hand back the decoded payload instead of a `.bmp`.
`audit_test_crcs.py` counts them separately so the distinction stays visible.

What the model has no opinion about is the **write** path: it only reads.
`roundtrip.py` covers that instead, by rebuilding an archive out of what was
extracted and checking the model can read the result back unchanged.

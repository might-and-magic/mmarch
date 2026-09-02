# Developer notes

Things that are worth knowing before touching the archive code, and that do not
belong in the user-facing README.

1. [Where expected values come from](#1-where-expected-values-come-from)
2. [Checking against real game data](#2-checking-against-real-game-data)
3. [Known-damaged game files](#3-known-damaged-game-files)
4. [Pictures](#4-pictures)
5. [Deliberate differences from the Delphi version](#5-deliberate-differences-from-the-delphi-version)
6. [Things that bite when porting from RSPak](#6-things-that-bite-when-porting-from-rspak)
7. [Performance baseline](#7-performance-baseline)
8. [Deliberately out of scope](#8-deliberately-out-of-scope)

---

## 1. Where expected values come from

**Never record an expected value by running mmarch and writing down what came
out.** Derive it from the archive format.

`mmarch-rust/tests/reference/` holds a read-only Python model of how GrayFace's
RSPak parses these archives (`rspak.py`), with every rule traced to a place in
`mmarch-delphi/RSPak/Extra/RSLod.pas`. `expected.py` turns an archive into the
names, sizes and CRCs a test should assert. See that directory's README.

This is not a style preference. `real_mm8loc.T.lod` shipped with
`D07.EVT = 382A0F77` as its expected CRC for two releases. That was the CRC of
768 bytes of *raw archive data*: extraction was reading the `TMMLodFile` header
at offset `$10` when MM8 puts it at `Options.NameSize = $40`, so every entry
looked like a 768-byte palette. The test passed for exactly as long as the bug
lived, and failed the moment it was fixed. A value taken from the
implementation asserts only that today's behaviour equals today's behaviour.

The model decodes everything RSPak decodes, pictures included, so there is no
category of test left whose expected value is a snapshot of mmarch's own
output. `audit_test_crcs.py` re-derives every CRC literal in
`tests/integration.rs` from the model and fails on any that disagrees; run it
after touching a fixture or adding a test. The handful of resources whose
`TMMLodFile` header does not actually describe a picture are counted
separately in its output, because for those both RSPak and mmarch hand back
the decoded payload rather than a `.bmp`.

## 2. Checking against real game data

```sh
python3 mmarch-rust/tests/reference/diff_against_mmarch.py \
        mmarch-rust/target/release/mmarch /path/to/games
```

It extracts one archive at a time into a temp directory, byte-compares every
file against the model and deletes the output again, so peak disk use is one
archive. A run over 770 archives (31.8 GB: MM6/7/8 in English, French,
Simplified and Traditional Chinese, across GOG, uPlay, retail and every patch
level present, plus MMMerge) had **726 clean**; every one of the 44 exceptions
is a damaged source file, listed below.

## 3. Known-damaged game files

These are defects in the shipped or copied game data. mmarch reports the entry
on stderr and carries on with the rest of the archive; the Delphi version's CLI
instead falls back to writing the still-compressed bytes under the same name,
with no message. mmarch's behaviour is the deliberate one: a silently wrong
file is worse than a named error.

### MM7 `Audio.snd`: `02Flame01`

Declares 9956 unpacked bytes, the stream stops short. Present since 1999.

* **damaged**: `mm7en`, `mm7engog`, `mm7enpatch24`, `mm7enpatch257`, `mm7chs`,
  `mm7cs`, `mm7ja`, `mm7kr`, `mm7pl`. All nine ship the *same* `Audio.snd`
  (identical md5), carrying the same 7818-byte blob.
* **intact**: `mm7de`, `mm7es`, `mm7fr`, `mm7it`, `mm7ru` (a different
  7818-byte blob that decodes); `mm7cht` stores the entry uncompressed.
* GrayFace's patches do **not** fix it: they ship `patch.*.lod` overlays and
  never replace `Audio.snd`.

### MM6 `new.lod` / `Saves/*.mm6`: `d04.dlv`

The deflate stream ends cleanly at 105478 bytes while the entry header declares
105841. One specific 2586-byte blob (md5 `447dd712`) has been copied into 39
files.

* **damaged**: `mm6engog13` (the GOG English release), `mm6enpatch11`,
  `mm6enpatch12`, `mm6enpatch20`, `mm6enpatch21`, `mm6chsnocdxiaohu`,
  `mm6chsnocdxiaohuinmm`
* **intact**: `mm6en` (retail English), `mm6enpatch21tcc`, `mm6enpatch257`,
  `mm6enuplay`, `mm6pl`, `mm6kr`, `mm6chtpatch`, `mm6zhfixsimp`,
  `mm6zhfixsimptcc`

It is not a "the Chinese release is broken" story: the GOG English release and
several English patch levels carry it, and the Simplified Chinese `zhfixsimp`
builds do not.

### GOG MM6 `Games.lod`: `CD3.blv`, `d08.blv`

The deflate stream is intact and produces exactly the declared size, but ~2.5 KB
of stale bytes follow it and the trailing Adler-32 no longer matches: it looks
like an in-place patch that was never truncated. These extract fine, because
RSPak's contract is "copy UnpackedSize bytes out of the decompression stream and
stop"; the checksum behind the stream is never reached, and the declared size is
the integrity check instead (`zlib_decompress_bounded` in `lod.rs`).

### Heroes 3 archives

127 archives checked across seven Heroes 3 distributions (Restoration of
Erathia, Armageddon's Blade, Shadow of Death, the HD releases and all eight
Heroes Chronicles chapters) covering `H3bitmap.lod`, `H3sprite.lod`,
`H3ab_bmp.lod`, `H3ab_spr.lod`, `Heroes3.snd`, `VIDEO.VID`, `Heroes3.vid` and
the Chronicles' `x*`/`l*` archives. Nothing damaged; both PCX depths decode.
Two resources did expose the `IsPacked` mistake described in §6, which
`real_h3_sizes.lod` now covers.

Getting the data out of those distributions is its own obstacle course, so if
you repeat this: `/usr/bin/7z` segfaults on their RAR files; `/usr/lib/7zip/7z`
opens them but silently truncates large members (`Heroes3.snd` came out 8388092
bytes instead of 44403960, which looks exactly like an mmarch bug until you
check the size); `/usr/bin/rar` is correct but its wildcards are case sensitive,
so `*.lod` skips `H3AB_BMP.LOD`. Take the names from `rar lb` and pass them
verbatim.

### MMMerge `Data/new.lod`

MMMerge puts MM6-format and MM7-format maps in one `MMVII` / `chapter` archive:
13 entries carry the Games7 signature and 68 do not, and those 68 are not zlib
at any offset. RSPak's model has one header layout per archive, so the Delphi
version cannot read them either: it throws and falls back to writing the raw
bytes, which is what mmarch ends up writing too. Not something we plan to
support.

## 4. Pictures

Textures, icons, sprites and Heroes PCX resources are stored in the game's own
image formats and come out as Windows `.bmp` files, the way
`TRSLod.DoExtract` does it. Three sources of palette, three different shapes:

| resource | palette | notes |
|---|---|---|
| texture / icon (`TMMLodFile`, `BmpSize != 0`) | the 768 bytes right behind the compressed pixels | `bitmaps.lod` textures decompress to more than `BmpWidth*BmpHeight`: three half-size mip levels follow the picture and are not part of it (`UnpSize = BmpSize * (1 + ¼ + ¹⁄₁₆ + ¹⁄₆₄)`) |
| sprite | a `palNNN` resource in a `bitmaps.lod` **next to the archive** | without one there is nothing to colour it with, so the stored bytes are written instead, which is also what the Delphi CLI ends up doing, since its extract loop catches the failure and re-extracts raw |
| Heroes PCX | behind the pixels when 8-bit; 24-bit resources have none | nothing to do with the real PCX format: a 12-byte header, raw pixels, palette |

Two MM sprites name a palette that is wrong or missing: MM6's bats point at
422, MM7's swamp trees at 940, which is in no `bitmaps.lod` at all. MMArchive
carries a table of corrections (`PalFix` in `RSLodEdt.pas`) and so does mmarch,
in `src/image.rs` and `tests/reference/palette_fixups.txt`. Each row is keyed by
the sprite's name **and** its 20 header bytes, so a sprite someone modified is
left alone.

`tests/reference/mmarchive/` holds `BATATA0.bmp` and `Swptree1.bmp` as
MMArchive itself wrote them: the repo's only piece of ground truth that did
not come from reading a format spec, which matters here because the palette
correction is the one rule in mmarch that no format describes: RSLodEdt.pas
simply hardcodes it. `test_real_sprite_palette` asserts their CRCs and
`tests/reference/audit_test_crcs.py` compares byte for byte against
MMArchive's files, so the claim is checked rather than asserted.

All 52 corrected sprites were compared this way once, during the v7.0.0 work,
against the `fixedsprites.zip` MMArchive's author published; every one matched.
That run needed the game archives, so it is not something CI repeats: these
two are the pair whose sprites are small enough to keep in a fixture. If a row
of `PALETTE_FIXUPS` is ever in doubt, re-run MMArchive over the sprite and
diff, the way `audit_test_crcs.py` does.

**The one place the two tools can disagree** is regenerating a texture's mip
levels when a `.bmp` is added. The averaging is a faithful port of
`MixCl`/`MixClTr`, but turning an averaged colour back into a palette index is
`GetNearestPaletteIndex` (a Windows GDI call) in the Delphi version, and
mmarch's own nearest-in-RGB search here. Measured against MM6 `bitmaps.lod`,
regenerated levels differ from the shipped ones on 14–18% of texels, and those
texels land on a neighbouring palette entry (median RGB distance 20–38 out of a
441 maximum). The picture itself, the palette, the palette index and every
header field come back identical, which `roundtrip.py` checks over whole
archives.

## 5. Deliberate differences from the Delphi version

The Delphi version is frozen at 5.0.0, so anything added since (currently the
`version` command) is Rust-only. Everything below is a place where both
versions do the same job and behave differently.

### Output that differs

* **zlib bytes.** Both produce valid zlib streams, not bit-identical ones
  (different libraries), so an archive built by one differs byte-wise from the
  other even when it holds identical resources. `compare` will then report every
  re-compressed resource as modified: for MM LODs it compares the stored bytes,
  exactly as the Delphi version does, because RSPak reports `IsPacked` as false
  for every MM LOD entry and its "compare the unpacked data instead" path never
  runs. That is a property of the format, not of either implementation.
* **Texture mipmaps.** A `bitmaps.lod` texture stores three half-size copies of
  itself behind the picture, and both versions regenerate them when a `.bmp` is
  added, averaging each 2x2 block and picking the closest palette entry. But
  "closest" is a Windows GDI call in the Delphi version and mmarch's own search
  here, so the two can disagree on a texel whose average falls between two
  entries; see §4 for how far apart the results are. The picture itself, the
  palette and every header field are identical.
* **`Bits` in written `TMMLodFile` headers.** For a picture mmarch computes it
  the way RSPak does. For a plain data resource it writes 0; real archives are
  not consistent there either (MM8 `English*.lod` and MM6/7 `icons.lod` use
  256 while MM6 `bitmaps.lod` uses 0), and Delphi's own write path stores the
  field at the wrong offset (`+ length(name)` instead of `+ NameSize`) so it
  effectively writes 0 too. If a data resource mmarch wrote is ever rejected by
  MMArchive or the game, 256 is the first thing to try.
* **`MinFileSize` for Heroes 3 LODs.** Both reserve 320092 bytes
  (`92 + 10000 * 32`, room for the entry table the original H3 archives have)
  before the first file, so an H3 LOD either version writes is at least 320 KB.
* **Palette naming on non-standard resources.** RSPak calls a resource a
  palette only when `DataSize == 0`, and otherwise leaves an extension-less name
  alone. mmarch keeps its documented "no extension -> `.bmp` / `.act`" mapping
  for that leftover case, so such a resource still lands on disk under a usable
  name. The two differ only on resources no shipped archive contains: a few MM6
  fan patches (`mm6enpatch21tcc`, `mm6zhfixsimptcc`) store palettes as
  `DataSize = 768` rather than `0`, which RSPak extracts as `pal165` and mmarch
  as `pal165.act`.

### What happens when something is wrong

* **Damaged resources.** Delphi's CLI catches the exception and writes the raw
  stored bytes under the extracted name with no message; mmarch names the
  resource and the reason on stderr, writes no file for it, and carries on with
  the rest of the archive. §3 lists the shipped game files this applies to.
* **Non-BMP files in a sprites archive.** The Delphi version refuses them;
  mmarch stores them, and hands them back as they went in.
* **A palette that cannot be found.** Both refuse to add the resource rather
  than store a picture under palette 0, a picture that would render in the
  wrong colours. Both also look in the archive currently being written when its
  own name matches `bitmaps.lod` / `*.bitmaps.lod`, so a palette added earlier
  in the same command counts.

  Note that the lookup is by *content*: the picture's colour table has to match
  a `palNNN` resource byte for byte. Three textures in the shipped games do not
  satisfy that against their own archive (`T5s1b` in MM7 `BITMAPS.LOD`, and
  `plant12` and `T03a05trim` in MM8 `bitmaps.lod`, whose inline palettes differ
  slightly from the `palNNN` their header names), so rebuilding one of those
  archives from extracted `.bmp` files needs `/p` for those three. The Delphi
  version matches by content too and has the same gap.

### Detection and matching

* **SND format detection.** RSPak reads the first entry and looks at whether its
  data starts with a zlib header; mmarch validates the entry table under both
  layouts. Both agree on all 48 real `.snd` files checked; mmarch's survives an
  archive whose first entry is damaged.
* **`IsPacked` for MM SND.** RSPak treats `Size <> UnpackedSize` as packed,
  which for `UnpackedSize == 0` means decompressing zero bytes, i.e. writing an
  empty file. mmarch treats that as stored-as-is. No real archive has such an
  entry.
* **Non-ASCII case-insensitive comparison.** The Delphi version uses Windows
  locale-aware `SameText` for case-insensitive name matching (accented
  characters and so on); mmarch uses ASCII-only case folding. This only matters
  for archive entries with non-ASCII names, which are rare in game archives.

### Environment

* **File enumeration order.** mmarch sorts directory listings alphabetically;
  the Delphi version uses the raw `FindFirst`/`FindNext` order (typically
  alphabetical on NTFS). `add *.ext` and batch wildcard operations can therefore
  produce entries in a slightly different order. Functionality is not affected.
* **Unicode filesystem paths.** mmarch handles them; the Delphi version uses
  ANSI Win32 APIs and cannot open a file whose path is not representable in the
  system code page.

## 6. Things that bite when porting from RSPak

Collected from bugs that were actually shipped.

* **`Options.SizeOffset` is `-1` for every MM LOD.** `TRSMMFiles.GetSize` then
  falls through to `UnpackedSizeOffset`, i.e. the field at `NameSize + 4`, which
  really is the stored size. Deriving a size from the gap to the next entry's
  address is wrong: MM6/7/8 `bitmaps.lod`, `icons.lod`, `sprites.lod` and MM8
  `English*.lod` all store their files out of entry order.
* **`Options.NameSize` is `$40` for MM8 and `$10` for everything else**, both in
  the entry table *and* in front of the `TMMLodFile` header inside each stored
  blob. Reading an MM8 blob at `$10` lands in the zero padding of the name and
  makes every entry look like a 768-byte palette.
* **`IsPacked` is a field, not a comparison.** For a Heroes LOD it is "the
  packed-size field is non-zero"; for an MM SND it is `Size <> UnpackedSize`;
  for every MM LOD it is false, because what is compressed there is described
  by the per-resource data header rather than the entry table. Deriving it
  from "the two sizes differ" looks equivalent and is not: Heroes archives
  contain resources that compress to exactly their own length: `Lcdesc.txt`
  in `H3ab_bmp.lod` (54 bytes) and `AVLXsu12.def` in `H3ab_spr.lod` (1683),
  and those came out still zipped.
* **Games/chapter LODs: the compressed length is `Size[i] - headerSize`.**
  `TRSLod.DoExtract` never reads the header's own `DataSize`, and it cannot:
  MM6 chapter LODs ship `.ddm` entries whose `DataSize` counts the 8-byte header
  as well.
* **Decompression stops at `UnpackedSize`.** Whatever follows the deflate stream
  in an entry (a palette, padding, stale bytes from a longer entry that a
  patcher overwrote) is never read, and the trailing checksum is never reached.
* **VID entries have no size field at all.** `TRSVid.GetFileSize` runs an entry
  to the nearest following address of *any* other entry, not the next one in the
  table, bounded by the optional size table MMArchive appends after the data.
* **`TMMLodFile` size fields are signed.** A negative one cast to a length asks
  for exabytes; with `panic = "abort"` that takes down the whole extraction and
  loses the files that would have been fine.
* **A failed conversion must not eat the resource it was replacing.** `add`
  looks up the entry with the same name and drops it: do that only once the
  new blob exists, or an add that fails (a sprite whose palette cannot be
  found) leaves the archive without either version.
* **Extraction order is observable.** Archives do contain duplicate names
  (MM6 `new.lod` has two `header.bin`). Delphi's sequential loop leaves the last
  one on disk; extracting in parallel has to reproduce that instead of letting
  the winner depend on which worker finished last.

## 7. Performance baseline

v7.0.0 on a 20-core machine, local NVMe, best of seven runs. Compare a future
release against this rather than against a feeling: the parallel compression
and extraction v6.0.0 introduced is easy to lose by accident.

| | v6.0.1 | v7.0.0 |
|---|---|---|
| `checksum GAMES.LOD` (19.6 MB, 152 resources) | 0.412 | **0.283** |
| `extract GAMES.LOD` | 0.052 | **0.041** |
| `checksum Audio.snd` (18 MB, 1526 resources) | 0.155 | **0.128** |
| `extract Audio.snd` | 0.065 | **0.057** |
| `create mmsnd` (1526 wav, 26 MB) | 0.167 | **0.128** |
| `create mmiconslod` (1526 files) | 0.176 | **0.129** |

Picture archives cannot be compared against v6.0.1 (its `checksum` aborted on
the first resource and printed nothing), so these are absolute figures:
`checksum` of MM6 `BITMAPS.LOD` (1958 resources, mipmapped) 0.407 s, `extract`
of the same 0.104 s, `checksum` of MM7 `ICONS.LOD` (4241) 0.425 s.

Parallel scaling, `create mmsnd` with 1526 wavs: 1 thread 0.602 s, 2 0.351,
4 0.215, 8 0.157, 20 **0.132** (4.6x). `RAYON_NUM_THREADS` sets the count.

## 8. Deliberately out of scope

* **`.pcx` conversion.** The `.pcx` resources inside MM archives are ordinary
  PCX files (magic `0x0A`, version 5, RLE, 8-bit) that any image viewer opens,
  and RSPak hands them out untouched. Nothing to do. Not to be confused with
  Heroes 3's "PCX", which is not PCX at all and *is* decoded (see §4).
* **`.fnt` fonts.** RSPak and MMArchive do not touch fonts, and mmarch's whole
  contract is behaving as RSPak does: a font converter would be the one
  feature here justified by something else and tested by something else. The
  format is understood, though, so for whoever picks it up:
  * `i` header (`0x08ff1f`, sometimes `0x08ff1e`), `I` height << 8, 24 reserved
    bytes, `768i` of (spaceBefore, width, spaceAfter) per character, `256I` of
    bitmap offsets, then the pixel block. The header is 4128 bytes.
  * It is self-verifying: the sum of `width * height` over the non-empty
    glyphs equals the pixel block exactly, for 41 of the 42 fonts MM6/7/8 ship.
    That is a real source of expected values, so tests would not have to record
    the implementation's own output.
  * Two catches: `calig.fnt` (in all three games) uses a different layout
    (offsets at `0x19C`, width = the difference between offsets divided by the
    height), and the pixel values are intensities rather than palette indices;
    shipped fonts only use 0, 255 and 1 (transparent, body, shadow).
  * `might-and-magic/fnt-generator` already writes this format, including the
    DBCS splitting, so the decode half belongs there.

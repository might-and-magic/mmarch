# Copyright and licences

mmarch's own code is **MIT**, © Tom Chen. See [LICENSE](LICENSE).

## Rust CLI

The released CLI is built from `mmarch-rust/`. Its direct dependencies are
`miniz_oxide` (MIT/Zlib/Apache-2.0) and `rayon` (MIT/Apache-2.0), with their
own notices and terms.
The npm launcher and platform packages distribute this same CLI.

## GrayFace's RSPak

`mmarch-delphi/` is built on GrayFace's RSPak, and `mmarch-rust/` is a port
of it. Most of [GrayFace/Misc](https://github.com/GrayFace/Misc) is GPLv2,
but RSPak is separately MIT, as stated in
[RSSysUtils.pas](https://github.com/GrayFace/Misc/blob/master/RSPak/RSSysUtils.pas#L3427).

## Legacy Delphi components

`mmarch-delphi/RSPak/Extra/` carries third-party Delphi components — a Pascal
translation of zlib (Mark Adler; translation by Jacques Nomssi Nzali),
Anders Melander's TGIFImage and Drag and Drop Component Suite, and JVCL
include files — each with its own author and terms stated in its files.

The Delphi version is frozen at v5.0.0 and is not part of current releases.
If you build or redistribute it, check those components' notices; their
terms are not restated here.

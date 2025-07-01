# wif-weave

[![Build Status](https://github.com/erys/wif-weave/actions/workflows/rust.yml/badge.svg)](https://github.com/erys/wif-weave/actions/workflows/rust.yml)
[![Crates.io](https://img.shields.io/crates/v/wif-weave)](https://crates.io/crates/wif-weave)
[![Released API docs](https://docs.rs/wif-weave/badge.svg)](https://docs.rs/wif-weave)
[![Maintenance](https://img.shields.io/maintenance/yes/2025)](https://github.com/erys/wif-weave)
[![dependency status](https://deps.rs/repo/github/erys/wif-weave/status.svg)](https://deps.rs/repo/github/erys/wif-weave)

`wif-weave` is a parsing utility built on [configparser](https://crates.io/crates/configparser) to parse `.wif`
files used for representing weaving patterns. The `.wif` specification was originally created in 1996, and has been
stable at [1.1](http://www.tantradharma.com/maplehill/wif/wif1-1.txt) since 1997. While many weaving programs have
their own proprietary formats, nearly all can export to and import from `.wif` files.

## Compatibility with draft creation programs

Notes about how specific programs export to `.wif` and which features are supported

### Fiberworks - Supported in 1.0+

[Fiberworks](http://www.fiberworks-pcw.com/download.htm) uses its own `.dtx ` format, but can also export to and
import from wif files.

#### Defaults

Color range in Fiberworks generated wif files is `0-999` rather than the more common `0-255`

#### Comments

Uses a comment in the `TEXT` section to note the creation date

#### Private sections

Does not use private sections

### PixeLoom - Partially supported

[PixeLoom](https://pixeloom.com/) uses the `.wif` format, but makes use of comments and private sections to save
extra data. It also sometimes saves files using the `.plm` extension, but with the same file format.

#### Defaults

The `Date` in the header seems to be the creation date of the file, and the `Developers` field is the email address
for PixeLoom.

#### Comments

Uses comments in the threading and treadling/liftplan to denote sections of the pattern. Comment format is `
[<section length>]<Section Name>`.

#### Private Sections

Uses private sections for PixeLoom specific settings.

## License

Licensed under the [Anti-Capitalist Software License](https://anticapitalist.software/) version 1.4.

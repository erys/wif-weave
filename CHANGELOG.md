# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.3.0](https://github.com/erys/wif-weave/compare/v0.2.1...v0.3.0) - 2025-07-02

### Other

- [**breaking**] switch to using usize for all integer values

## [0.2.1](https://github.com/erys/wif-weave/compare/v0.2.0...v0.2.1) - 2025-07-02

### Added

- more apis to access sequences
- add warp and weft sections

## [0.2.0](https://github.com/erys/wif-weave/compare/v0.1.2...v0.2.0) - 2025-07-01

### Added

- [**breaking**] add weaving section support, various breaking refactors

### Other

- [**breaking**] Lots more linters, renames WifSection to Section
- *(README)* Adds info about some weaving software

## [0.1.2](https://github.com/erys/wif-weave/compare/v0.1.1...v0.1.2) - 2025-06-23

### Added
- Release Automation
### Removed
- Unused dependency

## [0.1.0] - 2025-06-21

### Added

- Functionality to parse a `.wif` file, and additionally parse some sections
    - Treadling
    - Threading
    - Tie Up
    - Lift Plan
    - Color Palette
- Ability to turn sequence sections into a `Vec`
- Ability to write back to a `.wif`
- async file writes/reads with `async` feature

[0.1.0]: https://github.com/erys/wif-weave/releases/tag/v0.1.0

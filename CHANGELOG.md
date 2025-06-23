# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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

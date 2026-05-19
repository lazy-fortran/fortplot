# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project uses date-based versioning (`YYYY.MM.PATCH`).

## [Unreleased]

### Changed
- CI: added FPM binary caching to all workflows (ci.yml, downstream-test.yml, docs.yml) (#1647)
- CI: macOS flang now runs on push to main, not only on PR (#1646)
- CI: added approval gating for fork PRs (#1645)
- CI: added artifact gate to test-ci target (#1648)
- CI: added comprehensive docs media validation (#1649)
- CI: added retry loop for PNG file visibility on Windows (#1643)
- CI: updated GitHub Actions to Node.js 24 compatible versions (#1642)
- CI: updated .gitignore to cover fpm_* artifacts (#1651)
- Build: synchronized version between fpm.toml and CMakeLists.txt (#1644)

### Fixed
- Windows: animation_clear_regression intermittent exit code 1 (#1643)
- Streamplot: draws only arrowheads, not trajectory lines (#1913)
- PDF: preserved labels and UTF-8 glyphs (#1927)
- Scatter: aligned signature with matplotlib convention (#1660)
- Quiver: added pivot, scale_units, alpha, and color strings support (#1671)
- hist: extended with matplotlib kwargs (#1662)
- xscale/yscale: added pyplot-style wrappers (#1663)
- Arrowsize/arrowstyle: restored support in core_streamplot path (#1930)

### Refactored
- Split large modules to comply with 500-line limit (#1694)
- Reorganized test directory structure (#1697, #1702)
- Split figure rendering pipeline and core modules into submodules
- Split legend, axes, contour, and scatter modules into focused submodules

### Added
- Release process documentation (doc/RELEASES.md)
- CHANGELOG.md for tracking release history

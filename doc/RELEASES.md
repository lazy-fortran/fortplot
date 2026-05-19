# Release Process

This document describes the release process and versioning policy for fortplot.

## Versioning Scheme

fortplot uses **date-based versioning** in the format `YYYY.MM.PATCH`:

- **Year** (`YYYY`): release year
- **Month** (`MM`): release month
- **Patch** (`PATCH`): incremental patch number for the same month

Examples: `2025.06.25`, `2025.08.17`, `2026.05.01`

This scheme ensures:
- Versions are always monotonically increasing
- Users can determine release recency from the version string
- Patch releases within a month are clearly ordered

## Version Synchronization

All version strings must be kept in sync:

- `fpm.toml` → `version` field
- `CMakeLists.txt` → `project(... VERSION ...)` field

Before any release, verify both files have the same version string.

## Release Steps

1. **Update CHANGELOG.md** — add all notable changes since the last release under `[Unreleased]`, then rename to `[X.Y.Z]` with the release date.

2. **Synchronize versions** — update `fpm.toml` and `CMakeLists.txt` to the new version.

3. **Run full verification**:
   ```
   make build
   make test
   make verify-artifacts
   make example
   make doc
   ```

4. **Create a release commit** on `main`:
   ```
   git add CHANGELOG.md fpm.toml CMakeLists.txt
   git commit -m "release: vX.Y.Z"
   git tag -a vX.Y.Z -m "Version X.Y.Z"
   git push origin main --tags
   ```

5. **Publish GitHub Release** — create a GitHub Release from the tag with the changelog entry as release notes.

## Branch Policy

- **`main`**: unstable development branch. All features and fixes land here first.
- **Tags**: created on `main` at release time. Do not create release branches.
- **Feature branches**: short-lived branches for individual PRs, merged to `main` via PR.

## Downstream Usage

### FPM
Users can pin to a specific version in their `fpm.toml`:
```toml
[dependencies]
fortplot = { git = "https://github.com/fortran-lang/fortplot", tag = "v2025.06.25" }
```

### CMake FetchContent
Users can pin to a specific version:
```cmake
FetchContent_Declare(
  fortplot
  GIT_REPOSITORY https://github.com/fortran-lang/fortplot
  GIT_TAG v2025.06.25
)
```

## Breaking Changes

Breaking changes require a new month (or year) component. For example, if `2025.06.25` introduces a breaking API change, the next release should be `2025.07.0` or later. Document all breaking changes in the CHANGELOG.md under a `### Breaking changes` section.

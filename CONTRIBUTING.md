# Contributing

Use a short branch and a focused pull request. Describe the problem, the
resulting behaviour and the checks performed. Commit subjects use `fix:`,
`feat:`, `docs:`, `refactor:` or `cleanup:`.

The PNG encoder, compressor, PDF writer, SVG writer and text renderer are
self-contained by design. Preserve this property and the fpm and CMake
interfaces used by downstream projects.

## Build and test

```sh
make build
make test-ci
```

`make test` runs the full fpm suite. Use `fpm test --target <name>` for an
individual regression. Put fixtures and regression tests beside the existing
tests in `test/`; write artifacts through `fortplot_test_helpers` under
`build/test/output/`. Do not skip or weaken an existing check.

Changes to plots, labels, ticks or backends also require:

```sh
make verify-artifacts
```

Inspect the rendered output and include enough evidence in the pull request
to show the corrected curve, geometry or text. The independent Matplotlib
checks and their limits are described in
[verification scope](doc/internals/visual-parity.md).

Use free-form Fortran with explicit declarations and imports. Keep modules
and procedures focused: aim for fewer than 500 lines per module and 50 per
procedure, with ceilings of 1000 and 100 respectively. The public type-bound
API in `fortplot_figure_core` is an exception because its interfaces must be
in the type's defining module. Test directories should stay below 50 files.

## Documentation

The documentation source is `doc/`. `make doc` builds the examples, generates
their pages and media, and runs FORD into `build/doc/`. Install its dependencies
from `doc/requirements-doc.txt` separately from library dependencies.
Keep the gallery's per-example media directories intact: generated pages
refer to `../../media/examples/<example>/<file>.png`.

Update the user guide for public API changes and add user-visible changes to
[CHANGELOG.md](CHANGELOG.md). The [release process](doc/internals/releases.md)
describes versioning and release checks.

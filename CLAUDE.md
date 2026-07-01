# fortplot

Self-contained Fortran plotting library. Backends: PNG (raster), PDF (vector), ASCII. No libpng, no zlib, no Cairo — all custom by design; do not propose replacing them with external libraries.

## Layout

- `src/` — library (`backends/`, `figures/`, `plotting/`, `text/`, etc.)
- `test/` — FPM tests, organized into subdirectories by area
- `example/` — runnable examples, outputs to `output/example/<name>/`
- `doc/` — FORD source; built site lands in `build/doc/`
- `Makefile` — top-level entry; wraps fpm + verification gates
- `fpm.toml` — package config (auto-discovery on)
- `CMakeLists.txt` — alternative build for downstream `FetchContent`. Do not delete.

## Build, test, run

Primary dev workflow is `fo` (cmake/ninja under the hood, hash-cached; avoids the
flaky fpm parallel-build backend). Run it before every commit.

- `fo` — full pipeline: build, test, lint, fmt hint
- `fo build` — build only (`--debug` adds `-g -O0 -fcheck=all -fbacktrace`)
- `fo test <name>` — run one test target; `fo test --all` runs all
- `fo check` — build + test, one-line status
- `fo lint` / `fo fmt` (`fo fmt --check` to verify without editing)

Make/fpm/CMake stay the CI and downstream build (do not remove):

- `make build` / `fpm build`; `make test` / `fpm test` (all); `make test-ci` (fast CI subset)
- `make verify-artifacts` — rendering gate; no `fo` equivalent, so use `make` here. Required for any change that touches plots, ticks, labels, or backends.
- `make example` (regenerates `output/example/...`); `make doc` (FORD → `build/doc/index.html`); `make clean`

`fo test <name>` or `fpm test --target <name>` runs one test.

## Conventions

- Free-form Fortran, no implicit typing. Use `use fortplot, only: wp => real64`.
- Modules < ~500 lines (hard ceiling 1000). Functions < 50 lines (hard 100).
  - Exception: `src/figures/core/fortplot_figure_core_main.f90` (817 lines) is a
    public API facade where Fortran requires all `procedure ::` / `generic ::`
    type-bound interfaces in the same module as the type definition.
    Implementations are split across submodules. See issue #1925.
- Each `test/<area>/` directory under 50 files (soft 20). Tests named `test_*.f90`.
- Test artifacts go to `build/test/output/` via `src/testing/fortplot_test_helpers.f90`, never to repo root.
- Conventional Commit subjects: `fix:`, `feat:`, `docs:`, `refactor:`, `cleanup:`.

## Rendering gate (non-negotiable)

For any visual change: run `make verify-artifacts`, then attach in the PR the commands you ran, the artifact paths, and a short text excerpt (e.g. from `pdftotext`) proving the label or curve is correct. PRs that affect rendering without this evidence do not merge.

## GitHub Pages docs

GitHub Actions runs `make example` → copies outputs to `doc/media/examples/<example>/` → FORD builds. Markdown in `doc/examples/*.md` references images as `../../media/examples/<example>/<file>.png`. Preserve that subdirectory structure on both sides — don't flatten.

## Issue hygiene

- `gh issue list --state open --limit 500` (the default truncates to 30).
- Search before filing: `gh issue list --state open --limit 500 --search "<keyword>"`.
- Edit issue bodies (`gh issue edit`) instead of piling comments.
- Issues without external library replacement proposals — they violate the independence policy and get closed on sight.

## Quality gates before claiming done

1. `fo build` (or `make build`) succeeds.
2. `fo check` / `fo test --all` (or `make test`, at least `make test-ci`) passes — no skipped or weakened tests.
3. For rendering changes: `make verify-artifacts` passes and the evidence is attached.
4. Changed Fortran is fmt-clean (`fo fmt --check`), and no new file exceeds the size limits above.

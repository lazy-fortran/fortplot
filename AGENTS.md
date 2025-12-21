# Repository Guidelines

## Project Structure & Module Organization
- `src/`: Fortran library sources (e.g., `backends/`, `figures/`, `utilities/`).
- `test/`: FPM tests (`test_*.f90`). Generated test artifacts default to `build/test/output/` (Issue #820 via `src/testing/fortplot_test_helpers.f90`).
- `example/`: Fortran and Python examples; generated media in `output/example/...`.
- `python/fortplot/`: Python wrapper and helpers (`core.py`, `axes.py`, etc.).
- `cmake/`, `Makefile`, `fpm.toml`: Build systems; docs in `doc/`, FORD config in `doc.md`.

## Build, Test, and Development Commands
- Build (FPM): `make build` or `fpm build`.
- Run examples: `make example` (Fortran), `make example_python`, `make example_matplotlib`.
- Debug apps: `make debug` or `fpm run --target <app>`.
- Tests: `make test` or `fpm test`; CI-fast set: `make test-ci`.
- Docs: `make doc` (FORD → `build/doc/index.html`).
- Clean: `make clean`.
  Examples: `make test ARGS="--target test_public_api"`, `make example ARGS="basic_plots"`.

## Coding Style & Naming Conventions
- Fortran: free-form, no implicit typing (`fpm.toml` enforces), prefer `use fortplot, only: wp => real64`.
- Indentation: spaces, keep modules under ~1000 lines; group by subfolders (`backends/`, `figures/`).
- Names: tests `test_*.f90`; procedures snake_case; modules end with `_t` for derived types (e.g., `figure_t`).
- Python: follow PEP 8; modules split into focused files (`core.py`, `axes.py`, `advanced.py`).
- Tooling: pre-commit for whitespace/EOF/YAML/TOML checks (`.pre-commit-config.yaml`).

## Testing Guidelines
- Framework: FPM auto-discovers tests in `test/`. Name new tests `test_<topic>.f90` and keep output in `build/test/output/`.
- Run subsets: `fpm test --target <name>`.
- Env controls: `FORTPLOT_SUPPRESS_WARNINGS=1 make test` (quiet), `FORTPLOT_FORCE_WARNINGS=1 make test`.

## Rendering Evidence & Gates
- Required for any change affecting plots, ticks, labels, or backends.
- Run: `make verify-artifacts` (runs key examples and strict PDF/PNG/text checks).
- Evidence in PRs: include the exact commands run, artifact paths, and short excerpts from `pdftotext`/`.txt` (e.g., `x³ - 50x` visible; no malformed labels like `01000+03`).
- Do not close rendering issues or merge PRs without passing `make verify-artifacts` and attaching evidence.

## Commit & Pull Request Guidelines
- Commits: Conventional Commits style (`feat:`, `fix:`, `docs:`, `refactor:`, `cleanup:`). Example: `fix: resolve pcolormesh dimension validation (#600)`.
- PRs: concise description, link issues, list user-facing changes, include before/after images for rendering changes, and note test targets run (e.g., `test-ci`) plus `make verify-artifacts` output snippets.

## Security & Configuration Tips
- Animations require `ffmpeg` (optional). Verify with `ffprobe` if needed.
- Hardened builds: trampoline checks via `fpm build --flag "-Wtrampolines -Werror=trampolines"`.
- Verification: `make verify-functionality`, `make verify-size-compliance` for artifact integrity.

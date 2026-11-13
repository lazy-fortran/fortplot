# Contributor instructions

Read [CONTRIBUTING.md](CONTRIBUTING.md) before changing fortplot. Preserve the
self-contained PNG, PDF, SVG and text backends and the native fpm and CMake
builds. Put test output under `build/test/output/` using the existing helpers.
Run the affected tests and the required rendering checks for visual changes.
Keep generated example pages and media out of source changes.

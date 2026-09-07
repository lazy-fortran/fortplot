# Matplotlib visual parity

Fortplot's default profile targets Matplotlib's default appearance. PNG and PDF
must preserve data geometry, limits, ticks, layout, marker sizes, colors, and
math structure. Font family and rasterizer differences are recorded separately.
Full visual parity has not yet been established.

## Frontend and backend

| Component | Responsibility |
|---|---|
| [fortplot](https://github.com/lazy-fortran/fortplot) | Native Fortran plotting APIs, specification reader, custom PNG/PDF/ASCII/SVG renderers |
| [mplvega](https://github.com/lazy-fortran/mplvega) | Python pyplot-shaped API, Vega-Lite specification generation, browser output, optional fortplot rendering |
| [fortnb](https://github.com/lazy-fortran/fortnb) | Experimental Fortran notebook execution and figure capture; integration remains incomplete |
| `packages/` | Independently buildable portions of the Fortran rendering library |

The Python frontend lowers its state to a Vega-Lite-shaped JSON specification.
HTML output passes through Vega-Lite and Vega; PNG and PDF pass through the
`fortplot_render` executable. Native Fortran calls enter fortplot directly.
The JSON boundary implements a subset of Vega-Lite plus scalar/vector field
extensions, rather than the complete Vega-Lite compiler.

Both paths need independent tests. Vega symbol names, sizes, and padding have
their own semantics. A Matplotlib diamond and a Vega diamond have different
dimensions at the same nominal size, while a Vega cross is a filled polygon.
The Python frontend must encode those distinctions in portable specifications.
The reader must also honor `autosize.contains: "padding"` to preserve the
requested total canvas dimensions. See the [Vega-Lite size contract](https://vega.github.io/vega-lite/docs/size.html#autosize)
and [symbol implementation](https://github.com/vega/vega/blob/main/packages/vega-scenegraph/src/path/symbols.js).

## Reproducible checks

Install validation dependencies separately from the library:

```sh
python3 -m pip install -r scripts/requirements-visual.txt
make build
make test-ci
make verify-artifacts
make verify-matplotlib-primitives
make verify-matplotlib-parity
```

The primitive gate is required in Linux CI. It renders fresh Fortran fixtures,
then checks them against the installed Matplotlib renderer and font data:

- Every supported marker in two sizes, plus zero-area markers, in PNG and PDF.
- Horizontal and vertical error-bar cap lengths, solid stems, and independent
  line/error colors in PNG and PDF.
- Adobe Symbol glyph encodings and advances, nested math glyphs/font sizes,
  fraction and radical rules, and equivalent math syntax.
- PDF dimensions at several DPI values, outward major/minor ticks, log ticks,
  JSON canvas sizing, and Vega symbol geometry.

PDF marker measurements use 300-DPI rasterization to reduce whole-pixel stroke
snapping. Measurements remain in physical units; canvases are never resized or
aligned to conceal errors. The PNG measurements use the default 100 DPI.
The separate Vega symbol check compares centered shape footprints and reports
placement offsets explicitly; full-figure comparisons retain absolute positions.

The full-figure gate renders 13 paired cases with actual Matplotlib defaults:
lines, scatter, bars, histograms, error bars, log axes, markers/dashes, grids,
filled bands, subplots, scripts, fractions, and radicals. It checks individual
colored components, axes geometry, each label's position, and math rules.
Missing artifacts, dimension mismatches, missing series, or moved labels fail.
Mutation tests deliberately remove and move visual elements to verify those
failure paths. Remaining differences make this stricter target fail; its exit
status must not be ignored when claiming full parity.

References use `matplotlib.rcParamsDefault` in an isolated context. This prevents
a user's styles or `matplotlibrc` from changing the oracle. See
[Matplotlib customization](https://matplotlib.org/stable/users/explain/customizing.html).
Each report records the installed Matplotlib version; the September 2026 audit
used 3.11.1. No Python graphics package is added to fortplot's runtime.

## Evidence and coverage

Reports and comparison images are generated under `output/visual-audit/`;
test fixtures belong in `build/test/output/`. Linux CI uploads the oracle
reports. A comparison image shows Matplotlib, fortplot, and the amplified
difference in that order. Numerical scores accompany visual inspection.

The initial September 2026 audit inventoried all 1,123 historical fortplot
issues, with zero open issues, and inspected 145 valid PNG/PDF example
artifacts. Per-issue classification used title/body screening and focused
reading; it did not rerun every historical reproducer. Closed issues supplied
investigation leads, not proof that a defect was resolved. Some historical
reports contained incorrect arithmetic or incorrect claims about Matplotlib.

The related Python audit inspected every output of all ten mplvega example
scripts: 24 figures, each in PNG and PDF. Its former comparator examined only
the first output, resized unequal canvases, and could accept rendering or
comparison failures. Those checks cannot certify parity.

## Remaining work

The first measured improvements address page/canvas dimensions, marker paths
and sizes, error bars, PDF axis defaults, minor ticks, math parsing and Symbol
metrics, and labeled-array field orientation. Important unresolved comparisons
include subplot placement, legend layout/selection, quiver geometry, scalar
field/colorbar behavior, streamplots, mixed 3D rendering, and math spacing.
Existing 3D examples also lack equivalent PDF coverage.

Math glyph/rule checks establish structure, not complete typography. Formula
extent and positioning differences remain visible in the full-figure gate.
Similarly, a high global image similarity can hide a missing marker, a shifted
cell, or a hollow line. Neither global scores nor successful file generation
replace the per-feature behavioral checks.

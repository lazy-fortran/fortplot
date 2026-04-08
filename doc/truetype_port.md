# TrueType Port Notes

The text renderer now uses the pure Fortran `fortplot_truetype` implementation
for table parsing, outline extraction, curve flattening, and bitmap
rasterization. The old `stb_truetype` C wrapper has been removed from the
library build.

## Runtime Benchmark

Benchmark date: 2026-04-09

Machine:
- Linux 6.19.10-1-cachyos x86_64
- AMD Ryzen 9 5950X 16-Core Processor
- GNU Fortran (GCC) 15.2.1
- GCC 15.2.1 for the reference C build

Workload:
- Font: `/usr/share/fonts/TTF/DejaVuSans.ttf`
- Printable ASCII codepoints `33:126`
- Sizes: `16`, `24`, `32` px
- Iterations: `200`
- Total glyph renders: `56,400`
- Total bitmap pixels processed: `9,411,200`

Release-build timings:

```text
Fortran runs (ms): 136.000, 138.000, 143.000, 145.000, 143.000
C reference runs (ms): 108.926, 107.568, 108.967, 105.345, 105.887
Fortran median: 143.000 ms
C reference median: 107.568 ms
Ratio: 1.329x
Delta: 32.9% slower than the C reference
```

The earlier much larger gap came from comparing the Fortran path through
`fpm run` default debug settings rather than comparing optimized binaries.

## Profiling Summary

`perf record -F 199 -g` on the release benchmark showed the primary hotspot in
the scan-conversion path:

```text
61.2%  __fortplot_tt_rasterizer_MOD_tt_rasterize
 6.7%  __fortplot_tt_binary_MOD_tt_byte
 5.8%  __fortplot_tt_rasterizer_MOD_handle_clipped_edge
 5.8%  __fortplot_tt_curves_MOD_tt_flatten_curves
```

The low-hanging fix was to remove default field initializers from the hot-path
derived types used for edges, active edges, points, and vertices. Those
initializers forced whole-array zeroing on every allocation in glyph rendering
even though the code overwrote those values immediately.

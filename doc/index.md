title: Documentation
---

# Documentation

fortplot is a Fortran plotting library with no external dependencies, rendering
the same figure to PNG, PDF, and text.

The [front page](../index.html) covers installing it and the plotting calls
themselves. This page is the map of everything else.

- **[API reference](../fortplot/index.html)** — modules, derived types, and
  procedures, generated from the source.
- **[API compatibility](./api_compatibility.html)** — method mapping for
  projects moving from pyplot-fortran.
- **[Contributor notes](./internals/index.html)** — release process and
  implementation notes.

## Examples

The **[examples gallery](./examples/index.html)** shows every example with its
plot, its source, and the PNG, PDF, and text output it generates. The list is
generated from `example/fortran/`, so it is never out of step with the tree.

## Backends

One figure renders to three backends, chosen by the extension passed to
`savefig`:

| Extension | Backend | Notes |
|---|---|---|
| `.png` | raster | Anti-aliased, self-contained PNG encoder |
| `.pdf` | vector | Embedded TrueType text, scalable output |
| `.txt` | text | Terminal-friendly; ASCII charset by default |

None of these depend on an external library: the PNG encoder, the zlib
compressor, the PDF writer, and the TrueType rasteriser are all part of
fortplot.

## Braille text mode

The text backend can render line and scatter data as Unicode braille dots for
higher resolution inside each terminal cell, while axes, ticks, and labels stay
plain text. Each character cell holds a 2-by-4 dot grid encoded as
`U+2800 + bitmask`; the dot-bit layout matches Drawille (left column dots
1,2,3,7 -> `0x01,0x02,0x04,0x40`; right column dots 4,5,6,8 ->
`0x08,0x10,0x20,0x80`).

```fortran
type(figure_t) :: fig
call fig%initialize(80, 24)
call fig%set_text_charset('braille')
call fig%add_plot(x, y)
call fig%savefig('plot.txt')
```

Braille is opt-in: `.txt` output stays ASCII unless `set_text_charset('braille')`
is called.

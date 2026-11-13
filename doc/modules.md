title: Module Overview
---

# Module overview

Most applications need only `use fortplot`. Lower-level modules support the
public interfaces and the rendering backends; they need not be imported to
create a plot.

| Module or source area | Purpose |
|---|---|
| `fortplot` (`src/core/`) | Public plotting interface, `figure_t`, `wp` and animation exports |
| `fortplot_figure_core` (`src/figures/core/`) | Independent figure objects; implementations in figure submodules |
| `fortplot_matplotlib` (`src/interfaces/`) | Procedural plotting calls and current-figure state |
| `fortplot_fortarray` (`src/interfaces/`) | Optional labelled-array plotting adapter |
| `fortplot_animation` (`src/animation/`) | Animation construction, frame rendering and saving |
| `src/backends/` | PNG, PDF, SVG and text output |
| `src/text/` | Fonts, annotations and mathematical text |
| `src/plotting/`, `src/rendering/` | Data geometry and rendering support |
| `src/spec/` | Plot specification reader and renderer |
| `src/testing/` | Shared test fixtures and output helpers |

The generated [API reference](../module/fortplot.html) documents public types,
procedures and arguments. Start with the [user guide](./user-guide.html) for
working examples.

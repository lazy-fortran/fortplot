title: API Compatibility Guide
---

# API Compatibility with pyplot-fortran

This document describes the API compatibility between FortPlot and
[pyplot-fortran](https://github.com/jacobwilliams/pyplot-fortran) for users
migrating between libraries.

## Design Philosophy

FortPlot follows a design philosophy compatible with pyplot-fortran for the
OO interface while matching matplotlib conventions for the stateful interface.
The key differences arise from FortPlot being a native Fortran plotting library
(no Python dependency) versus pyplot-fortran being a Python wrapper generator.

## Method Mapping

### Initialization

**pyplot-fortran:**
```fortran
call plt%initialize(grid, xlabel, ylabel, zlabel, title, legend, ...)
```

**FortPlot:**
```fortran
type(figure_t) :: fig
call fig%initialize(width, height, backend, dpi)
call fig%set_xlabel(xlabel)
call fig%set_ylabel(ylabel)
call fig%set_title(title)
call fig%grid(.true.)
```

FortPlot uses separate methods for axis labels and titles to match matplotlib
patterns and allow dynamic updates.

### Line Plots

**pyplot-fortran:**
```fortran
call plt%add_plot(x, y, label, linestyle, markersize, linewidth, &
                  xlim, ylim, xscale, yscale, color, istat)
```

**FortPlot:**
```fortran
call fig%add_plot(x, y, label, linestyle, color)
call fig%set_xlim(xmin, xmax)
call fig%set_ylim(ymin, ymax)
call fig%set_xscale(scale)
call fig%set_yscale(scale)
```

FortPlot separates axis limits and scales into dedicated methods for clarity
and reusability. This matches matplotlib where `plt.xlim()` and `plt.ylim()`
are separate calls.

### Bar Charts

**pyplot-fortran:**
```fortran
call plt%add_bar(x, height, label, width, bottom, color, yerr, align, &
                 xlim, ylim, xscale, yscale, istat)
```

**FortPlot:**
```fortran
call fig%bar(x, heights, width, bottom, label, color)
```

### Histograms

**pyplot-fortran:**
```fortran
call plt%add_hist(x, label, xlim, ylim, xscale, yscale, bins, normed, &
                  cumulative, istat)
```

**FortPlot:**
```fortran
call fig%hist(data, bins, density, label, color)
```

FortPlot uses `density` (modern matplotlib) instead of `normed` (deprecated).

### Contour Plots

**pyplot-fortran:**
```fortran
call plt%add_contour(x, y, z, linestyle, linewidth, levels, color, &
                     filled, cmap, colorbar, istat)
```

**FortPlot:**
```fortran
call fig%add_contour(x_grid, y_grid, z_grid, levels, label)
call fig%add_contour_filled(x_grid, y_grid, z_grid, levels, colormap, &
                            show_colorbar, label)
```

FortPlot separates line contours from filled contours for clarity.

### Error Bars

**pyplot-fortran:**
```fortran
call plt%add_errorbar(x, y, label, linestyle, xerr, yerr, markersize, &
                      linewidth, xlim, ylim, xscale, yscale, color, istat)
```

**FortPlot:**
```fortran
call fig%errorbar(x, y, xerr, yerr, xerr_lower, xerr_upper, &
                  yerr_lower, yerr_upper, label, marker, markersize, &
                  ecolor, elinewidth, capsize, capthick, color)
```

FortPlot adds asymmetric error bar support and more matplotlib-compatible
options (capsize, capthick).

### Scatter Plots

**pyplot-fortran:** No dedicated scatter function.

**FortPlot:**
```fortran
call fig%scatter(x, y, s, c, marker, markersize, color, colormap, &
                 alpha, edgecolor, facecolor, linewidth, vmin, vmax, &
                 label, show_colorbar)
```

FortPlot provides full scatter plot support with size and color arrays.

### Surface Plots

**pyplot-fortran:**
```fortran
call plt%plot_surface(x, y, z, label, linestyle, linewidth, levels, &
                      color, cmap, colorbar, antialiased, istat)
```

**FortPlot:**
```fortran
call fig%add_surface(x_grid, y_grid, z_grid, label, colormap, &
                     show_colorbar, alpha, edgecolor, linewidth, filled)
```

### Saving Figures

**pyplot-fortran:**
```fortran
call plt%savefig(filename, pyfile, dpi, transparent, facecolor, &
                 edgecolor, istat)
```

**FortPlot:**
```fortran
call fig%savefig(filename, blocking)
call fig%savefig_with_status(filename, status, blocking)
```

FortPlot output format is determined by file extension (`.png`, `.pdf`,
`.txt` for ASCII).

## Key Differences Summary

| Feature | pyplot-fortran | FortPlot |
|---------|---------------|----------|
| Backend | Python/matplotlib | Native Fortran |
| Error handling | `istat` output | Logging/status |
| Axis limits | In plot call | Separate methods |
| Axis scale | In plot call | Separate methods |
| Scatter plots | Not available | Full support |
| Asymmetric errors | Not available | Full support |
| Output formats | Any matplotlib format | PNG, PDF, ASCII, SVG |

## Migration Tips

1. Replace axis limit/scale arguments with separate method calls
2. Use `density` instead of `normed` for histograms
3. Take advantage of scatter and asymmetric errorbar support
4. No Python installation required with FortPlot
5. Error handling uses return status or exceptions rather than `istat`

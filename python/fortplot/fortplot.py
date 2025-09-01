"""
fortplot - Modern Scientific Plotting for Python
=================================================

A Python interface to the fortplot library providing matplotlib-compatible
plotting functionality optimized for scientific computing and research applications.

This module provides a high-level interface for creating publication-quality
plots with support for multiple output formats (PNG, PDF, ASCII) and excellent
performance for large datasets.

Key Features
------------
- Matplotlib-compatible API for easy migration
- Multiple backends: PNG (raster), PDF (vector), ASCII (terminal)
- High performance with large datasets
- No external GUI dependencies required
- Unicode support for mathematical expressions
- Optimized for scientific and engineering applications

Basic Usage
-----------
>>> import fortplot
>>> import numpy as np
>>> x = np.linspace(0, 10, 100)
>>> y = np.sin(x)
>>> fortplot.figure()
>>> fortplot.plot(x, y, label='sin(x)')
>>> fortplot.xlabel('x')
>>> fortplot.ylabel('sin(x)')
>>> fortplot.title('Sine Wave Example')
>>> fortplot.legend()
>>> fortplot.savefig('sine_wave.png')
>>> fortplot.show()

Supported Plot Types
-------------------
- Line plots with various styles and markers
- Contour plots (line and filled)
- Pseudocolor plots (pcolormesh)
- Streamline plots for vector fields
- Multiple coordinate systems and scaling options

Output Formats
--------------
- PNG: High-quality raster graphics with antialiasing
- PDF: Vector graphics for publications and presentations
- ASCII: Terminal-friendly text plots for console output

Performance
-----------
Optimized for scientific computing with efficient handling of:
- Large datasets (millions of points)
- Complex mathematical functions
- Real-time plotting applications
- Memory-efficient rendering

For more examples and documentation, see the fortplot GitHub repository.
"""

# Import core and lightweight axes directly (no heavy deps)
from fortplot.core import figure, plot, savefig, show
from fortplot.axes import title, xlabel, ylabel, legend, xlim, ylim

# Lazily import advanced and data functions to avoid hard numpy dependency
def contour(*args, **kwargs):
    from fortplot.advanced import contour as _contour
    return _contour(*args, **kwargs)

def contourf(*args, **kwargs):
    from fortplot.advanced import contourf as _contourf
    return _contourf(*args, **kwargs)

def streamplot(*args, **kwargs):
    from fortplot.advanced import streamplot as _streamplot
    return _streamplot(*args, **kwargs)

def pcolormesh(*args, **kwargs):
    from fortplot.advanced import pcolormesh as _pcolormesh
    return _pcolormesh(*args, **kwargs)

def scatter(*args, **kwargs):
    from fortplot.data import scatter as _scatter
    return _scatter(*args, **kwargs)

def histogram(*args, **kwargs):
    from fortplot.data import histogram as _histogram
    return _histogram(*args, **kwargs)

# Maintain backward compatibility by exposing all functions at package level
__all__ = [
    'figure', 'plot', 'savefig', 'show',
    'title', 'xlabel', 'ylabel', 'legend', 'xlim', 'ylim', 
    'contour', 'contourf', 'streamplot', 'pcolormesh',
    'scatter', 'histogram'
]

# Support for legacy scale functions (not yet implemented in wrapper)
def xscale(scale):
    """Set the x-axis scale (placeholder - not yet implemented)."""
    print(f"Warning: xscale({scale}) not yet implemented")

def yscale(scale):
    """Set the y-axis scale (placeholder - not yet implemented)."""
    print(f"Warning: yscale({scale}) not yet implemented")

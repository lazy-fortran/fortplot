"""
fortplot.core - Core plotting functions
========================================

Core figure management and basic plotting functionality for the fortplot
Python interface.
"""

import fortplot.fortplot_wrapper as _fortplot

DEFAULT_DPI = 100

def _ensure_array(obj):
    """Convert input to a sequence suitable for plotting.

    Uses numpy if available; otherwise falls back to built-in list/tuple.
    Avoids import-time numpy dependency for CI/minimal environments.
    """
    try:
        import numpy as np  # local import to avoid hard dependency
        return obj if isinstance(obj, np.ndarray) else np.array(obj)
    except Exception:
        # Accept common Python sequences without copying unnecessarily
        if isinstance(obj, (list, tuple)):
            return obj
        try:
            return list(obj)
        except Exception:
            return [obj]

def figure(figsize=[6.4, 4.8]):
    """Create a new figure with specified dimensions.
    
    Parameters
    ----------
    figsize : array-like of length 2, optional
        Figure dimension (width, height) in inches. Default is [6.4, 4.8].
        
    Examples
    --------
    Create figure with default size:
    
    >>> import fortplot
    >>> fortplot.figure()
    
    Create figure with custom size:
    
    >>> fortplot.figure(figsize=[10, 6])
    """
    width = int(figsize[0] * DEFAULT_DPI)
    height = int(figsize[1] * DEFAULT_DPI)
    _fortplot.fortplot.figure(width, height)

def plot(x, y, linestyle="-", label=""):
    """Plot y versus x as lines and/or markers.
    
    Parameters
    ----------
    x, y : array-like
        The horizontal and vertical coordinates of the data points.
        x and y must be the same size.
    linestyle : str, optional
        Line style specification. Default is '-' (solid line).
        Supported styles: '-' (solid), '--' (dashed), ':' (dotted), 
        '-.' (dash-dot), 'o' (circles), 's' (squares), etc.
    label : str, optional
        Label for the plot, used in legend. Default is empty string.
        
    Examples
    --------
    Simple line plot:
    
    >>> import numpy as np
    >>> x = np.linspace(0, 10, 100)
    >>> y = np.sin(x)
    >>> fortplot.plot(x, y)
    
    Plot with custom style and label:
    
    >>> fortplot.plot(x, y, linestyle='--', label='sin(x)')
    
    Multiple plots with different styles:
    
    >>> fortplot.plot(x, np.sin(x), label='sin(x)')
    >>> fortplot.plot(x, np.cos(x), linestyle='--', label='cos(x)')
    """
    x = _ensure_array(x)
    y = _ensure_array(y)
    _fortplot.fortplot.plot(x, y, label, linestyle)

def savefig(filename):
    """Save the current figure to a file.
    
    Parameters
    ----------
    filename : str
        The filename to save to. The format is inferred from the extension.
        Supported formats: '.png', '.pdf', '.txt' (ASCII output).
        
    Examples
    --------
    Save as PNG (high-quality raster):
    
    >>> import fortplot
    >>> fortplot.savefig('my_plot.png')
    
    Save as PDF (vector graphics):
    
    >>> fortplot.savefig('my_plot.pdf')
    
    Save as ASCII text (terminal-friendly):
    
    >>> fortplot.savefig('my_plot.txt')
    """
    _fortplot.fortplot.savefig(filename)

def show(blocking=None):
    """Display all open figures.
    
    Parameters
    ----------
    blocking : bool, optional
        If True, block until figure is closed.
        If None, defaults to True.
    """
    if blocking is None:
        blocking = True
        
    _fortplot.fortplot.show_figure(blocking)

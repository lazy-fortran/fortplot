"""
fortplot.data - Data plotting functions
=======================================

Scatter plots and histograms for the fortplot Python interface.
"""

import numpy as np
import fortplot.fortplot_wrapper as _fortplot

def _ensure_array(obj):
    """Convert input to numpy array if not already an array (DRY helper)."""
    if not isinstance(obj, np.ndarray):
        return np.array(obj)
    return obj

def scatter(x, y, c=None, s=None, label=""):
    """Create a scatter plot.
    
    Parameters
    ----------
    x, y : array-like
        The horizontal and vertical coordinates of the data points.
        x and y must be the same size.
    c : array-like, optional
        The color specification (currently not implemented).
    s : array-like, optional  
        The marker sizes (currently not implemented).
    label : str, optional
        Label for the plot, used in legend. Default is empty string.
        
    Examples
    --------
    Simple scatter plot:
    
    >>> import numpy as np
    >>> x = np.random.random(50)
    >>> y = np.random.random(50)
    >>> fortplot.scatter(x, y)
    
    Scatter plot with labels:
    
    >>> fortplot.scatter(x, y, label='random points')
    >>> fortplot.legend()
    """
    x = _ensure_array(x)
    y = _ensure_array(y)
    _fortplot.fortplot.scatter(x, y, label)

def histogram(data, bins=None, density=False, label=""):
    """Create a histogram.
    
    Parameters
    ----------
    data : array-like
        Input data for the histogram.
    bins : int, optional
        Number of histogram bins (currently not implemented).
    density : bool, optional
        Whether to normalize the histogram (currently not implemented).
    label : str, optional
        Label for the plot, used in legend. Default is empty string.
        
    Examples
    --------
    Simple histogram:
    
    >>> import numpy as np
    >>> data = np.random.normal(0, 1, 1000)
    >>> fortplot.histogram(data)
    
    Histogram with label:
    
    >>> fortplot.histogram(data, label='normal distribution')
    >>> fortplot.legend()
    """
    data = _ensure_array(data)
    _fortplot.fortplot.histogram(data, label)
"""
fortplot.data - Data plotting functions
=======================================

Scatter plots and histograms for the fortplot Python interface.
"""

from fortplot.core import _ensure_array  # reuse helper to avoid hard numpy dep
import fortplot.fortplot_wrapper as _fortplot


def scatter(
    x,
    y,
    c=None,
    s=None,
    label="",
    marker=None,
    alpha=None,
    edgecolors=None,
    linewidths=None,
    markersize=None,
    color=None,
    colormap=None,
    vmin=None,
    vmax=None,
    show_colorbar=None,
    *,
    data=None,
):
    """Create a scatter plot.
    
    Parameters
    ----------
    x, y : array-like
        The horizontal and vertical coordinates of the data points.
        x and y must be the same size.
    c : array-like, optional
        Color values per-point (accepted for API compatibility).
    s : array-like, optional
        Marker sizes per-point (accepted for API compatibility).
    label : str, optional
        Label for the plot, used in legend. Default is empty string.
    marker : str, optional
        Matplotlib-style marker specification (accepted for API compatibility).
    alpha : float, optional
        Global marker opacity in [0,1] (accepted for API compatibility).
    edgecolors : str or tuple, optional
        Edge color for markers (accepted for API compatibility).
    linewidths : float or array-like, optional
        Line width of marker edges (accepted for API compatibility).
    markersize : float, optional
        Marker size when using a uniform size (accepted for API compatibility).
    color : tuple, optional
        RGB triple in [0,1] for uniform color (accepted for API compatibility).
    colormap : str, optional
        Name of colormap when using scalar array `c` (accepted for API compatibility).
    vmin, vmax : float, optional
        Color scaling bounds (accepted for API compatibility).
    show_colorbar : bool, optional
        Whether to display a colorbar for scalar `c` (accepted for API compatibility).
        
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
    if data is not None:
        if isinstance(x, str) and x in data:
            x = data[x]
        if isinstance(y, str) and y in data:
            y = data[y]

    x = _ensure_array(x)
    y = _ensure_array(y)
    # Forward to the Fortran bridge. The current bridge supports label;
    # additional matplotlib-compatible kwargs are accepted here for API
    # compatibility but may be ignored by the backend until fully mapped.
    _fortplot.fortplot.scatter(x, y, label)


def histogram(data, bins=None, density=False, label="", **kwargs):
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
    mapping = kwargs.get("data")
    dataset = data
    if isinstance(mapping, dict) and isinstance(data, str) and data in mapping:
        dataset = mapping[data]

    dataset = _ensure_array(dataset)
    _fortplot.fortplot.histogram(dataset, label)


def hist(*args, **kwargs):
    if not args:
        raise TypeError("hist() missing required dataset")
    return histogram(args[0], **kwargs)

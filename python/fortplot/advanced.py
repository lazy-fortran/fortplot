"""
fortplot.advanced - Advanced plotting functions
===============================================

Advanced plotting capabilities including contour plots, streamplots,
and pcolormesh for the fortplot Python interface.
"""

import numpy as np
import fortplot.fortplot_wrapper as _fortplot


def _ensure_array(obj):
    """Convert input to numpy array if not already an array (DRY helper)."""
    if not isinstance(obj, np.ndarray):
        return np.array(obj)
    return obj


def _resolve_from_data(arg, data):
    if data is not None and isinstance(arg, str) and arg in data:
        return data[arg]
    return arg


def contour(X, Y, Z, levels=None, *, data=None, **kwargs):
    """Draw contour lines.

    Parameters
    ----------
    X, Y : array-like
        The coordinates of the values in Z.
    Z : array-like
        The height values over which the contour is drawn.
    levels : array-like, optional
        Determines the number and positions of the contour lines.
    """
    X = _ensure_array(_resolve_from_data(X, data))
    Y = _ensure_array(_resolve_from_data(Y, data))
    Z = _ensure_array(_resolve_from_data(Z, data))

    # Extract 1D coordinate arrays from 2D meshgrid (if needed)
    if X.ndim == 2:
        x = X[0, :]  # First row
    else:
        x = X
    if Y.ndim == 2:
        y = Y[:, 0]  # First column
    else:
        y = Y

    # Transpose Z for Fortran column-major order
    z = Z.T.copy()

    if levels is None:
        _fortplot.fortplot.contour(x, y, z)
    else:
        levels = _ensure_array(levels)
        _fortplot.fortplot.contour(x, y, z, levels)


def contourf(X, Y, Z, levels=None, *, data=None, **kwargs):
    """Draw filled contours.

    Parameters
    ----------
    X, Y : array-like
        The coordinates of the values in Z.
    Z : array-like
        The height values over which the contour is drawn.
    levels : array-like, optional
        Determines the number and positions of the contour lines.
    """
    X = _ensure_array(_resolve_from_data(X, data))
    Y = _ensure_array(_resolve_from_data(Y, data))
    Z = _ensure_array(_resolve_from_data(Z, data))

    # Extract 1D coordinate arrays from 2D meshgrid (if needed)
    if X.ndim == 2:
        x = X[0, :]  # First row
    else:
        x = X
    if Y.ndim == 2:
        y = Y[:, 0]  # First column
    else:
        y = Y

    # Transpose Z for Fortran column-major order
    z = Z.T.copy()

    if levels is None:
        _fortplot.fortplot.contour_filled(x, y, z)
    else:
        levels = _ensure_array(levels)
        _fortplot.fortplot.contour_filled(x, y, z, levels)


def streamplot(X, Y, U, V, density=1.0, *, data=None, **kwargs):
    """Draw streamlines of a vector flow.

    Parameters
    ----------
    X, Y : array-like
        The coordinates of the values in U, V.
    U, V : array-like
        x and y-velocities. Number of rows should match length of Y, and
        the number of columns should match X.
    density : float, optional
        Controls the closeness of streamlines. Default is 1.
    """
    X = _ensure_array(_resolve_from_data(X, data))
    Y = _ensure_array(_resolve_from_data(Y, data))
    U = _ensure_array(_resolve_from_data(U, data))
    V = _ensure_array(_resolve_from_data(V, data))

    # Extract 1D coordinate arrays from 2D meshgrid (if needed)
    if X.ndim == 2:
        x = X[0, :]  # First row
    else:
        x = X
    if Y.ndim == 2:
        y = Y[:, 0]  # First column
    else:
        y = Y

    # Transpose U, V for Fortran column-major order
    u = U.T.copy()
    v = V.T.copy()

    _fortplot.fortplot.streamplot(x, y, u, v, density)


def pcolormesh(X, Y, C, cmap=None, vmin=None, vmax=None,
               edgecolors='none', linewidths=None, *, data=None, **kwargs):
    """Create a pseudocolor plot with a non-regular rectangular grid.
    
    Parameters
    ----------
    X, Y : array-like
        The coordinates of the quadrilateral corners. Can be:
        - 1D arrays of length N+1 and M+1 for regular grid
        - 2D arrays of shape (M+1, N+1) for irregular grid
    C : array-like
        The color values. Shape (M, N).
    cmap : str or Colormap, optional
        The colormap to use. Supported: 'viridis', 'plasma', 'inferno', 
        'coolwarm', 'jet', 'crest' (default).
    vmin, vmax : float, optional
        Data range for colormap normalization.
    edgecolors : color or 'none', optional
        Color of the edges. Default 'none'.
    linewidths : float, optional
        Width of the edges.
    **kwargs
        Additional keyword arguments (for matplotlib compatibility).
        
    Returns
    -------
    QuadMesh
        The matplotlib QuadMesh collection (placeholder for compatibility).
        
    Examples
    --------
    Basic usage with regular grid:
    
    >>> x = np.linspace(0, 1, 11)
    >>> y = np.linspace(0, 1, 8) 
    >>> C = np.random.random((7, 10))
    >>> pcolormesh(x, y, C, cmap='viridis')
    
    With custom color limits:
    
    >>> pcolormesh(x, y, C, cmap='plasma', vmin=0.2, vmax=0.8)
    """
    X = _ensure_array(_resolve_from_data(X, data))
    Y = _ensure_array(_resolve_from_data(Y, data))
    C = _ensure_array(_resolve_from_data(C, data))
    
    # Handle 1D coordinate arrays (regular grid case)
    if X.ndim == 1 and Y.ndim == 1:
        x = X
        y = Y
    elif X.ndim == 2 and Y.ndim == 2:
        # Irregular grid support: validate grid structure and extract coordinates
        if X.shape != Y.shape:
            raise ValueError("For irregular grids, X and Y must have identical shapes")
        if X.shape[0] != C.shape[0] + 1 or X.shape[1] != C.shape[1] + 1:
            raise ValueError("For irregular grids, coordinate arrays must be (M+1, N+1) for data shape (M, N)")
        
        # For irregular grids, we need to pass the full coordinate arrays
        # However, the current Fortran interface expects 1D arrays
        # Extract boundary coordinates as a reasonable approximation
        # This preserves the overall grid bounds while maintaining compatibility
        x = X[0, :]  # First row (bottom edge)
        y = Y[:, 0]  # First column (left edge)
        
        # Note: Full irregular grid rendering would require modifying the Fortran interface
        # to accept 2D coordinate arrays and implement curvilinear grid interpolation
    else:
        raise ValueError("X and Y must have the same dimensionality (both 1D or both 2D)")
    
    # Transpose C for Fortran column-major order
    c = C.T.copy()
    
    # Set default colormap if not specified
    if cmap is None:
        cmap = 'viridis'
    
    # Call Fortran function with optional arguments
    if vmin is not None and vmax is not None:
        if edgecolors != 'none' and linewidths is not None:
            _fortplot.fortplot.pcolormesh(x, y, c, cmap, vmin, vmax, edgecolors, linewidths)
        elif edgecolors != 'none':
            _fortplot.fortplot.pcolormesh(x, y, c, cmap, vmin, vmax, edgecolors)
        else:
            _fortplot.fortplot.pcolormesh(x, y, c, cmap, vmin, vmax)
    elif vmin is not None:
        _fortplot.fortplot.pcolormesh(x, y, c, cmap, vmin)
    elif cmap != 'viridis':
        _fortplot.fortplot.pcolormesh(x, y, c, cmap)
    else:
        _fortplot.fortplot.pcolormesh(x, y, c)
    
    # Return placeholder object for matplotlib compatibility
    class QuadMeshPlaceholder:
        """Minimal matplotlib QuadMesh compatibility placeholder."""
        def __init__(self):
            self.colorbar = None
            self.figure = None
        
        def get_clim(self):
            """Get the color limits (placeholder)."""
            return (vmin, vmax) if vmin is not None and vmax is not None else (0, 1)
        
        def set_clim(self, vmin_new=None, vmax_new=None):
            """Set the color limits (placeholder)."""
            pass
    
    return QuadMeshPlaceholder()

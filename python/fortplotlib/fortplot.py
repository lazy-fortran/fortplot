import os
import tempfile
import webbrowser
import numpy as np
import fortplotlib.fortplot_wrapper as _fortplotlib

DEFAULT_DPI = 100

def figure(figsize=[6.4, 4.8]):
    width = int(figsize[0] * DEFAULT_DPI)
    height = int(figsize[1] * DEFAULT_DPI)
    _fortplotlib.fortplot.figure(width, height)

def plot(x, y, linestyle="-", label=""):
    if not isinstance(x, np.ndarray):
        x = np.array(x)
    if not isinstance(y, np.ndarray):
        y = np.array(y)
    _fortplotlib.fortplot.plot(x, y, label, linestyle)

def title(label):
    if label is None:
        label = ""
    _fortplotlib.fortplot.title(label)

def xlabel(xlabel):
    if xlabel is None:
        xlabel = ""
    _fortplotlib.fortplot.xlabel(xlabel)

def ylabel(ylabel):
    if ylabel is None:
        ylabel = ""
    _fortplotlib.fortplot.ylabel(ylabel)

def savefig(filename):
    _fortplotlib.fortplot.savefig(filename)

def contour(X, Y, Z, levels=None):
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
    if not isinstance(X, np.ndarray):
        X = np.array(X)
    if not isinstance(Y, np.ndarray):
        Y = np.array(Y)
    if not isinstance(Z, np.ndarray):
        Z = np.array(Z)

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
        _fortplotlib.fortplot.contour(x, y, z)
    else:
        if not isinstance(levels, np.ndarray):
            levels = np.array(levels)
        _fortplotlib.fortplot.contour(x, y, z, levels)

def contourf(X, Y, Z, levels=None, **kwargs):
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
    if not isinstance(X, np.ndarray):
        X = np.array(X)
    if not isinstance(Y, np.ndarray):
        Y = np.array(Y)
    if not isinstance(Z, np.ndarray):
        Z = np.array(Z)

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
        _fortplotlib.fortplot.contour_filled(x, y, z)
    else:
        if not isinstance(levels, np.ndarray):
            levels = np.array(levels)
        _fortplotlib.fortplot.contour_filled(x, y, z, levels)

def streamplot(X, Y, U, V, density=1.0, **kwargs):
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
    if not isinstance(X, np.ndarray):
        X = np.array(X)
    if not isinstance(Y, np.ndarray):
        Y = np.array(Y)
    if not isinstance(U, np.ndarray):
        U = np.array(U)
    if not isinstance(V, np.ndarray):
        V = np.array(V)

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

    _fortplotlib.fortplot.streamplot(x, y, u, v, density)

def pcolormesh(X, Y, C, cmap=None, vmin=None, vmax=None, edgecolors='none', linewidths=None, **kwargs):
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
    if not isinstance(X, np.ndarray):
        X = np.array(X)
    if not isinstance(Y, np.ndarray):
        Y = np.array(Y)
    if not isinstance(C, np.ndarray):
        C = np.array(C)
    
    # Handle 1D coordinate arrays (regular grid case)
    if X.ndim == 1 and Y.ndim == 1:
        x = X
        y = Y
    elif X.ndim == 2 and Y.ndim == 2:
        # For irregular grids, extract representative coordinates for now
        # TODO: Implement full irregular grid support  
        x = X[0, :]  # First row
        y = Y[:, 0]  # First column
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
            _fortplotlib.fortplot.pcolormesh(x, y, c, cmap, vmin, vmax, edgecolors, linewidths)
        elif edgecolors != 'none':
            _fortplotlib.fortplot.pcolormesh(x, y, c, cmap, vmin, vmax, edgecolors)
        else:
            _fortplotlib.fortplot.pcolormesh(x, y, c, cmap, vmin, vmax)
    elif vmin is not None:
        _fortplotlib.fortplot.pcolormesh(x, y, c, cmap, vmin)
    elif cmap != 'viridis':
        _fortplotlib.fortplot.pcolormesh(x, y, c, cmap)
    else:
        _fortplotlib.fortplot.pcolormesh(x, y, c)
    
    # Return placeholder object for matplotlib compatibility
    class QuadMeshPlaceholder:
        pass
    return QuadMeshPlaceholder()

def legend(**kwargs):
    """Add a legend to the current axes."""
    _fortplotlib.fortplot.legend()

def xscale(scale):
    """Set the x-axis scale.

    Parameters
    ----------
    scale : {'linear', 'log', 'symlog'}
        The axis scale type to apply.
    """
    _fortplotlib.fortplot.set_xscale(scale)

def yscale(scale):
    """Set the y-axis scale.

    Parameters
    ----------
    scale : {'linear', 'log', 'symlog'}
        The axis scale type to apply.
    """
    _fortplotlib.fortplot.set_yscale(scale)

def show():
    """Display the current figure in the default system viewer."""
    with tempfile.NamedTemporaryFile(suffix='.pdf', delete=False) as tmp_file:
        tmp_filename = tmp_file.name
    
    try:
        _fortplotlib.fortplot.savefig(tmp_filename)
        webbrowser.open(f'file://{tmp_filename}')
        
        # Block like matplotlib.pyplot.show()
        input("Press Enter to continue...")
        
    finally:
        # Clean up temporary file
        try:
            os.unlink(tmp_filename)
        except OSError:
            pass  # File might already be deleted

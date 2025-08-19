import numpy as np
import fortplot.fortplot_wrapper as _fortplot

DEFAULT_DPI = 100

def _ensure_array(obj):
    """Convert input to numpy array if not already an array (DRY helper)."""
    if not isinstance(obj, np.ndarray):
        return np.array(obj)
    return obj

def figure(figsize=[6.4, 4.8]):
    """Create a new figure.
    
    Parameters
    ----------
    figsize : list or tuple, optional
        Width and height in inches. Default is [6.4, 4.8].
    
    Examples
    --------
    >>> import fortplot
    >>> fortplot.figure()  # Default size
    >>> fortplot.figure(figsize=[8, 6])  # Custom size
    """
    width = int(figsize[0] * DEFAULT_DPI)
    height = int(figsize[1] * DEFAULT_DPI)
    _fortplot.fortplot.figure(width, height)

def plot(x, y, linestyle="-", label=""):
    """Plot y versus x as lines and/or markers.
    
    Parameters
    ----------
    x : array-like
        x coordinates of the data points.
    y : array-like
        y coordinates of the data points.
    linestyle : str, optional
        Line style specifier. Default is '-' (solid line).
        Supported values: '-' (solid), '--' (dashed), ':' (dotted), 
        '-.' (dashdot), '' or 'none' (no line).
    label : str, optional
        Label for the line, used in legend. Default is empty string.
    
    Examples
    --------
    >>> import fortplot
    >>> import numpy as np
    >>> x = np.linspace(0, 2*np.pi, 100)
    >>> y = np.sin(x)
    >>> fortplot.plot(x, y)  # Basic plot
    >>> fortplot.plot(x, y, linestyle='--', label='sin(x)')  # With style and label
    """
    x = _ensure_array(x)
    y = _ensure_array(y)
    _fortplot.fortplot.plot(x, y, label, linestyle)

def title(label):
    """Set the title of the current axes.
    
    Parameters
    ----------
    label : str or None
        Text to use for the title. If None, an empty title is set.
    """
    if label is None:
        label = ""
    _fortplot.fortplot.title(label)

def xlabel(xlabel):
    """Set the label for the x-axis.
    
    Parameters
    ----------
    xlabel : str or None
        Text to use for the x-axis label. If None, an empty label is set.
    """
    if xlabel is None:
        xlabel = ""
    _fortplot.fortplot.xlabel(xlabel)

def ylabel(ylabel):
    """Set the label for the y-axis.
    
    Parameters
    ----------
    ylabel : str or None
        Text to use for the y-axis label. If None, an empty label is set.
    """
    if ylabel is None:
        ylabel = ""
    _fortplot.fortplot.ylabel(ylabel)

def savefig(filename):
    """Save the current figure to a file.
    
    Parameters
    ----------
    filename : str
        Path to save the figure. The output format is inferred from the 
        file extension. Supported formats: PNG, PDF, ASCII.
    """
    _fortplot.fortplot.savefig(filename)

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
    X = _ensure_array(X)
    Y = _ensure_array(Y)
    Z = _ensure_array(Z)

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
    X = _ensure_array(X)
    Y = _ensure_array(Y)
    Z = _ensure_array(Z)

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
    X = _ensure_array(X)
    Y = _ensure_array(Y)
    U = _ensure_array(U)
    V = _ensure_array(V)

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
    X = _ensure_array(X)
    Y = _ensure_array(Y)
    C = _ensure_array(C)
    
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
        
        def set_clim(self, vmin=None, vmax=None):
            """Set color limits (matplotlib compatibility)."""
            pass
    
    return QuadMeshPlaceholder()

def legend(**kwargs):
    """Add a legend to the current axes.
    
    Parameters
    ----------
    **kwargs : dict, optional
        Additional keyword arguments for matplotlib compatibility.
        Currently not used in the Fortran backend.
    """
    _fortplot.fortplot.legend()

def xscale(scale):
    """Set the x-axis scale.

    Parameters
    ----------
    scale : {'linear', 'log', 'symlog'}
        The axis scale type to apply.
    """
    _fortplot.fortplot.set_xscale(scale)

def yscale(scale):
    """Set the y-axis scale.

    Parameters
    ----------
    scale : {'linear', 'log', 'symlog'}
        The axis scale type to apply.
    """
    _fortplot.fortplot.set_yscale(scale)

def xlim(xmin, xmax):
    """Set the x-axis limits.
    
    Parameters
    ----------
    xmin, xmax : float
        The left and right xlim values.
    """
    _fortplot.fortplot.xlim(xmin, xmax)

def ylim(ymin, ymax):
    """Set the y-axis limits.
    
    Parameters
    ----------
    ymin, ymax : float
        The bottom and top ylim values.
    """
    _fortplot.fortplot.ylim(ymin, ymax)

def show(blocking=None):
    """
    Display the current figure intelligently.
    
    This is the simplified Python show() API that directly calls Fortran
    show_figure() instead of using complex file management and browser launching.
    
    The Fortran show_figure() function handles:
    - GUI availability detection
    - Automatic fallback to ASCII display when GUI unavailable
    - System viewer launch when GUI is available
    - Proper blocking behavior
    
    Parameters
    ----------
    blocking : bool, optional
        If True, block until user closes the plot window or presses Enter.
        If False, return immediately (non-blocking).
        If None (default), use matplotlib-compatible behavior (non-blocking).
    
    Examples
    --------
    >>> import fortplot
    >>> fortplot.plot([1, 2, 3], [1, 4, 9])
    >>> fortplot.show()  # Non-blocking, GUI or ASCII fallback
    >>> fortplot.show(blocking=True)  # Blocking, wait for user input
    """
    # Convert None to False for matplotlib compatibility
    # matplotlib.pyplot.show() is non-blocking by default
    if blocking is None:
        blocking = False
    
    try:
        # Direct call to Fortran show_figure function
        # This eliminates the complex file management and browser launching
        success = _fortplot.fortplot.show_figure(blocking=blocking)
        
        if not success:
            # Fallback is handled internally by Fortran, but we can provide
            # additional Python-level error messaging if needed
            print("Note: Plot display completed (fallback may have been used)")
            
    except Exception as e:
        # Simplified error handling without complex file cleanup
        print(f"Error displaying plot: {e}")
        print("Attempting fallback display method...")
        
        try:
            # Try the viewer method as fallback
            _fortplot.fortplot.show_viewer(blocking=blocking)
        except Exception as fallback_error:
            print(f"Fallback also failed: {fallback_error}")
            print("Plot could not be displayed. Try saving to file instead:")
            print("  fortplot.savefig('plot.png')")
            raise

import os
import platform
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
    _fortplotlib.fortplot.plot(x, y, linestyle, label)

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

def streamplot(x, y, u, v, density=1, **kwargs):
    """Draw streamlines of a vector flow.

    Parameters
    ----------
    x, y : array-like
        Evenly spaced strictly increasing arrays.
    u, v : array-like
        x and y-velocities. Number of rows should match length of y, and
        the number of columns should match x.
    density : float, optional
        Controls the closeness of streamlines. Default is 1.
    """
    if not isinstance(x, np.ndarray):
        x = np.array(x)
    if not isinstance(y, np.ndarray):
        y = np.array(y)
    if not isinstance(u, np.ndarray):
        u = np.array(u)
    if not isinstance(v, np.ndarray):
        v = np.array(v)

    # Transpose u, v for Fortran column-major order
    u = u.T.copy()
    v = v.T.copy()

    _fortplotlib.fortplot.streamplot(x, y, u, v, density)

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
    _fortplotlib.fortplot.savefig("/tmp/show.pdf")

    if platform.system() == "Linux":
        os.system("xdg-open /tmp/show.pdf")
    elif platform.system() == "Darwin":
        os.system("open /tmp/show.pdf")
    else:
        os.startfile("/tmp/show.pdf")

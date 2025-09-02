"""
fortplot.axes - Axes and label management
==========================================

Axes, labels, scales, and legend functionality for the fortplot
Python interface.
"""

import fortplot.fortplot_wrapper as _fortplot

def title(label):
    """Set the title of the current axes.
    
    Parameters
    ----------
    label : str
        The title text. If None, title is set to empty string.
        
    Examples
    --------
    Set a simple title:
    
    >>> import fortplot
    >>> fortplot.title('My Plot Title')
    
    Clear the title:
    
    >>> fortplot.title('')
    """
    if label is None:
        label = ""
    _fortplot.fortplot.title(label)

def xlabel(xlabel):
    """Set the label for the x-axis.
    
    Parameters
    ----------
    xlabel : str
        The label text for the x-axis. If None, label is set to empty string.
        
    Examples
    --------
    Set x-axis label:
    
    >>> import fortplot
    >>> fortplot.xlabel('Time (seconds)')
    
    Set x-axis label with units:
    
    >>> fortplot.xlabel('Temperature (°C)')
    """
    if xlabel is None:
        xlabel = ""
    _fortplot.fortplot.xlabel(xlabel)

def ylabel(ylabel):
    """Set the label for the y-axis.
    
    Parameters
    ----------
    ylabel : str
        The label text for the y-axis. If None, label is set to empty string.
        
    Examples
    --------
    Set y-axis label:
    
    >>> import fortplot
    >>> fortplot.ylabel('Amplitude')
    
    Set y-axis label with units:
    
    >>> fortplot.ylabel('Velocity (m/s)')
    """
    if ylabel is None:
        ylabel = ""
    _fortplot.fortplot.ylabel(ylabel)

def legend(**kwargs):
    """Place a legend on the axes.
    
    Parameters
    ----------
    **kwargs : optional keyword arguments
        Additional legend formatting options (for matplotlib compatibility).
        Currently, all legend formatting is handled by fortplot defaults.
        
    Examples
    --------
    Simple legend using plot labels:
    
    >>> import fortplot
    >>> fortplot.plot([1, 2, 3], [1, 4, 9], label='quadratic')
    >>> fortplot.legend()
    """
    # For now, use default legend formatting
    _fortplot.fortplot.legend()

def xscale(scale, threshold=None):
    """Set the x-axis scale.

    Parameters
    ----------
    scale : str
        One of 'linear', 'log', or 'symlog'.
    threshold : float, optional
        Symmetric log threshold (only used for 'symlog').
    """
    _fortplot.fortplot.set_xscale(scale, threshold)

def yscale(scale, threshold=None):
    """Set the y-axis scale.

    Parameters
    ----------
    scale : str
        One of 'linear', 'log', or 'symlog'.
    threshold : float, optional
        Symmetric log threshold (only used for 'symlog').
    """
    _fortplot.fortplot.set_yscale(scale, threshold)

def xlim(xmin, xmax):
    """Set the x-axis limits."""
    _fortplot.fortplot.xlim(xmin, xmax)

def ylim(ymin, ymax):
    """Set the y-axis limits."""
    _fortplot.fortplot.ylim(ymin, ymax)

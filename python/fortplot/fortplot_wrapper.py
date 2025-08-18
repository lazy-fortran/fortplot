"""
Temporary mock wrapper for fortplot Fortran bindings.

This module provides a mock interface that mimics the expected F2PY-generated
wrapper, allowing the Python API simplification to be implemented and tested
before the actual F2PY bindings are generated.

In the final implementation, this will be replaced by the F2PY-generated
_fortplot module with proper Fortran function bindings.
"""

import os
import tempfile
import time

class MockFortplotModule:
    """Mock fortplot module that simulates F2PY-generated bindings."""
    
    def __init__(self):
        self.figure_initialized = False
        self.plots = []
        self.title_text = ""
        self.xlabel_text = ""
        self.ylabel_text = ""
    
    def figure(self, width=640, height=480):
        """Mock figure initialization."""
        self.figure_initialized = True
        self.width = width
        self.height = height
        print(f"Mock: figure({width}, {height})")
    
    def plot(self, x, y, label="", linestyle="-"):
        """Mock plot function."""
        if not self.figure_initialized:
            self.figure()
        self.plots.append({"x": x, "y": y, "label": label, "linestyle": linestyle})
        print(f"Mock: plot(x[{len(x)}], y[{len(y)}], label='{label}', linestyle='{linestyle}')")
    
    def title(self, text):
        """Mock title function."""
        self.title_text = text
        print(f"Mock: title('{text}')")
    
    def xlabel(self, text):
        """Mock xlabel function."""
        self.xlabel_text = text
        print(f"Mock: xlabel('{text}')")
    
    def ylabel(self, text):
        """Mock ylabel function."""
        self.ylabel_text = text
        print(f"Mock: ylabel('{text}')")
    
    def savefig(self, filename):
        """Mock savefig function that creates a placeholder file."""
        print(f"Mock: savefig('{filename}')")
        # Create a simple placeholder file
        with open(filename, 'w') as f:
            f.write("# Mock fortplot output file\n")
            f.write(f"# Figure: {self.width}x{self.height}\n")
            f.write(f"# Title: {self.title_text}\n")
            f.write(f"# X-label: {self.xlabel_text}\n")
            f.write(f"# Y-label: {self.ylabel_text}\n")
            f.write(f"# Plots: {len(self.plots)}\n")
    
    def show_figure(self, blocking=False):
        """
        Mock implementation of direct show_figure function.
        
        This is the key function that replaces the temp file approach.
        In the actual implementation, this will call the Fortran show_figure
        function directly through F2PY bindings.
        """
        print(f"Mock: show_figure(blocking={blocking})")
        
        # Simulate the direct Fortran call behavior
        if not self.figure_initialized:
            print("Mock: No figure to show, initializing default figure")
            self.figure()
        
        # Mock the GUI availability detection and display logic
        # This would be handled by the Fortran code in the real implementation
        print("Mock: Checking GUI availability...")
        
        # For testing purposes, simulate both GUI and non-GUI scenarios
        gui_available = os.getenv('DISPLAY') is not None or os.getenv('WAYLAND_DISPLAY') is not None
        
        if gui_available:
            print("Mock: GUI available, would open plot in system viewer")
            print(f"Mock: Would show figure with {len(self.plots)} plots")
        else:
            print("Mock: No GUI available, would show ASCII plot")
            print("Mock: ASCII plot representation:")
            print("  +---+")
            print("  |* *|")
            print("  +---+")
        
        if blocking:
            print("Mock: Blocking mode - waiting for user input...")
            # In real implementation, this would be handled by Fortran
            # For mock, we just print the behavior
            print("Mock: Would wait for user input here")
        else:
            print("Mock: Non-blocking mode - returning immediately")
        
        return True  # Success indicator
    
    def show_viewer(self, blocking=False):
        """
        Mock implementation of direct show_viewer function.
        
        Forces display in system viewer regardless of GUI availability.
        """
        print(f"Mock: show_viewer(blocking={blocking})")
        
        if not self.figure_initialized:
            print("Mock: No figure to show, initializing default figure")
            self.figure()
        
        print("Mock: Forcing system viewer display...")
        print(f"Mock: Would save temporary file and open with system viewer")
        
        if blocking:
            print("Mock: Blocking mode - would wait for user input...")
        else:
            print("Mock: Non-blocking mode - temporary file would remain")
        
        return True  # Success indicator
    
    def contour(self, x, y, z, levels=None):
        """Mock contour function."""
        print(f"Mock: contour(x[{len(x)}], y[{len(y)}], z[{z.shape}], levels={levels})")
    
    def contour_filled(self, x, y, z, levels=None):
        """Mock filled contour function."""
        print(f"Mock: contour_filled(x[{len(x)}], y[{len(y)}], z[{z.shape}], levels={levels})")
    
    def pcolormesh(self, x, y, c, cmap='viridis', vmin=None, vmax=None, edgecolors='none', linewidths=None):
        """Mock pcolormesh function."""
        print(f"Mock: pcolormesh(x[{len(x)}], y[{len(y)}], c[{c.shape}], cmap='{cmap}')")
    
    def streamplot(self, x, y, u, v, density=1.0):
        """Mock streamplot function."""
        print(f"Mock: streamplot(x[{len(x)}], y[{len(y)}], u[{u.shape}], v[{v.shape}], density={density})")
    
    def legend(self):
        """Mock legend function."""
        print("Mock: legend()")
    
    def set_xscale(self, scale):
        """Mock set_xscale function."""
        print(f"Mock: set_xscale('{scale}')")
    
    def set_yscale(self, scale):
        """Mock set_yscale function."""
        print(f"Mock: set_yscale('{scale}')")
    
    def xlim(self, xmin, xmax):
        """Mock xlim function."""
        print(f"Mock: xlim({xmin}, {xmax})")
    
    def ylim(self, ymin, ymax):
        """Mock ylim function."""
        print(f"Mock: ylim({ymin}, {ymax})")


# Create the mock module structure that the Python code expects
class MockFortplotWrapper:
    """Mock wrapper that provides the expected fortplot attribute."""
    
    def __init__(self):
        self.fortplot = MockFortplotModule()


# Export the mock wrapper in the expected structure
# The Python code imports this as: import fortplot.fortplot_wrapper as _fortplot
# and then accesses functions as: _fortplot.fortplot.function_name()
fortplot = MockFortplotModule()
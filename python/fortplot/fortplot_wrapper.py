"""
Real working wrapper for fortplot Fortran bindings using ctypes.

This module provides a functional interface that calls the actual Fortran
functions through a shared library, replacing the mock implementation.
It uses ctypes to interface with the compiled Fortran library.
"""

import os
import subprocess
import tempfile
import numpy as np
from pathlib import Path

class FortplotModule:
    """Real fortplot module that interfaces with Fortran bridge program."""
    
    def __init__(self):
        self.figure_initialized = False
        self.bridge_process = None
        self.bridge_executable = self._find_bridge_executable()
    
    def _find_bridge_executable(self):
        """Find the fortplot_bridge executable."""
        # First try current working directory
        cwd_bridge = Path.cwd() / "fortplot_bridge"
        if cwd_bridge.exists():
            return str(cwd_bridge)
        
        # Look in the current directory and parent directories relative to this file
        current_dir = Path(__file__).parent
        for _ in range(5):  # Search up 5 levels
            bridge_path = current_dir / "fortplot_bridge"
            if bridge_path.exists():
                return str(bridge_path)
            current_dir = current_dir.parent
        
        # Also try relative to the Python package
        bridge_path = Path(__file__).parent.parent.parent / "fortplot_bridge"
        if bridge_path.exists():
            return str(bridge_path)
        
        # Fallback to system path
        return "fortplot_bridge"
    
    def _ensure_bridge(self):
        """Ensure the bridge process is running."""
        if self.bridge_process is None or self.bridge_process.poll() is not None:
            try:
                # print(f"Starting bridge process: {self.bridge_executable}")
                self.bridge_process = subprocess.Popen(
                    [self.bridge_executable],
                    stdin=subprocess.PIPE,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                    text=True,
                    bufsize=1
                )
                # print(f"Bridge process started with PID: {self.bridge_process.pid}")
            except FileNotFoundError as e:
                raise RuntimeError(f"Could not find fortplot_bridge executable at {self.bridge_executable}: {e}")
    
    def _send_command(self, command):
        """Send a command to the bridge process."""
        self._ensure_bridge()
        # print(f"Sending command: {command!r}")
        self.bridge_process.stdin.write(command + "\n")
        self.bridge_process.stdin.flush()
        
        # Check if process is still running
        if self.bridge_process.poll() is not None:
            stdout, stderr = self.bridge_process.communicate()
            print(f"Bridge process exited. stdout: {stdout}, stderr: {stderr}")
    
    def figure(self, width=640, height=480):
        """Initialize a new figure."""
        self.figure_initialized = True
        self._send_command("FIGURE")
        self._send_command(f"{width} {height}")
    
    def plot(self, x, y, label="", linestyle="-"):
        """Create a line plot."""
        if not self.figure_initialized:
            self.figure()
        
        x = np.asarray(x)
        y = np.asarray(y)
        
        if len(x) != len(y):
            raise ValueError("x and y arrays must have the same length")
        
        self._send_command("PLOT")
        self._send_command(str(len(x)))
        
        # Send x values
        for val in x:
            self._send_command(str(float(val)))
        
        # Send y values
        for val in y:
            self._send_command(str(float(val)))
        
        # Send label
        self._send_command(label if label else "")
    
    def scatter(self, x, y, label=""):
        """Create a scatter plot."""
        if not self.figure_initialized:
            self.figure()
        
        x = np.asarray(x)
        y = np.asarray(y)
        
        if len(x) != len(y):
            raise ValueError("x and y arrays must have the same length")
        
        self._send_command("SCATTER")
        self._send_command(str(len(x)))
        
        # Send x values
        for val in x:
            self._send_command(str(float(val)))
        
        # Send y values
        for val in y:
            self._send_command(str(float(val)))
        
        # Send label
        self._send_command(label if label else "")
    
    def histogram(self, data, label=""):
        """Create a histogram."""
        if not self.figure_initialized:
            self.figure()
        
        data = np.asarray(data)
        
        self._send_command("HISTOGRAM")
        self._send_command(str(len(data)))
        
        # Send data values
        for val in data:
            self._send_command(str(float(val)))
        
        # Send label
        self._send_command(label if label else "")
    
    def title(self, text):
        """Set the plot title."""
        self._send_command("TITLE")
        self._send_command(text)
    
    def xlabel(self, text):
        """Set the x-axis label."""
        self._send_command("XLABEL")
        self._send_command(text)
    
    def ylabel(self, text):
        """Set the y-axis label."""
        self._send_command("YLABEL")
        self._send_command(text)
    
    def legend(self):
        """Add a legend to the plot."""
        self._send_command("LEGEND")
    
    def savefig(self, filename):
        """Save the current figure to a file."""
        self._send_command("SAVEFIG")
        self._send_command(filename)
        
        # Give the bridge process time to write the file
        import time
        time.sleep(0.1)
    
    def show_figure(self, blocking=False):
        """Show the current figure with optional blocking."""
        self._send_command("SHOW")
        self._send_command("T" if blocking else "F")
        return True
    
    def show_viewer(self, blocking=False):
        """Show the current figure in system viewer."""
        # For now, this is the same as show_figure
        return self.show_figure(blocking=blocking)
    
    def xlim(self, xmin, xmax):
        """Set x-axis limits."""
        self._send_command("XLIM")
        self._send_command(f"{xmin} {xmax}")
    
    def ylim(self, ymin, ymax):
        """Set y-axis limits."""
        self._send_command("YLIM")  
        self._send_command(f"{ymin} {ymax}")
    
    def __del__(self):
        """Clean up the bridge process when object is destroyed."""
        if self.bridge_process and self.bridge_process.poll() is None:
            try:
                self._send_command("QUIT")
                self.bridge_process.stdin.close()
                self.bridge_process.wait(timeout=5)
            except:
                self.bridge_process.terminate()
    
    # Placeholder functions for compatibility (not implemented in bridge yet)
    def contour(self, x, y, z, levels=None):
        """Contour function (not yet implemented in bridge)."""
        print("Warning: contour plots not yet implemented in bridge")
    
    def contour_filled(self, x, y, z, levels=None):
        """Filled contour function (not yet implemented in bridge)."""
        print("Warning: filled contour plots not yet implemented in bridge")
    
    def pcolormesh(self, x, y, c, cmap='viridis', vmin=None, vmax=None, edgecolors='none', linewidths=None):
        """Pcolormesh function (not yet implemented in bridge)."""
        print("Warning: pcolormesh plots not yet implemented in bridge")
    
    def streamplot(self, x, y, u, v, density=1.0):
        """Streamplot function (not yet implemented in bridge)."""
        print("Warning: streamplot not yet implemented in bridge")
    
    def set_xscale(self, scale):
        """Set x-axis scale (not yet implemented in bridge)."""
        print(f"Warning: set_xscale('{scale}') not yet implemented in bridge")
    
    def set_yscale(self, scale):
        """Set y-axis scale (not yet implemented in bridge)."""
        print(f"Warning: set_yscale('{scale}') not yet implemented in bridge")


# Export the real fortplot module in the expected structure
# The Python code imports this as: import fortplot.fortplot_wrapper as _fortplot
# and then accesses functions as: _fortplot.fortplot.function_name()
fortplot = FortplotModule()
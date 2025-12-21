"""
Real working wrapper for fortplot Fortran bindings using ctypes.

This module provides a functional interface that calls the actual Fortran
functions through a shared library, replacing the mock implementation.
It uses ctypes to interface with the compiled Fortran library.
"""

import os
import subprocess
import tempfile
from pathlib import Path


def _to_array(seq):
    """Coerce input to an indexable sequence.

    Tries numpy if available; otherwise falls back to list().
    Importing numpy lazily prevents import‑time dependency failures in CI
    when only bridge discovery is needed.
    """
    try:
        import numpy as _np  # local import to avoid hard dependency at import time
        return _np.asarray(seq)
    except Exception:
        # Best‑effort fallback: ensure we can iterate over values
        try:
            return list(seq)
        except Exception:
            return [seq]


def _bool_to_flag(value):
    if value is None:
        return ""
    return "T" if bool(value) else "F"

class FortplotModule:
    """Real fortplot module that interfaces with Fortran bridge program."""
    
    def __init__(self):
        self.figure_initialized = False
        self.bridge_process = None
        self.bridge_executable = self._find_bridge_executable()
    
    def _find_bridge_executable(self):
        """Find the fortplot_python_bridge executable."""
        # First, look for the renamed executable fortplot_python_bridge
        # Try to find it in the build directories relative to the Python package
        package_dir = Path(__file__).parent
        
        # Go up to project root (from python/fortplot to root)
        project_root = package_dir.parent.parent
        
        # Check for FPM build directory structures
        # The executable should be in build/*/app/fortplot_python_bridge
        build_dir = project_root / "build"
        if build_dir.exists():
            # Look for any gfortran_* or similar compiler directories
            for compiler_dir in build_dir.iterdir():
                if compiler_dir.is_dir():
                    app_bridge = compiler_dir / "app" / "fortplot_python_bridge"
                    if app_bridge.exists() and app_bridge.is_file():
                        return str(app_bridge)
                    # Also check test directory for backwards compatibility
                    test_bridge = compiler_dir / "test" / "fortplot_python_bridge"
                    if test_bridge.exists() and test_bridge.is_file():
                        return str(test_bridge)
        
        # Try current working directory
        cwd_bridge = Path.cwd() / "fortplot_python_bridge"
        if cwd_bridge.exists():
            return str(cwd_bridge)
        
        # Look in parent directories relative to this file (up to 5 levels)
        current_dir = Path(__file__).parent
        for _ in range(5):
            bridge_path = current_dir / "fortplot_python_bridge"
            if bridge_path.exists():
                return str(bridge_path)
            current_dir = current_dir.parent
        
        # Fallback to system PATH
        return "fortplot_python_bridge"
    
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
                raise RuntimeError(f"Could not find fortplot_python_bridge executable at {self.bridge_executable}: {e}")
    
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
        
        x = _to_array(x)
        y = _to_array(y)
        
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
        
        x = _to_array(x)
        y = _to_array(y)
        
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
        
        data = _to_array(data)
        
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

    def grid(self, enabled=None, which=None, axis=None, alpha=None, linestyle=None):
        """Configure grid settings."""
        self._send_command("GRID")
        self._send_command(_bool_to_flag(enabled))
        self._send_command("" if which is None else str(which))
        self._send_command("" if axis is None else str(axis))
        self._send_command("" if alpha is None else str(float(alpha)))
        self._send_command("" if linestyle is None else str(linestyle))

    def savefig(self, filename):
        """Save the current figure to a file."""
        self._send_command("SAVEFIG")
        self._send_command(filename)
        # Wait briefly for the bridge process to write the file.
        # Streamplot and other heavy renders can take longer than a fixed 100 ms.
        # Poll up to ~2s for the file to appear and be non-empty.
        import time
        from pathlib import Path
        target = Path(filename)
        deadline = time.time() + 2.0
        # First, wait for the file to exist
        while time.time() < deadline and not target.exists():
            time.sleep(0.02)
        # Then, wait for it to be non-empty (size > 0)
        if target.exists():
            while time.time() < deadline and target.stat().st_size == 0:
                time.sleep(0.02)
    
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
        """Send contour command to bridge."""
        x = _to_array(x)
        y = _to_array(y)
        z = _to_array(z)
        if z.ndim != 2:
            raise ValueError("z must be 2D array")
        self._send_command("CONTOUR")
        self._send_command(f"{len(x)} {len(y)}")
        for val in x:
            self._send_command(str(float(val)))
        for val in y:
            self._send_command(str(float(val)))
        # z expected shape (len(y), len(x)) here
        ny, nx = z.shape
        for i in range(ny):
            for j in range(nx):
                self._send_command(str(float(z[i, j])))
        if levels is not None:
            levels_arr = _to_array(levels)
            self._send_command(str(len(levels_arr)))
            for lv in levels_arr:
                self._send_command(str(float(lv)))
        else:
            self._send_command("0")
    
    def contour_filled(self, x, y, z, levels=None, colormap=None, show_colorbar=None, label=None):
        """Send filled contour command to bridge."""
        x = _to_array(x)
        y = _to_array(y)
        z = _to_array(z)
        if z.ndim != 2:
            raise ValueError("z must be 2D array")
        self._send_command("CONTOURF")
        self._send_command(f"{len(x)} {len(y)}")
        for val in x:
            self._send_command(str(float(val)))
        for val in y:
            self._send_command(str(float(val)))
        ny, nx = z.shape
        for i in range(ny):
            for j in range(nx):
                self._send_command(str(float(z[i, j])))

        use_extended = colormap is not None or show_colorbar is not None or label is not None

        if levels is not None:
            levels_arr = _to_array(levels)
            if use_extended:
                self._send_command(str(-(len(levels_arr) + 1)))
            else:
                self._send_command(str(len(levels_arr)))
            for lv in levels_arr:
                self._send_command(str(float(lv)))
        else:
            if use_extended:
                self._send_command(str(-1))
            else:
                self._send_command("0")

        if use_extended:
            self._send_command("" if colormap is None else str(colormap))
            self._send_command(_bool_to_flag(show_colorbar))
            self._send_command("" if label is None else str(label))
    
    def pcolormesh(self, x, y, c, cmap='viridis', vmin=None, vmax=None, edgecolors='none', linewidths=None):
        """Send pcolormesh command to bridge (basic parameters only)."""
        x = _to_array(x)
        y = _to_array(y)
        c = _to_array(c)
        if c.ndim != 2:
            raise ValueError("c must be 2D array")
        # Expect edge coordinates: len(x)=nx, len(y)=ny, c shape (ny-1, nx-1)
        self._send_command("PCOLORMESH")
        self._send_command(f"{len(x)} {len(y)}")
        for val in x:
            self._send_command(str(float(val)))
        for val in y:
            self._send_command(str(float(val)))
        my, mx = c.shape
        for i in range(my):
            for j in range(mx):
                self._send_command(str(float(c[i, j])))
        # Note: advanced styling args (cmap, vmin, vmax, edgecolors, linewidths)
        # are not sent yet; defaults are used by Fortran side.
    
    def streamplot(self, x, y, u, v, density=1.0):
        """Send streamplot command to bridge."""
        x = _to_array(x)
        y = _to_array(y)
        u = _to_array(u)
        v = _to_array(v)
        if u.ndim != 2 or v.ndim != 2:
            raise ValueError("u and v must be 2D arrays")
        if u.shape != v.shape:
            raise ValueError("u and v must have the same shape")
        self._send_command("STREAMPLOT")
        self._send_command(f"{len(x)} {len(y)}")
        for val in x:
            self._send_command(str(float(val)))
        for val in y:
            self._send_command(str(float(val)))
        nx = len(x)
        ny = len(y)
        if u.shape != (nx, ny):
            # Try (ny, nx) then
            if u.shape == (ny, nx):
                u = u.T
                v = v.T
            else:
                raise ValueError("u,v shape must match (len(x), len(y)) or its transpose")
        for i in range(nx):
            for j in range(ny):
                self._send_command(str(float(u[i, j])))
        for i in range(nx):
            for j in range(ny):
                self._send_command(str(float(v[i, j])))
        self._send_command(str(float(density)))
    
    def set_xscale(self, scale, threshold=None):
        """Set x-axis scale via bridge (supports 'linear','log','symlog')."""
        self._send_command("XSCALE")
        self._send_command(str(scale))
        if threshold is not None:
            self._send_command(str(float(threshold)))
        else:
            # Send a line to allow Fortran to attempt read and fail gracefully
            self._send_command("")
    
    def set_yscale(self, scale, threshold=None):
        """Set y-axis scale via bridge (supports 'linear','log','symlog')."""
        self._send_command("YSCALE")
        self._send_command(str(scale))
        if threshold is not None:
            self._send_command(str(float(threshold)))
        else:
            self._send_command("")


# Export the real fortplot module in the expected structure
# The Python code imports this as: import fortplot.fortplot_wrapper as _fortplot
# and then accesses functions as: _fortplot.fortplot.function_name()
fortplot = FortplotModule()

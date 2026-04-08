"""
Fortplot Python wrapper using the Vega-Lite JSON pipe renderer.

Builds Vega-Lite specs as Python dicts and pipes JSON to the
fortplot_render binary for rendering to PNG/PDF/SVG.
Replaces the legacy fortplot_python_bridge stdin command protocol.
"""

import json
import math
import subprocess
import tempfile
from pathlib import Path


def _to_list(seq):
    """Coerce input to a plain Python list of floats, replacing NaN/Inf with None."""
    try:
        import numpy as _np
        if isinstance(seq, _np.ndarray):
            raw = seq.ravel().tolist()
        else:
            raw = [float(v) for v in seq]
    except Exception:
        raw = [float(v) for v in seq]
    return [None if (v != v or v == _INF or v == _NEG_INF) else v for v in raw]


_INF = float('inf')
_NEG_INF = float('-inf')


VL_SCHEMA = 'https://vega.github.io/schema/vega-lite/v5.json'


class FortplotModule:
    """Fortplot module that builds Vega-Lite specs and renders via JSON pipe."""

    def __init__(self):
        self._render_executable = self._find_render_executable()
        self._reset()

    def _reset(self):
        self._width = 640
        self._height = 480
        self._title = None
        self._xlabel = None
        self._ylabel = None
        self._layers = []
        self._show_grid = False
        self._xlim = None
        self._ylim = None
        self._xscale = None
        self._yscale = None

    def _find_render_executable(self):
        """Find the fortplot_render executable."""
        package_dir = Path(__file__).parent
        project_root = package_dir.parent.parent

        build_dir = project_root / "build"
        if build_dir.exists():
            candidates = []
            for compiler_dir in build_dir.iterdir():
                if compiler_dir.is_dir():
                    app_render = compiler_dir / "app" / "fortplot_render"
                    if app_render.exists() and app_render.is_file():
                        candidates.append(app_render)
            if candidates:
                newest = max(candidates, key=lambda path: path.stat().st_mtime)
                return str(newest)

        cwd_render = Path.cwd() / "fortplot_render"
        if cwd_render.exists():
            return str(cwd_render)

        current_dir = Path(__file__).parent
        for _ in range(5):
            render_path = current_dir / "fortplot_render"
            if render_path.exists():
                return str(render_path)
            current_dir = current_dir.parent

        return "fortplot_render"

    def _make_xy_values(self, x, y):
        """Build row-oriented data values from x/y arrays."""
        xlist = _to_list(x)
        ylist = _to_list(y)
        return [{"x": xv, "y": yv} for xv, yv in zip(xlist, ylist)]

    def _coerce_axis(self, values):
        """Return a flat numeric axis array."""
        try:
            import numpy as _np
            arr = _np.asarray(values, dtype=float).ravel()
            return arr.tolist()
        except Exception:
            return [float(v) for v in values]

    def _flatten_field_values(self, values):
        """Flatten 2D field data using Fortran order for JSON round-trips."""
        try:
            import numpy as _np
            arr = _np.asarray(values, dtype=float)
            shape = arr.shape
            flat = arr.reshape(-1, order="F").tolist()
            return flat, shape
        except Exception as exc:
            raise TypeError("field plots require array-like numeric data") from exc

    def _field_layer(self, mark, x, y, matrix, **field_kwargs):
        """Build a layer dict for contour/pcolormesh/streamplot rendering."""
        flat, shape = self._flatten_field_values(matrix)
        if len(shape) != 2:
            raise ValueError(f"{mark} requires 2D field data")
        layer = {"mark": {"type": mark}}
        field = {
            "x": self._coerce_axis(x),
            "y": self._coerce_axis(y),
            "nrows": int(shape[0]),
            "ncols": int(shape[1]),
        }
        if mark == "streamplot":
            field["u"] = flat
        else:
            field["z"] = flat
        field.update(field_kwargs)
        layer["fortplotField"] = field
        return layer

    def _x_encoding(self):
        """Build x-channel encoding dict."""
        ch = {"field": "x", "type": "quantitative"}
        axis = {}
        if self._xlabel:
            axis["title"] = self._xlabel
        if self._show_grid:
            axis["grid"] = True
        if axis:
            ch["axis"] = axis
        scale = {}
        if self._xlim:
            scale["domain"] = list(self._xlim)
        if self._xscale:
            scale["type"] = self._xscale
        if scale:
            ch["scale"] = scale
        return ch

    def _y_encoding(self):
        """Build y-channel encoding dict."""
        ch = {"field": "y", "type": "quantitative"}
        axis = {}
        if self._ylabel:
            axis["title"] = self._ylabel
        if self._show_grid:
            axis["grid"] = True
        if axis:
            ch["axis"] = axis
        scale = {}
        if self._ylim:
            scale["domain"] = list(self._ylim)
        if self._yscale:
            scale["type"] = self._yscale
        if scale:
            ch["scale"] = scale
        return ch

    def _build_spec(self):
        """Assemble the full Vega-Lite spec from accumulated state."""
        spec = {
            "$schema": VL_SCHEMA,
            "width": self._width,
            "height": self._height,
        }
        if self._title:
            spec["title"] = self._title

        x_enc = self._x_encoding()
        y_enc = self._y_encoding()
        has_field_layer = any("fortplotField" in layer for layer in self._layers)

        if len(self._layers) == 0:
            spec["data"] = {"values": []}
            spec["mark"] = "line"
            spec["encoding"] = {"x": x_enc, "y": y_enc}
        elif len(self._layers) == 1 and not has_field_layer:
            layer = self._layers[0]
            spec["data"] = {"values": layer["values"]}
            spec["mark"] = layer["mark"]
            enc = {"x": x_enc, "y": y_enc}
            if "label" in layer:
                enc["color"] = {"value": layer["label"]}
            spec["encoding"] = enc
        else:
            layers = []
            for layer in self._layers:
                if "fortplotField" in layer:
                    entry = {
                        "mark": layer["mark"],
                        "fortplotField": layer["fortplotField"],
                    }
                    if "label" in layer:
                        entry["encoding"] = {
                            "color": {"value": layer["label"]},
                        }
                    layers.append(entry)
                    continue
                enc = {"x": x_enc, "y": y_enc}
                if "label" in layer:
                    enc["color"] = {"value": layer["label"]}
                layers.append({
                    "mark": layer["mark"],
                    "encoding": enc,
                    "data": {"values": layer["values"]},
                })
            spec["encoding"] = {"x": x_enc, "y": y_enc}
            spec["layer"] = layers

        return spec

    def figure(self, width=640, height=480):
        """Initialize a new figure."""
        self._reset()
        self._width = width
        self._height = height

    def plot(self, x, y, label="", linestyle="-"):
        """Create a line plot."""
        values = self._make_xy_values(x, y)
        layer = {"mark": "line", "values": values}
        if label:
            layer["label"] = label
        self._layers.append(layer)

    def scatter(self, x, y, label=""):
        """Create a scatter plot."""
        values = self._make_xy_values(x, y)
        layer = {"mark": "point", "values": values}
        if label:
            layer["label"] = label
        self._layers.append(layer)

    def histogram(self, data, label=""):
        """Create a histogram via Python-side binning rendered as bar chart."""
        values = [v for v in _to_list(data) if v is not None]
        n = len(values)
        if n == 0:
            return
        nbins = max(1, int(math.ceil(math.sqrt(n))))
        lo = min(values)
        hi = max(values)
        if lo == hi:
            hi = lo + 1.0
        bw = (hi - lo) / nbins
        counts = [0] * nbins
        for v in values:
            idx = int((v - lo) / bw)
            if idx >= nbins:
                idx = nbins - 1
            counts[idx] += 1
        bar_values = []
        for i in range(nbins):
            center = lo + (i + 0.5) * bw
            bar_values.append({"x": center, "y": counts[i]})
        self._layers.append({
            "mark": "bar",
            "values": bar_values,
        })

    def title(self, text):
        """Set the plot title."""
        self._title = text

    def xlabel(self, text):
        """Set the x-axis label."""
        self._xlabel = text

    def ylabel(self, text):
        """Set the y-axis label."""
        self._ylabel = text

    def legend(self):
        """Request legend display (accepted for API compatibility)."""
        pass

    def grid(self, enabled=None, which=None, axis=None,
             alpha=None, linestyle=None):
        """Configure grid settings."""
        if enabled is None:
            self._show_grid = not self._show_grid
        else:
            self._show_grid = bool(enabled)

    def savefig(self, filename):
        """Save the current figure by piping JSON to fortplot_render."""
        spec = self._build_spec()
        json_str = json.dumps(spec, allow_nan=False)

        ext = Path(filename).suffix.lower()
        if ext == '.json' or filename.endswith('.vl.json'):
            Path(filename).write_text(json_str)
            return

        result = subprocess.run(
            [self._render_executable, '-o', filename],
            input=json_str,
            capture_output=True,
            text=True,
            timeout=30,
        )
        if result.returncode != 0:
            raise RuntimeError(
                f"fortplot_render failed (exit {result.returncode}): "
                f"{result.stderr.strip()}"
            )

    def show_figure(self, blocking=False):
        """Show the current figure via a temporary file."""
        import os
        with tempfile.NamedTemporaryFile(
            suffix='.png', delete=False
        ) as tmp:
            tmp_path = tmp.name
        try:
            self.savefig(tmp_path)
            if os.name == 'nt':
                os.startfile(tmp_path)
            elif os.name == 'posix':
                opener = 'open' if os.uname().sysname == 'Darwin' else 'xdg-open'
                subprocess.Popen([opener, tmp_path],
                                 stdout=subprocess.DEVNULL,
                                 stderr=subprocess.DEVNULL)
        except Exception:
            pass
        return True

    def show_viewer(self, blocking=False):
        """Show the current figure in system viewer."""
        return self.show_figure(blocking=blocking)

    def xlim(self, xmin, xmax):
        """Set x-axis limits."""
        self._xlim = (float(xmin), float(xmax))

    def ylim(self, ymin, ymax):
        """Set y-axis limits."""
        self._ylim = (float(ymin), float(ymax))

    def set_xscale(self, scale, threshold=None):
        """Set x-axis scale type."""
        self._xscale = str(scale)

    def set_yscale(self, scale, threshold=None):
        """Set y-axis scale type."""
        self._yscale = str(scale)

    def contour(self, x, y, z, levels=None):
        """Create a contour plot rendered through fortplot_render."""
        field = self._field_layer("contour", x, y, z)
        if levels is not None:
            field["fortplotField"]["levels"] = self._coerce_axis(levels)
        self._layers.append(field)

    def contour_filled(self, x, y, z, levels=None, colormap=None,
                       show_colorbar=None, label=None):
        """Create a filled contour plot rendered through fortplot_render."""
        field = self._field_layer("contour_filled", x, y, z)
        if levels is not None:
            field["fortplotField"]["levels"] = self._coerce_axis(levels)
        if colormap:
            field["fortplotField"]["colormap"] = str(colormap)
        if show_colorbar is not None:
            field["fortplotField"]["showColorbar"] = bool(show_colorbar)
        if label:
            field["label"] = label
        self._layers.append(field)

    def pcolormesh(self, x, y, c, cmap='viridis', vmin=None, vmax=None,
                   edgecolors='none', linewidths=None):
        """Create a pcolormesh rendered through fortplot_render."""
        field = self._field_layer("pcolormesh", x, y, c)
        if cmap:
            field["fortplotField"]["colormap"] = str(cmap)
        if vmin is not None:
            field["fortplotField"]["vmin"] = float(vmin)
        if vmax is not None:
            field["fortplotField"]["vmax"] = float(vmax)
        if linewidths is not None:
            field["fortplotField"]["linewidths"] = float(linewidths)
        if edgecolors not in (None, 'none'):
            field["fortplotField"]["edgecolors"] = str(edgecolors)
        self._layers.append(field)

    def streamplot(self, x, y, u, v, density=1.0):
        """Create a streamplot rendered through fortplot_render."""
        field = self._field_layer("streamplot", x, y, u, density=float(density))
        flat_v, shape_v = self._flatten_field_values(v)
        if shape_v != (field["fortplotField"]["nrows"], field["fortplotField"]["ncols"]):
            raise ValueError("streamplot requires U and V arrays with identical shapes")
        field["fortplotField"]["v"] = flat_v
        self._layers.append(field)


fortplot = FortplotModule()

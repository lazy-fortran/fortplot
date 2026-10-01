#!/usr/bin/env python3
"""Adversarial controls built independently with real Matplotlib artists."""
import tempfile
import unittest
from pathlib import Path

import matplotlib.colors as colors
import numpy as np
from PIL import Image

import verify_contour_line_colorbar as oracle


class RenderedOracleControls(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.path = Path(self.temp.name)
        self.ref = oracle.reference()

    def tearDown(self):
        oracle.plt.close("all")
        self.temp.cleanup()

    def render(self, *, wrong_geometry=False, bar=True, gradient=False,
               proportional=False, raw_normalization=False, linewidth=1.5,
               outline=True, labels=True, offset=0., scale=1., levels=oracle.LEVELS):
        x, y = np.linspace(0, 2, 31), np.linspace(0, 1, 19)
        field = x[None, :] + 10*y[:, None]
        if wrong_geometry:
            field = 5*x[None, :] + 2*y[:, None]
        fig, ax = oracle.plt.subplots(figsize=(6.4, 4.8), dpi=100)
        cs = ax.contour(x, y, offset+scale*field, levels=levels,
                        cmap="viridis", linewidths=linewidth)
        if raw_normalization:
            cs.set_norm(colors.Normalize(0, 12))
        if bar:
            mappable = cs
            if gradient:
                mappable = oracle.plt.cm.ScalarMappable(
                    norm=colors.Normalize(min(levels), max(levels)), cmap="viridis")
            cb = fig.colorbar(mappable, ax=ax, spacing="proportional" if proportional else "uniform")
            cb.outline.set_visible(outline)
            if not labels:
                cb.set_ticks([])
        ax.set_xlim(0, 2)
        ax.set_ylim(0, 1)
        png, pdf = self.path/"fixture.png", self.path/"fixture.pdf"
        fig.savefig(png)
        fig.savefig(pdf)
        return np.asarray(Image.open(png).convert("RGB"), dtype=float), pdf

    def test_valid_independent_matplotlib_render(self):
        rgb, pdf = self.render()
        oracle.check_geometry(rgb, self.ref)
        oracle.check_bar(rgb, self.ref)
        oracle.check_pdf_ticks(pdf, rgb, self.ref)

    def test_wrong_geometry_rejected(self):
        rgb, _ = self.render(wrong_geometry=True)
        with self.assertRaises(AssertionError):
            oracle.check_geometry(rgb, self.ref)

    def test_absent_bar_rejected(self):
        rgb, _ = self.render(bar=False)
        with self.assertRaises(AssertionError):
            oracle.check_bar(rgb, self.ref)

    def test_continuous_gradient_rejected(self):
        rgb, _ = self.render(gradient=True)
        with self.assertRaises(AssertionError):
            oracle.check_bar(rgb, self.ref)

    def test_proportional_nonuniform_spacing_rejected(self):
        rgb, _ = self.render(proportional=True)
        with self.assertRaises(AssertionError):
            oracle.check_bar(rgb, self.ref)

    def test_raw_data_normalization_rejected(self):
        rgb, _ = self.render(raw_normalization=True)
        with self.assertRaises(AssertionError):
            oracle.check_bar(rgb, self.ref)

    def test_wrong_stroke_width_rejected(self):
        rgb, _ = self.render(linewidth=5.)
        with self.assertRaises(AssertionError):
            oracle.check_bar(rgb, self.ref)

    def test_missing_outline_rejected(self):
        rgb, _ = self.render(outline=False)
        with self.assertRaises(AssertionError):
            oracle.check_bar(rgb, self.ref)

    def test_missing_value_labels_rejected(self):
        rgb, pdf = self.render(labels=False)
        with self.assertRaises(AssertionError):
            oracle.check_pdf_ticks(pdf, rgb, self.ref)

    def test_fractional_values_cannot_be_integer_labels(self):
        ref = oracle.reference(levels=None, offset=100.13, scale=.527)
        rgb, pdf = self.render(offset=100.13, scale=.527, levels=None)
        oracle.check_pdf_ticks(pdf, rgb, ref)
        cb = oracle.plt.gcf().axes[-1]
        cb.set_yticklabels([str(round(tick)) for tick in ref["ticks"]])
        oracle.plt.gcf().savefig(pdf)
        with self.assertRaises(AssertionError):
            oracle.check_pdf_ticks(pdf, rgb, ref)


if __name__ == "__main__":
    unittest.main()

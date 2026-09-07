#!/usr/bin/env python3
"""Mutation tests for the independent Matplotlib visual regression gate."""

from pathlib import Path
import shutil
import tempfile
import unittest

import numpy as np

from compare_matplotlib import (colored, compare_images, compare_suite,
                                horizontal_strokes, ink, load_rgb, region)
from ref_matplotlib import generate


class VisualOracleTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        base = Path(__file__).resolve().parents[1] / "build/test/output"
        base.mkdir(parents=True, exist_ok=True)
        cls.directory = tempfile.TemporaryDirectory(prefix="matplotlib-oracle-", dir=base)
        cls.root = Path(cls.directory.name)
        cls.manifest = generate(cls.root / "reference", ("line", "math_fraction", "grid", "scatter"))

    @classmethod
    def tearDownClass(cls):
        cls.directory.cleanup()

    def image(self, case="line"):
        return load_rgb(self.root / "reference" / f"mpl_{case}.png")

    def compare(self, actual, case="line"):
        return compare_images(self.image(case), actual, self.manifest["cases"][case],
                              self.manifest["colors"])

    def test_real_matplotlib_reference_passes_unchanged(self):
        for case in self.manifest["cases"]:
            with self.subTest(case=case):
                result = self.compare(self.image(case), case)
                self.assertTrue(result["pass"], result)

    def test_missing_curve_fails(self):
        actual = self.image().copy()
        # Drop the orange cosine curve while retaining axes and the blue sine.
        mask = (actual[:, :, 0] > 180) & (actual[:, :, 1] < 180) & (actual[:, :, 2] < 100)
        actual[mask] = 255
        self.assertFalse(self.compare(actual)["geometry"]["#ff7f0e"]["pass"])

    def test_shifted_curves_fail(self):
        source = self.image()
        actual = source.copy()
        mask = colored(source)
        actual[mask] = 255
        yy, xx = np.nonzero(mask)
        actual[yy + 4, xx + 4] = source[yy, xx]
        self.assertFalse(self.compare(actual)["geometry"]["colored_geometry"]["pass"])

    def test_one_missing_scatter_mark_fails(self):
        actual = self.image("scatter").copy()
        mask = colored(actual)
        _, xx = np.nonzero(mask)
        mask[:, int(xx.min()) + 12:] = False
        actual[mask] = 255
        result = self.compare(actual, "scatter")["geometry"]["colored_geometry"]
        self.assertFalse(result["pass"])
        self.assertGreater(result["largest_missing_component"], 16)

    def test_missing_title_fails(self):
        actual = self.image().copy()
        label = next(item for item in self.manifest["cases"]["line"]["labels"]
                     if item["text"] == "Sine and Cosine Functions")
        actual[region(label["bbox"], actual.shape)] = 255
        result = self.compare(actual)
        title = next(item for item in result["labels"] if item["text"] == label["text"])
        self.assertFalse(title["pass"])
        self.assertEqual(title["error"], "missing label")

    def test_shifted_title_fails(self):
        actual = self.image().copy()
        label = next(item for item in self.manifest["cases"]["line"]["labels"]
                     if item["text"] == "Sine and Cosine Functions")
        rows, cols = region(label["bbox"], actual.shape)
        title = actual[rows, cols].copy()
        actual[rows, cols] = 255
        actual[rows, slice(cols.start + 8, cols.stop + 8)] = title
        self.assertFalse(self.compare(actual)["pass"])

    def test_missing_math_fraction_bar_fails(self):
        actual = self.image("math_fraction").copy()
        label = next(item for item in self.manifest["cases"]["math_fraction"]["labels"]
                     if "frac" in item["text"])
        crop = region(label["bbox"], actual.shape)
        strokes = horizontal_strokes(ink(actual[crop]))
        self.assertGreater(int(strokes.sum()), 0)
        actual[crop][strokes] = 255
        result = self.compare(actual, "math_fraction")
        math = next(item for item in result["labels"] if "frac" in item["text"])
        self.assertFalse(math["pass"])
        self.assertLess(math["math_stroke_recall"], 0.9)

    def test_missing_grid_fails(self):
        actual = self.image("grid").copy()
        # Default Matplotlib grid is #b0b0b0; preserve the data and black spines.
        gray = actual.astype(np.int16)
        mask = (np.ptp(gray, axis=2) < 3) & (gray.min(2) > 150) & (gray.max(2) < 200)
        actual[mask] = 255
        self.assertFalse(self.compare(actual, "grid")["geometry"]["axes_and_monochrome_geometry"]["pass"])

    def test_pdf_compares_rasterized_pdf_reference(self):
        actual_dir = self.root / "pdf-actual"
        actual_dir.mkdir(exist_ok=True)
        shutil.copyfile(self.root / "reference/mpl_line.pdf", actual_dir / "fp_line.pdf")
        report = compare_suite(self.root / "reference", actual_dir, self.root / "pdf-report",
                               formats=("pdf",), cases=("line",))
        self.assertTrue(report["pass"], report)

    def test_missing_artifact_fails(self):
        report = compare_suite(self.root / "reference", self.root / "missing", self.root / "missing-report",
                               formats=("png",), cases=("line",))
        self.assertFalse(report["pass"])


if __name__ == "__main__":
    unittest.main()

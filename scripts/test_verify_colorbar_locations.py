#!/usr/bin/env python3
"""Reject absent, displaced, and mislabeled actual colorbar labels."""
import argparse
import sys
import tempfile
import unittest
from pathlib import Path

import numpy as np
from PIL import Image

import verify_colorbar_locations as oracle

ARTIFACTS = Path("build/test/output/fortplot_test_colorbar_location_contract")


class LabelControls(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.labels = []
        for style in ["line", "filled"]:
            for side in ["bottom", "top"]:
                name = f"label_{style}_{side}"
                pdf = ARTIFACTS/f"{name}.pdf"
                rgb = np.asarray(Image.open(ARTIFACTS/f"{name}.png").convert("RGB"))
                pm, pb = oracle.pdf_rectangles(pdf, (rgb.shape[1], rgb.shape[0]))
                main = oracle.image_rectangle(rgb, pm[2:]-pm[:2])
                bar = oracle.image_rectangle(rgb, pb[2:]-pb[:2])
                label = oracle.check_label(pdf, rgb, main, bar)
                cls.labels.append((name, pdf, rgb, main, bar, label["pdf_center"]))

    def test_missing_or_displaced_word_is_rejected(self):
        for name, pdf, rgb, main, bar, center in self.labels:
            cx, cy = np.rint(center).astype(int)
            x0, x1, y0, y1 = cx-42, cx+43, cy-5, cy+6
            for kind in ["missing", "down_10px", "right_10px"]:
                with self.subTest(case=name, control=kind):
                    changed = rgb.copy()
                    word = changed[y0:y1, x0:x1].copy()
                    changed[y0:y1, x0:x1] = 255
                    if kind == "down_10px":
                        changed[y0+10:y1+10, x0:x1] = word
                    elif kind == "right_10px":
                        changed[y0:y1, x0+10:x1+10] = word
                    with self.assertRaises(AssertionError):
                        oracle.check_label(pdf, changed, main, bar)

    def test_tick_and_noise_cannot_replace_a_word(self):
        for name, pdf, rgb, main, bar, center in self.labels:
            cx, cy = np.rint(center).astype(int)
            for kind in ["vertical_tick", "horizontal_stroke", "isolated_pixels"]:
                with self.subTest(case=name, control=kind):
                    changed = rgb.copy()
                    changed[cy-5:cy+6, cx-42:cx+43] = 255
                    if kind == "vertical_tick":
                        changed[cy-3:cy+4, cx] = 0
                    elif kind == "horizontal_stroke":
                        changed[cy, cx-15:cx+16] = 0
                    else:
                        changed[cy-2, cx-10] = 0
                        changed[cy+2, cx+10] = 0
                    with self.assertRaises(AssertionError):
                        oracle.check_label(pdf, changed, main, bar)

    def test_pdf_word_identity_is_preserved(self):
        for name, _, rgb, main, bar, _ in self.labels:
            with self.subTest(case=name), tempfile.TemporaryDirectory() as temp:
                pdf = Path(temp)/"wrong-word.pdf"
                fig = oracle.plt.figure(figsize=(rgb.shape[1]/100, rgb.shape[0]/100))
                fig.text(.5, .5, "OTHER")
                fig.savefig(pdf)
                oracle.plt.close(fig)
                with self.assertRaises(StopIteration):
                    oracle.check_label(pdf, rgb, main, bar)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--artifacts", type=Path, default=ARTIFACTS)
    arguments = parser.parse_args()
    ARTIFACTS = arguments.artifacts
    unittest.main(argv=[sys.argv[0]])

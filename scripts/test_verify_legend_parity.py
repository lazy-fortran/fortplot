#!/usr/bin/env python3
"""Independent controls for the legend placement comparison."""
import unittest

from matplotlib.legend import Legend
import numpy as np

from verify_legend_parity import comparison_errors, horizontal_anchor


class LegendComparisonTests(unittest.TestCase):
    def assert_accepted(self, actual, expected, anchor):
        self.assertTrue(np.all(comparison_errors(actual, expected, anchor)
                               <= [5, 4, 7, 4]))

    def assert_rejected(self, actual, expected, anchor):
        self.assertFalse(np.all(comparison_errors(actual, expected, anchor)
                                <= [5, 4, 7, 4]))

    def test_named_and_numeric_locations_follow_matplotlib(self):
        expected = {
            "upper right": 1, "upper left": 0, "lower left": 0,
            "lower right": 1, "right": 1, "center left": 0,
            "center right": 1, "lower center": 0.5,
            "upper center": 0.5, "center": 0.5,
        }
        for name, anchor in expected.items():
            with self.subTest(location=name):
                self.assertEqual(horizontal_anchor(name), anchor)
                self.assertEqual(horizontal_anchor(Legend.codes[name]), anchor)

    def test_right_anchor_accepts_allowed_independent_width_difference(self):
        # Both right edges are at x=190; the text widths differ by six pixels.
        self.assert_accepted([106, 64, 84, 25], [100, 64, 90, 25], 1)

    def test_center_anchor_accepts_allowed_independent_width_difference(self):
        # Both centers are at x=145.
        self.assert_accepted([103, 64, 84, 25], [100, 64, 90, 25], 0.5)

    def test_left_anchor_accepts_allowed_independent_width_difference(self):
        self.assert_accepted([100, 64, 84, 25], [100, 64, 90, 25], 0)

    def test_shift_over_five_pixels_fails_for_every_horizontal_anchor(self):
        cases = [
            (0, [105.1, 64, 84, 25]),
            (0.5, [108.1, 64, 84, 25]),
            (1, [111.1, 64, 84, 25]),
        ]
        for anchor, actual in cases:
            with self.subTest(anchor=anchor):
                self.assert_rejected(actual, [100, 64, 90, 25], anchor)

    def test_wrong_anchor_fails(self):
        self.assert_rejected([20, 64, 84, 25], [100, 64, 90, 25], 1)
        self.assert_rejected([180, 64, 84, 25], [100, 64, 90, 25], 0)

    def test_half_pixel_shift_over_limit_is_not_truncated(self):
        # Integer bounds still encode a 5.5-pixel difference between centers.
        self.assert_rejected([107, 64, 83, 25], [100, 64, 86, 25], 0.5)

    def test_width_error_over_seven_pixels_still_fails(self):
        self.assert_rejected([108, 64, 82, 25], [100, 64, 90, 25], 1)

    def test_vertical_shift_and_height_error_still_fail(self):
        self.assert_rejected([100, 68.1, 90, 25], [100, 64, 90, 25], 0)
        self.assert_rejected([100, 64, 90, 29.1], [100, 64, 90, 25], 0)


if __name__ == "__main__":
    unittest.main()

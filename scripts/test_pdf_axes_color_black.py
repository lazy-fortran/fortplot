#!/usr/bin/env python3
import os
import sys
import tempfile
import unittest

# Ensure local python package path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'python'))

try:
    import fortplot
    FORTPLOT_AVAILABLE = True
except Exception:
    FORTPLOT_AVAILABLE = False


@unittest.skipUnless(FORTPLOT_AVAILABLE, "fortplot not available")
class TestPdfAxesColor(unittest.TestCase):
    def test_axes_stroked_in_black(self):
        # Create simple figure and a default-colored line (likely non-black)
        fortplot.figure([6.4, 4.8])
        fortplot.plot([0, 1], [0, 1], label="line")
        fortplot.title("Axes Color Test")
        fortplot.xlabel("X")
        fortplot.ylabel("Y")

        with tempfile.TemporaryDirectory() as tmp:
            pdf_path = os.path.join(tmp, "axes_color_test.pdf")
            fortplot.savefig(pdf_path)

            # Ensure file exists and has content
            self.assertTrue(os.path.exists(pdf_path), "PDF file was not created")
            self.assertGreater(os.path.getsize(pdf_path), 200, "PDF file too small (likely empty)")

            # Read PDF content (fortplot writes uncompressed content streams)
            with open(pdf_path, 'rb') as f:
                data = f.read()

            # Heuristic: the axes frame is emitted as " re S" (rectangle then stroke)
            # Our fix sets stroke color to black ("0 0 0 RG") immediately before frame.
            idx = data.find(b" re S")
            self.assertNotEqual(idx, -1, "No rectangle stroke found for axes frame")

            # Search 200 bytes before the frame command for a black stroke color set
            window_start = max(0, idx - 200)
            pre_window = data[window_start:idx]
            self.assertIn(b"0 0 0 RG", pre_window, "Axes frame not forced to black stroke before drawing")


if __name__ == "__main__":
    unittest.main()


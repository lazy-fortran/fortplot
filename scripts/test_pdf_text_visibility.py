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
class TestPdfTextVisibility(unittest.TestCase):
    def test_pdf_contains_text_commands(self):
        # Create simple figure with visible text elements
        fortplot.figure([6.4, 4.8])
        fortplot.plot([0, 1], [0, 1], label="line")
        fortplot.title("PDF Text Visibility Test")
        fortplot.xlabel("X Label")
        fortplot.ylabel("Y Label")

        with tempfile.TemporaryDirectory() as tmp:
            pdf_path = os.path.join(tmp, "visibility_test.pdf")
            fortplot.savefig(pdf_path)

            # Sanity: file exists and is non-trivial size
            self.assertTrue(os.path.exists(pdf_path), "PDF file was not created")
            self.assertGreater(os.path.getsize(pdf_path), 200, "PDF file too small (likely empty)")

            # Check uncompressed content stream for text drawing operators
            data = open(pdf_path, 'rb').read()
            # PDF produced by fortplot is plain-text streams; search in bytes
            self.assertIn(b" Tj", data, "No text show (Tj) operator found in PDF")
            self.assertIn(b" Tm", data, "No text matrix (Tm) operator found in PDF")
            # Presence of Tj/Tm in stream indicates text is emitted and positioned


if __name__ == "__main__":
    unittest.main()

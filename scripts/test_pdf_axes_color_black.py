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

            # Read and normalize PDF content streams (handle Flate-compressed streams)
            data = _read_pdf_stream_text(pdf_path)

            # Heuristic: the axes frame is emitted as " re S" (rectangle then stroke)
            # Our fix sets stroke color to black ("0 0 0 RG") immediately before frame.
            idx = data.find(" re S")
            self.assertNotEqual(idx, -1, "No rectangle stroke found for axes frame")

            # Search 200 bytes before the frame command for a black stroke color set
            window_start = max(0, idx - 200)
            pre_window = data[window_start:idx]
            self.assertIn("0 0 0 RG", pre_window, "Axes frame not forced to black stroke before drawing")


def _read_pdf_stream_text(path: str) -> str:
    """Return concatenated text of all PDF content streams, decompressing Flate when present."""
    import re, zlib
    # Ensure the PDF file handle is properly closed to avoid ResourceWarning
    with open(path, 'rb') as f:
        b = f.read()
    s = b.decode('latin1', errors='ignore')
    out = []
    i = 0
    # Quick pass: try raw data if uncompressed
    if ' re S' in s and '0 0 0 RG' in s:
        return s
    # Otherwise, find dictionaries preceding streams and decompress if Flate
    while True:
        m = re.search(r"<<(?:.|\n|\r)*?>>\s*stream", s[i:])
        if not m:
            break
        dict_start = i + m.start()
        stream_kw_end = i + m.end()
        j = stream_kw_end
        # Skip EOL after 'stream'
        if s[j:j+2] == '\r\n':
            j += 2
        elif s[j:j+1] in ('\r', '\n'):
            j += 1
        k = s.find('endstream', j)
        if k < 0:
            break
        dict_txt = s[dict_start:stream_kw_end]
        stream_bytes = b[j:k]
        txt = ''
        if '/Filter' in dict_txt and '/FlateDecode' in dict_txt:
            try:
                try:
                    dcmp = zlib.decompress(stream_bytes)
                except Exception:
                    dcmp = zlib.decompress(stream_bytes, -15)
                txt = dcmp.decode('latin1', errors='ignore')
            except Exception:
                txt = ''
        else:
            try:
                txt = stream_bytes.decode('latin1', errors='ignore')
            except Exception:
                txt = ''
        out.append(txt)
        i = k + len('endstream')
    return "\n".join(out)


if __name__ == "__main__":
    unittest.main()

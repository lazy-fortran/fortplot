#!/usr/bin/env python3
"""
Scan a PDF for non-gray RGB fill color commands ("rg").
Handles Flate-compressed content streams by attempting zlib decompression.

Exit codes:
 0 - found non-gray rg command
 1 - no non-gray rg command found
 2 - usage or file error
"""

import re
import sys
import zlib

def find_streams(buf: bytes):
    i = 0
    ln = len(buf)
    while True:
        i = buf.find(b'stream', i)
        if i < 0:
            return
        # Move to end of keyword and optional EOL
        j = i + 6
        if j < ln and buf[j:j+2] == b'\r\n':
            j += 2
        elif j < ln and buf[j:j+1] in (b'\r', b'\n'):
            j += 1
        k = buf.find(b'endstream', j)
        if k < 0:
            return
        yield (i, j, k)
        i = k + 9

def has_nongray_rgb(txt: str) -> bool:
    # Match "R G B rg" (fill color)
    pat = re.compile(r"(?m)\b([0-9]*\.?[0-9]+)\s+([0-9]*\.?[0-9]+)\s+([0-9]*\.?[0-9]+)\s+rg\b")
    for m in pat.finditer(txt):
        r, g, b = (float(m.group(1)), float(m.group(2)), float(m.group(3)))
        if abs(r-g) > 1e-9 or abs(g-b) > 1e-9 or abs(r-b) > 1e-9:
            return True
    return False

def main(argv):
    if len(argv) != 2:
        print("usage: pdf_scan_rg.py <file.pdf>", file=sys.stderr)
        return 2
    path = argv[1]
    try:
        data = open(path, 'rb').read()
    except OSError as e:
        print(f"error reading {path}: {e}", file=sys.stderr)
        return 2

    # Quick pass on raw data
    try:
        if has_nongray_rgb(data.decode('latin1', errors='ignore')):
            return 0
    except Exception:
        pass

    # Iterate streams; if filtered with Flate, decompress
    found = False
    search_start = 0
    while True:
        m = re.search(br"<<(?:.|\n|\r)*?>>\s*stream", data[search_start:])
        if not m:
            break
        dict_start = search_start + m.start()
        for (i, j, k) in find_streams(data[dict_start:]):
            i += dict_start; j += dict_start; k += dict_start
            dict_bytes = data[dict_start:i]
            stream_bytes = data[j:k]
            filt = b'/Filter' in dict_bytes and b'/FlateDecode' in dict_bytes
            try:
                if filt:
                    try:
                        s = zlib.decompress(stream_bytes)
                    except Exception:
                        s = zlib.decompress(stream_bytes, -15)
                    txt = s.decode('latin1', errors='ignore')
                else:
                    txt = stream_bytes.decode('latin1', errors='ignore')
                if has_nongray_rgb(txt):
                    found = True
                    break
            except Exception:
                continue
        if found:
            break
        search_start = dict_start + 2

    return 0 if found else 1

if __name__ == '__main__':
    sys.exit(main(sys.argv))


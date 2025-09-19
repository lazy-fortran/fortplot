#!/usr/bin/env python3
"""
Search PDF content streams (handles Flate-compressed streams) for a given
substring or regular expression. Returns 0 if pattern is found, 1 otherwise.

Usage:
  pdf_expect_token.py <file.pdf> <pattern> [--regex]

Examples:
  pdf_expect_token.py out.pdf "(10) Tj"
  pdf_expect_token.py out.pdf "\(1\)\s*Tj\s*\(0\)\s*Tj" --regex
"""

import re
import sys
import zlib


def iter_stream_texts(data: bytes):
    # Iterate stream dictionaries followed by 'stream...endstream'
    search_start = 0
    while True:
        m = re.search(br"<<(?:.|\n|\r)*?>>\s*stream", data[search_start:])
        if not m:
            break
        dict_start = search_start + m.start()
        # Find the actual stream boundaries
        i = data.find(b"stream", dict_start)
        if i < 0:
            break
        j = i + 6
        if j < len(data) and data[j:j+2] == b"\r\n":
            j += 2
        elif j < len(data) and data[j:j+1] in (b"\r", b"\n"):
            j += 1
        k = data.find(b"endstream", j)
        if k < 0:
            break
        dict_bytes = data[dict_start:i]
        stream_bytes = data[j:k]
        # Decompress if Flate filtered
        filt = b"/Filter" in dict_bytes and b"/FlateDecode" in dict_bytes
        try:
            if filt:
                try:
                    s = zlib.decompress(stream_bytes)
                except Exception:
                    s = zlib.decompress(stream_bytes, -15)
                yield s.decode("latin1", errors="ignore")
            else:
                yield stream_bytes.decode("latin1", errors="ignore")
        except Exception:
            # Skip undecodable streams
            pass
        search_start = k + 9


def main(argv):
    if len(argv) < 3:
        print("usage: pdf_expect_token.py <file.pdf> <pattern> [--regex]", file=sys.stderr)
        return 2
    path = argv[1]
    pattern = argv[2]
    use_regex = len(argv) > 3 and argv[3] == "--regex"
    try:
        data = open(path, "rb").read()
    except OSError as e:
        print(f"error reading {path}: {e}", file=sys.stderr)
        return 2

    if not use_regex:
        # Quick raw check
        if pattern in data.decode("latin1", errors="ignore"):
            return 0
        for txt in iter_stream_texts(data):
            if pattern in txt:
                return 0
        return 1
    else:
        try:
            rx = re.compile(pattern)
        except re.error as e:
            print(f"invalid regex: {e}", file=sys.stderr)
            return 2
        # Try raw first, then per-stream
        if rx.search(data.decode("latin1", errors="ignore")):
            return 0
        for txt in iter_stream_texts(data):
            if rx.search(txt):
                return 0
        return 1


if __name__ == "__main__":
    sys.exit(main(sys.argv))


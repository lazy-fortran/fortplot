#!/usr/bin/env python3
"""Check emitted math against Matplotlib's parser and Adobe font data.

Run `fo test test_mathtext_layout_rendering` first. This checks glyph identity,
font scaling, vector rules and font metrics. Extent differences are reported
for the stricter visual comparison gate; they are never hidden by this check.
Requires Matplotlib, Pillow, fontTools and pypdf.
"""
from __future__ import annotations

import argparse
from collections import Counter
import json
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.font_manager import FontProperties
from matplotlib.mathtext import MathTextParser
from fontTools.agl import UV2AGL
import numpy as np
from PIL import Image
from pypdf import PdfReader
from pypdf.generic import ContentStream


def check_symbol_metrics(path: Path) -> int:
    afm = Path(matplotlib.get_data_path()) / "fonts/pdfcorefonts/Symbol.afm"
    glyphs = {}
    for line in afm.read_text().splitlines():
        if line.startswith("C "):
            fields = dict(field.strip().split(" ", 1)
                          for field in line.split(";") if field.strip())
            glyphs[fields["N"]] = (int(fields["C"]), int(fields["WX"]))
    # Adobe Glyph List uses compatibility characters for these Greek glyphs.
    aliases = {0x394: "Delta", 0x3A9: "Omega", 0x3BC: "mu"}
    count = 0
    for line in path.read_text().splitlines():
        codepoint, code, width = map(int, line.split())
        name = aliases.get(codepoint, UV2AGL.get(codepoint))
        assert name in glyphs, f"U+{codepoint:04X} has no Adobe Symbol glyph"
        assert (code, width) == glyphs[name], (
            f"U+{codepoint:04X}: encoding/advance {(code, width)} != "
            f"Adobe {name} {glyphs[name]}"
        )
        count += 1
    assert count >= 60, f"Incomplete exported symbol coverage: {count}"
    return count


def pdf_evidence(path: Path) -> tuple[str, set[float], int, int]:
    reader = PdfReader(path)
    page = reader.pages[0]
    text_runs = []
    sizes = set()

    def visit(text, cm, tm, font, size):
        del cm, tm, font
        if text.strip():
            text_runs.append(text)
            sizes.add(round(size, 1))

    page.extract_text(visitor_text=visit)
    stream = ContentStream(page.get_contents(), reader)
    lines = radicals = rules = 0
    dash = []
    dash_stack = []
    for operands, operator in stream.operations:
        if operator == b"q":
            dash_stack.append(dash[:])
        elif operator == b"Q":
            dash = dash_stack.pop()
        elif operator == b"d":
            dash = list(operands[0])
        if operator == b"m":
            lines = 0
        elif operator == b"l":
            lines += 1
        elif operator == b"S":
            assert not dash, "Math rule inherited a dashed plot line style"
            rules += 1
            if lines >= 3:
                radicals += 1
            lines = 0
    return "".join(text_runs), sizes, radicals, rules


def normalized_glyphs(text: str) -> Counter:
    return Counter(ch for ch in text.replace("−", "-") if not ch.isspace())


def check_layout(directory: Path, output: Path) -> dict:
    rows = [line.split("\t") for line in
            (directory / "math-widths.tsv").read_text().splitlines()]
    parser = MathTextParser("path")
    prop = FontProperties(size=36)
    expected_text = []
    expected_sizes = set()
    expected_rules = 0
    equivalent = [(r"$\frac{1}{2}$", r"$\frac {1} {2}$"),
                  (r"$\sqrt{x}$", r"$\sqrt {x}$")]
    for left, right in equivalent:
        a, b = [parser.parse(expr, dpi=72, prop=prop) for expr in (left, right)]
        assert (a.width, a.height, a.depth) == (b.width, b.height, b.depth)
        assert [g[1:] for g in a.glyphs] == [g[1:] for g in b.glyphs]
    extents = []
    figure = plt.figure(figsize=(900 / 72, 560 / 72), dpi=72)
    for i, (expression, raster_width, pdf_width) in enumerate(rows):
        parsed = parser.parse(expression, dpi=72, prop=prop)
        expected_text.extend(chr(glyph[2]) for glyph in parsed.glyphs)
        expected_sizes.update(round(glyph[1], 1) for glyph in parsed.glyphs
                              if chr(glyph[2]) != "√")
        expected_rules += len(parsed.rects)
        extents.append({
            "expression": expression,
            "matplotlib_width": float(parsed.width),
            "raster_width": int(raster_width),
            "pdf_width": float(pdf_width),
            "raster_ratio": int(raster_width) / float(parsed.width),
            "pdf_ratio": float(pdf_width) / float(parsed.width),
        })
        figure.text(30 / 900, 1 - (65 + i * 72) / 560, expression, fontsize=36)
    figure.savefig(output / "math-layout-matplotlib.png")
    figure.savefig(output / "math-layout-matplotlib.pdf")
    plt.close(figure)

    text, sizes, radical_paths, rules = pdf_evidence(directory / "math-layout.pdf")
    observed = normalized_glyphs(text + "√" * radical_paths)
    expected = normalized_glyphs("".join(expected_text))
    assert observed == expected, f"Glyph mismatch: missing {expected-observed}; extra {observed-expected}"
    assert expected_sizes <= sizes, f"Nested font sizes missing: {expected_sizes-sizes}"
    assert rules == expected_rules, f"Vector rule count {rules} != Matplotlib {expected_rules}"
    pixels = np.asarray(Image.open(directory / "math-layout.png").convert("RGB"))
    assert pixels.shape == (560, 900, 3)
    for i, row in enumerate(rows):
        center = 65 + i * 72
        ink = np.any(pixels[max(0, center - 65):center + 22] < 160, axis=2)
        assert np.count_nonzero(ink) > 20, f"Empty raster expression: {row[0]}"
    return {"matplotlib_version": matplotlib.__version__, "glyphs": sum(expected.values()),
            "font_sizes": sorted(sizes), "rules": rules, "extents": extents}


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--artifacts", type=Path,
                        default=Path("build/test/output/fortplot_test_mathtext_layout"))
    parser.add_argument("--output", type=Path,
                        default=Path("output/visual-audit/math"))
    args = parser.parse_args()
    args.output.mkdir(parents=True, exist_ok=True)
    symbols = check_symbol_metrics(args.artifacts / "symbol-metrics.tsv")
    result = check_layout(args.artifacts, args.output)
    result["symbol_glyphs"] = symbols
    (args.output / "mathtext-oracle.json").write_text(json.dumps(result, indent=2) + "\n")
    print(f"PASS: {symbols} Adobe Symbol mappings/advances; "
          f"{result['glyphs']} MathText glyphs; {result['rules']} vector rules")
    print("Measured widths versus Matplotlib are recorded in mathtext-oracle.json")


if __name__ == "__main__":
    main()

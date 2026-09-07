#!/usr/bin/env python3
"""Compare a quiver on nonlinear mixed-plot axes with actual Matplotlib."""
import argparse
import json
from pathlib import Path
import xml.etree.ElementTree as ET

import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import numpy as np

from verify_quiver_parity import compare, pdf_raster


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--artifacts', type=Path,
                        default=Path('build/test/output/fortplot_test_quiver_mixed_axes'))
    parser.add_argument('--output', type=Path,
                        default=Path('output/visual-audit/quiver-mixed-axes'))
    args = parser.parse_args()
    args.output.mkdir(parents=True, exist_ok=True)
    with matplotlib.rc_context(matplotlib.rcParamsDefault):
        fig, ax = plt.subplots(figsize=(8, 6), dpi=100)
        ax.plot([10000], [1], color='black')
        q = ax.quiver([10], [1], [10], [1], angles='xy', scale=35, color='#1f77b4')
        ax.set(xscale='log', xlim=(1, 10000), ylim=(0, 2))
        fig.canvas.draw()
        expected = q.get_transform().transform(q.get_paths()[0].vertices)
        expected += ax.transData.transform((10, 1))
        expected[:, 1] = 600 - expected[:, 1]
        tree = ET.parse(args.artifacts/'mixed_axes.svg')
        polygons = [node for node in tree.iter() if node.attrib.get('class') == 'quiver']
        actual = np.fromstring(polygons[0].attrib['points'].replace(',', ' '),
                               sep=' ').reshape(-1, 2)
        error = float(np.max(np.abs(actual - expected)))
        report = {'matplotlib': matplotlib.__version__,
                  'max_vertex_error_px': error,
                  'geometry_pass': len(polygons) == 1 and error < 1e-6}
        for suffix in ('png', 'pdf'):
            reference = args.output/f'matplotlib_mixed_axes.{suffix}'
            fig.savefig(reference)
            actual_path = args.artifacts/f'mixed_axes.{suffix}'
            if suffix == 'pdf':
                reference = pdf_raster(reference, args.output/'matplotlib_pdf')
                actual_path = pdf_raster(actual_path, args.output/'fortplot_pdf')
            report[suffix] = compare(actual_path, reference)
        plt.close(fig)
    report['pass'] = report['geometry_pass'] and report['png']['pass'] and report['pdf']['pass']
    (args.output/'report.json').write_text(json.dumps(report, indent=2)+'\n')
    print(json.dumps(report, indent=2))
    raise SystemExit(0 if report['pass'] else 1)


if __name__ == '__main__':
    main()

#!/usr/bin/env python3
"""Compare every quiver polygon and colored PNG/PDF ink to actual Matplotlib.

Run the test_quiver_arrow_length FPM fixture first. Font differences do not enter
this oracle: blue ink isolates arrows, while every local vertex is independently
read from the real Matplotlib Quiver PolyCollection.
"""
import argparse
import json
from pathlib import Path
import subprocess
import xml.etree.ElementTree as ET

import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import numpy as np
from PIL import Image


def dilate(mask):
    padded = np.pad(mask, 1)
    return np.logical_or.reduce([padded[y:y + mask.shape[0], x:x + mask.shape[1]]
                                 for y in range(3) for x in range(3)])


def ink(path):
    pixels = np.asarray(Image.open(path).convert('RGB')).astype(float)
    return np.clip((pixels[:, :, 2] - pixels[:, :, 0])/149.0, 0, 1)


def compare(actual, expected):
    a, e = ink(actual), ink(expected)
    if a.shape != e.shape:
        return {'pass': False, 'shape': [list(a.shape), list(e.shape)]}
    am, em = a > .35, e > .35
    missing = int(np.count_nonzero(em & ~dilate(am)))
    extra = int(np.count_nonzero(am & ~dilate(em)))
    area_error = float(abs(a.sum() - e.sum())/e.sum())
    # One antialiasing pixel is allowed around each edge, but a misplaced arrow,
    # a hollow head, or a wrong scale must fail either support or ink area.
    passed = missing <= .005*em.sum() and extra <= .005*am.sum() and area_error <= .08
    return {'pass': bool(passed), 'missing_pixels': missing, 'extra_pixels': extra,
            'relative_ink_area_error': area_error}


def pdf_raster(path, output):
    # Supersample both PDF renderers equally to avoid Poppler snapping tiny
    # arrows at the spines to whole opaque pixels at 100 DPI. Keep the same
    # physical comparison grid and reject page-size errors before reduction.
    with Image.open(path.with_suffix('.png')) as png:
        expected_size = png.size
    subprocess.run(['pdftoppm', '-r', '400', '-singlefile', '-png', str(path),
                    str(output)], check=True, capture_output=True)
    raster = output.with_suffix('.png')
    with Image.open(raster) as rendered:
        if rendered.size != tuple(4 * value for value in expected_size):
            raise ValueError(f'PDF canvas mismatch: {path}: {rendered.size}')
        reduced = rendered.resize(expected_size, Image.Resampling.BOX)
    reduced.save(raster)
    return raster


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--artifacts', type=Path,
                        default=Path('build/test/output/fortplot_test_quiver_matplotlib'))
    parser.add_argument('--output', type=Path, default=Path('output/visual-audit/quiver-parity'))
    args = parser.parse_args()
    args.output.mkdir(parents=True, exist_ok=True)
    table = np.loadtxt(args.artifacts/'quiver_oracle_vertices.dat')
    grid = np.linspace(-2, 2, 10)
    x, y = np.meshgrid(grid, grid)
    u, v = -y.copy(), x.copy()
    u[-1, -1] = v[-1, -1] = 0
    report = {'matplotlib': matplotlib.__version__, 'cases': {}}
    options = [{}, {'scale': 35}, {'scale': 70},
               {'scale': 35, 'width': .01, 'headwidth': 5, 'headlength': 7, 'pivot': 'middle'},
               {'scale': 1, 'angles': 'xy', 'scale_units': 'xy'},
               {'scale': 35, 'width': 2, 'units': 'dots', 'angles': 'xy', 'pivot': 'tip'},
               {'scale': 35, 'alpha': .45},
               {'scale': 1, 'width': .2, 'units': 'x'},
               {'scale': 1, 'width': .2, 'units': 'xy'}]
    for case, kwargs in enumerate(options, 1):
        with matplotlib.rc_context(matplotlib.rcParamsDefault):
            fig, ax = plt.subplots(figsize=(8, 6), dpi=100)
            px, py = x, y
            if case >= 8:
                px, py = 10**((x+2)/2), y+2
                ax.set(xscale='log', xlim=(1, 100), ylim=(0, 10))
            q = ax.quiver(px, py, u, v, color='#1f77b4', **kwargs)
            ax.set(xlabel='X', ylabel='Y', title='Matplotlib quiver oracle')
            fig.canvas.draw()
            expected = np.concatenate([q.get_transform().transform(path.vertices)
                                       for path in q.get_paths()])
            actual = table[table[:, 0] == case, 3:5]
            vertex_error = float(np.max(np.abs(actual - expected)))
            results = {'max_vertex_error_px': vertex_error,
                       'geometry_pass': vertex_error < 1e-10}
            for suffix in ('png', 'pdf'):
                reference = args.output/f'matplotlib_quiver_{case}.{suffix}'
                fig.savefig(reference)
                actual_path = args.artifacts/f'quiver_oracle_{case}.{suffix}'
                if suffix == 'pdf':
                    reference = pdf_raster(reference, args.output/f'matplotlib_{case}_pdf')
                    actual_path = pdf_raster(actual_path, args.output/f'fortplot_{case}_pdf')
                results[suffix] = compare(actual_path, reference)
                if case <= 4 or case == 7:
                    cx, cy = ax.transData.transform((x[-1, -1], y[-1, -1]))
                    cx, cy = round(cx), round(600-cy)
                    aa = ink(actual_path)[cy-10:cy+11, cx-10:cx+11].sum()
                    ee = ink(reference)[cy-10:cy+11, cx-10:cx+11].sum()
                    zero_pass = bool(ee > 0 and .65*ee <= aa <= 1.35*ee)
                    results[suffix]['zero_arrow_ink'] = [float(aa), float(ee)]
                    results[suffix]['zero_arrow_pass'] = zero_pass
                    results[suffix]['pass'] &= zero_pass
            plt.close(fig)
        tree = ET.parse(args.artifacts/f'quiver_oracle_{case}.svg')
        vertices = [np.fromstring(node.attrib['points'].replace(',', ' '), sep=' ').reshape(-1, 2)
                    for node in tree.iter() if node.attrib.get('class') == 'quiver']
        joined = np.concatenate(vertices)
        results['svg_clip_pass'] = bool(len(vertices) == 100 and
            np.all(joined >= (100 - 1e-6, 72 - 1e-6)) and
            np.all(joined <= (720 + 1e-6, 534 + 1e-6)))
        results['pass'] = bool(results['geometry_pass'] and results['svg_clip_pass'] and
                               results['png']['pass'] and results['pdf']['pass'])
        report['cases'][str(case)] = results
    (args.output/'report.json').write_text(json.dumps(report, indent=2) + '\n')
    passed = all(case['pass'] for case in report['cases'].values())
    print(json.dumps(report, indent=2))
    raise SystemExit(0 if passed else 1)


if __name__ == '__main__':
    main()

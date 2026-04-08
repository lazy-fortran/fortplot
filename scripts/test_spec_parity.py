#!/usr/bin/env python3
"""Spec parity test: Fortran and Python frontends must produce identical specs.

Builds a set of Vega-Lite specs via both the Fortran builder API and the
Python fortplot API, then compares the parsed JSON structures.
"""

import json
import os
import subprocess
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'python'))

import fortplot


def find_fortplot_app(name):
    """Find a compiled fpm app binary."""
    build_dir = Path(__file__).resolve().parents[1] / 'build'
    if build_dir.exists():
        for compiler_dir in build_dir.iterdir():
            if compiler_dir.is_dir():
                app = compiler_dir / 'app' / name
                if app.exists() and app.is_file():
                    return str(app)
    raise FileNotFoundError(f'{name} not found in {build_dir}')


def normalize(obj):
    """Normalize a parsed JSON value for comparison.

    - Convert all numbers to float so 1 == 1.0
    - Sort dict keys for deterministic comparison
    - Recurse into nested structures
    """
    if isinstance(obj, dict):
        return {k: normalize(v) for k, v in sorted(obj.items())}
    if isinstance(obj, list):
        return [normalize(v) for v in obj]
    if isinstance(obj, (int, float)):
        return float(obj)
    return obj


def compare_specs(fortran_path, python_spec, label):
    """Compare a Fortran-generated JSON file against a Python spec dict."""
    with open(fortran_path, encoding='utf-8') as f:
        fortran_spec = json.load(f)

    fn = normalize(fortran_spec)
    pn = normalize(python_spec)

    if fn != pn:
        print(f'FAIL: {label}', file=sys.stderr)
        print(f'  Fortran: {json.dumps(fn, indent=2)}', file=sys.stderr)
        print(f'  Python:  {json.dumps(pn, indent=2)}', file=sys.stderr)
        return False

    print(f'OK: {label}')
    return True


def build_python_line_spec():
    """Build a line spec matching the Fortran reference."""
    fortplot.figure(figsize=(4, 3), dpi=100)
    fortplot.plot([1, 2, 3, 4, 5], [1, 4, 9, 16, 25])
    fortplot.title('Parity Line')
    fortplot.xlabel('X Axis')
    fortplot.ylabel('Y Axis')

    with tempfile.NamedTemporaryFile(
        suffix='.vl.json', delete=False, mode='w'
    ) as f:
        tmp = f.name
    fortplot.savefig(tmp)

    with open(tmp, encoding='utf-8') as f:
        spec = json.load(f)
    os.unlink(tmp)
    return spec


def build_python_scatter_spec():
    """Build a scatter spec matching the Fortran reference."""
    fortplot.figure(figsize=(4, 3), dpi=100)
    fortplot.scatter([1, 2, 3, 4, 5], [1, 4, 9, 16, 25])
    fortplot.title('Parity Scatter')
    fortplot.xlabel('X Axis')
    fortplot.ylabel('Y Axis')

    with tempfile.NamedTemporaryFile(
        suffix='.vl.json', delete=False, mode='w'
    ) as f:
        tmp = f.name
    fortplot.savefig(tmp)

    with open(tmp, encoding='utf-8') as f:
        spec = json.load(f)
    os.unlink(tmp)
    return spec


def build_python_bar_spec():
    """Build a bar spec matching the Fortran reference."""
    fortplot.figure(figsize=(4, 3), dpi=100)
    fortplot.histogram([1, 2, 3, 4, 5])
    fortplot.title('Parity Bar')
    fortplot.xlabel('X Axis')
    fortplot.ylabel('Y Axis')

    with tempfile.NamedTemporaryFile(
        suffix='.vl.json', delete=False, mode='w'
    ) as f:
        tmp = f.name
    fortplot.savefig(tmp)

    with open(tmp, encoding='utf-8') as f:
        spec = json.load(f)
    os.unlink(tmp)
    return spec


def test_label_emission():
    """Verify that labels are emitted in the Vega-Lite spec."""
    fortplot.figure(figsize=(4, 3), dpi=100)
    fortplot.plot([1, 2, 3], [1, 4, 9], label='quadratic')
    fortplot.plot([1, 2, 3], [1, 8, 27], label='cubic')

    with tempfile.NamedTemporaryFile(
        suffix='.vl.json', delete=False, mode='w'
    ) as f:
        tmp = f.name
    fortplot.savefig(tmp)

    with open(tmp, encoding='utf-8') as f:
        spec = json.load(f)
    os.unlink(tmp)

    if 'layer' not in spec:
        print('FAIL: label emission - no layers in spec', file=sys.stderr)
        return False

    labels_found = []
    for layer in spec['layer']:
        enc = layer.get('encoding', {})
        color = enc.get('color', {})
        if 'value' in color:
            labels_found.append(color['value'])

    if labels_found != ['quadratic', 'cubic']:
        print(
            f'FAIL: label emission - expected [quadratic, cubic], '
            f'got {labels_found}',
            file=sys.stderr,
        )
        return False

    print('OK: label emission')
    return True


def main():
    parity_app = find_fortplot_app('fortplot_spec_parity')

    with tempfile.TemporaryDirectory() as fortran_dir:
        result = subprocess.run(
            [parity_app, fortran_dir],
            capture_output=True, text=True, timeout=30,
        )
        if result.returncode != 0:
            print(f'fortplot_spec_parity failed: {result.stderr}',
                  file=sys.stderr)
            sys.exit(1)

        passed = 0
        failed = 0

        # Line plot parity
        py_line = build_python_line_spec()
        if compare_specs(
            Path(fortran_dir) / 'line.vl.json', py_line, 'line plot'
        ):
            passed += 1
        else:
            failed += 1

        # Scatter plot parity
        py_scatter = build_python_scatter_spec()
        if compare_specs(
            Path(fortran_dir) / 'scatter.vl.json', py_scatter,
            'scatter plot',
        ):
            passed += 1
        else:
            failed += 1

        # Label emission (independent of Fortran structure)
        if test_label_emission():
            passed += 1
        else:
            failed += 1

    print(f'\nSpec parity: {passed} passed, {failed} failed')
    if failed > 0:
        sys.exit(1)


if __name__ == '__main__':
    main()

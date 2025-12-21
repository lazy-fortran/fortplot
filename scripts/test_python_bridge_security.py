#!/usr/bin/env python3
"""
Security regression tests for fortplot_python_bridge stdin handling.

Covers:
- Command length limit enforcement
- Excessive array size rejection for PLOT
- SAVEFIG empty path validation
- `--help`/`-h` argument prints usage and exits

Intended to be fast and run in CI-fast set.
"""

import subprocess
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / 'python'))
from fortplot.fortplot_wrapper import FortplotModule  # type: ignore


def run_bridge():
    exe = FortplotModule()._find_bridge_executable()
    proc = subprocess.Popen(
        [exe],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        bufsize=1,
    )
    return proc

def test_help_argument_exits_quickly():
    exe = FortplotModule()._find_bridge_executable()
    result = subprocess.run(
        [exe, "--help"], stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, timeout=5
    )
    out = result.stdout or ""
    err = result.stderr or ""
    combined = out + err
    assert result.returncode == 0, f"--help should exit 0, got {result.returncode}. Output:\n{combined}"
    assert "fortplot_python_bridge" in combined and "Usage:" in combined, (
        "Help output missing expected markers. Output was:\n" + combined
    )
    print("PASS: --help prints usage and exits")


def test_long_command_rejected():
    proc = run_bridge()
    try:
        long_cmd = 'X' * 200  # exceeds MAX_CMD_LEN=64
        assert proc.stdin is not None
        proc.stdin.write(long_cmd + "\n")
        proc.stdin.flush()
        proc.wait(timeout=5)
        out = proc.stdout.read() if proc.stdout else ''
        err = proc.stderr.read() if proc.stderr else ''
        combined = (out or '') + (err or '')
        assert 'Input line exceeds maximum allowed length' in combined, (
            'Expected length limit error not found. Output was:\n' + combined
        )
        print("PASS: Long command correctly rejected")
    finally:
        if proc.poll() is None:
            proc.terminate()


def test_plot_excessive_array_size_rejected():
    proc = run_bridge()
    try:
        assert proc.stdin is not None
        # Minimal valid startup
        proc.stdin.write('FIGURE\n')
        proc.stdin.write('640 480\n')
        # Send PLOT with excessive n (MAX_INPUT_ARRAY=1_000_000)
        proc.stdin.write('PLOT\n')
        proc.stdin.write(str(1000001) + '\n')
        proc.stdin.write('QUIT\n')
        proc.stdin.flush()
        proc.wait(timeout=5)
        out = proc.stdout.read() if proc.stdout else ''
        err = proc.stderr.read() if proc.stderr else ''
        combined = (out or '') + (err or '')
        assert 'Invalid or excessive array size for PLOT' in combined, (
            'Expected excessive array size error not found. Output was:\n' + combined
        )
        print("PASS: Excessive array size correctly rejected")
    finally:
        if proc.poll() is None:
            proc.terminate()


def test_savefig_empty_path_rejected():
    proc = run_bridge()
    try:
        assert proc.stdin is not None
        proc.stdin.write('FIGURE\n')
        proc.stdin.write('640 480\n')
        proc.stdin.write('SAVEFIG\n')
        proc.stdin.write('\n')  # empty path
        proc.stdin.write('QUIT\n')
        proc.stdin.flush()
        proc.wait(timeout=5)
        out = proc.stdout.read() if proc.stdout else ''
        err = proc.stderr.read() if proc.stderr else ''
        combined = (out or '') + (err or '')
        assert 'Invalid file path in SAVEFIG' in combined, (
            'Expected invalid SAVEFIG path error not found. Output was:\n' + combined
        )
        print("PASS: Empty SAVEFIG path correctly rejected")
    finally:
        if proc.poll() is None:
            proc.terminate()


def main():
    test_help_argument_exits_quickly()
    test_long_command_rejected()
    test_plot_excessive_array_size_rejected()
    test_savefig_empty_path_rejected()
    print('Python bridge security tests passed')


if __name__ == '__main__':
    main()

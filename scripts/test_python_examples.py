#!/usr/bin/env python3
"""Run fortplot-backed Python examples to guard wrapper regressions."""

from __future__ import annotations

import os
import subprocess
import sys
from pathlib import Path


def find_examples(root: Path) -> list[Path]:
    scripts: list[Path] = []
    for script in sorted(root.glob("*/**/*.py")):
        try:
            text = script.read_text(encoding="utf-8")
        except OSError:
            continue
        if "import fortplot" in text:
            scripts.append(script)
    return scripts


def run_example(script: Path, python_root: Path) -> None:
    env = os.environ.copy()
    env.setdefault("FORTPLOT_SUPPRESS_WARNINGS", "1")
    python_path = str(python_root)
    if python_path not in sys.path:
        sys.path.insert(0, python_path)
    existing = env.get("PYTHONPATH")
    if existing:
        env["PYTHONPATH"] = f"{python_path}{os.pathsep}{existing}"
    else:
        env["PYTHONPATH"] = python_path
    subprocess.run([
        sys.executable,
        str(script.name),
    ], check=True, cwd=script.parent, env=env)


def main() -> None:
    repo_root = Path(__file__).resolve().parents[1]
    examples_root = repo_root / "example" / "python"
    python_root = repo_root / "python"
    scripts = find_examples(examples_root)
    if not scripts:
        print("[WARN] No fortplot Python examples discovered", file=sys.stderr)
        return
    for script in scripts:
        print(f"[INFO] Running fortplot example: {script.relative_to(repo_root)}")
        run_example(script, python_root)


if __name__ == "__main__":
    main()

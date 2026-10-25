#!/usr/bin/env python3
"""Reject stale or external source aliases in the fpm packages."""

import os
from pathlib import Path
from tempfile import TemporaryDirectory


def check_links(root):
    errors = []
    checked = 0
    for directory, dirs, files in os.walk(root / "packages", followlinks=False):
        dirs[:] = [name for name in dirs if name not in ("build", ".git")]
        for name in dirs + files:
            path = Path(directory) / name
            if not path.is_symlink():
                continue
            checked += 1
            try:
                if Path(os.readlink(path)).is_absolute():
                    raise ValueError("source alias must be relative")
                target = path.resolve(strict=True)
                target.relative_to(root)
                if not target.is_file():
                    raise ValueError("target is not a source file")
            except (OSError, RuntimeError, ValueError) as error:
                errors.append(f"{path.relative_to(root)}: {error}")
    if not checked:
        errors.append("no package source aliases found")
    return errors


def check_regressions():
    with TemporaryDirectory(prefix="fortplot-package-links-") as directory:
        container = Path(directory).resolve()
        root = container / "repo"
        source = root / "src" / "value.f90"
        source.parent.mkdir(parents=True)
        source.write_text("module value\nend module value\n")
        package = root / "packages" / "sample" / "src"
        package.mkdir(parents=True)
        link = package / "value.f90"
        link.symlink_to("../../../src/value.f90")
        if check_links(root):
            raise RuntimeError("valid relative source alias was rejected")
        source.unlink()
        if not check_links(root):
            raise RuntimeError("dangling source alias was accepted")
        source.write_text("module value\nend module value\n")
        link.unlink()
        link.symlink_to(source)
        if not check_links(root):
            raise RuntimeError("absolute source alias was accepted")
        link.unlink()
        (container / "outside.f90").write_text("module outside\nend module outside\n")
        link.symlink_to("../../../../outside.f90")
        if not check_links(root):
            raise RuntimeError("escaping source alias was accepted")


if __name__ == "__main__":
    check_regressions()
    failures = check_links(Path(__file__).resolve().parents[1])
    for failure in failures:
        print(f"FAIL: {failure}")
    if failures:
        raise SystemExit(1)
    print("PASS: package source aliases resolve to files inside the repository")

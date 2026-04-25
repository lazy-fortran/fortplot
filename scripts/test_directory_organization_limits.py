import os
import sys


def list_items(path):
    try:
        return [name for name in os.listdir(path) if not name.startswith('.')]
    except FileNotFoundError:
        return []


def test_src_subfolder_item_limits():
    # Project guideline: aim <=20 items per folder (hard <=50)
    src_root = os.path.join(os.path.dirname(__file__), "..", "src")
    src_root = os.path.abspath(src_root)

    assert os.path.isdir(src_root), f"src directory missing: {src_root}"

    # Check ALL subfolders under src recursively to prevent drift in deeper trees
    subdirs = []
    for root, dirs, _files in os.walk(src_root):
        for d in dirs:
            subdirs.append(os.path.join(root, d))

    soft_limit = 20
    hard_limit = 50

    violations = []
    hard_violations = []
    for d in subdirs:
        count = len(list_items(d))
        if count > hard_limit:
            hard_violations.append((d, count))
        elif count > soft_limit:
            violations.append((d, count))

    assert not hard_violations, (
        f"Hard limit exceeded (> {hard_limit}) in: "
        + ", ".join(f"{path} ({count})" for path, count in hard_violations)
    )

    # Soft limit is guidance; failing this test encourages keeping folders tidy.
    assert not violations, (
        f"Folder item count exceeds guidance (> {soft_limit}) in: "
        + ", ".join(f"{path} ({count})" for path, count in violations)
    )


def test_output_no_artifacts():
    # Policy: test/output/ must not accumulate runtime artifacts (issue #1707 / #820).
    # Test artifacts belong in build/test/output/. Only .gitkeep is allowed.
    test_output = os.path.join(os.path.dirname(__file__), "..", "test", "output")
    test_output = os.path.abspath(test_output)

    if not os.path.isdir(test_output):
        return  # Directory doesn't exist; nothing to enforce

    artifacts = [name for name in os.listdir(test_output) if not name.startswith('.')]
    assert not artifacts, (
        f"Runtime artifacts found in test/output/ (should be in build/test/output/): "
        + ", ".join(artifacts)
    )


def main() -> int:
    ok = 0
    try:
        test_src_subfolder_item_limits()
    except AssertionError as e:
        print(str(e), file=sys.stderr)
        ok = 1

    try:
        test_output_no_artifacts()
    except AssertionError as e:
        print(str(e), file=sys.stderr)
        ok = 1

    if ok:
        return ok
    print("PASS: src/* subfolder item limits respected (<=20 soft, <=50 hard)")
    print("PASS: test/output/ contains no runtime artifacts")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

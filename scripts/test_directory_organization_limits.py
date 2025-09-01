import os


def list_items(path):
    try:
        return [name for name in os.listdir(path) if not name.startswith('.')]
    except FileNotFoundError:
        return []


def test_src_subfolder_item_limits():
    # Project guideline: aim ≤20 items per folder (hard ≤50)
    src_root = os.path.join(os.path.dirname(__file__), "..", "src")
    src_root = os.path.abspath(src_root)

    assert os.path.isdir(src_root), f"src directory missing: {src_root}"

    # Only check immediate subfolders of src
    subdirs = [
        os.path.join(src_root, d)
        for d in os.listdir(src_root)
        if os.path.isdir(os.path.join(src_root, d))
    ]

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
        "Hard limit exceeded (>",
        hard_limit,
        ") in: "
        + ", ".join(f"{path} ({count})" for path, count in hard_violations)
    )

    # Soft limit is guidance; failing this test encourages keeping folders tidy.
    assert not violations, (
        "Folder item count exceeds guidance (>",
        soft_limit,
        ") in: "
        + ", ".join(f"{path} ({count})" for path, count in violations)
    )


#!/bin/bash

set -euo pipefail

makefile=${1:-Makefile}
tmp=$(mktemp /tmp/fortplot_test_ci_artifact_gate.XXXXXX)
cleanup() {
    rm -f "$tmp"
}
trap cleanup EXIT

awk '
    /^test-ci:/ { in_target = 1; print; next }
    in_target && /^[A-Za-z0-9_.-]+:/ { in_target = 0 }
    in_target && /^[[:space:]]/ {
        line = $0
        sub(/^[[:space:]]+/, "", line)
        while (line ~ /^[@+-]/) {
            line = substr(line, 2)
        }
        sub(/^[[:space:]]+/, "", line)
        sub(/[[:space:]]+#.*$/, "", line)
        if (line !~ /^#/) {
            print line
        }
    }
' "$makefile" > "$tmp"

if ! grep -Eq '(\$\(MAKE\)|make)[[:space:]]+verify-artifacts' "$tmp"; then
    echo "FAIL: test-ci does not invoke verify-artifacts"
    exit 1
fi

echo "PASS: test-ci invokes verify-artifacts"

#!/usr/bin/env bash
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

count=$(find test/ -maxdepth 1 -type f -name "test_*pcolormesh*.f90" ! -name "test_pdf_pcolormesh_inline_image.f90" | wc -l | tr -d ' ')
if [[ "$count" != "1" ]]; then
  echo "FAIL: Expected exactly 1 pcolormesh Fortran test file, found $count" >&2
  echo "List of matching files (pattern: test_*pcolormesh*.f90):" >&2
  find test/ -maxdepth 1 -type f -name "test_*pcolormesh*.f90" ! -name "test_pdf_pcolormesh_inline_image.f90" -print >&2
  exit 1
fi
echo "PASS: pcolormesh test dedup guard (1 Fortran test file)"

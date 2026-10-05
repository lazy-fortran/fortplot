#!/usr/bin/env bash
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

expected_tests=(
  "test/plot_types/2d/test_pcolormesh_cell_geometry.f90"
  "test/plot_types/2d/test_pcolormesh_fast_negative.f90"
  "test/plot_types/2d/test_pcolormesh_rectangular_orientation.f90"
  "test/validation/test_pcolormesh_shading_forwarding.f90"
)
# Portable array fill: macOS ships bash 3.2, which has no mapfile/readarray.
actual_tests=()
while IFS= read -r test_file; do
  actual_tests+=("$test_file")
done < <(
  find test/ -type f -name "test_*pcolormesh*.f90" \
    ! -name "test_pdf_pcolormesh_inline_image.f90" | sort
)

if [[ "${#actual_tests[@]}" != "${#expected_tests[@]}" ]] || \
   ! diff -u <(printf '%s\n' "${expected_tests[@]}") \
            <(printf '%s\n' "${actual_tests[@]}"); then
  echo "FAIL: pcolormesh test manifest changed unexpectedly" >&2
  echo "Expected the distinct orientation, shading, and negative-range tests." >&2
  exit 1
fi
echo "PASS: pcolormesh test manifest (${#actual_tests[@]} distinct tests)"

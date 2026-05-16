#!/bin/bash

set -euo pipefail

repo_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
tmp_root=$(mktemp -d /tmp/fortplot_media_validation.XXXXXX)

cleanup() {
    rm -rf "$tmp_root"
}
trap cleanup EXIT

make_fixture() {
    local root="$1"

    mkdir -p "$root/build/doc/page/examples"
    mkdir -p "$root/build/doc/page/media/examples/basic_plots"
    mkdir -p "$root/build/doc/media/examples/basic_plots"
    mkdir -p "$root/doc/examples"
    mkdir -p "$root/example/fortran/basic_plots"
    mkdir -p "$root/example/fortran/display_demo"
    mkdir -p "$root/output/example/fortran/basic_plots"

    printf '<html></html>\n' > "$root/build/doc/page/examples/basic_plots.html"
    printf '<html></html>\n' > "$root/build/doc/page/examples/display_demo.html"
    printf 'call savefig("output/example/fortran/basic_plots/basic.png")\n' \
        > "$root/example/fortran/basic_plots/basic_plots.f90"
    printf 'call show()\n' > "$root/example/fortran/display_demo/display_demo.f90"
    printf '![plot](../../media/examples/basic_plots/basic.png)\n' \
        > "$root/doc/examples/basic_plots.md"
    printf 'png\n' > "$root/output/example/fortran/basic_plots/basic.png"
    printf 'png\n' > "$root/build/doc/page/media/examples/basic_plots/basic.png"
    printf 'png\n' > "$root/build/doc/media/examples/basic_plots/basic.png"
}

run_validator() {
    local root="$1"

    DOC_BUILD_ROOT="$root/build/doc" \
    DOC_EXAMPLES_ROOT="$root/doc/examples" \
    EXAMPLE_SOURCE_ROOT="$root/example/fortran" \
    EXAMPLE_OUTPUT_ROOT="$root/output/example/fortran" \
    SKIP_MEDIA_EXAMPLES="display_demo" \
    bash "$repo_root/scripts/validate_github_pages_images.sh" >"$tmp_root/validator.out" 2>&1
}

passing="$tmp_root/passing"
make_fixture "$passing"
run_validator "$passing"

missing_page_media="$tmp_root/missing_page_media"
make_fixture "$missing_page_media"
rm "$missing_page_media/build/doc/page/media/examples/basic_plots/basic.png"
if run_validator "$missing_page_media"; then
    echo "FAIL: validator accepted missing page-staged media"
    exit 1
fi

missing_root_media="$tmp_root/missing_root_media"
make_fixture "$missing_root_media"
rm "$missing_root_media/build/doc/media/examples/basic_plots/basic.png"
if run_validator "$missing_root_media"; then
    echo "FAIL: validator accepted missing root-staged media"
    exit 1
fi

missing_output="$tmp_root/missing_output"
make_fixture "$missing_output"
rm "$missing_output/output/example/fortran/basic_plots/basic.png"
if run_validator "$missing_output"; then
    echo "FAIL: validator accepted missing generated media"
    exit 1
fi

missing_output_dir="$tmp_root/missing_output_dir"
make_fixture "$missing_output_dir"
rm -rf "$missing_output_dir/output/example/fortran/basic_plots"
if run_validator "$missing_output_dir"; then
    echo "FAIL: validator accepted missing generated media directory"
    exit 1
fi

echo "PASS: validate_github_pages_images behavior"

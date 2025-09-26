#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <target> [<target>...]" >&2
  exit 2
fi

extra_args=()
if [[ -n "${FPM_FLAGS_TEST:-}" ]]; then
  # shellcheck disable=SC2206
  extra_args=(${FPM_FLAGS_TEST})
fi

mapfile -t available < <(fpm test "${extra_args[@]}" --list 2>&1 \
  | awk '$1 != "Matched" {print $1}')

missing=()
for target in "$@"; do
  if ! printf '%s\n' "${available[@]}" | grep -Fxq "${target}"; then
    missing+=("${target}")
  fi
done

if (( ${#missing[@]} )); then
  echo "Missing fpm test targets:" >&2
  for target in "${missing[@]}"; do
    echo "  ${target}" >&2
  done
  exit 1
fi

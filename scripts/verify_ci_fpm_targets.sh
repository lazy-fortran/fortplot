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

fpm_cmd=(fpm test)
if (( ${#extra_args[@]} > 0 )); then
  fpm_cmd+=("${extra_args[@]}")
fi
fpm_cmd+=(--list)

available=()
while IFS= read -r line; do
  [[ -n "${line}" ]] || continue
  available+=("${line}")
done < <("${fpm_cmd[@]}" 2>&1 \
  | awk '$1 != "Matched" {print $1}')

missing=()
for target in "$@"; do
  found=1
  if (( ${#available[@]} > 0 )); then
    for listed_target in "${available[@]}"; do
      if [[ "${listed_target}" == "${target}" ]]; then
        found=0
        break
      fi
    done
  fi
  if (( found != 0 )); then
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

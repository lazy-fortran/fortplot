#!/usr/bin/env bash
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

count=$(find test/ -maxdepth 1 -name "*pcolormesh*" | wc -l | tr -d ' ')
if [[ "$count" != "1" ]]; then
  echo "FAIL: Expected exactly 1 pcolormesh test file, found $count" >&2
  echo "List of matching files:" >&2
  find test/ -maxdepth 1 -name "*pcolormesh*" -print >&2
  exit 1
fi
echo "PASS: pcolormesh test dedup guard (1 file)"


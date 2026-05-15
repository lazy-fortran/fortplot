#!/usr/bin/env bash
set -euo pipefail
if [[ $# -lt 1 ]]; then
  echo "usage: scripts/pr_merge.sh PR [gh-pr-merge-args...]" >&2
  exit 2
fi
pr="$1"
shift
if [[ $# -eq 0 ]]; then
  set -- --squash
fi
exec gh pr merge "$pr" "$@" --delete-branch

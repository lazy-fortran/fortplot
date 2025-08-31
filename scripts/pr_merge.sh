#!/usr/bin/env bash
# Merge a PR if CI is green and branch up to date
# Usage: scripts/pr_merge.sh <pr_number> [--rebase|--merge|--squash]
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <pr_number> [--rebase|--merge|--squash]" >&2
  exit 2
fi

pr_num="$1"; shift || true
method="--merge"
case "${1:-}" in
  --rebase|--merge|--squash) method="$1" ;;
esac

if ! command -v gh >/dev/null 2>&1; then
  echo "gh CLI not found. Install and run 'gh auth login'." >&2
  exit 1
fi

# Ensure checks are passing (best-effort gate; branch protection is authoritative)
status_line=$(scripts/pr_ci_status.sh "$pr_num" || true)
if [[ -n "$status_line" ]]; then
  conclusion=$(echo "$status_line" | awk '{for(i=1;i<=NF;i++){if($i ~ /^conclusion=/){print substr($i,12)}}}')
  url=$(echo "$status_line" | awk '{for(i=1;i<=NF;i++){if($i ~ /^url=/){print substr($i,5)}}}')
  echo "CI: $conclusion -> $url"
  if [[ "$conclusion" != "success" ]]; then
    echo "CI not successful. Aborting merge." >&2
    exit 1
  fi
else
  echo "Unable to determine CI status. Aborting to be safe." >&2
  exit 1
fi

# Merge with the chosen strategy
gh pr merge "$pr_num" "$method" --delete-branch --admin=false


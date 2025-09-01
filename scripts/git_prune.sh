#!/usr/bin/env bash
set -euo pipefail

# Safe Git branch pruning utility
# - Prunes remote-tracking branches that have been removed from origin
# - Optionally deletes local branches fully merged into main (older than N days)
#
# Usage:
#   scripts/git_prune.sh            # dry-run (prints planned actions)
#   scripts/git_prune.sh --force    # apply actions
#   scripts/git_prune.sh --days 30 --force
#
# Notes:
# - Never touches current branch, 'main', or 'master'
# - Remote deletion is NOT performed by this tool

DAYS=30
FORCE=0

while [[ $# -gt 0 ]]; do
  case "$1" in
    --force)
      FORCE=1
      shift
      ;;
    --days)
      DAYS=${2:-30}
      shift 2
      ;;
    -h|--help)
      sed -n '1,80p' "$0" | sed 's/^# \{0,1\}//'
      exit 0
      ;;
    *)
      echo "Unknown argument: $1" >&2
      exit 2
      ;;
  esac
done

if ! git rev-parse --git-dir >/dev/null 2>&1; then
  echo "ERROR: Not a git repository" >&2
  exit 2
fi

CURRENT_BRANCH=$(git rev-parse --abbrev-ref HEAD)
DEFAULT_MAIN="main"
if git show-ref --verify --quiet refs/heads/$DEFAULT_MAIN; then
  MAIN=$DEFAULT_MAIN
elif git show-ref --verify --quiet refs/heads/master; then
  MAIN="master"
else
  MAIN=$DEFAULT_MAIN
fi

echo "== Git Prune (dry-run: $((1-FORCE))) =="
echo "Current branch: $CURRENT_BRANCH"
echo "Main branch:    $MAIN"
echo "Threshold:      ${DAYS} days"

echo "-- Step 1: Prune remote-tracking branches (origin) --"
if [[ $FORCE -eq 1 ]]; then
  git remote prune origin || true
else
  echo "DRY-RUN: would run 'git remote prune origin'"
fi

echo "-- Step 2: Delete local branches fully merged into $MAIN and older than ${DAYS} days --"
merged_branches=$(git for-each-ref --format='%(refname:short) %(committerdate:iso8601)' refs/heads | \
  while read -r name date; do
    [[ "$name" == "$MAIN" || "$name" == "master" || "$name" == "$CURRENT_BRANCH" ]] && continue
    if git merge-base --is-ancestor "$name" "$MAIN" 2>/dev/null; then
      # Compute age in days
      ts_branch=$(date -d "$date" +%s 2>/dev/null || gdate -d "$date" +%s)
      ts_now=$(date +%s)
      age_days=$(( (ts_now - ts_branch) / 86400 ))
      if [[ $age_days -ge $DAYS ]]; then
        echo "$name"
      fi
    fi
  done)

if [[ -z "$merged_branches" ]]; then
  echo "No eligible local merged branches older than ${DAYS} days."
else
  echo "Eligible branches:"
  printf '  - %s\n' $merged_branches
  if [[ $FORCE -eq 1 ]]; then
    for b in $merged_branches; do
      git branch -D "$b" || true
    done
  else
    echo "DRY-RUN: would delete the above local branches"
  fi
fi

echo "Done."


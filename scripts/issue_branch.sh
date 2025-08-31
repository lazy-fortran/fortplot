#!/usr/bin/env bash
# Create and push a feature branch for a GitHub issue
# Usage: scripts/issue_branch.sh <issue_number>
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <issue_number>" >&2
  exit 2
fi

issue_num="$1"

if [[ "${DRY_RUN:-}" != "1" ]]; then
  if ! command -v gh >/dev/null 2>&1; then
    echo "gh CLI not found. Install and run 'gh auth login'." >&2
    exit 1
  fi
fi

# Ensure repo is clean (skip hard failure in dry-run)
if ! git diff --quiet || ! git diff --cached --quiet; then
  if [[ "${DRY_RUN:-}" == "1" ]]; then
    echo "DRY_RUN: working tree not clean (would abort)." >&2
  else
    echo "Working tree not clean. Commit or stash first." >&2
    exit 1
  fi
fi

if [[ "${DRY_RUN:-}" == "1" ]]; then
  : # no-op
else
  # Sync main
  git fetch origin
  git checkout main
  git pull --rebase origin main
fi

if [[ "${DRY_RUN:-}" == "1" ]]; then
  title=$(gh issue view "$issue_num" --json title --jq '.title' 2>/dev/null || echo "dry-run")
else
  # Derive slug from issue title
  title=$(gh issue view "$issue_num" --json title --jq '.title' || true)
  if [[ -z "${title}" ]]; then
    echo "Failed to get issue title for #${issue_num}" >&2
    exit 1
  fi
fi

slug=$(echo "$title" | tr '[:upper:]' '[:lower:]' | sed -E 's/[^a-z0-9]+/-/g; s/^-+|-+$//g' | cut -c1-40)
branch="fix/issue-${issue_num}-${slug}"

if [[ "${DRY_RUN:-}" == "1" ]]; then
  echo "DRY_RUN branch: $branch"
else
  # Create branch if not existing
  if git rev-parse --verify "$branch" >/dev/null 2>&1; then
    git checkout "$branch"
  else
    git checkout -b "$branch"
  fi
  # Push upstream and set tracking
  git push -u origin "$branch"
  echo "$branch"
fi

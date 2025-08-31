#!/usr/bin/env bash
# Fully autonomous issue loop orchestrator
# - Iterates issues (default: all), per-issue: clean state, relevance-check with auto-close,
#   create branch, attempt auto-fix, commit/push if changes, open draft PR, wait CI, merge or cleanup,
#   then return to clean main and continue.
#
# Usage:
#   scripts/issue_orchestrate_auto.sh [--label <name>|--all] [--limit N]
# Env:
#   TEST_CMD     Optional explicit test command (e.g., "make test-ci")
#   AUTO_FIX_CMD Optional fix command to run on the feature branch; if unset, attempts a no-op
#   MERGE_METHOD One of --merge|--squash|--rebase (default --merge)
set -euo pipefail

label=""     # default all issues
limit=999999
merge_method="--merge"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --label) label="$2"; shift 2;;
    --all) label=""; shift 1;;
    --limit) limit="$2"; shift 2;;
    --merge|--squash|--rebase) merge_method="$1"; shift 1;;
    *) echo "Unknown arg: $1" >&2; exit 2;;
  esac
done

need() { command -v "$1" >/dev/null 2>&1 || { echo "Missing dependency: $1" >&2; exit 1; }; }
need gh

# Clean-state helpers
has_untracked() { [[ -n "$(git ls-files --others --exclude-standard)" ]]; }
has_mods() { ! git diff --quiet || ! git diff --cached --quiet; }
print_dirty() { echo "Working tree has changes:" >&2; git status --short >&2 || true; }

ensure_clean_main() {
  if has_mods || has_untracked; then
    print_dirty
    git reset --hard
    git clean -fd
  fi
  git fetch origin
  local cur
  cur=$(git rev-parse --abbrev-ref HEAD || echo "")
  if [[ "$cur" != "main" ]]; then git checkout main; fi
  git pull --rebase origin main
}

run_tests() {
  # $1: issue_num (for logs)
  local inum="$1"
  if [[ -n "${TEST_CMD:-}" ]]; then
    echo "Running TEST_CMD: $TEST_CMD" >&2
    timeout 15m bash -lc "$TEST_CMD" >"/tmp/auto_issue_${inum}_tests.log" 2>&1
  elif command -v make >/dev/null 2>&1 && grep -qE '^test-ci:' Makefile 2>/dev/null; then
    echo "Running make test-ci" >&2
    timeout 15m make test-ci >"/tmp/auto_issue_${inum}_tests.log" 2>&1
  elif command -v fpm >/dev/null 2>&1; then
    echo "Running fpm test" >&2
    timeout 15m fpm test >"/tmp/auto_issue_${inum}_tests.log" 2>&1
  elif command -v pytest >/dev/null 2>&1; then
    echo "Running pytest -q" >&2
    timeout 15m pytest -q >"/tmp/auto_issue_${inum}_tests.log" 2>&1
  elif command -v npm >/dev/null 2>&1 && [[ -f package.json ]]; then
    echo "Running npm test" >&2
    timeout 15m npm test >"/tmp/auto_issue_${inum}_tests.log" 2>&1
  else
    echo "No known test runner found; treating as pass by configuration." >&2
    return 0
  fi
}

open_pr_if_missing() {
  local inum="$1"
  local existing
  existing=$(gh pr list --search "in:title #$inum in:body #$inum" --json number --jq '.[].number' || true)
  if [[ -z "$existing" ]]; then
    /home/ert/code/prompts/scripts/issue_open_pr.sh "$inum" | tail -n1
  else
    gh pr view "$existing" --json url --jq '.url'
  fi
}

# Fetch issues
if [[ -n "$label" ]]; then
  issues=$(gh issue list --state open --label "$label" --json number --limit 500 --jq '.[].number' || true)
else
  issues=$(gh issue list --state open --json number --limit 500 --jq '.[].number' || true)
fi
if [[ -z "${issues:-}" ]]; then
  echo "No open issues to process." >&2
  exit 0
fi

count=0
for inum in $issues; do
  ensure_clean_main

  # Relevance check with auto-close if obsolete
  if /home/ert/code/prompts/scripts/issue_check_relevance.sh "$inum" --auto-close; then
    :
  else
    rc=$?
    if [[ $rc -eq 11 || $rc -eq 10 ]]; then
      # Closed or obsolete -> skip
      continue
    fi
  fi

  echo "=== [AUTO] Processing issue #$inum ==="

  # Create and switch to branch (also pushes)
  branch=$(/home/ert/code/prompts/scripts/issue_branch.sh "$inum")
  echo "Branch: $branch"

  # Attempt auto-fix if configured
  if [[ -n "${AUTO_FIX_CMD:-}" ]]; then
    echo "Running AUTO_FIX_CMD on $branch: $AUTO_FIX_CMD" >&2
    ISSUE_NUM="$inum" BRANCH="$branch" REPO_PATH="$PWD" bash -lc "$AUTO_FIX_CMD"
  else
    echo "AUTO_FIX_CMD not set; skipping auto-fix step." >&2
  fi

  # Auto-commit any changes (stage specific files only)
  if ! git diff --quiet || ! git diff --cached --quiet || [[ -n "$(git ls-files --others --exclude-standard)" ]]; then
    # Determine changed files
    mapfile -t changed < <(git status --porcelain | awk '{print $2}')
    if (( ${#changed[@]} > 0 )); then
      for f in "${changed[@]}"; do
        git add -- "$f"
      done
      git commit -m "fix: attempt to resolve issue #$inum (auto)"
      git push
    fi
  fi

  # Open draft PR if missing
  pr_url=$(open_pr_if_missing "$inum")
  echo "PR: $pr_url"

  # Wait for CI
  if pr_number=$(gh pr view "$pr_url" --json number --jq '.number' 2>/dev/null); then
    echo "Waiting for CI on PR #$pr_number" >&2
    status_line=$(/home/ert/code/prompts/scripts/pr_ci_status.sh "$pr_number" --watch || true)
    echo "$status_line" >&2
    conclusion=$(echo "$status_line" | awk '{for(i=1;i<=NF;i++){if($i ~ /^conclusion=/){print substr($i,12)}}}')
    if [[ "$conclusion" == "success" ]]; then
      /home/ert/code/prompts/scripts/pr_merge.sh "$pr_number" "$merge_method"
    else
      echo "CI not successful for PR #$pr_number; cleaning up." >&2
      /home/ert/code/prompts/scripts/pr_cleanup.sh "$pr_number"
    fi
  else
    echo "Could not resolve PR number for URL: $pr_url" >&2
  fi

  # Clean back to main
  ensure_clean_main

  ((count++))
  if (( count >= limit )); then break; fi
done

echo "Autonomous issue processing complete (processed $count)."

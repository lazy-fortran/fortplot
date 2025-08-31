#!/usr/bin/env bash
# Orchestrate issue-by-issue workflow for Codex sessions
# - Select issues by label (opt-in), create branch, open PR (draft), and print next steps
# Usage: scripts/issue_loop.sh [--label codex-auto] [--limit 1] [--dry-run]
set -euo pipefail

label="codex-auto"
limit=1
dry_run=0
while [[ $# -gt 0 ]]; do
  case "$1" in
    --label) label="$2"; shift 2;;
    --limit) limit="$2"; shift 2;;
    --all) label=""; shift 1;;
    --dry-run) dry_run=1; shift 1;;
    *) echo "Unknown arg: $1" >&2; exit 2;;
  esac
done

exit 0

if ! command -v gh >/dev/null 2>&1; then
  echo "gh CLI not found. Install and run 'gh auth login'." >&2
  if [[ $dry_run -eq 0 ]]; then
    exit 1
  fi
fi

# --- Clean-state helpers (prompted, conservative) ---
has_untracked() {
  [[ -n "$(git ls-files --others --exclude-standard)" ]]
}

has_mods() {
  ! git diff --quiet || ! git diff --cached --quiet
}

print_dirty_summary() {
  echo "Working tree has changes:" >&2
  git status --short >&2 || true
}

ensure_clean_main_state() {
  local phase="$1" # before|after
  if has_mods || has_untracked; then
    print_dirty_summary
    if [[ "${CLEAN_FORCE:-}" == "1" ]]; then
      ans="y"
    else
      echo
      read -r -p "${phase^^}: Discard all local changes and untracked files, then checkout up-to-date 'main'? [y/N] " ans || ans="n"
    fi
    if [[ "$ans" =~ ^[Yy]$ ]]; then
      git reset --hard
      git clean -fd
    else
      echo "Aborting to avoid destructive cleanup. Set CLEAN_FORCE=1 to auto-clean." >&2
      exit 1
    fi
  fi
  git fetch origin
  # Switch to main, update
  current_branch=$(git rev-parse --abbrev-ref HEAD || echo "")
  if [[ "$current_branch" != "main" ]]; then
    git checkout main
  fi
  git pull --rebase origin main
}

# Pre-loop: ensure clean state on main
if [[ $dry_run -eq 1 ]]; then
  echo "DRY_RUN: would ensure clean 'main' state"
else
  ensure_clean_main_state "before"
fi

# Fetch candidate issues (open, optional label filter)
if [[ $dry_run -eq 1 ]]; then
  # Try to list, but tolerate failure
  if [[ -n "$label" ]]; then
    issues=$(gh issue list --state open --label "$label" --json number --limit 5 --jq '.[].number' 2>/dev/null || true)
  else
    issues=$(gh issue list --state open --json number --limit 5 --jq '.[].number' 2>/dev/null || true)
  fi
else
  if [[ -n "$label" ]]; then
    issues=$(gh issue list --state open --label "$label" --json number,title,labels --limit 500 --jq '.[].number' || true)
  else
    issues=$(gh issue list --state open --json number,title,labels --limit 500 --jq '.[].number' || true)
  fi
fi
if [[ -z "$issues" ]]; then
  if [[ -n "$label" ]]; then
    echo "No open issues with label '$label' found." >&2
  else
    echo "No open issues found." >&2
  fi
  exit 0
fi

count=0
for num in $issues; do
  # Ensure clean state at the start of each iteration
  if [[ $dry_run -eq 1 ]]; then
    echo "DRY_RUN: would ensure clean 'main' state"
  else
    ensure_clean_main_state "before"
  fi
  # Early relevance check; auto-close if clearly obsolete
  echo "Pre-checking relevance of issue #$num..."
  if [[ $dry_run -eq 1 ]]; then
    scripts/issue_check_relevance.sh "$num" >/dev/null 2>&1 || true
  else
    if scripts/issue_check_relevance.sh "$num" --auto-close; then
      # Exit code 0 (relevant) -> proceed; 11 (obsolete & closed) returns non-zero? Script returns 11.
      true
    else
      code=$?
      if [[ $code -eq 11 ]]; then
        echo "Issue #$num closed as obsolete; skipping."
        continue
      elif [[ $code -eq 10 ]]; then
        echo "Issue #$num considered obsolete (not closed); skipping."
        continue
      elif [[ $code -eq 20 ]]; then
        echo "Issue #$num relevance unknown; proceeding to branch/PR."
      else
        echo "Relevance check failed (exit $code); proceeding cautiously."
      fi
    fi
  fi

  # Skip if PR already exists that references this issue
  if gh pr list --search "in:title #$num in:body #$num" --json number --jq '.[].number' | grep -qE '^[0-9]+'; then
    echo "Issue #$num already has an open PR. Skipping."
    continue
  fi

  echo "=== Processing issue #$num ==="
  if [[ $dry_run -eq 1 ]]; then
    branch=$(DRY_RUN=1 scripts/issue_branch.sh "$num" 2>&1 | awk -F': ' '/^DRY_RUN branch: /{val=$2} END{print val}')
    pr_url=$(DRY_RUN=1 scripts/issue_open_pr.sh "$num" 2>&1 | awk 'NF{line=$0} END{print line}')
  else
    branch=$(scripts/issue_branch.sh "$num")
    pr_url=$(scripts/issue_open_pr.sh "$num")
  fi

  echo "Branch: $branch"
  echo "PR: $pr_url"
  echo
  echo "Next steps:"
  echo "- Run: make test-ci (baseline)"
  echo "- Start a Codex session focused on issue #$num"
  echo "- Commit focused changes only; push to $branch"
  echo "- When ready, convert draft PR to ready for review and post Evidence (local logs + CI URL)"
  echo

  # Post-iteration: prompt to return to clean main without untracked files
  if [[ $dry_run -eq 1 ]]; then
    echo "DRY_RUN: would return to clean 'main' state"
  else
    if [[ "${CLEAN_FORCE:-}" == "1" ]]; then
      ensure_clean_main_state "after"
    else
      read -r -p "AFTER: Return to clean 'main' and remove untracked files now? [y/N] " ans || ans="n"
      if [[ "$ans" =~ ^[Yy]$ ]]; then
        ensure_clean_main_state "after"
      else
        echo "Leaving current branch/state as-is per user choice." >&2
      fi
    fi
  fi

  ((count++))
  true
  if (( count >= limit )); then
    break
  fi
done

#!/usr/bin/env bash
# Open a draft PR for current branch linked to an issue
# Usage: scripts/issue_open_pr.sh <issue_number> [--ready]
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <issue_number> [--ready]" >&2
  exit 2
fi

issue_num="$1"; shift || true
is_ready=false
if [[ "${1:-}" == "--ready" ]]; then is_ready=true; fi

if [[ "${DRY_RUN:-}" != "1" ]]; then
  if ! command -v gh >/dev/null 2>&1; then
    echo "gh CLI not found. Install and run 'gh auth login'." >&2
    exit 1
  fi
fi

branch=$(git rev-parse --abbrev-ref HEAD)
if [[ "$branch" == "main" && "${DRY_RUN:-}" != "1" ]]; then
  echo "You are on 'main'. Checkout a feature branch first." >&2
  exit 1
fi

title=$(gh issue view "$issue_num" --json title --jq '.title')
short=$(echo "$title" | sed -E 's/^\s+|\s+$//g' | cut -c1-64)
pr_title="fix: ${short} (fixes #${issue_num})"

# Prepare PR body with evidence placeholders
body=$(cat <<'EOF'
## Summary
- Change: <concise one-liner>
- Scope: <files/modules>

## Evidence
- Local tests: attach or paste `make test-ci` summary
- Coverage: attach `coverage.txt` or `gcovr` summary
- CI URL: <paste Actions run URL>

## Notes
- Risk: <low/med/high>
- Backward-compat: <yes/no>
EOF
)

if [[ "${DRY_RUN:-}" == "1" ]]; then
  echo "DRY_RUN PR title: $pr_title"
  echo "DRY_RUN PR branch: $branch"
  echo "DRY_RUN PR body template emitted"
  echo "https://example.invalid/pr/dry-run"
else
  args=(pr create --title "$pr_title" --body "$body" --base main --head "$branch")
  if [[ "$is_ready" == false ]]; then
    args+=(--draft)
  fi
  if [[ -n "${REVIEWERS:-}" ]]; then
    args+=(--reviewer "$REVIEWERS")
  fi
  url=$(gh "${args[@]}")
  echo "$url"
fi

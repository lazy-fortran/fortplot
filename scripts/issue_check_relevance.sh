#!/usr/bin/env bash
# Determine if an issue is still relevant against current repo state.
# Heuristics:
# - Extract referenced test targets (e.g., test_foo) and run them; failures => relevant.
# - Extract file paths and code identifiers; if none exist anymore and tests (if any) pass => obsolete.
# - Conservative default: unknown if not enough evidence.
#
# Usage:
#   scripts/issue_check_relevance.sh <issue_number> [--auto-close] [--run-tests]
# Exit codes:
#   0  -> relevant
#   10 -> obsolete (not closed)
#   11 -> obsolete and closed (only with --auto-close)
#   20 -> unknown
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <issue_number> [--auto-close] [--run-tests]" >&2
  exit 2
fi

issue_num="$1"; shift || true
auto_close=false
run_tests=false
while [[ $# -gt 0 ]]; do
  case "$1" in
    --auto-close) auto_close=true ;;
    --run-tests) run_tests=true ;;
    *) echo "Unknown arg: $1" >&2; exit 2 ;;
  esac
  shift || true
done

if ! command -v gh >/dev/null 2>&1; then
  echo "gh CLI not found. Install and run 'gh auth login'." >&2
  exit 1
fi

# Fetch issue details
json=$(gh issue view "$issue_num" --json number,title,body,labels,state,url,author,updatedAt,createdAt || true)
if [[ -z "$json" ]]; then
  echo "Failed to fetch issue #$issue_num" >&2
  exit 20
fi

title=$(echo "$json" | jq -r '.title')
body=$(echo "$json" | jq -r '.body // ""')
url=$(echo "$json" | jq -r '.url')

# Extract test targets, file paths, and code identifiers
extract_tests() {
  printf "%s\n" "$title" "$body" | grep -oE 'test_[A-Za-z0-9_]+' | sort -u | tr '\n' ' '
}

extract_paths() {
  printf "%s\n" "$title" "$body" | grep -oE '[A-Za-z0-9_./-]+\.(f90|F90|py|sh|md|toml|ya?ml|cmake|txt)' | sort -u | tr '\n' ' '
}

extract_identifiers() {
  # Inside backticks or common Fortran/Python identifiers
  printf "%s\n" "$title" "$body" | perl -ne 'while(/`([^`]+)`/g){print "$1\n"}' | sed -E 's/\s+/ /g' | sort -u | tr '\n' ' '
}

tests=( $(extract_tests || true) )
paths=( $(extract_paths || true) )
idents=( $(extract_identifiers || true) )

evidence_msgs=()
relevant=false
all_tests_pass=true

# Check referenced file paths
missing_paths=()
present_paths=()
for p in "${paths[@]:-}"; do
  [[ -z "$p" ]] && continue
  if [[ -f "$p" ]]; then
    present_paths+=("$p")
  else
    # Try searching anywhere in repo for renamed/moved
    if rg -n --hidden --glob '!build/**' --glob '!.git/**' --fixed-strings "$p" >/dev/null 2>&1; then
      present_paths+=("$p (referenced string exists)")
    else
      missing_paths+=("$p")
    fi
  fi
done

if (( ${#present_paths[@]} > 0 )); then
  evidence_msgs+=("present_paths: ${present_paths[*]}")
fi
if (( ${#missing_paths[@]} > 0 )); then
  evidence_msgs+=("missing_paths: ${missing_paths[*]}")
fi

# Check identifiers presence
missing_idents=()
present_idents=()
for id in "${idents[@]:-}"; do
  [[ -z "$id" ]] && continue
  if rg -n --hidden --glob '!build/**' --glob '!.git/**' --fixed-strings -- "$id" >/dev/null 2>&1; then
    present_idents+=("$id")
  else
    missing_idents+=("$id")
  fi
done

if (( ${#present_idents[@]} > 0 )); then
  evidence_msgs+=("present_identifiers: ${present_idents[*]}")
fi
if (( ${#missing_idents[@]} > 0 )); then
  evidence_msgs+=("missing_identifiers: ${missing_idents[*]}")
fi

# Run referenced tests if requested or if any tests are detected
if (( ${#tests[@]} > 0 )) || [[ "$run_tests" == true ]]; then
  # Prefer explicit TEST_CMD if provided
  if [[ -n "${TEST_CMD:-}" ]]; then
    echo "Running TEST_CMD for referenced tests: ${TEST_CMD}" >&2
    if timeout 10m bash -lc "$TEST_CMD" >/tmp/issue_check_"$issue_num"_suite.log 2>&1; then
      evidence_msgs+=("test_cmd_pass: ${TEST_CMD}")
    else
      all_tests_pass=false
      relevant=true
      evidence_msgs+=("test_cmd_fail: ${TEST_CMD}")
    fi
  else
    # Try per-target with known frameworks
    for t in "${tests[@]:-}"; do
      [[ -z "$t" ]] && continue
      if command -v fpm >/dev/null 2>&1; then
        echo "Running referenced test target via fpm: $t" >&2
        if timeout 5m fpm test --target "$t" >/tmp/issue_check_"$issue_num"_"$t".log 2>&1; then
          evidence_msgs+=("test_target_pass: $t")
        else
          all_tests_pass=false
          relevant=true
          evidence_msgs+=("test_target_fail: $t (fpm)")
        fi
      elif command -v pytest >/dev/null 2>&1; then
        echo "Running referenced test via pytest -k: $t" >&2
        if timeout 5m pytest -k "$t" >/tmp/issue_check_"$issue_num"_"$t".log 2>&1; then
          evidence_msgs+=("pytest_k_pass: $t")
        else
          all_tests_pass=false
          relevant=true
          evidence_msgs+=("pytest_k_fail: $t")
        fi
      elif command -v npm >/dev/null 2>&1 && [[ -f package.json ]]; then
        echo "Running npm test (no per-target filtering available): $t" >&2
        if timeout 10m npm test --silent >/tmp/issue_check_"$issue_num"_npm.log 2>&1; then
          evidence_msgs+=("npm_test_pass (generic)")
        else
          all_tests_pass=false
          relevant=true
          evidence_msgs+=("npm_test_fail (generic)")
        fi
      else
        echo "No known test runner detected; skipping test execution for $t" >&2
        evidence_msgs+=("test_skipped_no_runner: $t")
      fi
    done
  fi
fi

# Decision logic
decision="unknown"
if [[ "$relevant" == true ]]; then
  decision="relevant"
elif (( ${#paths[@]} == 0 && ${#idents[@]} == 0 && ${#tests[@]} == 0 )); then
  # No concrete references; cannot determine safely
  decision="unknown"
elif (( ${#missing_paths[@]} > 0 )) && (( ${#present_paths[@]} == 0 )) && (( ${#present_idents[@]} == 0 )) && [[ "$all_tests_pass" == true ]]; then
  decision="obsolete"
elif (( ${#present_paths[@]} == 0 )) && (( ${#present_idents[@]} == 0 )) && [[ "$all_tests_pass" == true ]]; then
  decision="obsolete"
else
  decision="unknown"
fi

timestamp=$(date -Iseconds)
echo "ISSUE=#${issue_num} URL=${url} DECISION=${decision} TIME=${timestamp}"
for m in "${evidence_msgs[@]:-}"; do
  echo "EVIDENCE: $m"
done

if [[ "$decision" == "relevant" ]]; then
  exit 0
elif [[ "$decision" == "unknown" ]]; then
  exit 20
else
  # obsolete
  if [[ "$auto_close" == true ]]; then
    comment_body=$(cat <<EOF
Automated relevance check indicates this issue is obsolete in the current codebase.

Evidence (automated):
- Timestamp: ${timestamp}
- Title: ${title}
- Extracted tests: ${tests[*]:-none}
- Extracted file paths: ${paths[*]:-none}
- Missing paths: ${missing_paths[*]:-none}
- Present identifiers: ${present_idents[*]:-none}
- Missing identifiers: ${missing_idents[*]:-none}
- Test targets pass: ${all_tests_pass}

If this assessment is incorrect, please comment with a current reproduction (commands, inputs, versions), and we will reopen.
EOF
)
    gh issue comment "$issue_num" --body "$comment_body" >/dev/null
    gh issue close "$issue_num" >/dev/null
    echo "Closed issue #$issue_num as obsolete"
    exit 11
  else
    exit 10
  fi
fi

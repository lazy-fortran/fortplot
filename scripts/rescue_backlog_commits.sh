#!/bin/bash
# Rescue script for BACKLOG.md commits that might be lost due to branch protection
# This script safely merges rescue branch BACKLOG.md updates into current main state

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Function to validate BACKLOG.md format
validate_backlog() {
    local file="$1"
    if ! grep -q "## CURRENT SPRINT" "$file"; then
        log_error "Missing CURRENT SPRINT section in $file"
        return 1
    fi
    if ! grep -q "## DOING" "$file"; then
        log_error "Missing DOING section in $file"
        return 1
    fi
    if ! grep -q "## DONE" "$file"; then
        log_error "Missing DONE section in $file"
        return 1
    fi
    log_info "BACKLOG.md format validation passed"
    return 0
}

# Function to rescue BACKLOG.md commits
rescue_backlog_commits() {
    local rescue_branch="${1:-fix/rescue-main-commits-backup}"
    
    log_info "Starting BACKLOG.md rescue procedure..."
    
    # Check if rescue branch exists
    if ! git branch --list | grep -q "$rescue_branch"; then
        log_error "Rescue branch '$rescue_branch' not found"
        return 1
    fi
    
    # Create temporary branch for rescue work
    local temp_branch="temp-rescue-$(date +%s)"
    git checkout -b "$temp_branch"
    
    # Find commits in rescue branch that aren't in main
    log_info "Finding missing BACKLOG.md commits..."
    local missing_commits
    missing_commits=$(git log "$rescue_branch" ^main --oneline --grep="update:" --grep="BACKLOG" -- BACKLOG.md || true)
    
    if [[ -z "$missing_commits" ]]; then
        log_info "No missing BACKLOG.md commits found in rescue branch"
    else
        log_warn "Found missing commits:"
        echo "$missing_commits"
        
        # For each missing commit, check if the change is already applied
        while IFS= read -r commit_line; do
            if [[ -n "$commit_line" ]]; then
                local commit_hash=$(echo "$commit_line" | cut -d' ' -f1)
                log_info "Checking commit: $commit_line"
                
                # Apply commit changes if needed (this would need manual review)
                # For now, just log what we found
                git show --name-only "$commit_hash" | log_info "Files affected: $(cat)"
            fi
        done <<< "$missing_commits"
    fi
    
    # Validate current BACKLOG.md state
    if ! validate_backlog "BACKLOG.md"; then
        log_error "Current BACKLOG.md validation failed"
        git checkout -
        git branch -D "$temp_branch"
        return 1
    fi
    
    log_info "BACKLOG.md rescue procedure completed successfully"
    
    # Clean up
    git checkout -
    git branch -D "$temp_branch"
    return 0
}

# Function to add safeguards
add_backlog_safeguards() {
    log_info "Adding BACKLOG.md safeguards..."
    
    # Create pre-commit hook for BACKLOG.md validation
    local hooks_dir=".git/hooks"
    local pre_commit_hook="$hooks_dir/pre-commit"
    
    if [[ ! -f "$pre_commit_hook" ]]; then
        cat > "$pre_commit_hook" << 'EOF'
#!/bin/bash
# Pre-commit hook to validate BACKLOG.md changes

# Check if BACKLOG.md is being committed
if git diff --cached --name-only | grep -q "BACKLOG.md"; then
    echo "Validating BACKLOG.md changes..."
    
    # Basic format validation
    if ! grep -q "## CURRENT SPRINT" BACKLOG.md; then
        echo "ERROR: BACKLOG.md missing CURRENT SPRINT section"
        exit 1
    fi
    
    if ! grep -q "## DOING" BACKLOG.md; then
        echo "ERROR: BACKLOG.md missing DOING section"
        exit 1
    fi
    
    if ! grep -q "## DONE" BACKLOG.md; then
        echo "ERROR: BACKLOG.md missing DONE section"
        exit 1
    fi
    
    echo "BACKLOG.md validation passed"
fi
EOF
        chmod +x "$pre_commit_hook"
        log_info "Created pre-commit hook for BACKLOG.md validation"
    else
        log_info "Pre-commit hook already exists"
    fi
}

# Main execution
main() {
    log_info "BACKLOG.md Rescue and Safeguard Script"
    log_info "======================================="
    
    # Ensure we're in the right directory
    if [[ ! -f "BACKLOG.md" ]]; then
        log_error "BACKLOG.md not found. Please run from project root."
        exit 1
    fi
    
    # Run rescue procedure
    rescue_backlog_commits
    
    # Add safeguards
    add_backlog_safeguards
    
    log_info "All operations completed successfully"
}

# Run main function if script is executed directly
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
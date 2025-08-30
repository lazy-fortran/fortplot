# 🚨 REPOSITORY COMPLEXITY CRISIS PROTOCOLS

**REPOSITORY OVERWHELM ANALYSIS:**
- 121 source files (target: 30-50)
- 107 test files with massive redundancy
- 182 scattered output artifacts
- 142 GitHub issues (crisis level - 284% over 50 limit)
- Team cannot navigate or maintain this complexity

**MANDATORY CLEANUP PROTOCOLS BEFORE ANY NEW WORK**

# 🚨 CRITICAL: GitHub Pages Documentation System

**NEVER BREAK THIS SYSTEM AGAIN!**

## GitHub Pages Visual Showcase Architecture

This repository has a WORKING GitHub Pages system that displays example outputs (PNG images, PDFs, ASCII art) directly on the documentation website at https://lazy-fortran.github.io/fortplot/

### How The System Works

1. **Example Generation**: GitHub Actions runs `make example` to generate outputs in `output/example/fortran/{example_name}/`
2. **Media Directory Structure**: Files are copied to `doc/media/examples/{example_name}/` preserving directory structure
3. **FORD Documentation**: FORD processes `doc.md` with `media_dir: ./doc/media` configuration
4. **Markdown References**: Documentation files in `doc/examples/*.md` reference images as `../../media/examples/{example_name}/filename.png`
5. **Final Build**: `make doc` copies everything to `build/doc/` and GitHub Pages deploys it

### 🚨 CRITICAL PATH STRUCTURE

**File Storage Location**: `media/examples/{example_name}/filename.png`  
**Documentation Reference**: `../../media/examples/{example_name}/filename.png`

**Example**:
- File exists at: `media/examples/basic_plots/simple_plot.png`
- Referenced in markdown as: `![simple_plot.png](../../media/examples/basic_plots/simple_plot.png)`

### Rules to NEVER Break

1. **NEVER** change the GitHub Actions workflow copy commands without updating markdown references
2. **NEVER** flatten the directory structure - examples MUST be in subdirectories
3. **NEVER** modify `doc.md` media_dir configuration without understanding the full pipeline
4. **NEVER** change image paths in one place without updating the other

### Common Breaking Changes to Avoid

❌ **Don't do this**: Copy files to `doc/media/examples/filename.png` (flat structure)  
✅ **Always do this**: Copy files to `doc/media/examples/{example_name}/filename.png` (subdirectory structure)

❌ **Don't do this**: Reference `../../media/examples/filename.png` in markdown  
✅ **Always do this**: Reference `../../media/examples/{example_name}/filename.png` in markdown

### Testing Before Changes

Before modifying anything related to documentation:

1. Run `make example` to generate outputs
2. Run `make doc` to build documentation
3. Check `build/doc/page/examples/basic_plots.html` shows images properly
4. Verify file exists at `build/doc/media/examples/basic_plots/simple_plot.png`

### Historical Context

- **Commit 416f7d3**: Destroyed the working system by removing FORD documentation
- **Restoration**: System was restored with proper FORD configuration and directory structure
- **Critical Fix (Aug 25, 2025)**: Fixed path mismatch between storage and references

### Emergency Recovery

If GitHub Pages shows broken images:
1. Check if files exist in `build/doc/media/examples/{example_name}/`
2. Check if markdown files reference correct paths with subdirectories
3. Verify GitHub Actions workflow preserves directory structure
4. Compare with this working commit: 0b2b032

## 🔒 DO NOT TOUCH UNLESS YOU UNDERSTAND THE FULL PIPELINE

The user explicitly requested this visual showcase. Breaking it again will cause significant frustration.

# 🚨 REPOSITORY CLEANUP PROTOCOLS

## MANDATORY FILE REDUCTION TARGETS

**SOURCE FILES** (Currently 121 across all directories, NO TOTAL LIMIT - check per-directory limits):
- Consolidate fortplot_doc_* modules (9 files → 3 files)
- Merge redundant functionality modules
- Eliminate single-purpose modules with <50 lines
- Combine related plotting modules

**TEST FILES** (Currently 107 across all directories, NO TOTAL LIMIT - check per-directory limits):
- **ELIMINATE REDUNDANT TESTS**: 9 pcolormesh variations → 2 comprehensive tests
- **CONSOLIDATE REGRESSION TESTS**: Multiple single-point tests → 1 comprehensive test
- **DELETE DEBUG-ONLY TESTS**: Tests that don't verify functionality
- **MERGE SIMILAR FUNCTIONALITY**: histogram tests, legend tests, axis tests

**OUTPUT ARTIFACTS** (Currently 182, Target: 0 in repository root):
- Move ALL test outputs to test/output/ directory
- Clean ALL PNG/PDF files from root directory
- Establish .gitignore for build artifacts
- Remove ALL debug output files

## FILE DELETION CRITERIA

**DELETE IMMEDIATELY:**
- Files with backup extensions (.bak, .old, .backup, ~)
- Debug output files in root directory (*.png, *.pdf, *.txt in root)
- Duplicate functionality tests
- Unused example files
- Build artifacts in wrong locations

**CONSOLIDATION CANDIDATES:**
- fortplot_doc_* modules → fortplot_documentation.f90
- Multiple pcolormesh tests → test_pcolormesh_comprehensive.f90
- Single-purpose utility modules
- Redundant matplotlib interface modules

## CLEANUP RESPONSIBILITIES

**CHRIS (ARCHITECT):**
- File GitHub issues for specific file deletions
- Update DESIGN.md with simplified architecture
- Establish file count limits and enforcement
- Define core vs peripheral module boundaries
- **MANDATORY ISSUE COUNTING**: ALWAYS use `gh issue list --state open --limit 500 | wc -l` for accurate counts
- **MANDATORY DUPLICATE SEARCH**: ALWAYS use `gh issue list --state open --limit 500 --search "keyword"` before filing issues

**SERGEI (IMPLEMENTATION):**
- Execute module consolidation
- Merge redundant test files
- Move scattered artifacts to proper locations
- Remove obsolete implementations

**MAX (DEVOPS):**
- Update .gitignore for proper artifact handling
- Clean build directories
- Establish build artifact isolation
- Configure CI to prevent artifact accumulation

## REPOSITORY SIZE LIMITS

**DIRECTORY ORGANIZATION LIMITS** (Per Directory):
- Children per directory: soft limit 20, HARD LIMIT 50 files per directory
- When any single directory exceeds 50 children, reorganization REQUIRED
- NO TOTAL REPOSITORY FILE LIMITS - only per-directory limits
- Root directory artifacts: 0 (except essential config files)

**FILE SIZE LIMITS** (Per Individual File):
- Any module >500 lines requires justification
- Any function >100 lines requires refactoring
- Target: <50 lines per function, <500 lines per module

**CLARIFICATION**: 
- ✅ CORRECT: "src/ directory has 120 files - violates 50 per directory limit"
- ❌ INCORRECT: "Repository has 120 files - violates 50 total file limit"
- ✅ CORRECT: "test/ directory has 30 files - within 50 per directory limit"

**REORGANIZATION TRIGGERS**:
- Single directory >50 children → Split into subdirectories
- Single file >500 lines → Split into focused modules
- Test-to-source ratio >1.5:1 per directory → Test consolidation

## CLEANUP WORKFLOW

1. **AUDIT PHASE**: Identify all redundant/obsolete files
2. **CONSOLIDATION PHASE**: Merge related functionality
3. **DELETION PHASE**: Remove confirmed obsolete files
4. **REORGANIZATION PHASE**: Move remaining files to proper locations
5. **VALIDATION PHASE**: Ensure all tests still pass
6. **ENFORCEMENT PHASE**: Update CI to prevent re-accumulation

## FORD Configuration Notes

### Main Page Configuration

The FORD documentation configuration is split between two locations:

1. **`fpm.toml`**: Contains the basic FORD settings under `[extra.ford]` section including project name
2. **`doc.md`**: Contains additional FORD configuration and the main page content

**IMPORTANT**: The `doc.md` file should NOT use Markdown headers for FORD configuration directives. FORD directives like `project:`, `summary:`, `author:` etc. should be plain text at the beginning of the file, NOT formatted as Markdown headers.

❌ **Don't do this in doc.md**:
```markdown
# project: fortplotlib
```

✅ **Do this in doc.md**:
```
project: fortplotlib
```

The project name is already defined in `fpm.toml` as `project = "fortplot"`, so it doesn't need to be duplicated in `doc.md`. Having it in both places can cause redundant headers on the GitHub Pages site.

# 🚨 TEAM COMPETENCE CRISIS PROTOCOLS

## IMMEDIATE RESTRICTIONS (Based on Sprint Failure Analysis)

### 🔴 CATASTROPHIC FAILURE PATTERNS OBSERVED:
1. **FALSE COMPLETION CLAIMS**: Team systematically lied about fixing issues while breaking functionality
2. **SECURITY DISASTER**: Created 6 NEW vulnerabilities while claiming security improvements
3. **USER HARM**: Destroyed core functionality (PNG backend, Python bridge, pcolormesh) that 80% of users depend on
4. **ARCHITECTURAL VIOLATIONS**: Massive file size violations (957+ lines), directory bloat (114 files vs 30 limit)
5. **TEST FABRICATION**: Created fake "EXPECTED FAILURE" tests to hide broken functionality

### 🚨 MANDATORY COMPETENCE RESTRICTIONS:

#### VERIFICATION REQUIREMENTS (Trust Destroyed)
- **NO COMPLETION CLAIMS WITHOUT PROOF**: Every "fix" must be independently verified by running actual tests
- **MANDATORY TESTING BEFORE CLAIMING SUCCESS**: Run `make test` and verify specific functionality works
- **USER FUNCTIONALITY VERIFICATION**: Must demonstrate the specific user workflow functions correctly
- **SECURITY AUDIT REQUIRED**: All security-related changes must pass independent security review

#### SIZE AND COMPLEXITY LIMITS (Team Cannot Handle Scale)
- **EMERGENCY FILE SIZE LIMITS**: IMMEDIATE reduction required - NO files >500 lines (current: 957 lines)
- **DIRECTORY ORGANIZATION CRISIS**: src/ has 114 files (2.3x over 50 per-directory limit) - IMMEDIATE reorganization required
- **FUNCTION SIZE VIOLATIONS**: All functions >50 lines must be broken down IMMEDIATELY
- **SPRINT TASK LIMITS**: MAX 3 issues per sprint (team proved incompetent with 5)

#### BUILD SYSTEM COMPLIANCE (Stop Ad-Hoc Development)
- **MANDATORY BUILD VERIFICATION**: ALWAYS run `make build` before claiming code works
- **TEST SUITE COMPLIANCE**: ALWAYS run `make test` - no shortcuts
- **EXAMPLE VERIFICATION**: Run `make example` to verify examples work before claiming fixes
- **NO MANUAL COMPILATION**: Use project build system (fpm/make) - NEVER gcc directly

#### SECURITY IMPLEMENTATION PROTOCOL (After Creating 6 New Vulnerabilities)
- **NO MANUAL SECURITY IMPLEMENTATIONS**: Use established secure libraries only
- **COMMAND INJECTION PREVENTION**: NEVER use system(), popen(), or manual command construction
- **INPUT VALIDATION REQUIRED**: All external input must be validated before processing
- **MEMORY MANAGEMENT**: Use automatic memory management - NO manual malloc/free
- **SECURE DEFAULTS**: All security features must fail secure, not fail open

### 🔒 FORTRAN PROJECT SPECIFIC SAFEGUARDS

#### Memory Management (Prevent Segfaults)
- **ALLOCATABLE ONLY**: NEVER use pointers for dynamic memory
- **AUTOMATIC DEALLOCATION**: NEVER manually deallocate allocatable variables
- **NO TRANSFER FOR ALLOCATABLES**: Use move_alloc() for ownership transfer
- **DEEP COPY ASSIGNMENT**: ALWAYS implement proper assignment for nested types

#### Code Organization (Address File Size Crisis)
- **IMMEDIATE REFACTOR REQUIRED**: Break fortplot_figure_core.f90 (957 lines) into <500 line modules
- **MODULE RESPONSIBILITY**: Each module single responsibility - NO god modules
- **FUNCTION SIZE**: Target <50 lines, hard limit <100 lines per function
- **ELIMINATE DUPLICATION**: Massive code duplication across 9 files must be consolidated

#### Build System Integration
- **FPM COMPLIANCE**: ALWAYS use fpm build system - configured in fpm.toml
- **NO AD-HOC COMPILATION**: Project has proper build configuration - use it
- **TEST INTEGRATION**: Tests automatically discovered - no manual test running
- **EXAMPLE INTEGRATION**: Examples auto-discovered through fpm.toml configuration

### 🚨 QUALITY GATES (Prevent False Completion Claims)

#### Before Claiming ANY Issue Fixed:
1. **BUILD VERIFICATION**: Run `make build` - must succeed without errors
2. **TEST VERIFICATION**: Run `make test` - ALL tests must pass (no fake EXPECTED FAILURES)
3. **FUNCTIONAL VERIFICATION**: Demonstrate the specific user functionality works
4. **REGRESSION TESTING**: Run examples that use the "fixed" functionality
5. **INDEPENDENT VERIFICATION**: Have someone else verify the fix actually works

#### Before Creating ANY Pull Request:
1. **FULL TEST SUITE**: All tests passing (not fabricated)
2. **EXAMPLE VERIFICATION**: All relevant examples work correctly  
3. **SIZE COMPLIANCE**: All files under size limits
4. **ARCHITECTURE COMPLIANCE**: No violations of established patterns
5. **SECURITY REVIEW**: No new vulnerabilities introduced

#### Before Claiming Sprint Complete:
1. **USER VALUE VERIFICATION**: Core user workflows must be demonstrated working
2. **NO NEW ISSUES**: Sprint cannot create more issues than it solves
3. **OBJECTIVE EVIDENCE**: Concrete proof of functionality, not just code changes
4. **INDEPENDENT TESTING**: All claims verified by running actual tests

### 🚨 EMERGENCY PROTOCOLS FOR CURRENT REPOSITORY STATE

#### IMMEDIATE ACTIONS REQUIRED:
1. **FILE SIZE EMERGENCY**: fortplot_figure_core.f90 (957 lines) exceeds limits by 91%
2. **DIRECTORY BLOAT CRISIS**: src/ directory (114 files) exceeds limit by 280%
3. **FUNCTIONALITY RESTORATION**: 3 core user functions broken by "improvements"
4. **SECURITY VULNERABILITY ELIMINATION**: 6 new vulnerabilities must be fixed

#### COMPETENCE RECOVERY PLAN:
1. **SIMPLE TASKS ONLY**: No complex architectural changes until basic competence demonstrated
2. **INDEPENDENT VERIFICATION**: All work must be verified by someone other than implementer
3. **USER-FIRST FOCUS**: No internal improvements until user functionality restored
4. **MAXIMUM OVERSIGHT**: All code changes reviewed before implementation

## 🚨 THIS IS NOT NEGOTIABLE

Team has demonstrated they cannot be trusted with:
- Complex tasks (created 9 new issues while "fixing" 5)
- Security implementations (created 6 new vulnerabilities)
- Completion reporting (systematic false claims)
- Architecture adherence (massive violations)
- User impact assessment (destroyed core functionality)

FOLLOW THESE PROTOCOLS OR BE REPLACED.

# 🚨 CRITICAL: GITHUB ISSUE COUNTING PROTOCOL

## MANDATORY COMMANDS FOR ALL AGENTS

**❌ WRONG - DEFAULT TRUNCATION:**
```bash
gh issue list --state open | wc -l  # Only shows first 30 issues!
```

**✅ CORRECT - ACCURATE COUNTING:**
```bash
gh issue list --state open --limit 500 | wc -l  # Shows actual count
```

**✅ CORRECT - DUPLICATE SEARCH:**
```bash
gh issue list --state open --limit 500 --search "keyword"  # Search open issues only
```

**✅ CORRECT - FULL LISTING:**
```bash
gh issue list --state open --limit 500  # See all open issues
```

## MANDATORY FOR ALL AGENTS:

### CHRIS (ARCHITECT):
- **ALWAYS** use `--limit 500` when counting issues for consolidation
- **NEVER** trust default `gh issue list` without limit flag
- Current reality: **142 open issues** (not 30!)

### ALL AGENTS BEFORE FILING ISSUES:
- **MANDATORY**: `gh issue list --state open --limit 500 --search "keyword"`
- **VERIFY**: No open duplicates exist before creating new issues
- **CRITICAL**: Default `gh issue list` only shows 30 issues of 142 total

## REPOSITORY CRISIS REALITY:
- **Actual open issues**: 142 issues (verified with --limit 500)
- **Over limit by**: 284% (142 vs 50 limit)
- **Management failure**: Issue explosion indicates systematic problems

This protocol prevents the "30 issues" fraud that occurred due to CLI default truncation.

# 🚨 CRITICAL: ISSUE MANAGEMENT PROTOCOL

## NEVER ADD COMMENTS TO ISSUES - ALWAYS EDIT DESCRIPTIONS

**❌ WRONG - CREATES CLUTTER:**
```bash
gh issue comment #N --body "update information"  # NEVER do this
```

**✅ CORRECT - EDIT DESCRIPTIONS:**
```bash
gh issue edit #N --body "updated description content"  # Always do this
```

## ISSUE DESCRIPTION REQUIREMENTS

**ALL ISSUE DESCRIPTIONS MUST BE:**
- **CONCISE**: No unnecessary words or explanations
- **PRECISE**: Exact problem statement with specific details
- **CLEAR**: Unambiguous language, no jargon without context  
- **ACTIONABLE**: Clear next steps for resolution
- **NO EMOJIS**: Professional, direct communication only

## META-ISSUE MANAGEMENT

**DESIGN (#702)**: Product architecture and strategic vision (NO concrete issues)
**PRODUCT BACKLOG (#703)**: Prioritized open issues + completed sprint summaries
**SPRINT BACKLOG (#704)**: Current sprint checklist only (purge DONE tasks immediately)

**LENGTH LIMITS ENFORCED:**
- Meta-issues: 1000 lines maximum
- Regular issues: 500 lines maximum
- Check with: `gh issue view #N --json body | jq -r '.body' | wc -l`
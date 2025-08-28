# 🚨 CATASTROPHIC TEAM FAILURE RECOVERY PROTOCOL

**SPRINT JUST FAILED: 0% SUCCESS RATE, 9 NEW CRITICAL ISSUES CREATED**

Team systematically failed basic tasks while claiming completion. IMMEDIATE COMPETENCE RESTRICTIONS REQUIRED.

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
- **DIRECTORY ORGANIZATION CRISIS**: src/ has 114 files (4x over limit) - IMMEDIATE consolidation required
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
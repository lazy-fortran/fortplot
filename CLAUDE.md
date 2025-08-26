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
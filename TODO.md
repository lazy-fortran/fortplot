# Unicode Support Status - December 2024

## Current Issues

### PDF Backend
- ❌ **PDF shows "omega" literals instead of Greek symbols**
- ❌ **Helvetica font doesn't support Unicode characters**
- ❌ **Need proper Unicode font embedding for PDF**

### PNG Backend  
- ✅ **PNG properly renders Unicode characters via STB TrueType**
- ✅ **Shows actual α, β, γ characters correctly**

## Problem Analysis

The current PDF approach converts Unicode → ASCII names ("alpha", "omega") because:
1. **Helvetica font limitation**: Standard PDF fonts (Helvetica, Times, Arial) don't include Greek characters
2. **Font embedding required**: To show actual Unicode in PDF, we need to embed a Unicode-capable font
3. **PDF standard limitations**: PDF/A compliance and viewer compatibility issues

## Proposed Solutions

### Option 1: Embed Unicode Font in PDF (RECOMMENDED)
- **Embed DejaVu Sans** or **Liberation Sans** in PDF structure
- Use embedded font for Unicode text, Helvetica for ASCII
- Maintains vector quality and searchability
- More complex implementation but professional result

### Option 2: Convert to Font-Specific Symbols
- Map Greek letters to Symbol font encoding (φ → Symbol font φ)
- Use PDF's Symbol font which has Greek characters
- Simpler than embedding, maintains vector quality
- Limited to Symbol font character set

### Option 3: Keep Current ASCII Approach 
- Document as "readable ASCII equivalents"
- Simpler implementation, guaranteed compatibility
- Less aesthetically pleasing

## Implementation Priority

1. **High**: Investigate PDF Symbol font approach for Greek letters
2. **Medium**: Research DejaVu Sans embedding for full Unicode support  
3. **Low**: Document current ASCII approach as feature

## Technical Requirements

- Maintain backward compatibility with Helvetica default
- Ensure PDF/A compliance if possible
- Keep file size reasonable
- Support both embedded and system fonts

## Previous Investigation Notes

> commit and push. However, the PDF in my MacOS Preview shows garbled characters, and the PNG still shows the replacement "omega" literals etc

⏺ I need to investigate the issues you're seeing. Let me first check the current PNG output to see if it's actually showing the replacement literals:

⏺ Read(example/fortran/unicode_demo/unicode_demo.png)
  ⎿  Read image (67.6KB)

⏺ You're absolutely right! The PNG is still showing the ASCII replacement strings like "omega", "alpha", "beta", etc. instead of the actual Unicode characters. This means the LaTeX
  to Unicode conversion isn't working properly. Let me check the actual Unicode demo source to see what text is being processed:

⏺ Read(example/fortran/unicode_demo/unicode_demo.f90)
  ⎿  Read 50 lines (ctrl+r to expand)

⏺ I can see the issue! The demo is using LaTeX commands like \psi, \omega, \alpha, etc. These should be converted to Unicode characters, but it looks like the LaTeX to Unicode
  conversion is not working properly. Let me check what's happening with the LaTeX parser:

⏺ Bash(make test ARGS="--target test_latex_to_unicode_mapping" 2>&1 | grep -A 10 -B 10 "alpha\|beta\|gamma\|omega\|psi")
  ⎿  Error: Error

⏺ Let me trace through the text processing to see where the LaTeX to Unicode conversion is happening:

⏺ Search(pattern: "process_latex_in_text", path: "src")
  ⎿  Found 4 files (ctrl+r to expand)

⏺ The LaTeX to Unicode conversion is happening in process_latex_in_text, but notice that the PNG backend (fortplot_png.f90) is not using this function. Let me check the PNG backend
  text rendering:

⏺ Search(pattern: "process_latex_in_text", path: "src/fortplot_png.f90")

**FIXED**: PNG Unicode rendering now works correctly. PDF needs Symbol font or font embedding.
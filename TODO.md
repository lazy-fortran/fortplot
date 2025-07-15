# Unicode and Math Mode Support - TDD Plan

## Overview
Implement Unicode letter support and basic math mode with LaTeX-style Greek letter commands using strict Test-Driven Development.

## TDD Implementation Checklist

### Phase 1: Unicode Character Support Tests
- [ ] **Test Unicode character detection**
  - [ ] Write test for identifying Unicode characters in strings
  - [ ] Write test for UTF-8 byte sequence validation
  - [ ] Write test for character width calculation
  - [ ] Implement minimal code to pass tests

- [ ] **Test Unicode rendering in ASCII backend**
  - [ ] Write test for rendering Unicode box-drawing characters
  - [ ] Write test for rendering Greek letters directly
  - [ ] Write test for fallback to ASCII when Unicode unavailable
  - [ ] Implement Unicode rendering in ASCII backend

- [ ] **Test Unicode rendering in PNG backend**
  - [ ] Write test for glyph lookup in font files
  - [ ] Write test for Unicode codepoint to glyph mapping
  - [ ] Write test for rendering non-ASCII characters
  - [ ] Implement Unicode support in PNG backend

- [ ] **Test Unicode rendering in PDF backend**
  - [ ] Write test for PDF Unicode encoding
  - [ ] Write test for font embedding with Unicode
  - [ ] Write test for text positioning with variable-width chars
  - [ ] Implement Unicode support in PDF backend

### Phase 2: Math Mode Parser Tests
- [ ] **Test math mode delimiter detection**
  - [ ] Write test for finding $ delimiters in text
  - [ ] Write test for paired delimiter validation
  - [ ] Write test for nested delimiter handling
  - [ ] Write test for escaped dollar signs (\$)
  - [ ] Implement math mode delimiter parser

- [ ] **Test math mode text extraction**
  - [ ] Write test for extracting content between delimiters
  - [ ] Write test for multiple math expressions in one string
  - [ ] Write test for mixed normal and math text
  - [ ] Implement math mode content extraction

### Phase 3: LaTeX Command Parser Tests
- [ ] **Test LaTeX command recognition**
  - [ ] Write test for \alpha command recognition
  - [ ] Write test for all lowercase Greek letters
  - [ ] Write test for all uppercase Greek letters
  - [ ] Write test for invalid command handling
  - [ ] Implement LaTeX command parser

- [ ] **Test LaTeX to Unicode mapping**
  - [ ] Write test for \alpha → α conversion
  - [ ] Write test for complete Greek alphabet mapping
  - [ ] Write test for preserving non-command text
  - [ ] Write test for multiple commands in sequence
  - [ ] Implement LaTeX to Unicode converter

### Phase 4: Integration Tests
- [ ] **Test text rendering with math mode**
  - [ ] Write test for axis labels with Greek letters
  - [ ] Write test for plot titles with math expressions
  - [ ] Write test for legend entries with Unicode
  - [ ] Write test for mixed ASCII/Unicode rendering

- [ ] **Test backend-specific rendering**
  - [ ] Write test for ASCII backend math rendering
  - [ ] Write test for PNG backend math rendering
  - [ ] Write test for PDF backend math rendering
  - [ ] Write test for consistent rendering across backends

### Phase 5: Edge Cases and Error Handling Tests
- [ ] **Test malformed input handling**
  - [ ] Write test for unclosed math delimiters
  - [ ] Write test for invalid LaTeX commands
  - [ ] Write test for unsupported Unicode characters
  - [ ] Write test for empty math expressions

- [ ] **Test performance with Unicode**
  - [ ] Write test for large Unicode strings
  - [ ] Write test for many math expressions
  - [ ] Write test for complex mixed content

## Test File Structure
```
test/
├── test_unicode_detection.f90
├── test_unicode_rendering_ascii.f90
├── test_unicode_rendering_png.f90
├── test_unicode_rendering_pdf.f90
├── test_math_mode_parser.f90
├── test_latex_command_parser.f90
├── test_latex_unicode_mapping.f90
├── test_math_integration.f90
└── test_unicode_edge_cases.f90
```

## Implementation Modules
```
src/
├── fortplot_unicode.f90         # Unicode utilities
├── fortplot_math_parser.f90     # Math mode parser
├── fortplot_latex_parser.f90    # LaTeX command parser
└── fortplot_greek_letters.f90   # Greek letter mappings
```

## Greek Letter Mapping Table
| LaTeX Command | Unicode Character | UTF-8 Bytes |
|--------------|-------------------|-------------|
| \alpha       | α                 | CE B1       |
| \beta        | β                 | CE B2       |
| \gamma       | γ                 | CE B3       |
| \delta       | δ                 | CE B4       |
| \epsilon     | ε                 | CE B5       |
| \zeta        | ζ                 | CE B6       |
| \eta         | η                 | CE B7       |
| \theta       | θ                 | CE B8       |
| \iota        | ι                 | CE B9       |
| \kappa       | κ                 | CE BA       |
| \lambda      | λ                 | CE BB       |
| \mu          | μ                 | CE BC       |
| \nu          | ν                 | CE BD       |
| \xi          | ξ                 | CE BE       |
| \omicron     | ο                 | CE BF       |
| \pi          | π                 | CF 80       |
| \rho         | ρ                 | CF 81       |
| \sigma       | σ                 | CF 83       |
| \tau         | τ                 | CF 84       |
| \upsilon     | υ                 | CF 85       |
| \phi         | φ                 | CF 86       |
| \chi         | χ                 | CF 87       |
| \psi         | ψ                 | CF 88       |
| \omega       | ω                 | CF 89       |

## Example Usage After Implementation
```fortran
call fig%set_xlabel("Time ($\mu$s)")
call fig%set_ylabel("Amplitude ($\beta$)")
call fig%set_title("$\alpha$ = 2$\pi$ rad")
call fig%add_plot(x, y, label="$\sin(\omega t)$")
```

## Success Criteria
- [ ] All tests pass before implementation
- [ ] Each test drives minimal implementation
- [ ] No code without failing test first
- [ ] Full Unicode support in all backends
- [ ] Math mode parsing works correctly
- [ ] All Greek letters render properly
- [ ] Performance remains acceptable
- [ ] Examples demonstrate all features
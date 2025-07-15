# Unicode and LaTeX Support in fortplotlib

## Overview

fortplotlib provides comprehensive support for Unicode characters and LaTeX-style Greek letter commands across all backends (PNG, PDF, ASCII). This enables professional scientific plotting with mathematical notation directly from Fortran code.

## Features

### LaTeX-Style Greek Letters

All 24 Greek letters are supported in both uppercase and lowercase forms:

#### Lowercase Greek Letters
- `\alpha` → α, `\beta` → β, `\gamma` → γ, `\delta` → δ
- `\epsilon` → ε, `\zeta` → ζ, `\eta` → η, `\theta` → θ
- `\iota` → ι, `\kappa` → κ, `\lambda` → λ, `\mu` → μ
- `\nu` → ν, `\xi` → ξ, `\omicron` → ο, `\pi` → π
- `\rho` → ρ, `\sigma` → σ, `\tau` → τ, `\upsilon` → υ
- `\phi` → φ, `\chi` → χ, `\psi` → ψ, `\omega` → ω

#### Uppercase Greek Letters
- `\Alpha` → Α, `\Beta` → Β, `\Gamma` → Γ, `\Delta` → Δ
- `\Epsilon` → Ε, `\Zeta` → Ζ, `\Eta` → Η, `\Theta` → Θ
- `\Iota` → Ι, `\Kappa` → Κ, `\Lambda` → Λ, `\Mu` → Μ
- `\Nu` → Ν, `\Xi` → Ξ, `\Omicron` → Ο, `\Pi` → Π
- `\Rho` → Ρ, `\Sigma` → Σ, `\Tau` → Τ, `\Upsilon` → Υ
- `\Phi` → Φ, `\Chi` → Χ, `\Psi` → Ψ, `\Omega` → Ω

### Backend-Specific Rendering

#### ASCII Backend
- Renders actual Unicode characters in terminal output
- Compatible with modern terminals that support UTF-8
- Fallback handling for unsupported characters

#### PNG Backend
- High-quality antialiased Unicode rendering via STB TrueType
- Professional font rendering with proper spacing
- Supports all Unicode characters available in system fonts

#### PDF Backend
- Vector Unicode characters for scalable, professional output
- Embedded in PDF for universal compatibility
- Maintains crisp rendering at any zoom level

## Usage Examples

### Basic Usage

```fortran
use fortplot
type(figure_t) :: fig

call fig%initialize()
call fig%set_title("Schrödinger Equation: i\hbar\partial\psi/\partial t = H\psi")
call fig%set_xlabel("Position \xi")
call fig%set_ylabel("Wavefunction \Psi(\xi)")
call fig%add_plot(x, psi_real, label="Re[\psi]")
call fig%add_plot(x, psi_imag, label="Im[\psi]")
call fig%legend()

! Works with all backends
call fig%savefig("schrodinger.png")  ! PNG with Unicode
call fig%savefig("schrodinger.pdf")  ! PDF with Unicode
call fig%savefig("schrodinger.txt")  ! ASCII with Unicode
```

### Common Mathematical Expressions

```fortran
! Wave equations
call fig%set_title("Wave: \psi = A e^{i(\omega t - kx)}")

! Thermodynamics
call fig%set_xlabel("Temperature T (K)")
call fig%set_ylabel("Entropy \Delta S")

! Electromagnetic fields
call fig%add_plot(time, electric_field, label="E-field: \epsilon E")
call fig%add_plot(time, magnetic_field, label="B-field: \mu B")

! Statistical mechanics
call fig%set_title("Distribution: P(\xi) = e^{-\beta H(\xi)}/Z")
```

### Complex Mathematical Notation

```fortran
! Maxwell's equations
call fig%set_title("Maxwell: \nabla \times E = -\partial B/\partial t")

! Quantum mechanics
call fig%set_xlabel("Momentum p = \hbar k")
call fig%set_ylabel("Energy E = \hbar \omega")

! Statistical physics
call fig%add_plot(temp, entropy, label="S = k_B ln(\Omega)")
```

## Implementation Details

### Text Processing Pipeline

1. **LaTeX Recognition**: Identifies `\command` patterns in text
2. **Command Validation**: Checks against supported Greek letter commands
3. **Unicode Conversion**: Maps LaTeX commands to Unicode codepoints
4. **UTF-8 Encoding**: Converts Unicode codepoints to UTF-8 byte sequences
5. **Backend Rendering**: Renders Unicode characters using backend-specific methods

### Architecture

The Unicode support is implemented through several modules:

- **`fortplot_latex_parser`**: LaTeX command recognition and parsing
- **`fortplot_unicode`**: Unicode detection and UTF-8 handling
- **Backend Integration**: Unified text processing across all backends

### Performance Considerations

- LaTeX processing is performed at render time, not during plot data operations
- Unicode conversion uses efficient lookup tables
- Minimal overhead for text without LaTeX commands
- Font rendering leverages optimized STB TrueType library

## Limitations and Known Issues

### Supported Commands
- Currently supports Greek letters only
- Mathematical operators (`\nabla`, `\partial`, etc.) are preserved as-is
- Subscripts and superscripts are not yet supported

### Font Dependencies
- PNG/PDF backends require system fonts with Unicode support
- ASCII backend depends on terminal UTF-8 support
- Fallback to ASCII representation when Unicode is unavailable

### Character Encoding
- Input text should be UTF-8 compatible
- Mixed encoding may produce unexpected results

## Testing

Comprehensive test coverage includes:

- All 24 Greek letters (uppercase and lowercase)
- Edge cases: empty strings, invalid commands, mixed content
- Cross-backend compatibility testing
- Unicode character validation
- LaTeX command parsing accuracy

Run Unicode tests:
```bash
make test ARGS="--target test_comprehensive_unicode_coverage"
make test ARGS="--target test_unicode_png_pdf_rendering"
make test ARGS="--target test_unicode_ascii_rendering_simple"
```

## Examples

See the complete working example:
```bash
make example ARGS="unicode_demo"
```

This generates demonstration plots showing Unicode rendering across all backends with comprehensive mathematical notation examples.

## Future Enhancements

Potential future additions:
- Mathematical operators (`\nabla`, `\partial`, `\int`, etc.)
- Subscripts and superscripts
- Additional symbol sets (arrows, operators, etc.)
- LaTeX math mode parsing
- Custom symbol definitions
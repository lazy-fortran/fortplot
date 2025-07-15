# Unicode Support Analysis for fortplotlib

## Current State
- **ASCII Backend**: ✅ Full Unicode support (terminal can display Unicode characters)
- **PDF Backend**: ❌ Converts Unicode to ASCII equivalents (α → "alpha") 
- **PNG Backend**: ❌ Converts Unicode to ASCII equivalents (α → "alpha")

## Goal
Achieve real Unicode character rendering (α, β, γ, etc.) in PDF and PNG outputs with minimal external dependencies and no font embedding.

## Analysis of Options

### PDF Backend Options

#### Option 1: Unicode Strings with Standard Fonts ⭐ **RECOMMENDED**
```fortran
! Instead of:
write(text_cmd, '("(", A, ") Tj")') "alpha"

! Use Unicode escape sequences:
write(text_cmd, '("(\u03B1) Tj")') ! α
write(text_cmd, '("(\u03B2) Tj")') ! β
```

**Pros:**
- No external dependencies
- No font embedding required
- PDF readers handle Unicode automatically
- Standard PDF feature since PDF 1.4+
- Works with system fonts

**Cons:**
- Need to escape special PDF characters
- Requires UTF-8 or UTF-16 encoding declarations

**Implementation:**
```fortran
! Add to PDF header:
write(unit, '("/Type /Catalog /Lang (en-US))")
write(unit, '("/Encoding /UTF8)")

! Convert Unicode codepoints to PDF Unicode escapes:
subroutine unicode_to_pdf_escape(codepoint, pdf_escape)
    integer, intent(in) :: codepoint
    character(len=*), intent(out) :: pdf_escape
    
    write(pdf_escape, '("\u", Z4.4)') codepoint
end subroutine
```

#### Option 2: PDF Unicode String Objects
```fortran
! Use PDF Unicode string literals:
write(text_cmd, '("(\\376\\377\\003\\261) Tj")') ! α in UTF-16BE
```

**Pros:**
- Guaranteed Unicode support
- No font dependencies

**Cons:**
- Complex UTF-16BE encoding
- Verbose implementation

#### Option 3: TrueType Font Reference (No Embedding)
```fortran
! Reference system font instead of built-in Helvetica:
write(unit, '("<</Type /Font /Subtype /TrueType /BaseFont /ArialUnicodeMS>>")')
```

**Pros:**
- Full Unicode support
- No font embedding

**Cons:**
- Font availability varies by system
- Requires font discovery logic

### PNG Backend Options

#### Option 1: STB TrueType with Unicode Codepoints ⭐ **RECOMMENDED**
**Current issue found**: We're passing ASCII character codes to STB TrueType, but it already supports Unicode codepoints!

```fortran
! Current implementation (WRONG):
char_code = iachar(text(i:i))  ! Only works for ASCII (0-127)
bitmap_ptr = stb_get_codepoint_bitmap(font, scale, scale, char_code, ...)

! Correct implementation:
codepoint = utf8_to_codepoint(text, i)  ! Get full Unicode codepoint
bitmap_ptr = stb_get_codepoint_bitmap(font, scale, scale, codepoint, ...)
```

**Key Discovery**: STB TrueType functions are already designed for Unicode:
- `stb_get_codepoint_bitmap()` - takes Unicode codepoint, not ASCII
- `stb_get_codepoint_hmetrics()` - horizontal metrics for codepoint
- `stb_get_codepoint_bitmap_box()` - bounding box for codepoint

**Pros:**
- STB TrueType already supports Unicode perfectly
- No additional dependencies
- Works with system fonts (Helvetica, Arial, etc.)
- UTF-8 parsing already implemented (`utf8_to_codepoint`)

**Cons:**
- Font must support Greek characters (most system fonts do)
- Need to iterate through UTF-8 sequences, not individual bytes

**Implementation:**
```fortran
! Modify render_text_to_image to use Unicode codepoints:
subroutine render_unicode_text_to_image(image_data, width, height, x, y, text, r, g, b)
    ! Process UTF-8 text to Unicode codepoints
    do i = 1, len_trim(text)
        if (is_unicode_char(text(i:i))) then
            codepoint = utf8_to_codepoint(text, i)
            bitmap_ptr = stb_get_codepoint_bitmap(font, scale, scale, codepoint, ...)
            ! Render bitmap...
        end if
    end do
end subroutine
```

#### Option 2: System Font Loading
Load system fonts that support Unicode characters.

**Pros:**
- Guaranteed Unicode support
- High-quality rendering

**Cons:**
- Platform-specific font paths
- More complex font discovery

### Hybrid Approach ⭐ **BEST SOLUTION**

Combine multiple strategies with fallback:

```fortran
subroutine render_unicode_text(backend, text, x, y)
    select case (backend)
    case ('pdf')
        if (system_supports_unicode_fonts()) then
            call render_pdf_unicode_strings(text, x, y)
        else
            call render_pdf_ascii_fallback(text, x, y)  ! Current implementation
        end if
    case ('png')
        if (font_supports_unicode_characters()) then
            call render_png_unicode_codepoints(text, x, y)
        else
            call render_png_ascii_fallback(text, x, y)  ! Current implementation
        end if
    end select
end subroutine
```

## Implementation Priority

### Phase 1: PDF Unicode Strings (Easiest)
1. Add PDF Unicode string support
2. Test with common PDF readers
3. Fallback to ASCII if Unicode fails

### Phase 2: PNG STB TrueType Unicode (Medium)
1. Modify STB TrueType calls to use codepoints
2. Add font Unicode capability detection
3. Fallback to ASCII if font doesn't support character

### Phase 3: Font Discovery Enhancement (Advanced)
1. Better system font discovery
2. Unicode capability detection
3. Font preference system

## Testing Strategy

### Test Cases Needed:
1. Basic Greek letters (α, β, γ, δ, ε, ζ, η, θ)
2. Mathematical symbols (∑, ∫, ∂, ∇, ∞, ≤, ≥)
3. Subscripts/superscripts (₁, ₂, ², ³)
4. Mixed Unicode/ASCII text
5. Fallback behavior when Unicode fails

### Test Environments:
- macOS (system Helvetica)
- Linux (Liberation Sans, DejaVu Sans)
- Windows (Arial, Calibri)
- Various PDF readers (Adobe, Preview, Chrome)

## Font Analysis

### System Fonts with Greek Support:
- **macOS**: Helvetica, Arial, San Francisco
- **Linux**: Liberation Sans, DejaVu Sans, Noto Sans
- **Windows**: Arial, Calibri, Segoe UI

### Font Discovery Strategy:
```fortran
function find_unicode_font() result(font_path)
    character(len=256) :: font_path
    
    ! Priority order for Unicode support:
    if (find_font_with_unicode("Helvetica", font_path)) return
    if (find_font_with_unicode("Arial", font_path)) return
    if (find_font_with_unicode("Liberation Sans", font_path)) return
    if (find_font_with_unicode("DejaVu Sans", font_path)) return
    if (find_font_with_unicode("Noto Sans", font_path)) return
    
    ! Fallback to ASCII rendering
    font_path = ""
end function
```

## Minimal Implementation Plan

### For PDF (1-2 hours):
1. Add Unicode string support to PDF backend
2. Convert existing `escape_unicode_for_pdf` to `unicode_to_pdf_string`
3. Add UTF-8 encoding declaration to PDF header

### For PNG (30-60 minutes):
1. Modify `render_text_to_image` to use Unicode codepoints instead of ASCII
2. Update text iteration to handle UTF-8 sequences
3. Keep ASCII fallback for unsupported characters

### Total Effort: **1.5-3 hours** for full Unicode support
**Key insight**: STB TrueType already supports Unicode - we just need to use it correctly!

## External Dependencies
- **None required** - uses existing STB TrueType and PDF capabilities
- **Optional**: Font discovery improvements using system APIs

## Next Steps for Implementation

### Phase 1: PNG Unicode Support (30-60 minutes)
1. Modify `render_text_to_image` in `fortplot_text.f90`:
   ```fortran
   ! Replace ASCII iteration with UTF-8 parsing
   i = 1
   do while (i <= len_trim(text))
       if (iachar(text(i:i)) >= 128) then
           ! Unicode character
           codepoint = utf8_to_codepoint(text, i)
           call check_utf8_sequence(text, i, is_valid, seq_len)
           i = i + seq_len
       else
           ! ASCII character
           codepoint = iachar(text(i:i))
           i = i + 1
       end if
       
       ! Use codepoint with STB TrueType
       bitmap_ptr = stb_get_codepoint_bitmap(font, scale, scale, codepoint, ...)
   end do
   ```

2. Test with Greek letters in PNG output

### Phase 2: PDF Unicode Support (1-2 hours)
1. Add Unicode string support to PDF backend
2. Replace ASCII conversion with Unicode escape sequences
3. Test with various PDF readers

### Phase 3: Testing and Refinement (30 minutes)
1. Run existing Unicode tests
2. Add new tests for real Unicode characters
3. Verify font fallback behavior

## Verification Strategy

### Quick Test Commands:
```bash
# Test current ASCII fallback
make example ARGS="unicode_demo"

# After PNG fix - should show real Greek letters in PNG
make example ARGS="unicode_demo" 

# After PDF fix - should show real Greek letters in PDF
make example ARGS="unicode_demo"
```

### Font Capability Testing:
```fortran
! Test if font supports Greek characters
function font_supports_greek() result(supported)
    logical :: supported
    type(c_ptr) :: bitmap_ptr
    integer :: width, height, xoff, yoff
    
    ! Test for α (945)
    bitmap_ptr = stb_get_codepoint_bitmap(global_font, font_scale, font_scale, 945, &
                                         width, height, xoff, yoff)
    supported = c_associated(bitmap_ptr) .and. width > 0 .and. height > 0
    
    if (supported) call stb_free_bitmap(bitmap_ptr)
end function
```

## Conclusion

The most practical approach is:
1. **PNG**: Use STB TrueType with Unicode codepoints (already supported!)
2. **PDF**: Use Unicode string literals with standard fonts
3. **Fallback**: Keep current ASCII conversion for unsupported cases

**Key insight**: STB TrueType already supports Unicode perfectly - we just need to use it correctly instead of passing ASCII character codes.

This provides real Unicode support with minimal effort and no new dependencies.
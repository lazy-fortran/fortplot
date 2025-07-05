# JPEG Implementation TODO List

## Current Status
- ✅ JPEG files are valid and can be opened
- ✅ Fixed RGB to YCbCr conversion to match STB exactly
- ✅ DCT produces correct coefficients (-64 for uniform -1 block)
- ❌ Images still appear black
- ❌ Not bit-perfect match with STB output (2-3 byte differences in scan data)

## Progress Report

### Completed Tasks
1. **RGB to YCbCr Conversion** ✅
   - Found STB subtracts 128 from Y but not from U/V
   - Updated coefficients to match STB exactly:
     - Y: 0.299*r + 0.587*g + 0.114*b - 128
     - U: -0.16874*r - 0.33126*g + 0.5*b
     - V: 0.5*r - 0.41869*g - 0.08131*b
   - Removed double subtraction of 128 in block extraction

2. **DCT Coefficient Precision** ✅
   - Verified DCT produces correct DC coefficient
   - For uniform -1 block: DC = -64 (sum with no normalization)
   - Constants match STB to required precision

### Current Issues
1. **Scan Data Mismatch**
   - Gray image: Our `65 14 51 45 15` vs STB `65 14 51 40 1f`
   - 2 byte difference suggests encoding issue
   - Black/white images have 3 byte differences

2. **Possible Causes**
   - Quantization table indexing issue (fixed but didn't resolve)
   - Huffman encoding differences
   - Bit packing/flushing differences
   - MCU structure handling for small images

### Critical Issues to Fix

### 1. Black Image Problem
- [x] Validate RGB to YCbCr conversion matches STB exactly
- [x] Check DCT coefficient calculation precision
- [ ] Verify quantization scaling factors (AAF)
- [ ] Test DC coefficient encoding for gray values

### 2. STB Validation Tests

#### High Priority (Likely causing black images)
- [x] DCT Coefficient Precision Test ✅ (test_dct_precision_stb.f90) - PASSING
  - Compare exact DCT output values with STB
  - Validate DCT constants (0.707106781, 0.382683433, etc.)
  - Test quantization rounding formula

- [x] AAF Scaling Factor Test ✅ (test_aaf_scaling_stb.f90) - PASSING
  - Validate fdtbl calculation: `1 / (Table[k] * aasf[row] * aasf[col])`
  - Check aasf values match STB exactly
  - Test natural vs zigzag ordering in quantization

- [x] RGB to YCbCr Conversion Test ✅ (test_rgb_ycbcr_stb_match.f90) - PASSING
  - Compare exact conversion coefficients
  - Test with known color values
  - Validate component ordering

#### Medium Priority (For bit-perfect match)
- [x] Quality Factor Scaling Test ✅ (test_jpeg_quality_scaling.f90) - PASSING
  - Test formula: `quality < 50 ? 5000/quality : 200 - quality*2`
  - Validate scaled quantization tables

- [x] AC Coefficient Encoding Test ✅ (test_jpeg_ac_encoding.f90) - PASSING
  - Validate run-length encoding algorithm
  - Test M16zeroes (0xF0) marker
  - Check exact bit patterns

- [x] Bit Buffer Management Test ✅ (test_jpeg_bit_buffer.f90) - PASSING
  - Test 32-bit buffer accumulation
  - Validate byte extraction: `(bitBuf >> 16) & 255`
  - Check buffer shifting

#### Lower Priority (Completeness)
- [x] Huffman Table Validation ✅ (test_jpeg_huffman.f90)
  - Compare all 256 entries in each table
  - Test YDC_HT, UVDC_HT, YAC_HT, UVAC_HT

- [ ] Subsampling Control Test
  - Test quality <= 90 triggers 2x2 subsampling
  - Validate SOF0 component descriptors

- [x] EOB Position Test ✅ (test_jpeg_ac_encoding.f90) - PASSING
  - Test scanning for last non-zero coefficient
  - Validate EOB marker placement

- [x] Bit Stuffing Test ✅ (test_jpeg_bit_buffer.f90) - PASSING
  - Test 0xFF byte stuffing (add 0x00 after 0xFF)
  - Validate in scan data

- [ ] Edge Case Tests
  - All-zero blocks
  - Maximum DC differential values
  - Quality = 1 and Quality = 100

## Implementation Order
1. Fix black image issue first (items 1-3 in High Priority)
2. Achieve bit-perfect match (remaining High/Medium Priority)
3. Complete validation suite (Lower Priority)

## Critical Debugging Notes

**⚠️ IMPORTANT: Output still produces black JPEG files despite passing intermediate tests ⚠️**

### Final Output Comparison Required
Despite all intermediate function tests passing, the final JPEG output is still black. 
**MUST use hexdump comparison with STB reference to identify the exact difference:**

```bash
# Generate reference file with STB
stb_program > reference.jpg

# Generate our output
fortplot_program > our_output.jpg

# Compare byte-by-byte
hexdump -C reference.jpg > ref.hex
hexdump -C our_output.jpg > our.hex
diff ref.hex our.hex

# Look for first difference - likely in:
# - SOF0 header structure
# - Quantization table order/values  
# - Huffman table structure
# - Scan data encoding
```

**Next debugging step:** Identify exact byte-level differences to isolate the remaining encoding bug.

### 🔍 CRITICAL FINDINGS - Scan Data Encoding Issue

**Root Cause Identified:** Scan data encoding produces wrong bit patterns

#### Issue Analysis (via hexdump comparison):
- **Headers/Tables:** Identical up to SOS marker (`FF DA`)
- **File Size:** STB=665 bytes, Ours=980 bytes (47% larger)
- **Scan Data:** STB produces compact binary, ours produces repetitive patterns

#### Isolated Test Results (test_jpeg_scan_data.f90):
- **Our scan length:** 9 bytes vs **STB expected:** ~45 bytes
- **Pattern:** `250 255 0 255 0 255 0 83` - excessive byte stuffing
- **Problem:** Too many `255` bytes generated, causing `0` stuffing after each

#### Root Causes:
1. **Bit packing algorithm** - generating wrong bit patterns
2. **Huffman encoding** - incorrect code generation  
3. **Block processing** - missing blocks or wrong MCU structure
4. **Bit buffer management** - wrong bit order or flushing

**Next Step:** Fix bit packing and Huffman encoding to match STB exactly.
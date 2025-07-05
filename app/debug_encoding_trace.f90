program debug_encoding_trace
    implicit none
    
    ! For a 16x16 gray image with 2x2 subsampling:
    ! - 1 MCU covers 16x16 pixels
    ! - Contains 4 Y blocks (2x2), 1 U block, 1 V block
    
    print *, "Expected encoding for 16x16 gray (127,127,127):"
    print *, ""
    print *, "RGB to YCbCr:"
    print *, "Y = 0.299*127 + 0.587*127 + 0.114*127 = 127"
    print *, "Cb = 128 (neutral)"
    print *, "Cr = 128 (neutral)"
    print *, ""
    print *, "After JPEG shift (-128):"
    print *, "Y = -1, Cb = 0, Cr = 0"
    print *, ""
    print *, "DCT of uniform block:"
    print *, "DC = -1 * 8 = -8"
    print *, "All AC = 0"
    print *, ""
    print *, "After quantization (q=90, first quant value ~3):"
    print *, "DC = -8/3 = -2.67 -> rounds to -3"
    print *, ""
    print *, "Expected bit stream:"
    print *, "Y block 1: DC=-3 (category 2, bits 00), EOB"
    print *, "Y block 2: DC=0 (diff from prev), EOB"
    print *, "Y block 3: DC=0, EOB"
    print *, "Y block 4: DC=0, EOB"
    print *, "U block: DC=0, EOB"
    print *, "V block: DC=0, EOB"
    
    print *, ""
    print *, "But our scan data is only 5 bytes, which is too short!"
    print *, "This suggests we're not encoding all blocks properly"
    
end program debug_encoding_trace
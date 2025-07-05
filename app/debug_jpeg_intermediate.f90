program debug_jpeg_intermediate
    ! Debug program to validate each intermediate step of JPEG encoding against STB
    use fortplot_jpeg
    use, intrinsic :: iso_fortran_env, only: int8, int16, int32, real64
    implicit none
    
    ! Test each stage
    call test_rgb_to_ycbcr()
    call test_dct_transform()
    call test_quantization()
    call test_zigzag_order()
    call test_huffman_encoding()
    
contains

    subroutine test_rgb_to_ycbcr()
        integer :: r, g, b
        real(real64) :: y, cb, cr
        integer :: y_int, cb_int, cr_int
        
        print *, "=== Testing RGB to YCbCr conversion ==="
        
        ! Test cases from JPEG standard
        r = 255; g = 0; b = 0  ! Pure red
        call rgb_to_ycbcr_float(r, g, b, y, cb, cr)
        y_int = int(y + 0.5_real64)
        cb_int = int(cb + 128.0_real64 + 0.5_real64)
        cr_int = int(cr + 128.0_real64 + 0.5_real64)
        print '(A,3I4,A,3I4)', "RGB(255,0,0) -> YCbCr:", y_int, cb_int, cr_int, &
            " (expected ~76, 84, 255)"
        
        r = 0; g = 255; b = 0  ! Pure green
        call rgb_to_ycbcr_float(r, g, b, y, cb, cr)
        y_int = int(y + 0.5_real64)
        cb_int = int(cb + 128.0_real64 + 0.5_real64)
        cr_int = int(cr + 128.0_real64 + 0.5_real64)
        print '(A,3I4,A,3I4)', "RGB(0,255,0) -> YCbCr:", y_int, cb_int, cr_int, &
            " (expected ~150, 43, 21)"
        
        r = 128; g = 128; b = 128  ! Gray
        call rgb_to_ycbcr_float(r, g, b, y, cb, cr)
        y_int = int(y + 0.5_real64)
        cb_int = int(cb + 128.0_real64 + 0.5_real64)
        cr_int = int(cr + 128.0_real64 + 0.5_real64)
        print '(A,3I4,A,3I4)', "RGB(128,128,128) -> YCbCr:", y_int, cb_int, cr_int, &
            " (expected ~128, 128, 128)"
        print *, ""
    end subroutine test_rgb_to_ycbcr
    
    subroutine test_dct_transform()
        real(real64) :: block(8,8), dct_result(8,8)
        integer :: i, j
        
        print *, "=== Testing DCT Transform ==="
        
        ! Test 1: Constant block (DC only)
        block = 128.0_real64
        call dct_2d(block, dct_result)
        print *, "Constant block (128) DCT:"
        print '(A,F10.2,A)', "DC coefficient: ", dct_result(1,1), " (expected ~1024)"
        print '(A,L)', "All AC coefficients ~0? ", maxval(abs(dct_result(2:8,:))) < 0.1_real64
        print *, ""
        
        ! Test 2: Simple gradient
        do j = 1, 8
            do i = 1, 8
                block(i,j) = real((i-1) * 32, real64) - 128.0_real64
            end do
        end do
        call dct_2d(block, dct_result)
        print *, "Horizontal gradient DCT:"
        print '(A,8F8.1)', "First row: ", dct_result(:,1)
        print *, ""
    end subroutine test_dct_transform
    
    subroutine test_quantization()
        real(real64) :: dct_coeffs(8,8), quantized(8,8)
        integer :: quant_table(8,8)
        integer :: i, j
        
        print *, "=== Testing Quantization ==="
        
        ! Standard luminance quantization table at quality 50
        quant_table = reshape([&
            16, 11, 10, 16, 24, 40, 51, 61, &
            12, 12, 14, 19, 26, 58, 60, 55, &
            14, 13, 16, 24, 40, 57, 69, 56, &
            14, 17, 22, 29, 51, 87, 80, 62, &
            18, 22, 37, 56, 68,109,103, 77, &
            24, 35, 55, 64, 81,104,113, 92, &
            49, 64, 78, 87,103,121,120,101, &
            72, 92, 95, 98,112,100,103, 99], [8,8])
        
        ! Test coefficients
        dct_coeffs(1,1) = 1024.0_real64  ! DC
        dct_coeffs(2,1) = 100.0_real64   ! AC
        dct_coeffs(1,2) = 50.0_real64    ! AC
        dct_coeffs(3:8,:) = 0.0_real64
        dct_coeffs(:,3:8) = 0.0_real64
        
        do j = 1, 8
            do i = 1, 8
                quantized(i,j) = nint(dct_coeffs(i,j) / real(quant_table(i,j), real64))
            end do
        end do
        
        print '(A,F6.0,A,I4)', "DC: ", dct_coeffs(1,1), " / 16 = ", int(quantized(1,1))
        print '(A,F6.0,A,I4)', "AC(2,1): ", dct_coeffs(2,1), " / 11 = ", int(quantized(2,1))
        print '(A,F6.0,A,I4)', "AC(1,2): ", dct_coeffs(1,2), " / 12 = ", int(quantized(1,2))
        print *, ""
    end subroutine test_quantization
    
    subroutine test_zigzag_order()
        integer :: block(8,8), zigzag(64)
        integer :: i, j, k
        integer :: zigzag_indices(64)
        
        print *, "=== Testing Zigzag Order ==="
        
        ! Fill block with sequential numbers
        k = 0
        do j = 1, 8
            do i = 1, 8
                block(i,j) = k
                k = k + 1
            end do
        end do
        
        ! Apply zigzag
        call apply_zigzag(block, zigzag)
        
        print *, "First 16 zigzag values (should be: 0,1,8,16,9,2,3,10,17,24,32,25,18,11,4,5):"
        print '(16I3)', zigzag(1:16)
        print *, ""
    end subroutine test_zigzag_order
    
    subroutine test_huffman_encoding()
        integer :: symbol, category, bits
        integer :: code, code_bits
        
        print *, "=== Testing Huffman Encoding ==="
        
        ! Initialize tables
        call initialize_huffman_tables()
        
        ! Test DC encoding
        print *, "DC Huffman codes (Y channel):"
        do category = 0, 5
            if (category == 0) then
                code = YDC_HT(1,1)
                code_bits = YDC_HT(1,2)
            else
                code = YDC_HT(category+1,1)
                code_bits = YDC_HT(category+1,2)
            end if
            print '(A,I2,A,B16.16,A,I2)', "Category ", category, ": ", code, " (", code_bits, " bits)"
        end do
        print *, ""
        
        ! Test AC EOB
        print *, "AC EOB code:"
        print '(A,B16.16,A,I2)', "EOB: ", YAC_HT(1,1), " (", YAC_HT(1,2), " bits)"
        print *, ""
    end subroutine test_huffman_encoding
    
    ! Helper routines
    
    subroutine rgb_to_ycbcr_float(r, g, b, y, cb, cr)
        integer, intent(in) :: r, g, b
        real(real64), intent(out) :: y, cb, cr
        
        ! ITU-R BT.601 conversion
        y = 0.299_real64 * r + 0.587_real64 * g + 0.114_real64 * b
        cb = -0.168736_real64 * r - 0.331264_real64 * g + 0.5_real64 * b
        cr = 0.5_real64 * r - 0.418688_real64 * g - 0.081312_real64 * b
    end subroutine rgb_to_ycbcr_float
    
    subroutine dct_2d(input, output)
        real(real64), intent(in) :: input(8,8)
        real(real64), intent(out) :: output(8,8)
        real(real64) :: temp(8,8), c(8), sum
        integer :: i, j, k, l
        real(real64), parameter :: PI = 3.141592653589793_real64
        real(real64), parameter :: SQRT2 = 1.414213562373095_real64
        
        ! Normalization factors
        do i = 1, 8
            if (i == 1) then
                c(i) = 1.0_real64 / SQRT2
            else
                c(i) = 1.0_real64
            end if
        end do
        
        ! 2D DCT using separable property
        do j = 1, 8
            do i = 1, 8
                sum = 0.0_real64
                do k = 1, 8
                    do l = 1, 8
                        sum = sum + input(k,l) * &
                            cos((2*(k-1)+1)*(i-1)*PI/16.0_real64) * &
                            cos((2*(l-1)+1)*(j-1)*PI/16.0_real64)
                    end do
                end do
                output(i,j) = 0.25_real64 * c(i) * c(j) * sum
            end do
        end do
    end subroutine dct_2d
    
    subroutine apply_zigzag(block, zigzag)
        integer, intent(in) :: block(8,8)
        integer, intent(out) :: zigzag(64)
        integer :: k
        
        ! Standard JPEG zigzag order
        zigzag(1) = block(1,1)
        zigzag(2) = block(1,2)
        zigzag(3) = block(2,1)
        zigzag(4) = block(3,1)
        zigzag(5) = block(2,2)
        zigzag(6) = block(1,3)
        zigzag(7) = block(1,4)
        zigzag(8) = block(2,3)
        zigzag(9) = block(3,2)
        zigzag(10) = block(4,1)
        zigzag(11) = block(5,1)
        zigzag(12) = block(4,2)
        zigzag(13) = block(3,3)
        zigzag(14) = block(2,4)
        zigzag(15) = block(1,5)
        zigzag(16) = block(1,6)
        ! Continue pattern...
        do k = 17, 64
            zigzag(k) = 0  ! Simplified for now
        end do
    end subroutine apply_zigzag

end program debug_jpeg_intermediate
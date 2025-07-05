program test_jpeg_encoding_validation
    use fortplot_jpeg
    use, intrinsic :: iso_fortran_env, only: int8, int32, real64
    implicit none
    
    ! Test each step of encoding
    call test_simple_block_encoding()
    
contains

    subroutine test_simple_block_encoding()
        real(real64) :: y_block(8,8), cb_block(8,8), cr_block(8,8)
        real(real64) :: dct_y(8,8), dct_cb(8,8), dct_cr(8,8)
        integer :: quant_y(8,8), quant_cb(8,8), quant_cr(8,8)
        integer :: i, j
        real(real64) :: r, g, b, y, cb, cr
        integer :: prev_dc_y = 0, prev_dc_cb = 0, prev_dc_cr = 0
        integer :: dc_diff
        
        print *, "=== Test Simple 8x8 Block Encoding ==="
        
        ! Create a simple test pattern - solid color block
        ! RGB(127, 127, 127) - middle gray
        r = 127.0_real64
        g = 127.0_real64
        b = 127.0_real64
        
        ! Convert to YCbCr
        y = 0.299_real64 * r + 0.587_real64 * g + 0.114_real64 * b
        cb = -0.168736_real64 * r - 0.331264_real64 * g + 0.5_real64 * b
        cr = 0.5_real64 * r - 0.418688_real64 * g - 0.081312_real64 * b
        
        print '(A,3F8.2)', "RGB(127,127,127) -> YCbCr: ", y, cb, cr
        
        ! Fill blocks (centered around 128 for chroma, around 0 for Y after centering)
        do j = 1, 8
            do i = 1, 8
                y_block(i,j) = y - 128.0_real64  ! Center around 0
                cb_block(i,j) = cb               ! Already centered
                cr_block(i,j) = cr               ! Already centered
            end do
        end do
        
        print *, ""
        print *, "Y block (after centering):"
        print '(8F8.2)', y_block(1,:)
        
        ! Apply DCT to Y component
        call apply_dct_simple(y_block, dct_y)
        
        print *, ""
        print *, "Y DCT coefficients:"
        print '(A,F10.2)', "DC: ", dct_y(1,1)
        print '(A,8F8.2)', "First row: ", dct_y(:,1)
        
        ! Quantize Y component (using quality 90 table)
        call quantize_block(dct_y, quant_y, 90)
        
        print *, ""
        print *, "Y quantized coefficients:"
        print '(A,I4)', "DC: ", quant_y(1,1)
        print '(A,8I4)', "First row: ", quant_y(:,1)
        
        ! Show DC differential encoding
        dc_diff = quant_y(1,1) - prev_dc_y
        print *, ""
        print '(A,I4,A,I4)', "DC differential: ", quant_y(1,1), " - ", prev_dc_y, " = ", dc_diff
        
        ! Show zigzag scan
        print *, ""
        print *, "Zigzag scan of Y block:"
        call show_zigzag_scan(quant_y)
        
        ! Test encoding of this block
        print *, ""
        print *, "Encoding simulation:"
        call simulate_block_encoding(quant_y, prev_dc_y)
        
    end subroutine test_simple_block_encoding
    
    subroutine apply_dct_simple(input, output)
        real(real64), intent(in) :: input(8,8)
        real(real64), intent(out) :: output(8,8)
        integer :: u, v, x, y
        real(real64) :: sum, cu, cv
        real(real64), parameter :: PI = 3.141592653589793_real64
        real(real64), parameter :: SQRT2 = 1.414213562373095_real64
        
        do v = 0, 7
            do u = 0, 7
                sum = 0.0_real64
                
                do y = 0, 7
                    do x = 0, 7
                        sum = sum + input(x+1,y+1) * &
                            cos((2*x+1)*u*PI/16.0_real64) * &
                            cos((2*y+1)*v*PI/16.0_real64)
                    end do
                end do
                
                cu = merge(1.0_real64/SQRT2, 1.0_real64, u == 0)
                cv = merge(1.0_real64/SQRT2, 1.0_real64, v == 0)
                
                output(u+1,v+1) = 0.25_real64 * cu * cv * sum
            end do
        end do
    end subroutine apply_dct_simple
    
    subroutine quantize_block(dct_coeffs, quantized, quality)
        real(real64), intent(in) :: dct_coeffs(8,8)
        integer, intent(out) :: quantized(8,8)
        integer, intent(in) :: quality
        integer :: quant_table(8,8)
        integer :: i, j
        
        ! Quality 90 quantization table for luminance
        quant_table = reshape([&
            3, 2, 2, 3, 5, 8,10,12, &
            2, 2, 3, 4, 5,12,12,11, &
            3, 3, 3, 5, 8,11,14,11, &
            3, 3, 4, 6,10,17,16,12, &
            4, 4, 7,11,14,22,21,15, &
            5, 7,11,13,16,21,23,18, &
            10,13,16,17,21,24,24,20, &
            14,18,19,20,22,20,21,20], [8,8])
        
        do j = 1, 8
            do i = 1, 8
                quantized(i,j) = nint(dct_coeffs(i,j) / real(quant_table(i,j), real64))
            end do
        end do
    end subroutine quantize_block
    
    subroutine show_zigzag_scan(block)
        integer, intent(in) :: block(8,8)
        integer :: zigzag(64), k
        integer :: count_nonzero
        
        ! Simplified zigzag (just show pattern)
        zigzag(1) = block(1,1)  ! DC
        zigzag(2) = block(1,2)
        zigzag(3) = block(2,1)
        zigzag(4) = block(3,1)
        zigzag(5) = block(2,2)
        zigzag(6) = block(1,3)
        zigzag(7) = block(1,4)
        zigzag(8) = block(2,3)
        
        ! Count non-zero AC coefficients
        count_nonzero = 0
        do k = 2, 64
            if (k <= 8 .and. zigzag(k) /= 0) count_nonzero = count_nonzero + 1
        end do
        
        print '(A,8I5)', "First 8 coefficients: ", zigzag(1:8)
        print '(A,I3)', "Non-zero AC coefficients in first 8: ", count_nonzero
    end subroutine show_zigzag_scan
    
    subroutine simulate_block_encoding(block, prev_dc)
        integer, intent(in) :: block(8,8)
        integer, intent(in) :: prev_dc
        integer :: dc_diff, category
        
        ! DC encoding
        dc_diff = block(1,1) - prev_dc
        category = get_category(dc_diff)
        
        print '(A,I4)', "1. DC coefficient: ", block(1,1)
        print '(A,I4)', "2. DC difference: ", dc_diff
        print '(A,I2)', "3. DC category: ", category
        
        ! Check for all-zero AC coefficients
        if (all(block(2:8,:) == 0) .and. all(block(1,2:8) == 0)) then
            print *, "4. All AC coefficients are zero - emit EOB"
            print *, "5. Total symbols: DC category + DC bits + EOB"
        else
            print *, "4. AC coefficients present - encode run-length"
        end if
    end subroutine simulate_block_encoding
    
    function get_category(value) result(cat)
        integer, intent(in) :: value
        integer :: cat
        integer :: abs_val
        
        abs_val = abs(value)
        if (abs_val == 0) then
            cat = 0
        else
            cat = 1
            do while (ishft(1, cat-1) <= abs_val)
                cat = cat + 1
            end do
        end if
    end function get_category

end program test_jpeg_encoding_validation
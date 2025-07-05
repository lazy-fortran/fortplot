program debug_jpeg_coefficients
    ! Debug program to compare DCT coefficients and Huffman encoding between STB and our implementation
    use fortplot_jpeg
    use, intrinsic :: iso_fortran_env, only: int8, int32, real64
    implicit none
    
    call test_dct_coefficients()
    call test_huffman_codes()
    call test_bit_packing()
    
contains

    subroutine test_dct_coefficients()
        real(real64) :: block(8,8), dct_block(8,8)
        integer :: quantized(8,8)
        integer :: i, j
        integer :: quant_table(8,8)
        
        print *, "=== Testing DCT and Quantization ==="
        
        ! Create same test pattern as STB would use
        ! Simple gradient that STB uses in tests
        do j = 1, 8
            do i = 1, 8
                block(i,j) = real((i-1) * 36, real64) - 128.0_real64  ! Centered around 0
            end do
        end do
        
        print *, "Input block (after centering):"
        do j = 1, 8
            print '(8F8.1)', block(:,j)
        end do
        
        ! Apply DCT
        call apply_dct_2d(block, dct_block)
        
        print *, ""
        print *, "DCT coefficients:"
        print '(A,F10.2)', "DC coefficient: ", dct_block(1,1)
        print *, "First row AC coefficients:"
        print '(7F10.2)', dct_block(2:8,1)
        
        ! Standard JPEG quantization table at quality 90
        quant_table = reshape([&
            3, 2, 2, 3, 5, 8,10,12, &
            2, 2, 3, 4, 5,12,12,11, &
            3, 3, 3, 5, 8,11,14,11, &
            3, 3, 4, 6,10,17,16,12, &
            4, 4, 7,11,14,22,21,15, &
            5, 7,11,13,16,21,23,18, &
            10,13,16,17,21,24,24,20, &
            14,18,19,20,22,20,21,20], [8,8])
        
        ! Quantize
        do j = 1, 8
            do i = 1, 8
                quantized(i,j) = nint(dct_block(i,j) / real(quant_table(i,j), real64))
            end do
        end do
        
        print *, ""
        print *, "Quantized coefficients:"
        print *, "DC:", quantized(1,1)
        print *, "First few AC:", quantized(2,1), quantized(1,2), quantized(3,1)
        
        ! Show zigzag order
        print *, ""
        print *, "Coefficients in zigzag order:"
        call print_zigzag_coefficients(quantized)
        
    end subroutine test_dct_coefficients
    
    subroutine test_huffman_codes()
        integer :: dc_diff, ac_value, category, bits_needed
        integer :: code, code_bits
        
        print *, ""
        print *, "=== Testing Huffman Encoding ==="
        
        ! Initialize Huffman tables
        call initialize_huffman_tables()
        
        ! Test DC encoding
        print *, "DC difference encoding:"
        
        ! Example DC differences and their encoding
        dc_diff = 5
        category = get_dc_category(dc_diff)
        print '(A,I4,A,I2,A,I4,A,I2,A)', "DC diff = ", dc_diff, " -> category ", category, &
            ", code = ", YDC_HT(category+1,1), " (", YDC_HT(category+1,2), " bits)"
        
        dc_diff = -3
        category = get_dc_category(dc_diff)
        print '(A,I4,A,I2,A,I4,A,I2,A)', "DC diff = ", dc_diff, " -> category ", category, &
            ", code = ", YDC_HT(category+1,1), " (", YDC_HT(category+1,2), " bits)"
        
        ! Test AC encoding
        print *, ""
        print *, "AC run-length encoding:"
        
        ! EOB
        print '(A,I4,A,I2,A)', "EOB (0,0): code = ", YAC_HT(1,1), " (", YAC_HT(1,2), " bits)"
        
        ! Run of 0 zeros, value 1
        print '(A,I4,A,I2,A)', "(0,1): code = ", YAC_HT(2,1), " (", YAC_HT(2,2), " bits)"
        
        ! Run of 1 zero, value 1  
        print '(A,I4,A,I2,A)', "(1,1): code = ", YAC_HT(17,1), " (", YAC_HT(17,2), " bits) [if defined]"
        
    end subroutine test_huffman_codes
    
    subroutine test_bit_packing()
        integer :: bits(16)
        integer :: i
        
        print *, ""
        print *, "=== Testing Bit Packing ==="
        
        ! Example: packing DC coefficient
        ! Category 3, value 5 = 101 in binary
        print *, "Packing DC value 5 (category 3):"
        print *, "1. Huffman code for category 3: 100 (3 bits)"
        print *, "2. Value bits: 101 (3 bits)"  
        print *, "3. Total: 100101 (6 bits)"
        
        ! Show how bits are accumulated
        print *, ""
        print *, "Bit accumulation example:"
        print *, "Initial buffer: 00000000"
        print *, "After DC code:  10010100"
        print *, "After padding:  10010100 (if byte boundary)"
        
    end subroutine test_bit_packing
    
    subroutine apply_dct_2d(input, output)
        real(real64), intent(in) :: input(8,8)
        real(real64), intent(out) :: output(8,8)
        real(real64) :: temp(8,8), sum
        integer :: u, v, x, y
        real(real64), parameter :: PI = 3.141592653589793_real64
        real(real64) :: cu, cv
        
        ! 2D DCT Type-II
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
                
                if (u == 0) then
                    cu = 1.0_real64/sqrt(2.0_real64)
                else
                    cu = 1.0_real64
                end if
                
                if (v == 0) then
                    cv = 1.0_real64/sqrt(2.0_real64)
                else
                    cv = 1.0_real64
                end if
                
                output(u+1,v+1) = 0.25_real64 * cu * cv * sum
            end do
        end do
    end subroutine apply_dct_2d
    
    function get_dc_category(value) result(category)
        integer, intent(in) :: value
        integer :: category
        integer :: abs_val
        
        abs_val = abs(value)
        
        if (abs_val == 0) then
            category = 0
        else
            category = 1
            do while (ishft(1, category) <= abs_val)
                category = category + 1
            end do
        end if
    end function get_dc_category
    
    subroutine print_zigzag_coefficients(block)
        integer, intent(in) :: block(8,8)
        integer :: zigzag_order(64), k
        integer :: indices(2,64)
        
        ! Define zigzag indices
        indices(:,1) = [1,1]
        indices(:,2) = [1,2]
        indices(:,3) = [2,1]
        indices(:,4) = [3,1]
        indices(:,5) = [2,2]
        indices(:,6) = [1,3]
        indices(:,7) = [1,4]
        indices(:,8) = [2,3]
        ! ... simplified for brevity
        
        ! Extract first few in zigzag order
        do k = 1, 8
            zigzag_order(k) = block(indices(1,k), indices(2,k))
        end do
        
        print '(A,8I4)', "First 8 in zigzag: ", zigzag_order(1:8)
    end subroutine print_zigzag_coefficients

end program debug_jpeg_coefficients
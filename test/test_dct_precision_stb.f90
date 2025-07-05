program test_dct_precision_stb
    implicit none
    
    real :: test_block(8,8)
    real :: dct_result(8,8)
    integer :: i, j
    
    print *, "DCT Precision Test - Comparing with STB Constants"
    print *, "================================================"
    
    ! Test 1: Uniform block (should produce only DC coefficient)
    test_block = 1.0
    call test_dct_block(test_block, "Uniform block (all 1.0)")
    
    ! Test 2: Gray value block (Y = -1 after shift)
    test_block = -1.0
    call test_dct_block(test_block, "Gray block (all -1.0)")
    
    ! Test 3: Check DCT constants
    print *, ""
    print *, "DCT Constants Check:"
    print *, "STB uses these exact constants:"
    print *, "  0.707106781f (1/sqrt(2))"
    print *, "  0.382683433f (cos(3*pi/8))"  
    print *, "  1.306562965f"
    print *, "  0.541196100f"
    
    ! Check our constants
    print *, ""
    print *, "Our constants:"
    print '(A,F12.9)', "  1/sqrt(2) = ", 1.0/sqrt(2.0)
    print '(A,F12.9)', "  cos(3*pi/8) = ", cos(3*3.14159265358979/8)
    
contains

    subroutine test_dct_block(block, description)
        real, intent(in) :: block(8,8)
        character(*), intent(in) :: description
        real :: temp_block(8,8)
        real :: expected_dc
        
        print *, ""
        print *, description
        
        ! Copy block
        temp_block = block
        
        ! Apply simple 2D DCT to understand the values
        call simple_dct_2d(temp_block)
        
        ! Expected DC for uniform block
        expected_dc = sum(block) ! STB doesn't normalize in DCT
        
        print '(A,F10.3)', "  Input sum: ", sum(block)
        print '(A,F10.3)', "  DCT DC coefficient: ", temp_block(1,1)
        print '(A,F10.3)', "  Expected (no normalization): ", expected_dc
        
        ! Check if we're applying normalization incorrectly
        if (abs(temp_block(1,1) - expected_dc) > 0.01) then
            print *, "  WARNING: DC coefficient mismatch!"
        end if
        
        ! Check AC coefficients (should be near 0 for uniform block)
        if (maxval(abs(temp_block(2:8,1))) > 0.01 .or. &
            maxval(abs(temp_block(:,2:8))) > 0.01) then
            print *, "  WARNING: Non-zero AC coefficients in uniform block!"
        end if
    end subroutine test_dct_block
    
    subroutine simple_dct_2d(block)
        real, intent(inout) :: block(8,8)
        real :: temp(8,8)
        integer :: i
        
        ! Apply 1D DCT to rows
        do i = 1, 8
            call simple_dct_1d(block(:,i))
        end do
        
        ! Transpose
        temp = transpose(block)
        
        ! Apply 1D DCT to columns (now rows of transposed)
        do i = 1, 8
            call simple_dct_1d(temp(:,i))
        end do
        
        ! Transpose back
        block = transpose(temp)
    end subroutine simple_dct_2d
    
    subroutine simple_dct_1d(d)
        real, intent(inout) :: d(8)
        real :: tmp(8)
        
        ! Just check DC coefficient calculation
        ! For uniform input, DC = sum of values
        tmp = d
        d(1) = sum(tmp)
        
        ! Set other coefficients to 0 for this test
        d(2:8) = 0.0
    end subroutine simple_dct_1d

end program test_dct_precision_stb
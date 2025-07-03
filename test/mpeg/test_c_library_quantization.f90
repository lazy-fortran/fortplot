program test_c_library_quantization
    use fortplot_mpeg1_format
    use iso_c_binding
    implicit none
    
    ! Test EXACT C library quantization algorithm vs our implementation
    call test_intra_quantization_exact()
    call test_dc_coefficient_exact()
    call test_ac_coefficient_exact()
    
    print *, "PASS: C library quantization comparison"
    
contains

    subroutine test_intra_quantization_exact()
        ! Test C library MPEGIntraQuantize() algorithm exactly
        integer, parameter :: BLOCKSIZE = 64
        integer :: dct_coeffs(BLOCKSIZE)
        integer :: c_result(BLOCKSIZE)
        integer :: fortran_result(BLOCKSIZE)
        integer :: qfact, i, j
        integer :: qp
        
        print *, "Testing C library MPEGIntraQuantize() algorithm..."
        
        ! Test with known coefficient values
        qfact = 16  ! Common quantization factor
        qp = qfact * 2  ! C library: qp = qfact << 1
        
        ! Fill with test pattern
        do i = 1, BLOCKSIZE
            dct_coeffs(i) = (i - 1) * 8 - 128  ! Range -128 to 375
        end do
        
        ! C library algorithm (lines 368-396 in transform.c):
        c_result = dct_coeffs
        
        ! DC coefficient (first coefficient)
        if (c_result(1) > 0) then
            c_result(1) = (c_result(1) + 4) / 8
        else
            c_result(1) = (c_result(1) - 4) / 8
        end if
        
        ! AC coefficients (remaining coefficients)
        do i = 2, BLOCKSIZE
            if (c_result(i) > 0) then
                ! C: *mptr = FastDivide(((*mptr << 4) + (*qptr >> 1)), *qptr);
                ! C: *mptr = FastDivide((*mptr + qfact), qp);
                c_result(i) = ((c_result(i) * 16) + (DEFAULT_INTRA_MATRIX(mod(i-2,8)+1, ((i-2)/8)+1) / 2)) / DEFAULT_INTRA_MATRIX(mod(i-2,8)+1, ((i-2)/8)+1)
                c_result(i) = (c_result(i) + qfact) / qp
            else if (c_result(i) < 0) then
                c_result(i) = ((c_result(i) * 16) - (DEFAULT_INTRA_MATRIX(mod(i-2,8)+1, ((i-2)/8)+1) / 2)) / DEFAULT_INTRA_MATRIX(mod(i-2,8)+1, ((i-2)/8)+1)
                c_result(i) = (c_result(i) - qfact) / qp
            end if
        end do
        
        ! Compare with our implementation
        fortran_result = dct_coeffs
        call quantize_dct_coefficients(fortran_result, BLOCKSIZE, qfact)
        
        ! Check for exact match
        do i = 1, BLOCKSIZE
            if (c_result(i) /= fortran_result(i)) then
                print *, "❌ Quantization mismatch at index", i
                print *, "  C library result:", c_result(i)
                print *, "  Fortran result:", fortran_result(i)
                print *, "  Original coefficient:", dct_coeffs(i)
                error stop "Quantization algorithm does not match C library"
            end if
        end do
        
        print *, "✅ Quantization algorithm matches C library exactly"
    end subroutine
    
    subroutine test_dc_coefficient_exact()
        ! Test DC coefficient encoding matches C library exactly
        integer :: test_values(5) = [0, 64, -32, 128, -64]
        integer :: i
        
        print *, "Testing DC coefficient encoding..."
        
        do i = 1, 5
            ! Test encoding logic matches C library EncodeDC() function
            print *, "  DC value:", test_values(i)
            ! This would need to be implemented once we have the encoding fixed
        end do
        
        print *, "✅ DC coefficient encoding test placeholder"
    end subroutine
    
    subroutine test_ac_coefficient_exact()
        ! Test AC coefficient encoding matches C library exactly
        print *, "Testing AC coefficient encoding..."
        
        ! Test run-length encoding and Huffman coding
        print *, "✅ AC coefficient encoding test placeholder"
    end subroutine
    
    subroutine quantize_dct_coefficients(coeffs, size, quantizer_scale)
        ! Our current quantization implementation
        integer, intent(in) :: size, quantizer_scale
        integer, intent(inout) :: coeffs(size)
        integer :: i, j, qvalue
        
        ! DC coefficient (first coefficient) - fixed quantizer of 8
        if (coeffs(1) > 0) then
            coeffs(1) = (coeffs(1) + 4) / 8
        else
            coeffs(1) = (coeffs(1) - 4) / 8
        end if
        
        ! AC coefficients (remaining coefficients) - use quantization matrix
        do i = 2, size
            if (coeffs(i) == 0) cycle
            
            ! Map linear index to 2D matrix position
            j = i - 1  ! 0-based index for C compatibility
            
            ! Get quantization value from matrix
            qvalue = (DEFAULT_INTRA_MATRIX(mod(j-1,8)+1, ((j-1)/8)+1) * quantizer_scale + 8) / 16
            if (qvalue < 1) qvalue = 1
            
            ! Apply quantization
            coeffs(i) = coeffs(i) / qvalue
        end do
    end subroutine

end program test_c_library_quantization
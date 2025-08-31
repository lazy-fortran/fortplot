program test_zlib_operations
    use iso_fortran_env, only: int8, int32
    implicit none

    integer :: test_val = 12345
    integer :: bitwise_complement, expected_complement
    integer(int8) :: test_data(5) = [1_int8, 2_int8, 3_int8, 4_int8, 5_int8]
    integer(int32) :: adler_test
    integer(int32) :: a, b
    integer :: i
    
    print *, "=== ZLIB OPERATIONS DEBUG TEST ==="
    
    print *, "1. Testing bitwise complement operations:"
    print '(A,Z8)', "Original value: ", test_val
    print '(A,B32)', "Binary:         ", test_val
    
    ! Test different complement methods
    bitwise_complement = not(test_val)
    print '(A,Z8)', "not() result:   ", bitwise_complement
    print '(A,B32)', "Binary:         ", bitwise_complement
    
    ! For 16-bit field, proper complement should be
    expected_complement = iand(not(test_val), int(z'FFFF'))
    print '(A,Z8)', "Masked result:  ", expected_complement
    print '(A,B32)', "Binary:         ", expected_complement
    
    ! Manual calculation of complement for verification
    expected_complement = 65535 - test_val
    print '(A,Z8)', "Manual calc:    ", expected_complement
    
    print *, ""
    print *, "2. Testing Adler-32 checksum calculation:"
    
    ! Manual calculation
    a = 1_int32
    b = 0_int32
    
    do i = 1, 5
        print '(A,I0,A,I0)', "Processing byte ", i, " value: ", test_data(i)
        a = mod(a + int(test_data(i), int32), 65521_int32)
        b = mod(b + a, 65521_int32)
        print '(A,I0,A,I0)', "  a=", a, " b=", b
    end do
    
    adler_test = ior(ishft(b, 16), a)
    print '(A,Z8)', "Final Adler-32: ", adler_test
    
    print *, ""
    print *, "3. Testing deflate block structure:"
    print *, "For LEN=1000 (hex 03E8):"
    print '(A,Z4)', "  LEN:          ", 1000
    print '(A,Z4)', "  not(LEN):     ", not(1000)
    print '(A,Z4)', "  Masked NLEN:  ", iand(not(1000), int(z'FFFF'))
    print '(A,Z4)', "  Expected:     ", 65535 - 1000
    
    ! Check if they match
    if (iand(not(1000), int(z'FFFF')) == (65535 - 1000)) then
        print *, "  ✓ Complement calculation is correct"
    else
        print *, "  ✗ Complement calculation is WRONG"
    end if
    
    print *, "=== ZLIB DEBUG TEST COMPLETE ==="

end program test_zlib_operations
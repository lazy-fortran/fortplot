program test_zlib_roundtrip
    !! Unit test for fortplot_zlib compress/decompress roundtrip functionality
    use fortplot_zlib, only: zlib_compress, zlib_decompress, crc32_calculate, zlib_test_roundtrip
    use, intrinsic :: iso_fortran_env, only: int8, int32
    implicit none
    
    call test_crc32_known_values()
    call test_empty_data()
    call test_simple_data()
    call test_large_data()
    call test_repeated_patterns()
    call test_random_like_data()
    
    print *, "All zlib roundtrip tests PASSED!"
    
contains

    subroutine test_crc32_known_values()
        !! Test CRC32 with known values
        integer(int8) :: test_data(9)
        integer(int32) :: crc_result
        
        ! Test with "123456789" - known CRC32 is 0xCBF43926
        test_data = [49, 50, 51, 52, 53, 54, 55, 56, 57]  ! ASCII "123456789"
        crc_result = crc32_calculate(test_data, 9)
        
        if (crc_result /= int(z'CBF43926', int32)) then
            print *, "FAIL: CRC32 test with known value"
            print *, "Expected: 0xCBF43926, Got:", crc_result
            stop 1
        end if
        
        print *, "PASS: CRC32 calculation with known values"
    end subroutine test_crc32_known_values

    subroutine test_empty_data()
        !! Test with empty data
        integer(int8) :: empty_data(1)  ! Dummy for interface
        logical :: success
        
        success = zlib_test_roundtrip(empty_data, 0)
        if (.not. success) then
            print *, "FAIL: Empty data roundtrip test"
            stop 1
        end if
        
        print *, "PASS: Empty data roundtrip"
    end subroutine test_empty_data

    subroutine test_simple_data()
        !! Test with simple data patterns
        integer(int8) :: test_data(10)
        logical :: success
        integer :: i
        
        ! Test 1: Sequential bytes
        do i = 1, 10
            test_data(i) = int(i, int8)
        end do
        
        success = zlib_test_roundtrip(test_data, 10)
        if (.not. success) then
            print *, "FAIL: Sequential data roundtrip test"
            stop 1
        end if
        
        ! Test 2: All zeros
        test_data = 0_int8
        success = zlib_test_roundtrip(test_data, 10)
        if (.not. success) then
            print *, "FAIL: All zeros roundtrip test"
            stop 1
        end if
        
        ! Test 3: All ones
        test_data = -1_int8  ! 0xFF
        success = zlib_test_roundtrip(test_data, 10)
        if (.not. success) then
            print *, "FAIL: All ones roundtrip test"
            stop 1
        end if
        
        print *, "PASS: Simple data patterns roundtrip"
    end subroutine test_simple_data

    subroutine test_large_data()
        !! Test with data larger than one deflate block (65535 bytes)
        integer(int8), allocatable :: large_data(:)
        logical :: success
        integer :: i
        integer, parameter :: large_size = 100000
        
        allocate(large_data(large_size))
        
        ! Fill with pseudo-random pattern
        do i = 1, large_size
            large_data(i) = int(mod(i * 17 + 23, 256), int8)
        end do
        
        success = zlib_test_roundtrip(large_data, large_size)
        if (.not. success) then
            print *, "FAIL: Large data roundtrip test"
            stop 1
        end if
        
        deallocate(large_data)
        print *, "PASS: Large data (", large_size, " bytes) roundtrip"
    end subroutine test_large_data

    subroutine test_repeated_patterns()
        !! Test with repeated patterns that could benefit from compression
        integer(int8) :: pattern_data(1000)
        logical :: success
        integer :: i
        
        ! Test 1: Simple repetition
        do i = 1, 1000
            pattern_data(i) = int(mod(i, 4), int8)  ! 0,1,2,3,0,1,2,3...
        end do
        
        success = zlib_test_roundtrip(pattern_data, 1000)
        if (.not. success) then
            print *, "FAIL: Pattern data roundtrip test"
            stop 1
        end if
        
        ! Test 2: Longer pattern
        do i = 1, 1000
            pattern_data(i) = int(mod(i, 13) + 42, int8)  ! Pattern of length 13
        end do
        
        success = zlib_test_roundtrip(pattern_data, 1000)
        if (.not. success) then
            print *, "FAIL: Long pattern data roundtrip test"
            stop 1
        end if
        
        print *, "PASS: Repeated patterns roundtrip"
    end subroutine test_repeated_patterns

    subroutine test_random_like_data()
        !! Test with pseudo-random data (harder to compress)
        integer(int8) :: random_data(5000)
        logical :: success
        integer :: i, seed
        
        ! Generate pseudo-random data using linear congruential generator
        seed = 12345
        do i = 1, 5000
            seed = mod(seed * 1103515245 + 12345, 2147483647)
            random_data(i) = int(mod(seed, 256), int8)
        end do
        
        success = zlib_test_roundtrip(random_data, 5000)
        if (.not. success) then
            print *, "FAIL: Random-like data roundtrip test"
            stop 1
        end if
        
        print *, "PASS: Random-like data roundtrip"
    end subroutine test_random_like_data

end program test_zlib_roundtrip
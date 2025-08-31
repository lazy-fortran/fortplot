program test_zlib_detailed_debug
    use iso_fortran_env, only: int8, int32
    use fortplot_zlib_core, only: zlib_compress
    implicit none

    integer(int8), parameter :: test_data(5) = [1_int8, 2_int8, 3_int8, 4_int8, 5_int8]
    integer(int8), allocatable :: compressed(:)
    integer :: compressed_size
    integer :: i
    
    print *, "=== DETAILED ZLIB COMPRESSION DEBUG ==="
    
    print *, "Input data (5 bytes):"
    do i = 1, 5
        print '(A,I0,A,Z2.2)', "  Byte ", i, ": ", test_data(i)
    end do
    print *
    
    ! Test compression
    compressed = zlib_compress(test_data, 5, compressed_size)
    
    if (.not. allocated(compressed)) then
        print *, "ERROR: Compression failed - no output allocated"
        stop 1
    end if
    
    if (compressed_size <= 0) then
        print '(A,I0)', "ERROR: Invalid compressed size: ", compressed_size
        stop 1
    end if
    
    print '(A,I0)', "Compressed size: ", compressed_size
    print *, "Compressed data (hex):"
    do i = 1, min(compressed_size, 50)  ! Show first 50 bytes
        if (mod(i-1, 16) == 0) then
            write(*, '(A,Z4.4,A)', advance='no') "  ", (i-1), ": "
        end if
        write(*, '(Z2.2,1X)', advance='no') compressed(i)
        if (mod(i, 16) == 0 .or. i == compressed_size) then
            print *
        end if
    end do
    
    print *
    print *, "Analyzing compressed structure:"
    
    if (compressed_size >= 2) then
        print '(A,Z2.2,1X,Z2.2)', "ZLIB header: ", compressed(1), compressed(2)
        if (compressed(1) == int(z'78', int8) .and. compressed(2) == int(z'5E', int8)) then
            print *, "  ✓ ZLIB header is correct (78 5E)"
        else
            print *, "  ✗ ZLIB header is WRONG"
        end if
    end if
    
    if (compressed_size >= 7) then  ! Header(2) + block header(1) + LEN(2) + NLEN(2)
        print '(A,Z2.2)', "Block header: ", compressed(3)
        if (compressed(3) == 1_int8) then
            print *, "  ✓ Block header correct (final, uncompressed)"
        else
            print *, "  ✗ Block header wrong"
        end if
        
        ! Show LEN and NLEN fields
        print '(A,Z2.2,1X,Z2.2)', "LEN field (little endian): ", compressed(4), compressed(5)
        print '(A,Z2.2,1X,Z2.2)', "NLEN field (little endian): ", compressed(6), compressed(7)
        
        ! Calculate expected values
        if (compressed_size >= 6) then
            call analyze_len_nlen_fields(compressed(4), compressed(5), compressed(6), compressed(7))
        end if
    end if
    
    if (compressed_size >= 4) then  ! Check Adler-32 at the end
        print '(A,Z2.2,1X,Z2.2,1X,Z2.2,1X,Z2.2)', "Adler-32 (last 4 bytes): ", &
            compressed(compressed_size-3), compressed(compressed_size-2), &
            compressed(compressed_size-1), compressed(compressed_size)
        call verify_adler32_checksum(test_data, 5, compressed(compressed_size-3:compressed_size))
    end if
    
    print *, "=== DETAILED DEBUG COMPLETE ==="

contains

    subroutine analyze_len_nlen_fields(len_low, len_high, nlen_low, nlen_high)
        integer(int8), intent(in) :: len_low, len_high, nlen_low, nlen_high
        integer :: len_val, nlen_val, expected_nlen
        
        ! Reconstruct LEN value (little endian)
        len_val = int(len_low) + 256 * int(len_high)
        if (len_low < 0) len_val = len_val + 256  ! Handle signed byte
        if (len_high < 0) len_val = len_val + 256 * 256  ! Handle signed byte
        
        ! Reconstruct NLEN value (little endian) 
        nlen_val = int(nlen_low) + 256 * int(nlen_high)
        if (nlen_low < 0) nlen_val = nlen_val + 256  ! Handle signed byte
        if (nlen_high < 0) nlen_val = nlen_val + 256 * 256  ! Handle signed byte
        
        print '(A,I0)', "  LEN value: ", len_val
        print '(A,I0)', "  NLEN value: ", nlen_val
        
        ! Expected NLEN should be one's complement of LEN (for 16-bit)
        expected_nlen = 65535 - len_val
        print '(A,I0)', "  Expected NLEN: ", expected_nlen
        
        if (nlen_val == expected_nlen) then
            print *, "  ✓ NLEN field is correct"
        else
            print *, "  ✗ NLEN field is WRONG!"
            print '(A,Z4.4,A,Z4.4)', "    Got: ", nlen_val, " Expected: ", expected_nlen
        end if
    end subroutine analyze_len_nlen_fields
    
    subroutine verify_adler32_checksum(data, data_len, checksum_bytes)
        integer(int8), intent(in) :: data(*), checksum_bytes(4)
        integer, intent(in) :: data_len
        integer(int32) :: calculated_adler, extracted_adler
        integer(int32) :: a, b
        integer :: i
        
        ! Calculate expected Adler-32
        a = 1_int32
        b = 0_int32
        
        do i = 1, data_len
            a = mod(a + int(data(i), int32), 65521_int32)
            b = mod(b + a, 65521_int32)
        end do
        
        calculated_adler = ior(ishft(b, 16), a)
        
        ! Extract Adler-32 from bytes (big endian)
        extracted_adler = 0_int32
        do i = 1, 4
            extracted_adler = ior(ishft(extracted_adler, 8), iand(int(checksum_bytes(i), int32), 255_int32))
        end do
        
        print '(A,Z8)', "  Calculated Adler-32: ", calculated_adler
        print '(A,Z8)', "  Extracted Adler-32:  ", extracted_adler
        
        if (calculated_adler == extracted_adler) then
            print *, "  ✓ Adler-32 checksum is correct"
        else
            print *, "  ✗ Adler-32 checksum is WRONG!"
        end if
    end subroutine verify_adler32_checksum

end program test_zlib_detailed_debug
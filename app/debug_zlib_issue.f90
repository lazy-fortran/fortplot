program debug_zlib_issue
    !! Debug zlib compression issue causing filter byte corruption
    use fortplot_zlib, only: zlib_compress, crc32_calculate
    use, intrinsic :: iso_fortran_env, only: int8
    implicit none
    
    ! Test data: simple PNG-style rows with filter bytes
    integer, parameter :: test_size = 8
    integer(1) :: test_data(test_size)
    integer(1), allocatable :: compressed(:)
    integer :: compressed_size, i
    
    ! Create test data: two rows with filter byte 0 each
    ! Row 1: filter=0, RGB=(255,0,0) 
    ! Row 2: filter=0, RGB=(0,255,0)
    test_data = [0_1, int(-1,1), 0_1, 0_1, 0_1, 0_1, int(-1,1), 0_1]
    
    print *, "=== Zlib Compression Debug ==="
    print *, "Original data:"
    do i = 1, test_size
        print *, "Byte", i, ":", int(test_data(i)), "(", test_data(i), ")"
    end do
    print *, ""
    
    ! Compress with our zlib implementation
    compressed = zlib_compress(test_data, test_size, compressed_size)
    
    if (.not. allocated(compressed)) then
        print *, "❌ Compression failed!"
        stop 1
    end if
    
    print *, "Compressed size:", compressed_size
    print *, "Compression ratio:", real(compressed_size) / real(test_size)
    print *, ""
    
    print *, "=== Zlib Header Analysis ==="
    print *, "CMF (Compression Method & Flags):", int(compressed(1))
    print *, "FLG (Flags):", int(compressed(2)) 
    print *, ""
    
    ! Check zlib header validity
    if (int(compressed(1)) == 120 .and. int(compressed(2)) == 94) then
        print *, "✅ Zlib header is valid"
    else
        print *, "❌ Zlib header is INVALID"
    end if
    
    ! Check Adler-32 checksum (last 4 bytes)
    if (compressed_size >= 6) then
        print *, "=== Adler-32 Checksum ==="
        print *, "Stored checksum bytes:", &
            int(compressed(compressed_size-3)), &
            int(compressed(compressed_size-2)), &
            int(compressed(compressed_size-1)), &
            int(compressed(compressed_size))
        print *, ""
    end if
    
    print *, "=== Deflate Block Analysis ==="
    print *, "Deflate block starts at byte 3"
    if (compressed_size > 6) then
        print *, "First few deflate bytes:"
        do i = 3, min(10, compressed_size-4)
            print *, "Deflate byte", i-2, ":", int(compressed(i))
        end do
    end if
    
    print *, ""
    print *, "=== Complete Compressed Data (Hex) ==="
    write(*, '(A)', advance='no') "Hex bytes: "
    do i = 1, compressed_size
        if (compressed(i) >= 0) then
            write(*, '(Z2.2)', advance='no') compressed(i)
        else
            write(*, '(Z2.2)', advance='no') 256 + compressed(i)
        end if
    end do
    print *, ""
    print *, ""
    
    print *, "=== Issue Analysis ==="
    print *, "Filter bytes in original:", int(test_data(1)), int(test_data(5))
    print *, "If PNG readers get filter=255, the deflate decompression is corrupting data"
    print *, "This suggests either:"
    print *, "1. Deflate compression implementation error"
    print *, "2. Bit ordering issue in Huffman coding" 
    print *, "3. Signed/unsigned conversion problem"
    
    deallocate(compressed)
end program debug_zlib_issue
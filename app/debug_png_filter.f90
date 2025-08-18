program debug_png_filter
    !! Debug PNG filter byte corruption during compression
    use fortplot_raster, only: bitmap_to_png_buffer
    use fortplot_zlib, only: zlib_compress
    use, intrinsic :: iso_fortran_env, only: int8
    implicit none
    
    ! Small test image: 2x2 pixels
    integer, parameter :: width = 2, height = 2
    integer(1) :: bitmap(width, height, 3)
    integer(1) :: png_buffer(height * (1 + width * 3))  ! 2 rows * (1 filter + 2*3 RGB)
    integer(1), allocatable :: compressed_data(:)
    integer :: compressed_size, i
    
    ! Create simple test pattern: red and blue pixels
    bitmap(1, 1, :) = [int(-1,1), 0_1, 0_1]    ! Red (255 = -1 in signed byte)
    bitmap(2, 1, :) = [0_1, 0_1, int(-1,1)]    ! Blue  
    bitmap(1, 2, :) = [0_1, int(-1,1), 0_1]    ! Green
    bitmap(2, 2, :) = [int(-1,1), int(-1,1), 0_1]  ! Yellow
    
    print *, "=== PNG Filter Byte Corruption Debug ==="
    print *, "Image size:", width, "x", height
    print *, "Buffer size:", size(png_buffer)
    print *, ""
    
    ! Convert bitmap to PNG buffer format
    call bitmap_to_png_buffer(bitmap, width, height, png_buffer)
    
    print *, "=== PNG Buffer BEFORE Compression ==="
    print *, "Row 1 filter byte:", int(png_buffer(1))
    print *, "Row 1 data:", (int(png_buffer(i)), i=2, 7)
    print *, "Row 2 filter byte:", int(png_buffer(8))
    print *, "Row 2 data:", (int(png_buffer(i)), i=9, 14)
    print *, ""
    
    ! Compress the PNG buffer
    compressed_data = zlib_compress(png_buffer, size(png_buffer), compressed_size)
    
    if (.not. allocated(compressed_data)) then
        print *, "❌ Compression FAILED!"
        stop 1
    end if
    
    print *, "=== Compression Results ==="
    print *, "Original size:", size(png_buffer)
    print *, "Compressed size:", compressed_size
    print *, "Compression ratio:", real(compressed_size) / real(size(png_buffer))
    print *, ""
    
    print *, "=== Compressed Data (first 20 bytes) ==="
    do i = 1, min(20, compressed_size)
        print *, "Byte", i, ":", int(compressed_data(i))
    end do
    print *, ""
    
    ! The critical issue: after decompression, are the filter bytes still 0?
    print *, "✅ Next: Decompress and verify filter bytes remain 0"
    print *, "❌ Current Issue: Filter bytes 0 -> 255 during compression/decompression"
    
    deallocate(compressed_data)
end program debug_png_filter
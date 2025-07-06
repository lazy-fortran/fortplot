program debug_final_bits
    use fortplot_jpeg, only: create_jpeg_canvas, jpeg_context, get_jpeg_data
    use, intrinsic :: iso_fortran_env, only: int8
    implicit none
    
    type(jpeg_context) :: ctx
    integer(int8), allocatable :: our_data(:)
    integer :: i, data_size
    
    ! Create minimal test JPEG
    ctx = create_jpeg_canvas(8, 8, 90)
    
    ! Fill with uniform gray  
    do i = 1, 192  ! 8*8*3 = 192 bytes
        ctx%raster%image_data(i) = int(127, int8)
    end do
    
    ! Get JPEG data
    call get_jpeg_data(8, 8, ctx%raster%image_data, 90, our_data)
    data_size = size(our_data)
    
    print *, "=== Final bytes analysis ==="
    print *, "Total size:", data_size
    print *, "Last 10 bytes (hex):"
    do i = data_size-9, data_size
        write(*,'(1X,Z2.2)',advance='no') our_data(i)
    end do
    print *
    
    ! Focus on the 2-byte difference
    print *, "Critical bytes (40 1f vs 45 15):"
    print *, "STB:  40 1f = 01000000 00011111"
    print *, "Ours: 45 15 = 01000101 00010101"
    
    ! Analyze bit patterns
    print *, "Bit difference analysis:"
    print *, "  40 vs 45: differs in bits 0 and 2"
    print *, "  1f vs 15: differs in bits 1 and 3"
    print *, "This suggests different final padding/EOB encoding"
    
end program debug_final_bits
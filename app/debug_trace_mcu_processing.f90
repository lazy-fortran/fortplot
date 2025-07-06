program debug_trace_mcu_processing
    use fortplot_jpeg
    use iso_fortran_env, only: int8
    implicit none
    
    ! Override the module to add debug output
    
    integer(1) :: image_data(32 * 32 * 3)  ! Smaller for clarity
    integer :: x, y, idx
    
    ! Create simple pattern
    do y = 0, 31
        do x = 0, 31
            idx = (y * 32 + x) * 3 + 1
            image_data(idx) = int(x * 8, 1)
            image_data(idx + 1) = int(y * 8, 1)
            image_data(idx + 2) = 127_int8
        end do
    end do
    
    print *, "Creating 32x32 test image"
    print *, "Expected: 2x2 = 4 MCUs with subsampling"
    print *, "Each MCU should have 4Y + 1U + 1V = 6 blocks"
    print *, "Total expected blocks: 24"
    
    call write_jpeg_file("debug_trace_32x32.jpg", 32, 32, image_data, 85)
    
end program debug_trace_mcu_processing
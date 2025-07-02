program debug_memory_partial
    use fortplot_mpeg_memory
    use iso_c_binding
    implicit none
    
    type(mem_t) :: full_image, loaded_partial
    integer :: i, j, expected_val
    integer, parameter :: full_width = 4, full_height = 3
    integer, parameter :: partial_width = 2, partial_height = 2  
    integer, parameter :: start_x = 1, start_y = 1  ! 0-based or 1-based?
    character(len=*), parameter :: full_file = "debug_full.dat"
    
    ! Create a 4x3 full image with identifiable pattern
    full_image = mem_create(full_width, full_height)
    print *, "Creating full image pattern:"
    do i = 1, full_height
        do j = 1, full_width
            full_image%data((i-1)*full_width + j) = int(i * 10 + j, c_int8_t)
            write(*, '(I3)', advance='no') int(full_image%data((i-1)*full_width + j))
        end do
        print *
    end do
    
    ! Save full image
    call mem_save(full_image, full_file)
    
    ! Load partial 2x2 region starting at (1,1)
    print *, "Loading partial region from (", start_x, ",", start_y, ") size", partial_width, "x", partial_height
    loaded_partial = mem_load_partial(full_file, start_x, start_y, &
                                    partial_width, partial_height, full_width)
    
    print *, "Loaded partial region:"
    do i = 1, partial_height
        do j = 1, partial_width
            write(*, '(I3)', advance='no') int(loaded_partial%data((i-1)*partial_width + j))
        end do
        print *
    end do
    
    print *, "Expected values should be:"
    do i = 1, partial_height
        do j = 1, partial_width
            expected_val = (start_y + i - 1) * 10 + (start_x + j - 1)
            write(*, '(I3)', advance='no') expected_val
        end do
        print *
    end do
    
    call mem_destroy(full_image)
    call mem_destroy(loaded_partial)
    
end program debug_memory_partial
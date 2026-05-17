program test_raster_text_rendering_split
    use fortplot_raster_text_rendering, only: render_text_to_image
    use fortplot_text_fonts, only: init_text_system
    implicit none

    integer, parameter :: width = 64
    integer, parameter :: height = 32
    integer(1), allocatable :: image_data(:)
    integer :: i, changed
    logical :: ok

    allocate(image_data(width * height * 3))
    image_data = -1_1

    ok = init_text_system()
    if (.not. ok) then
        error stop "text system init failed"
    end if

    call render_text_to_image(image_data, width, height, 5, 20, 'A', 0_1, 0_1, 0_1)

    changed = 0
    do i = 1, size(image_data)
        if (image_data(i) /= -1_1) then
            changed = changed + 1
        end if
    end do

    if (changed <= 0) then
        error stop "raster text rendering did not modify image"
    end if

    deallocate(image_data)

end program test_raster_text_rendering_split

program test_quad_fill_edges
    !! Regression test: ensure filled quads reach image borders (no 1px cut)
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_raster_primitives, only: draw_filled_quad_raster, color_to_byte
    implicit none

    integer, parameter :: width = 128, height = 96
    integer(1), allocatable :: image_data(:)
    real(wp) :: xq(4), yq(4)
    integer :: idx_center_right, idx_top_center
    logical :: pass

    allocate(image_data(width*height*3))
    image_data = 0_1

    ! Full-coverage quad: corners at the raster bounds
    xq = [1.0_wp, real(width, wp), real(width, wp), 1.0_wp]
    yq = [1.0_wp, 1.0_wp, real(height, wp), real(height, wp)]

    call draw_filled_quad_raster(image_data, width, height, xq, yq, 1.0_wp, 0.0_wp, 0.0_wp)

    ! Sample a pixel on the right edge midpoint (should be filled red)
    idx_center_right = 3 * ((height/2 - 1) * width + (width - 1)) + 1
    ! Sample a pixel on the top edge midpoint (should be filled red)
    idx_top_center = 3 * ((0) * width + (width/2 - 1)) + 1

    pass = (image_data(idx_center_right) == color_to_byte(1.0_wp)) .and. &
           (image_data(idx_center_right+1) == 0_1) .and. &
           (image_data(idx_center_right+2) == 0_1) .and. &
           (image_data(idx_top_center) == color_to_byte(1.0_wp))

    if (pass) then
        print *, 'PASS: Filled quad reaches image borders (no 1px cut)'
    else
        print *, 'FAIL: Filled quad does not reach image borders'
        stop 1
    end if
end program test_quad_fill_edges


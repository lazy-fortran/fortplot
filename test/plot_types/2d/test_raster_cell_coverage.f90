program test_raster_cell_coverage
    !! Edge-defined pcolormesh cells must cover the raster without seams.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_raster, only: create_raster_canvas, raster_context
    implicit none

    integer, parameter :: width = 64, height = 48
    real(wp), parameter :: x_edges(3) = [0.0_wp, 0.5_wp, 1.0_wp]
    real(wp), parameter :: y_edges(3) = [0.0_wp, 0.5_wp, 1.0_wp]
    real(wp), parameter :: values(2, 2) = reshape([ &
        0.25_wp, 0.25_wp, 0.25_wp, 0.25_wp], [2, 2])
    type(raster_context) :: context
    integer :: pixel, covered_pixels

    context = create_raster_canvas(width, height, 100.0_wp)
    context%x_min = 0.0_wp
    context%x_max = 1.0_wp
    context%y_min = 0.0_wp
    context%y_max = 1.0_wp
    context%plot_area%left = 1
    context%plot_area%bottom = 1
    context%plot_area%width = width
    context%plot_area%height = height
    call context%fill_heatmap( &
        x_edges, y_edges, values, 0.0_wp, 1.0_wp, 'viridis')

    covered_pixels = 0
    do pixel = 1, width*height
        if (.not. (context%raster%image_data(3*pixel - 2) == int(-1, 1) .and. &
            context%raster%image_data(3*pixel - 1) == int(-1, 1) .and. &
            context%raster%image_data(3*pixel) == int(-1, 1))) then
            covered_pixels = covered_pixels + 1
        end if
    end do

    if (covered_pixels /= width*height) then
        error stop 'edge-defined pcolormesh left raster seams or uncovered pixels'
    end if
    print *, 'PASS: edge-defined pcolormesh covers the raster without seams'
end program test_raster_cell_coverage

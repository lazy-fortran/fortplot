program test_ylabel_rotation_clipping
    !! Validate rotated Y-axis label renders without top-row clipping (issue #996)
    use fortplot_raster_axes,  only: raster_render_ylabel
    use fortplot_raster_core,  only: raster_image_t, create_raster_image, destroy_raster_image
    use fortplot_layout,       only: plot_margins_t, plot_area_t, calculate_plot_area
    use, intrinsic :: iso_fortran_env, only: error_unit, wp => real64
    implicit none

    type(raster_image_t) :: raster
    type(plot_margins_t) :: margins
    type(plot_area_t)    :: plot_area
    integer :: width, height
    integer :: left_band_x0, left_band_x1, y0, y1
    integer :: nonwhite_count
    character(len=*), parameter :: label = 'bdfgjqy'  ! includes ascenders/descenders

    ! Create a small raster canvas and standard plot area
    width = 400
    height = 300
    raster = create_raster_image(width, height)
    call calculate_plot_area(width, height, margins, plot_area)

    ! Render rotated Y label next to the plot area
    call raster_render_ylabel(raster, width, height, plot_area, label)

    ! Inspect a vertical band to the left of the plot area where the ylabel is placed
    ! Expect at least some non-white pixels from the rendered text
    left_band_x1 = max(1, plot_area%left - 1)
    left_band_x0 = max(1, plot_area%left - 80)  ! generous band width to catch the label
    y0 = max(1, plot_area%bottom)
    y1 = min(height, plot_area%bottom + plot_area%height)

    nonwhite_count = count_non_white_in_band(raster%image_data, width, height, left_band_x0, left_band_x1, y0, y1)

    if (nonwhite_count <= 0) then
        write(error_unit,*) 'ERROR: Rotated ylabel appears missing or fully white (possible clipping).'
        write(error_unit,*) '       Expected non-white pixels in band x=', left_band_x0, '-', left_band_x1, ' y=', y0, '-', y1
        call destroy_raster_image(raster)
        stop 1
    end if

    call destroy_raster_image(raster)
    print *, 'PASS: Rotated ylabel rendered with non-white pixels (no clipping).'

contains

    function count_non_white_in_band(img, w, h, x0, x1, y0, y1) result(cnt)
        integer(1), intent(in) :: img(*)
        integer, intent(in) :: w, h, x0, x1, y0, y1
        integer :: cnt, x, y, idx
        cnt = 0
        do y = y0, y1
            do x = x0, x1
                idx = ((y - 1) * w + (x - 1)) * 3 + 1
                if (idx >= 1 .and. idx + 2 <= w * h * 3) then
                    if (img(idx) /= -1_1 .or. img(idx+1) /= -1_1 .or. img(idx+2) /= -1_1) then
                        cnt = cnt + 1
                    end if
                end if
            end do
        end do
    end function count_non_white_in_band

end program test_ylabel_rotation_clipping


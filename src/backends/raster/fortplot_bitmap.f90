module fortplot_bitmap
    use iso_c_binding
    use fortplot_text_fonts, only: init_text_system, is_font_initialized, &
                                   get_font_scale_for_size, get_global_font
    use fortplot_text_rendering, only: render_text_to_image, render_text_with_size
    use fortplot_stb_truetype, only: stb_get_font_vmetrics, stb_fontinfo_t
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: initialize_white_background, composite_image, composite_bitmap_to_raster
    public :: render_text_to_bitmap, rotate_bitmap_90_cw, rotate_bitmap_90_ccw
    public :: composite_bitmap_to_raster_0
    public :: render_text_to_bitmap_with_size
    public :: rotate_bitmap_about_anchor
    public :: get_text_bitmap_metrics

contains

    subroutine initialize_white_background(image_data, w, h)
        integer(1), intent(out) :: image_data(:)
        integer, intent(in) :: w, h
        integer :: expected_size

        ! Validate inputs
        if (w <= 0 .or. h <= 0) return

        expected_size = w*h*3

        ! Validate array size matches expected size
        if (size(image_data) < expected_size) then
            return
        end if

        ! Use intrinsic assignment to initialize entire array at once - safer
        image_data = -1_1  ! White = 255 = -1 in signed byte

    end subroutine initialize_white_background

    subroutine composite_image(main_image, main_width, main_height, &
                               overlay_image, overlay_width, overlay_height, &
                               dest_x, dest_y)
        integer(1), intent(inout) :: main_image(*)
        integer, intent(in) :: main_width, main_height
        integer(1), intent(in) :: overlay_image(*)
        integer, intent(in) :: overlay_width, overlay_height, dest_x, dest_y
        integer :: x, y, src_idx, dst_idx, img_x, img_y

        do y = 1, overlay_height
            do x = 1, overlay_width
                img_x = dest_x + x - 1
                img_y = dest_y + y - 1

                if (img_x >= 1 .and. img_x <= main_width .and. &
                    img_y >= 1 .and. img_y <= main_height) then

                    src_idx = ((y - 1)*overlay_width + (x - 1))*3 + 1
                    dst_idx = ((img_y - 1)*main_width + (img_x - 1))*3 + 1

                    if (overlay_image(src_idx) /= -1_1 .or. &
                        overlay_image(src_idx + 1) /= -1_1 .or. &
                        overlay_image(src_idx + 2) /= -1_1) then
                        main_image(dst_idx:dst_idx + 2) = &
                            overlay_image(src_idx:src_idx + 2)
                    end if
                end if
            end do
        end do
    end subroutine composite_image

    subroutine composite_bitmap_to_raster(raster_buffer, raster_width, raster_height, &
                                          bitmap, bitmap_width, bitmap_height, &
                                          dest_x, dest_y)
        !! Composite 3D RGB bitmap directly onto raster image buffer
        integer(1), intent(inout) :: raster_buffer(*)
        integer, intent(in) :: raster_width, raster_height
        integer(1), intent(in) :: bitmap(:, :, :)
        integer, intent(in) :: bitmap_width, bitmap_height, dest_x, dest_y
        integer :: x, y, raster_x, raster_y, raster_idx

        do y = 1, bitmap_height
            do x = 1, bitmap_width
                raster_x = dest_x + x - 1
                raster_y = dest_y + y - 1

                ! Check bounds
                if (raster_x >= 1 .and. raster_x <= raster_width .and. &
                    raster_y >= 1 .and. raster_y <= raster_height) then

                    ! Skip white pixels (don't composite background)
                    if (bitmap(x, y, 1) /= -1_1 .or. &
                        bitmap(x, y, 2) /= -1_1 .or. &
                        bitmap(x, y, 3) /= -1_1) then

                        raster_idx = ((raster_y - 1)*raster_width + (raster_x &
                                                                     - 1))*3 + 1
                        raster_buffer(raster_idx) = bitmap(x, y, 1)  ! R
                        raster_buffer(raster_idx + 1) = bitmap(x, y, 2)  ! G
                        raster_buffer(raster_idx + 2) = bitmap(x, y, 3)  ! B
                    end if
                end if
            end do
        end do
    end subroutine composite_bitmap_to_raster

    subroutine composite_bitmap_to_raster_0(raster_buffer, raster_width, &
                                            raster_height, &
                                            bitmap, bitmap_width, bitmap_height, &
                                            dest_x0, dest_y0)
        !! Composite 3D RGB bitmap onto raster buffer using 0-based destination coords
        integer(1), intent(inout) :: raster_buffer(:)
        integer, intent(in) :: raster_width, raster_height
        integer(1), intent(in) :: bitmap(:, :, :)
        integer, intent(in) :: bitmap_width, bitmap_height
        integer, intent(in) :: dest_x0, dest_y0

        integer :: x, y
        integer :: raster_x0, raster_y0
        integer :: raster_idx

        do y = 1, bitmap_height
            do x = 1, bitmap_width
                raster_x0 = dest_x0 + (x - 1)
                raster_y0 = dest_y0 + (y - 1)

                if (raster_x0 >= 0 .and. raster_x0 < raster_width .and. &
                    raster_y0 >= 0 .and. raster_y0 < raster_height) then
                    if (bitmap(x, y, 1) /= -1_1 .or. &
                        bitmap(x, y, 2) /= -1_1 .or. &
                        bitmap(x, y, 3) /= -1_1) then
                        raster_idx = (raster_y0*raster_width + raster_x0)*3 + 1
                        if (raster_idx >= 1 .and. raster_idx + 2 <= &
                            raster_width*raster_height*3) then
                            raster_buffer(raster_idx) = bitmap(x, y, 1)
                            raster_buffer(raster_idx + 1) = bitmap(x, y, 2)
                            raster_buffer(raster_idx + 2) = bitmap(x, y, 3)
                        end if
                    end if
                end if
            end do
        end do
    end subroutine composite_bitmap_to_raster_0

    subroutine render_text_to_bitmap(bitmap, width, height, x, y, text)
        !! Render text to RGB bitmap by using existing PNG rendering then converting
        integer(1), intent(inout) :: bitmap(:, :, :)
        integer, intent(in) :: width, height, x, y
        character(len=*), intent(in) :: text

        ! Create temporary PNG buffer for text rendering
        integer(1), allocatable :: temp_buffer(:)
        integer :: i, j, buf_idx

        allocate (temp_buffer(width*height*3))
        call initialize_white_background(temp_buffer, width, height)
        call render_text_to_image(temp_buffer, width, height, x, y, text, 0_1, 0_1, 0_1)

        ! Convert PNG buffer to bitmap
        do j = 1, height
            do i = 1, width
                buf_idx = ((j - 1)*width + (i - 1))*3 + 1
                bitmap(i, j, 1) = temp_buffer(buf_idx)     ! R
                bitmap(i, j, 2) = temp_buffer(buf_idx + 1) ! G
                bitmap(i, j, 3) = temp_buffer(buf_idx + 2) ! B
            end do
        end do

        if (allocated(temp_buffer)) deallocate (temp_buffer)
    end subroutine render_text_to_bitmap

    subroutine render_text_to_bitmap_with_size(bitmap, width, height, x0, y0, text, &
                                               r, g, b, pixel_height)
        !! Render text to an RGB bitmap using a given font size (pixel_height).
        integer(1), intent(inout) :: bitmap(:, :, :)
        integer, intent(in) :: width, height
        integer, intent(in) :: x0, y0
        character(len=*), intent(in) :: text
        integer(1), intent(in) :: r, g, b
        real(wp), intent(in) :: pixel_height

        integer(1), allocatable :: temp_buffer(:)
        integer :: i, j, buf_idx

        allocate (temp_buffer(width*height*3))
        call initialize_white_background(temp_buffer, width, height)
        call render_text_with_size(temp_buffer, width, height, x0, y0, text, r, g, b, &
                                   pixel_height)

        do j = 1, height
            do i = 1, width
                buf_idx = ((j - 1)*width + (i - 1))*3 + 1
                bitmap(i, j, 1) = temp_buffer(buf_idx)
                bitmap(i, j, 2) = temp_buffer(buf_idx + 1)
                bitmap(i, j, 3) = temp_buffer(buf_idx + 2)
            end do
        end do

        deallocate (temp_buffer)
    end subroutine render_text_to_bitmap_with_size

    subroutine get_text_bitmap_metrics(pixel_height, ascent_px, descent_px, height_px, &
                                       success)
        !! Get ascent/descent/height for a given pixel_height.
        real(wp), intent(in) :: pixel_height
        real(wp), intent(out) :: ascent_px, descent_px
        integer, intent(out) :: height_px
        logical, intent(out) :: success

        type(stb_fontinfo_t) :: font
        real(wp) :: scale
        integer :: ascent, descent, line_gap

        success = .false.
        ascent_px = 0.0_wp
        descent_px = 0.0_wp
        height_px = 0

        if (.not. is_font_initialized()) then
            if (.not. init_text_system()) return
        end if

        font = get_global_font()
        scale = get_font_scale_for_size(max(1.0_wp, pixel_height))

        call stb_get_font_vmetrics(font, ascent, descent, line_gap)
        ascent_px = real(ascent, wp)*scale
        descent_px = real(descent, wp)*scale
        height_px = max(1, int((real(ascent - descent, wp)*scale)))

        success = .true.
    end subroutine get_text_bitmap_metrics

    subroutine rotate_bitmap_90_ccw(src_bitmap, dst_bitmap, src_width, src_height)
        !! Rotate bitmap 90 degrees counter-clockwise
        !! For arrays: (i,j) maps to (height-j+1, i) with swapped dimensions
        integer(1), intent(in) :: src_bitmap(:, :, :)
        integer(1), intent(out) :: dst_bitmap(:, :, :)
        integer, intent(in) :: src_width, src_height
        integer :: i, j

        do j = 1, src_height
            do i = 1, src_width
                ! CCW: each point (i,j) goes to position that produces CCW rotation
                dst_bitmap(j, src_width - i + 1, :) = src_bitmap(i, j, :)
            end do
        end do
    end subroutine rotate_bitmap_90_ccw

    subroutine rotate_bitmap_90_cw(src_bitmap, dst_bitmap, src_width, src_height)
        !! Rotate bitmap 90 degrees clockwise
        !! For arrays: (i,j) maps to (j, width-i+1) with swapped dimensions
        integer(1), intent(in) :: src_bitmap(:, :, :)
        integer(1), intent(out) :: dst_bitmap(:, :, :)
        integer, intent(in) :: src_width, src_height
        integer :: i, j

        do j = 1, src_height
            do i = 1, src_width
                ! CW: each point (i,j) goes to position that produces CW rotation
                dst_bitmap(src_height - j + 1, i, :) = src_bitmap(i, j, :)
            end do
        end do
    end subroutine rotate_bitmap_90_cw

    subroutine rotate_bitmap_about_anchor(src_bitmap, src_width, src_height, &
                                          angle_deg, &
                                          anchor_x, anchor_y, dst_bitmap, dst_width, &
                                          dst_height, dst_anchor_x, dst_anchor_y)
        !! Rotate an RGB bitmap around an anchor point, returning the rotated bitmap
        !! and the anchor location in the destination bitmap.
        !!
        !! Coordinates are in pixel boundary space:
        !! - source bitmap spans x=[0,src_width], y=[0,src_height]
        !! - anchor_x/anchor_y can be any value in that space (e.g. 0, width/2, width)
        integer(1), intent(in) :: src_bitmap(:, :, :)
        integer, intent(in) :: src_width, src_height
        real(wp), intent(in) :: angle_deg
        real(wp), intent(in) :: anchor_x, anchor_y
        integer(1), allocatable, intent(out) :: dst_bitmap(:, :, :)
        integer, intent(out) :: dst_width, dst_height
        real(wp), intent(out) :: dst_anchor_x, dst_anchor_y

        real(wp) :: angle_norm
        real(wp) :: theta, cos_t, sin_t
        real(wp) :: corners_x(4), corners_y(4)
        real(wp) :: rx(4), ry(4)
        real(wp) :: min_x, max_x, min_y, max_y
        real(wp) :: min_xi, min_yi
        integer :: i, j
        real(wp) :: xr, yr, xs, ys
        real(wp) :: ix_f, iy_f, tx, ty
        integer :: ix0, iy0, ix1, iy1
        real(wp) :: c00, c10, c01, c11
        real(wp) :: v0, v1, v
        integer :: ch

        angle_norm = modulo(angle_deg, 360.0_wp)
        if (angle_norm > 180.0_wp) angle_norm = angle_norm - 360.0_wp

        if (abs(angle_norm) < 1.0e-9_wp) then
            dst_width = src_width
            dst_height = src_height
            allocate (dst_bitmap(dst_width, dst_height, 3))
            dst_bitmap = src_bitmap
            dst_anchor_x = anchor_x
            dst_anchor_y = anchor_y
            return
        end if

        if (abs(angle_norm - 90.0_wp) < 1.0e-9_wp) then
            dst_width = src_height
            dst_height = src_width
            allocate (dst_bitmap(dst_width, dst_height, 3))
            call rotate_bitmap_90_ccw(src_bitmap, dst_bitmap, src_width, src_height)
            dst_anchor_x = anchor_y
            dst_anchor_y = real(src_width, wp) - anchor_x
            return
        end if

        if (abs(angle_norm + 90.0_wp) < 1.0e-9_wp) then
            dst_width = src_height
            dst_height = src_width
            allocate (dst_bitmap(dst_width, dst_height, 3))
            call rotate_bitmap_90_cw(src_bitmap, dst_bitmap, src_width, src_height)
            dst_anchor_x = real(src_height, wp) - anchor_y
            dst_anchor_y = anchor_x
            return
        end if

        if (abs(angle_norm - 180.0_wp) < 1.0e-9_wp .or. abs(angle_norm + 180.0_wp) < &
            1.0e-9_wp) then
            dst_width = src_width
            dst_height = src_height
            allocate (dst_bitmap(dst_width, dst_height, 3))
            dst_bitmap = -1_1
            do j = 1, src_height
                do i = 1, src_width
                    dst_bitmap(src_width - i + 1, src_height - j + 1, :) = &
                        src_bitmap(i, j, :)
                end do
            end do
            dst_anchor_x = real(src_width, wp) - anchor_x
            dst_anchor_y = real(src_height, wp) - anchor_y
            return
        end if

        theta = angle_norm*acos(-1.0_wp)/180.0_wp
        cos_t = cos(theta)
        sin_t = sin(theta)

        corners_x = [0.0_wp, real(src_width, wp), 0.0_wp, real(src_width, wp)] - &
                    anchor_x
        corners_y = [0.0_wp, 0.0_wp, real(src_height, wp), real(src_height, wp)] - &
                    anchor_y

        do i = 1, 4
            rx(i) = corners_x(i)*cos_t - corners_y(i)*sin_t
            ry(i) = corners_x(i)*sin_t + corners_y(i)*cos_t
        end do

        min_x = minval(rx)
        max_x = maxval(rx)
        min_y = minval(ry)
        max_y = maxval(ry)

        dst_width = max(1, int(ceiling(max_x - min_x)))
        dst_height = max(1, int(ceiling(max_y - min_y)))
        allocate (dst_bitmap(dst_width, dst_height, 3))
        dst_bitmap = -1_1

        dst_anchor_x = -min_x
        dst_anchor_y = -min_y

        min_xi = min_x
        min_yi = min_y

        do j = 1, dst_height
            do i = 1, dst_width
                xr = min_xi + (real(i, wp) - 0.5_wp)
                yr = min_yi + (real(j, wp) - 0.5_wp)

                xs = xr*cos_t + yr*sin_t + anchor_x
                ys = -xr*sin_t + yr*cos_t + anchor_y

                ix_f = min(max(xs + 0.5_wp, 1.0_wp), real(src_width, wp))
                iy_f = min(max(ys + 0.5_wp, 1.0_wp), real(src_height, wp))

                ix0 = int(floor(ix_f))
                iy0 = int(floor(iy_f))
                ix1 = min(ix0 + 1, src_width)
                iy1 = min(iy0 + 1, src_height)

                tx = ix_f - real(ix0, wp)
                ty = iy_f - real(iy0, wp)

                do ch = 1, 3
                    c00 = real(iand(int(src_bitmap(ix0, iy0, ch)), 255), wp)
                    c10 = real(iand(int(src_bitmap(ix1, iy0, ch)), 255), wp)
                    c01 = real(iand(int(src_bitmap(ix0, iy1, ch)), 255), wp)
                    c11 = real(iand(int(src_bitmap(ix1, iy1, ch)), 255), wp)

                    v0 = c00*(1.0_wp - tx) + c10*tx
                    v1 = c01*(1.0_wp - tx) + c11*tx
                    v = v0*(1.0_wp - ty) + v1*ty

                    dst_bitmap(i, j, ch) = int(nint(min(max(v, 0.0_wp), 255.0_wp)), 1)
                end do
            end do
        end do
    end subroutine rotate_bitmap_about_anchor

end module fortplot_bitmap

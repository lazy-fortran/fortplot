module fortplot_raster
    use iso_c_binding
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: raster_image_t, create_raster_image, destroy_raster_image
    public :: initialize_white_background, color_to_byte
    public :: draw_line_distance_aa, blend_pixel, composite_image
    public :: distance_point_to_line_segment, ipart, fpart, rfpart
    public :: render_text_to_bitmap, rotate_bitmap_90_cw, bitmap_to_png_buffer

    integer, parameter :: DEFAULT_RASTER_LINE_WIDTH_SCALING = 10

    type :: raster_image_t
        integer(1), allocatable :: image_data(:)
        integer :: width, height
        real(wp) :: current_r = 0.0_wp, current_g = 0.0_wp, current_b = 1.0_wp
        real(wp) :: current_line_width = 1.0_wp
    contains
        procedure :: set_color => raster_set_color
        procedure :: get_color_bytes => raster_get_color_bytes
    end type raster_image_t

contains

    function create_raster_image(width, height) result(image)
        integer, intent(in) :: width, height
        type(raster_image_t) :: image

        image%width = width
        image%height = height
        allocate(image%image_data(height * (1 + width * 3)))
        call initialize_white_background(image%image_data, width, height)
    end function create_raster_image

    subroutine destroy_raster_image(image)
        type(raster_image_t), intent(inout) :: image
        if (allocated(image%image_data)) deallocate(image%image_data)
    end subroutine destroy_raster_image

    subroutine raster_set_color(this, r, g, b)
        class(raster_image_t), intent(inout) :: this
        real(wp), intent(in) :: r, g, b

        this%current_r = r
        this%current_g = g
        this%current_b = b
    end subroutine raster_set_color

    subroutine raster_get_color_bytes(this, r, g, b)
        class(raster_image_t), intent(in) :: this
        integer(1), intent(out) :: r, g, b

        r = color_to_byte(this%current_r)
        g = color_to_byte(this%current_g)
        b = color_to_byte(this%current_b)
    end subroutine raster_get_color_bytes

    subroutine initialize_white_background(image_data, w, h)
        integer(1), intent(out) :: image_data(*)
        integer, intent(in) :: w, h
        integer :: i, j, k

        k = 1
        do i = 1, h
            image_data(k) = 0_1
            k = k + 1
            do j = 1, w
                image_data(k) = -1_1
                image_data(k+1) = -1_1
                image_data(k+2) = -1_1
                k = k + 3
            end do
        end do
    end subroutine initialize_white_background

    function color_to_byte(color_val) result(byte_val)
        real(wp), intent(in) :: color_val
        integer(1) :: byte_val
        integer :: int_val

        int_val = int(color_val * 255.0_wp)
        if (int_val > 127) then
            byte_val = int(int_val - 256, 1)
        else
            byte_val = int(int_val, 1)
        end if
    end function color_to_byte

    subroutine draw_line_distance_aa(image_data, img_w, img_h, x0, y0, x1, y1, r, g, b, width)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: img_w, img_h
        real(wp), intent(in) :: x0, y0, x1, y1
        integer(1), intent(in) :: r, g, b
        real(wp), intent(in) :: width
        real(wp) :: line_width

        real(wp) :: dx, dy, length, nx, ny
        integer :: x_min, x_max, y_min, y_max, x, y
        real(wp) :: px, py, dist, alpha

        line_width = width

        dx = x1 - x0
        dy = y1 - y0
        length = sqrt(dx*dx + dy*dy)

        if (length < 1e-6_wp) return

        nx = -dy / length
        ny = dx / length

        x_min = max(1, int(min(x0, x1) - line_width - 1.0_wp))
        x_max = min(img_w, int(max(x0, x1) + line_width + 1.0_wp))
        y_min = max(1, int(min(y0, y1) - line_width - 1.0_wp))
        y_max = min(img_h, int(max(y0, y1) + line_width + 1.0_wp))

        do y = y_min, y_max
            do x = x_min, x_max
                px = real(x, wp)
                py = real(y, wp)

                dist = distance_point_to_line_segment(px, py, x0, y0, x1, y1)
                if (dist <= line_width) then
                    alpha = 1.0_wp
                else if (dist <= line_width + 1.0_wp) then
                    alpha = line_width + 1.0_wp - dist
                else
                    alpha = 0.0_wp
                end if

                if (alpha > 0.0_wp) then
                    call blend_pixel(image_data, img_w, img_h, x, y, alpha, r, g, b)
                end if
            end do
        end do
    end subroutine draw_line_distance_aa

    real(wp) function distance_point_to_line_segment(px, py, x0, y0, x1, y1) result(dist)
        real(wp), intent(in) :: px, py, x0, y0, x1, y1
        real(wp) :: dx, dy, length_sq, t, closest_x, closest_y

        dx = x1 - x0
        dy = y1 - y0
        length_sq = dx*dx + dy*dy

        if (length_sq < 1e-12_wp) then
            dist = sqrt((px - x0)**2 + (py - y0)**2)
            return
        end if

        t = ((px - x0) * dx + (py - y0) * dy) / length_sq
        t = max(0.0_wp, min(1.0_wp, t))

        closest_x = x0 + t * dx
        closest_y = y0 + t * dy

        dist = sqrt((px - closest_x)**2 + (py - closest_y)**2)
    end function distance_point_to_line_segment

    subroutine blend_pixel(image_data, img_w, img_h, x, y, alpha, new_r, new_g, new_b)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: img_w, img_h, x, y
        real(wp), intent(in) :: alpha
        integer(1), intent(in) :: new_r, new_g, new_b
        integer :: k
        real(wp) :: inv_alpha
        integer :: bg_r, bg_g, bg_b, fg_r, fg_g, fg_b

        if (x < 1 .or. x > img_w .or. y < 1 .or. y > img_h) return
        if (alpha <= 0.0_wp) return

        k = (y - 1) * (1 + img_w * 3) + 1 + (x - 1) * 3 + 1

        if (alpha >= 1.0_wp) then
            image_data(k) = new_r
            image_data(k+1) = new_g
            image_data(k+2) = new_b
        else
            inv_alpha = 1.0_wp - alpha

            bg_r = int(image_data(k))
            bg_g = int(image_data(k+1))
            bg_b = int(image_data(k+2))
            if (bg_r < 0) bg_r = bg_r + 256
            if (bg_g < 0) bg_g = bg_g + 256
            if (bg_b < 0) bg_b = bg_b + 256

            fg_r = int(new_r)
            fg_g = int(new_g)
            fg_b = int(new_b)
            if (fg_r < 0) fg_r = fg_r + 256
            if (fg_g < 0) fg_g = fg_g + 256
            if (fg_b < 0) fg_b = fg_b + 256

            bg_r = int(inv_alpha * real(bg_r) + alpha * real(fg_r))
            bg_g = int(inv_alpha * real(bg_g) + alpha * real(fg_g))
            bg_b = int(inv_alpha * real(bg_b) + alpha * real(fg_b))

            if (bg_r > 127) bg_r = bg_r - 256
            if (bg_g > 127) bg_g = bg_g - 256
            if (bg_b > 127) bg_b = bg_b - 256

            image_data(k) = int(bg_r, 1)
            image_data(k+1) = int(bg_g, 1)
            image_data(k+2) = int(bg_b, 1)
        end if
    end subroutine blend_pixel

    subroutine composite_image(main_image, main_width, main_height, &
                              overlay_image, overlay_width, overlay_height, dest_x, dest_y)
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
                    
                    src_idx = (y - 1) * (1 + overlay_width * 3) + 1 + (x - 1) * 3 + 1
                    dst_idx = (img_y - 1) * (1 + main_width * 3) + 1 + (img_x - 1) * 3 + 1
                    
                    if (overlay_image(src_idx) /= -1_1 .or. &
                        overlay_image(src_idx+1) /= -1_1 .or. &
                        overlay_image(src_idx+2) /= -1_1) then
                        main_image(dst_idx:dst_idx+2) = overlay_image(src_idx:src_idx+2)
                    end if
                end if
            end do
        end do
    end subroutine composite_image

    real(wp) function ipart(x)
        real(wp), intent(in) :: x
        ipart = floor(x)
    end function ipart

    real(wp) function fpart(x)
        real(wp), intent(in) :: x
        fpart = x - floor(x)
    end function fpart

    real(wp) function rfpart(x)
        real(wp), intent(in) :: x
        rfpart = 1.0_wp - fpart(x)
    end function rfpart

    subroutine render_text_to_bitmap(bitmap, width, height, x, y, text)
        !! Render text to RGB bitmap by using existing PNG rendering then converting
        use fortplot_text, only: render_text_to_image
        integer(1), intent(inout) :: bitmap(:,:,:)
        integer, intent(in) :: width, height, x, y
        character(len=*), intent(in) :: text
        
        ! Create temporary PNG buffer for text rendering
        integer(1), allocatable :: temp_buffer(:)
        integer :: i, j, buf_idx
        
        allocate(temp_buffer(height * (1 + width * 3)))
        call initialize_white_background(temp_buffer, width, height)
        call render_text_to_image(temp_buffer, width, height, x, y, text, 0_1, 0_1, 0_1)
        
        ! Convert PNG buffer to bitmap
        do j = 1, height
            do i = 1, width
                buf_idx = (j - 1) * (1 + width * 3) + 1 + (i - 1) * 3 + 1
                bitmap(i, j, 1) = temp_buffer(buf_idx)     ! R
                bitmap(i, j, 2) = temp_buffer(buf_idx + 1) ! G  
                bitmap(i, j, 3) = temp_buffer(buf_idx + 2) ! B
            end do
        end do
        
        deallocate(temp_buffer)
    end subroutine render_text_to_bitmap

    subroutine rotate_bitmap_90_cw(src_bitmap, dst_bitmap, src_width, src_height)
        !! Rotate bitmap 90 degrees clockwise: (x,y) -> (y, src_width-x+1)
        integer(1), intent(in) :: src_bitmap(:,:,:)
        integer(1), intent(out) :: dst_bitmap(:,:,:)
        integer, intent(in) :: src_width, src_height
        integer :: i, j
        
        do j = 1, src_height
            do i = 1, src_width
                dst_bitmap(j, src_width - i + 1, :) = src_bitmap(i, j, :)
            end do
        end do
    end subroutine rotate_bitmap_90_cw

    subroutine bitmap_to_png_buffer(bitmap, width, height, buffer)
        !! Convert 3D RGB bitmap to PNG buffer format
        integer(1), intent(in) :: bitmap(:,:,:)
        integer, intent(in) :: width, height
        integer(1), intent(out) :: buffer(:)
        integer :: i, j, buf_idx
        
        call initialize_white_background(buffer, width, height)
        
        do j = 1, height
            do i = 1, width
                buf_idx = (j - 1) * (1 + width * 3) + 1 + (i - 1) * 3 + 1
                buffer(buf_idx)     = bitmap(i, j, 1) ! R
                buffer(buf_idx + 1) = bitmap(i, j, 2) ! G
                buffer(buf_idx + 2) = bitmap(i, j, 3) ! B
            end do
        end do
    end subroutine bitmap_to_png_buffer

end module fortplot_raster
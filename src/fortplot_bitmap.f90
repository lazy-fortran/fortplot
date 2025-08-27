module fortplot_bitmap
    use iso_c_binding
    use fortplot_text, only: render_text_to_image
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: initialize_white_background, composite_image, composite_bitmap_to_raster
    public :: render_text_to_bitmap, rotate_bitmap_90_cw, rotate_bitmap_90_ccw

contains

    subroutine initialize_white_background(image_data, w, h)
        integer(1), intent(out) :: image_data(:)
        integer, intent(in) :: w, h
        integer :: expected_size

        ! Validate inputs
        if (w <= 0 .or. h <= 0) return
        
        expected_size = w * h * 3
        
        ! Validate array size matches expected size
        if (size(image_data) < expected_size) then
            return
        end if
        
        ! Use intrinsic assignment to initialize entire array at once - safer
        image_data = -1_1  ! White = 255 = -1 in signed byte
        
    end subroutine initialize_white_background

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
                    
                    src_idx = ((y - 1) * overlay_width + (x - 1)) * 3 + 1
                    dst_idx = ((img_y - 1) * main_width + (img_x - 1)) * 3 + 1
                    
                    if (overlay_image(src_idx) /= -1_1 .or. &
                        overlay_image(src_idx+1) /= -1_1 .or. &
                        overlay_image(src_idx+2) /= -1_1) then
                        main_image(dst_idx:dst_idx+2) = overlay_image(src_idx:src_idx+2)
                    end if
                end if
            end do
        end do
    end subroutine composite_image

    subroutine composite_bitmap_to_raster(raster_buffer, raster_width, raster_height, &
                                         bitmap, bitmap_width, bitmap_height, dest_x, dest_y)
        !! Composite 3D RGB bitmap directly onto raster image buffer
        integer(1), intent(inout) :: raster_buffer(*)
        integer, intent(in) :: raster_width, raster_height
        integer(1), intent(in) :: bitmap(:,:,:)
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
                        
                        raster_idx = ((raster_y - 1) * raster_width + (raster_x - 1)) * 3 + 1
                        raster_buffer(raster_idx)     = bitmap(x, y, 1)  ! R
                        raster_buffer(raster_idx + 1) = bitmap(x, y, 2)  ! G
                        raster_buffer(raster_idx + 2) = bitmap(x, y, 3)  ! B
                    end if
                end if
            end do
        end do
    end subroutine composite_bitmap_to_raster

    subroutine render_text_to_bitmap(bitmap, width, height, x, y, text)
        !! Render text to RGB bitmap by using existing PNG rendering then converting
        use fortplot_text, only: render_text_to_image
        integer(1), intent(inout) :: bitmap(:,:,:)
        integer, intent(in) :: width, height, x, y
        character(len=*), intent(in) :: text
        
        ! Create temporary PNG buffer for text rendering
        integer(1), allocatable :: temp_buffer(:)
        integer :: i, j, buf_idx
        
        allocate(temp_buffer(width * height * 3))
        call initialize_white_background(temp_buffer, width, height)
        call render_text_to_image(temp_buffer, width, height, x, y, text, 0_1, 0_1, 0_1)
        
        ! Convert PNG buffer to bitmap
        do j = 1, height
            do i = 1, width
                buf_idx = ((j - 1) * width + (i - 1)) * 3 + 1
                bitmap(i, j, 1) = temp_buffer(buf_idx)     ! R
                bitmap(i, j, 2) = temp_buffer(buf_idx + 1) ! G  
                bitmap(i, j, 3) = temp_buffer(buf_idx + 2) ! B
            end do
        end do
        
        if (allocated(temp_buffer)) deallocate(temp_buffer)
    end subroutine render_text_to_bitmap

    subroutine rotate_bitmap_90_ccw(src_bitmap, dst_bitmap, src_width, src_height)
        !! Rotate bitmap 90 degrees counter-clockwise
        !! For arrays: (i,j) maps to (height-j+1, i) with swapped dimensions
        integer(1), intent(in) :: src_bitmap(:,:,:)
        integer(1), intent(out) :: dst_bitmap(:,:,:)
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
        integer(1), intent(in) :: src_bitmap(:,:,:)
        integer(1), intent(out) :: dst_bitmap(:,:,:)
        integer, intent(in) :: src_width, src_height
        integer :: i, j
        
        do j = 1, src_height
            do i = 1, src_width
                ! CW: each point (i,j) goes to position that produces CW rotation
                dst_bitmap(src_height - j + 1, i, :) = src_bitmap(i, j, :)
            end do
        end do
    end subroutine rotate_bitmap_90_cw

end module fortplot_bitmap
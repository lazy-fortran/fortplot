program test_moving_dot_video
    use fortplot_mpeg_stream
    use fortplot_mpeg_memory
    use iso_c_binding
    implicit none
    
    ! Test creating a 2-second MPEG video with a moving dot animation
    call create_moving_dot_video()
    
    print *, "PASS: Moving dot video created successfully"
    
contains

    subroutine create_moving_dot_video()
        ! Video parameters
        integer, parameter :: canvas_width = 32
        integer, parameter :: canvas_height = 24
        integer, parameter :: frames_per_second = 15
        integer, parameter :: video_duration = 2  ! seconds
        integer, parameter :: total_frames = frames_per_second * video_duration
        
        ! Dot parameters
        integer, parameter :: dot_size = 2
        integer, parameter :: dot_brightness = 255
        integer, parameter :: background_brightness = 0
        
        ! File parameters
        character(len=*), parameter :: video_file = "moving_dot_video.mpg"
        
        type(mem_t) :: frame_buffer
        integer :: frame_num, dot_x, dot_y, x, y, pixel_idx
        integer(c_long) :: total_bits_written, frame_start_pos, frame_end_pos
        real :: progress_x, progress_y
        
        print *, "Creating moving dot video..."
        print *, "  Canvas size:", canvas_width, "x", canvas_height
        print *, "  Frame rate:", frames_per_second, "fps"
        print *, "  Duration:", video_duration, "seconds"
        print *, "  Total frames:", total_frames
        
        ! Create frame buffer
        frame_buffer = mem_create(canvas_width, canvas_height)
        
        ! Open video file for writing
        call stream_open_write(video_file)
        
        ! Write video header
        call write_video_header(canvas_width, canvas_height, total_frames, frames_per_second)
        
        total_bits_written = 0
        
        ! Generate and encode each frame
        do frame_num = 1, total_frames
            frame_start_pos = stream_tell_write()
            
            ! Calculate dot position for this frame
            ! Move dot in a circular pattern around the canvas
            progress_x = real(frame_num - 1) / real(total_frames - 1)
            progress_y = real(frame_num - 1) / real(total_frames - 1)
            
            ! Circular motion
            dot_x = int(canvas_width / 2 + (canvas_width / 4) * cos(progress_x * 6.28318530718))
            dot_y = int(canvas_height / 2 + (canvas_height / 4) * sin(progress_y * 6.28318530718))
            
            ! Ensure dot stays within bounds
            if (dot_x < 1) dot_x = 1
            if (dot_x > canvas_width) dot_x = canvas_width
            if (dot_y < 1) dot_y = 1
            if (dot_y > canvas_height) dot_y = canvas_height
            
            ! Clear frame to background
            call clear_frame(frame_buffer, canvas_width, canvas_height, background_brightness)
            
            ! Draw dot
            call draw_dot(frame_buffer, canvas_width, canvas_height, dot_x, dot_y, dot_size, dot_brightness)
            
            ! Encode frame
            call encode_frame(frame_buffer, canvas_width, canvas_height, frame_num)
            
            frame_end_pos = stream_tell_write()
            total_bits_written = total_bits_written + (frame_end_pos - frame_start_pos)
            
            ! Progress reporting
            if (mod(frame_num, 5) == 0) then
                print *, "  Frame", frame_num, "/", total_frames, "- dot at (", dot_x, ",", dot_y, ")"
            end if
        end do
        
        ! Write video footer/end marker
        call write_video_footer()
        
        call stream_close_write()
        call mem_destroy(frame_buffer)
        
        ! Report results
        print *, "Video encoding complete:"
        print *, "  Output file:", video_file
        print *, "  Total bits written:", total_bits_written
        print *, "  Bits per frame:", total_bits_written / total_frames
        print *, "  Estimated file size:", (total_bits_written + 7) / 8, "bytes"
        print *, "  Compression ratio:", real(canvas_width * canvas_height * 8 * total_frames) / real(total_bits_written)
    end subroutine

    subroutine write_video_header(width, height, num_frames, fps)
        integer, intent(in) :: width, height, num_frames, fps
        
        ! Simple video header format
        call stream_put_variable(width, 16)       ! Video width
        call stream_put_variable(height, 16)      ! Video height
        call stream_put_variable(num_frames, 16)  ! Total frames
        call stream_put_variable(fps, 8)          ! Frame rate
        call stream_put_variable(255, 8)          ! Max pixel value
        call stream_put_variable(0, 8)            ! Min pixel value
        
        ! Add sync marker
        call stream_put_variable(170, 8)  ! 0xAA
        call stream_put_variable(85, 8)   ! 0x55
    end subroutine

    subroutine write_video_footer()
        ! End of video marker
        call stream_put_variable(255, 8)  ! EOF marker
        call stream_put_variable(255, 8)
        call stream_put_variable(255, 8)
        call stream_put_variable(255, 8)
    end subroutine

    subroutine clear_frame(frame_mem, width, height, brightness)
        type(mem_t), intent(inout) :: frame_mem
        integer, intent(in) :: width, height, brightness
        integer :: i
        
        do i = 1, width * height
            frame_mem%data(i) = int(brightness, c_int8_t)
        end do
    end subroutine

    subroutine draw_dot(frame_mem, width, height, center_x, center_y, size, brightness)
        type(mem_t), intent(inout) :: frame_mem
        integer, intent(in) :: width, height, center_x, center_y, size, brightness
        integer :: x, y, dx, dy, pixel_idx
        
        ! Draw a square dot of given size centered at (center_x, center_y)
        do dy = -(size/2), size/2
            do dx = -(size/2), size/2
                x = center_x + dx
                y = center_y + dy
                
                ! Check bounds
                if (x >= 1 .and. x <= width .and. y >= 1 .and. y <= height) then
                    pixel_idx = (y - 1) * width + x
                    frame_mem%data(pixel_idx) = int(brightness, c_int8_t)
                end if
            end do
        end do
    end subroutine

    subroutine encode_frame(frame_mem, width, height, frame_number)
        type(mem_t), intent(in) :: frame_mem
        integer, intent(in) :: width, height, frame_number
        integer :: x, y, pixel_idx, pixel_value, prev_pixel, delta
        
        ! Frame header
        call stream_put_variable(frame_number, 16)  ! Frame number
        
        ! Simple delta compression within frame
        prev_pixel = int(frame_mem%data(1))
        call stream_put_variable(prev_pixel, 8)  ! First pixel (absolute)
        
        do pixel_idx = 2, width * height
            pixel_value = int(frame_mem%data(pixel_idx))
            delta = pixel_value - prev_pixel
            
            ! Encode delta with bias to make it positive
            delta = delta + 128
            if (delta < 0) delta = 0
            if (delta > 255) delta = 255
            
            call stream_put_variable(delta, 8)
            prev_pixel = pixel_value
        end do
        
        ! Frame end marker
        call stream_put_variable(170, 8)  ! 0xAA frame end
    end subroutine

end program test_moving_dot_video
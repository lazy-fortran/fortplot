program test_mpeg_size_validation_comprehensive
    use fortplot
    use fortplot_security, only: safe_remove_file, safe_check_program_available
    use iso_fortran_env, only: real64
    implicit none

    ! Given: MPEG files should have adequate size for their content (Issue #32)
    ! When: We validate file sizes against content expectations
    ! Then: Tests should detect suspiciously small files that indicate encoding problems

    type(figure_t) :: test_fig
    real(real64), dimension(10) :: test_x, test_y
    integer :: i
    logical :: ffmpeg_available

    print *, "=== COMPREHENSIVE MPEG SIZE VALIDATION TESTS ==="
    
    ! Check if ffmpeg is available before running tests
    ffmpeg_available = safe_check_program_available('ffmpeg')
    if (.not. ffmpeg_available) then
        print *, "FFmpeg not available - skipping all size validation tests"
        stop 0
    end if
    
    call test_minimum_size_validation()
    call test_frame_count_size_correlation()
    call test_resolution_size_correlation()
    call test_duration_size_correlation()
    call test_fps_size_correlation()

    print *, "=== Size validation tests completed ==="

contains

    subroutine test_minimum_size_validation()
        ! Given: Any valid video file should meet absolute minimum size
        ! When: We create animations and check file sizes
        ! Then: Files below minimum threshold should be flagged as invalid

        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: meets_minimum
        integer :: file_size, minimum_threshold

        print *, ""
        print *, "TEST: Minimum Size Validation"
        print *, "============================"

        test_file = "size_minimum_test.mp4"
        minimum_threshold = 2000  ! 2KB absolute minimum
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = test_x

        call test_fig%initialize(width=320, height=240)
        call test_fig%add_plot(test_x, test_y)

        anim = FuncAnimation(update_minimum_data, frames=5, interval=100, fig=test_fig)
        call anim%save(test_file, fps=10)

        inquire(file=test_file, size=file_size)
        meets_minimum = (file_size >= minimum_threshold)

        print *, "File size:", file_size, "bytes"
        print *, "Minimum threshold:", minimum_threshold, "bytes"
        print *, "Meets minimum size:", meets_minimum

        if (.not. meets_minimum) then
            print *, "*** SIZE VALIDATION FAILURE ***"
            print *, "File below absolute minimum size for valid video"
        end if

        block
        logical :: remove_success
        call safe_remove_file(test_file, remove_success)
        if (.not. remove_success) then
            print *, "Warning: Could not remove temporary file: " // trim(test_file)
        end if
        end block
    end subroutine

    subroutine update_minimum_data(frame)
        integer, intent(in) :: frame
        test_y = test_x * real(frame, real64)
        call test_fig%set_ydata(1, test_y)
    end subroutine

    subroutine test_frame_count_size_correlation()
        ! Given: More frames should result in larger file sizes
        ! When: We create animations with different frame counts
        ! Then: File size should scale reasonably with frame count

        type(animation_t) :: anim
        character(len=200) :: test_file_5, test_file_15, test_file_30
        integer :: size_5, size_15, size_30
        logical :: correlation_valid

        print *, ""
        print *, "TEST: Frame Count Size Correlation"
        print *, "================================="

        test_x = [(real(i, real64), i=1,10)]
        test_y = sin(test_x)

        ! Test with 5 frames
        test_file_5 = "size_frames_5.mp4"
        call test_fig%initialize(width=400, height=300)
        call test_fig%add_plot(test_x, test_y)
        anim = FuncAnimation(update_frame_data, frames=5, interval=50, fig=test_fig)
        call anim%save(test_file_5, fps=15)
        inquire(file=test_file_5, size=size_5)

        ! Test with 15 frames
        test_file_15 = "size_frames_15.mp4"
        call test_fig%initialize(width=400, height=300)
        call test_fig%add_plot(test_x, test_y)
        anim = FuncAnimation(update_frame_data, frames=15, interval=50, fig=test_fig)
        call anim%save(test_file_15, fps=15)
        inquire(file=test_file_15, size=size_15)

        ! Test with 30 frames
        test_file_30 = "size_frames_30.mp4"
        call test_fig%initialize(width=400, height=300)
        call test_fig%add_plot(test_x, test_y)
        anim = FuncAnimation(update_frame_data, frames=30, interval=50, fig=test_fig)
        call anim%save(test_file_30, fps=15)
        inquire(file=test_file_30, size=size_30)

        ! Validate correlation - more frames should mean larger size
        correlation_valid = (size_15 > size_5 .and. size_30 > size_15)

        print *, "5 frames size:", size_5, "bytes"
        print *, "15 frames size:", size_15, "bytes"
        print *, "30 frames size:", size_30, "bytes"
        print *, "Frame count correlation valid:", correlation_valid

        if (.not. correlation_valid) then
            print *, "*** FRAME COUNT CORRELATION FAILURE ***"
            print *, "File sizes don't correlate properly with frame count"
        end if

        block
        logical :: remove_success
        call safe_remove_file(test_file_5, remove_success)
        call safe_remove_file(test_file_15, remove_success)
        call safe_remove_file(test_file_30, remove_success)
        end block
    end subroutine

    subroutine update_frame_data(frame)
        integer, intent(in) :: frame
        test_y = sin(test_x + real(frame, real64) * 0.1_real64)
        call test_fig%set_ydata(1, test_y)
    end subroutine

    subroutine test_resolution_size_correlation()
        ! Given: Higher resolution should result in larger file sizes
        ! When: We create animations with different resolutions
        ! Then: File size should scale with resolution

        type(animation_t) :: anim
        character(len=200) :: test_file_low, test_file_med, test_file_high
        integer :: size_low, size_med, size_high
        logical :: resolution_correlation_valid

        print *, ""
        print *, "TEST: Resolution Size Correlation"
        print *, "================================"

        test_x = [(real(i, real64), i=1,10)]
        test_y = test_x**2

        ! Low resolution
        test_file_low = "size_res_320x240.mp4"
        call test_fig%initialize(width=320, height=240)
        call test_fig%add_plot(test_x, test_y)
        anim = FuncAnimation(update_resolution_data, frames=10, interval=50, fig=test_fig)
        call anim%save(test_file_low, fps=20)
        inquire(file=test_file_low, size=size_low)

        ! Medium resolution
        test_file_med = "size_res_640x480.mp4"
        call test_fig%initialize(width=640, height=480)
        call test_fig%add_plot(test_x, test_y)
        anim = FuncAnimation(update_resolution_data, frames=10, interval=50, fig=test_fig)
        call anim%save(test_file_med, fps=20)
        inquire(file=test_file_med, size=size_med)

        ! High resolution
        test_file_high = "size_res_1024x768.mp4"
        call test_fig%initialize(width=1024, height=768)
        call test_fig%add_plot(test_x, test_y)
        anim = FuncAnimation(update_resolution_data, frames=10, interval=50, fig=test_fig)
        call anim%save(test_file_high, fps=20)
        inquire(file=test_file_high, size=size_high)

        resolution_correlation_valid = (size_med > size_low .and. size_high > size_med)

        print *, "320x240 size:", size_low, "bytes"
        print *, "640x480 size:", size_med, "bytes"
        print *, "1024x768 size:", size_high, "bytes"
        print *, "Resolution correlation valid:", resolution_correlation_valid

        if (.not. resolution_correlation_valid) then
            print *, "*** RESOLUTION CORRELATION FAILURE ***"
            print *, "File sizes don't correlate with resolution"
        end if

        block
        logical :: remove_success
        call safe_remove_file(test_file_low, remove_success)
        call safe_remove_file(test_file_med, remove_success)
        call safe_remove_file(test_file_high, remove_success)
        end block
    end subroutine

    subroutine update_resolution_data(frame)
        integer, intent(in) :: frame
        test_y = test_x**2 + real(frame, real64) * 2.0_real64
        call test_fig%set_ydata(1, test_y)
    end subroutine

    subroutine test_duration_size_correlation()
        ! Given: Longer duration should result in larger files
        ! When: We create animations with different durations
        ! Then: File size should correlate with total duration

        type(animation_t) :: anim
        character(len=200) :: test_file_short, test_file_medium, test_file_long
        integer :: size_short, size_medium, size_long
        logical :: duration_correlation_valid

        print *, ""
        print *, "TEST: Duration Size Correlation"
        print *, "=============================="

        test_x = [(real(i, real64), i=1,10)]
        test_y = cos(test_x)

        ! Short duration (5 frames at 25 fps = 0.2s)
        test_file_short = "size_duration_short.mp4"
        call test_fig%initialize(width=500, height=400)
        call test_fig%add_plot(test_x, test_y)
        anim = FuncAnimation(update_duration_data, frames=5, interval=40, fig=test_fig)
        call anim%save(test_file_short, fps=25)
        inquire(file=test_file_short, size=size_short)

        ! Medium duration (15 frames at 25 fps = 0.6s)
        test_file_medium = "size_duration_medium.mp4"
        call test_fig%initialize(width=500, height=400)
        call test_fig%add_plot(test_x, test_y)
        anim = FuncAnimation(update_duration_data, frames=15, interval=40, fig=test_fig)
        call anim%save(test_file_medium, fps=25)
        inquire(file=test_file_medium, size=size_medium)

        ! Long duration (25 frames at 25 fps = 1.0s)
        test_file_long = "size_duration_long.mp4"
        call test_fig%initialize(width=500, height=400)
        call test_fig%add_plot(test_x, test_y)
        anim = FuncAnimation(update_duration_data, frames=25, interval=40, fig=test_fig)
        call anim%save(test_file_long, fps=25)
        inquire(file=test_file_long, size=size_long)

        duration_correlation_valid = (size_medium > size_short .and. size_long > size_medium)

        print *, "Short (0.2s) size:", size_short, "bytes"
        print *, "Medium (0.6s) size:", size_medium, "bytes"
        print *, "Long (1.0s) size:", size_long, "bytes"
        print *, "Duration correlation valid:", duration_correlation_valid

        if (.not. duration_correlation_valid) then
            print *, "*** DURATION CORRELATION FAILURE ***"
            print *, "File sizes don't correlate with duration"
        end if

        block
        logical :: remove_success
        call safe_remove_file(test_file_short, remove_success)
        call safe_remove_file(test_file_medium, remove_success)
        call safe_remove_file(test_file_long, remove_success)
        end block
    end subroutine

    subroutine update_duration_data(frame)
        integer, intent(in) :: frame
        test_y = cos(test_x + real(frame, real64) * 0.15_real64)
        call test_fig%set_ydata(1, test_y)
    end subroutine

    subroutine test_fps_size_correlation()
        ! Given: Frame rate affects compression but should maintain reasonable sizes
        ! When: We create animations with different frame rates
        ! Then: All frame rates should produce adequate file sizes

        type(animation_t) :: anim
        character(len=200) :: test_file_10fps, test_file_24fps, test_file_60fps
        integer :: size_10fps, size_24fps, size_60fps
        logical :: all_adequate

        print *, ""
        print *, "TEST: FPS Size Validation"
        print *, "========================"

        test_x = [(real(i, real64), i=1,10)]
        test_y = sin(test_x) * cos(test_x)

        ! 10 FPS
        test_file_10fps = "size_fps_10.mp4"
        call test_fig%initialize(width=600, height=450)
        call test_fig%add_plot(test_x, test_y)
        anim = FuncAnimation(update_fps_data, frames=20, interval=100, fig=test_fig)
        call anim%save(test_file_10fps, fps=10)
        inquire(file=test_file_10fps, size=size_10fps)

        ! 24 FPS
        test_file_24fps = "size_fps_24.mp4"
        call test_fig%initialize(width=600, height=450)
        call test_fig%add_plot(test_x, test_y)
        anim = FuncAnimation(update_fps_data, frames=20, interval=42, fig=test_fig)
        call anim%save(test_file_24fps, fps=24)
        inquire(file=test_file_24fps, size=size_24fps)

        ! 60 FPS
        test_file_60fps = "size_fps_60.mp4"
        call test_fig%initialize(width=600, height=450)
        call test_fig%add_plot(test_x, test_y)
        anim = FuncAnimation(update_fps_data, frames=20, interval=17, fig=test_fig)
        call anim%save(test_file_60fps, fps=60)
        inquire(file=test_file_60fps, size=size_60fps)

        ! All should be above minimum threshold
        all_adequate = (size_10fps >= 3000 .and. size_24fps >= 3000 .and. size_60fps >= 3000)

        print *, "10 FPS size:", size_10fps, "bytes"
        print *, "24 FPS size:", size_24fps, "bytes"
        print *, "60 FPS size:", size_60fps, "bytes"
        print *, "All FPS rates adequate:", all_adequate

        if (.not. all_adequate) then
            print *, "*** FPS SIZE VALIDATION FAILURE ***"
            print *, "Some frame rates produce inadequate file sizes"
        end if

        block
        logical :: remove_success
        call safe_remove_file(test_file_10fps, remove_success)
        call safe_remove_file(test_file_24fps, remove_success)
        call safe_remove_file(test_file_60fps, remove_success)
        end block
    end subroutine

    subroutine update_fps_data(frame)
        integer, intent(in) :: frame
        test_y = sin(test_x + real(frame, real64) * 0.2_real64) * cos(test_x + real(frame, real64) * 0.1_real64)
        call test_fig%set_ydata(1, test_y)
    end subroutine

end program test_mpeg_size_validation_comprehensive
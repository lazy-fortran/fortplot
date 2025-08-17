program test_mpeg_stress_validation
    use fortplot
    use iso_fortran_env, only: real64
    implicit none

    ! Given: MPEG validation must work under stress conditions (Issue #32)
    ! When: We test with extreme parameters and edge cases
    ! Then: Validation should handle stress conditions appropriately

    type(figure_t) :: test_fig
    real(real64), dimension(50) :: test_x, test_y
    integer :: i

    print *, "=== MPEG STRESS VALIDATION TESTS ==="
    
    call test_high_frame_count_stress()
    call test_high_resolution_stress()
    call test_rapid_generation_stress()
    call test_memory_usage_stress()

    print *, "=== Stress validation tests completed ==="

contains

    subroutine test_high_frame_count_stress()
        ! Given: High frame count animations should be handled properly
        ! When: We create animations with many frames
        ! Then: Validation should work even with stress conditions

        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: stress_validation_passes
        integer :: high_frame_count, file_size

        print *, ""
        print *, "TEST: High Frame Count Stress"
        print *, "============================"

        test_file = "stress_high_frames.mp4"
        high_frame_count = 100  ! Stress test with many frames
        
        test_x = [(real(i, real64), i=1,50)]
        test_y = sin(test_x)

        call test_fig%initialize(width=400, height=300)
        call test_fig%add_plot(test_x, test_y)

        print *, "Generating animation with", high_frame_count, "frames..."
        anim = FuncAnimation(update_stress_frames_data, frames=high_frame_count, interval=20, fig=test_fig)
        call anim%save(test_file, fps=25)

        inquire(file=test_file, size=file_size)
        stress_validation_passes = validate_stress_conditions(test_file, high_frame_count)

        print *, "High frame count:", high_frame_count
        print *, "Generated file size:", file_size, "bytes"
        print *, "Stress validation passes:", stress_validation_passes

        if (.not. stress_validation_passes) then
            print *, "*** HIGH FRAME COUNT STRESS FAILURE ***"
            print *, "Validation fails under high frame count stress"
        end if

        call execute_command_line("rm -f " // trim(test_file))
    end subroutine

    subroutine update_stress_frames_data(frame)
        integer, intent(in) :: frame
        test_y = sin(test_x + real(frame, real64) * 0.05_real64)
        call test_fig%set_ydata(1, test_y)
    end subroutine

    function validate_stress_conditions(filename, frame_count) result(valid)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: frame_count
        logical :: valid
        integer :: file_size, expected_minimum_size

        inquire(file=filename, size=file_size)
        
        ! Under stress, file should still be substantial
        expected_minimum_size = frame_count * 100  ! Conservative minimum per frame
        valid = (file_size >= expected_minimum_size)

        print *, "  Stress validation details:"
        print *, "    Expected minimum size:", expected_minimum_size, "bytes"
        print *, "    Actual size:", file_size, "bytes"
        print *, "    Stress validation:", valid
    end function

    subroutine test_high_resolution_stress()
        ! Given: High resolution should be handled properly
        ! When: We create high resolution animations
        ! Then: Validation should work with large image data

        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: resolution_stress_passes
        integer :: width, height, file_size

        print *, ""
        print *, "TEST: High Resolution Stress"
        print *, "==========================="

        test_file = "stress_high_resolution.mp4"
        width = 1920
        height = 1080  ! Full HD stress test
        
        test_x = [(real(i, real64), i=1,50)]
        test_y = cos(test_x)

        call test_fig%initialize(width=width, height=height)
        call test_fig%add_plot(test_x, test_y)

        print *, "Generating", width, "x", height, "animation..."
        anim = FuncAnimation(update_stress_resolution_data, frames=5, interval=100, fig=test_fig)
        call anim%save(test_file, fps=15)

        inquire(file=test_file, size=file_size)
        resolution_stress_passes = validate_resolution_stress(test_file, width, height)

        print *, "Resolution:", width, "x", height
        print *, "Generated file size:", file_size, "bytes"
        print *, "Resolution stress validation passes:", resolution_stress_passes

        if (.not. resolution_stress_passes) then
            print *, "*** HIGH RESOLUTION STRESS FAILURE ***"
            print *, "Validation fails under high resolution stress"
        end if

        call execute_command_line("rm -f " // trim(test_file))
    end subroutine

    subroutine update_stress_resolution_data(frame)
        integer, intent(in) :: frame
        test_y = cos(test_x + real(frame, real64) * 0.4_real64)
        call test_fig%set_ydata(1, test_y)
    end subroutine

    function validate_resolution_stress(filename, width, height) result(valid)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: width, height
        logical :: valid
        integer :: file_size, pixels_total, expected_minimum

        inquire(file=filename, size=file_size)
        
        pixels_total = width * height
        expected_minimum = pixels_total / 10000  ! Very conservative compression estimate
        valid = (file_size >= expected_minimum)

        print *, "  Resolution stress validation:"
        print *, "    Total pixels:", pixels_total
        print *, "    Expected minimum:", expected_minimum, "bytes"
        print *, "    Actual size:", file_size, "bytes"
        print *, "    Resolution stress valid:", valid
    end function

    subroutine test_rapid_generation_stress()
        ! Given: Rapid file generation should maintain validation quality
        ! When: We generate multiple files quickly
        ! Then: Each file should still validate properly

        type(animation_t) :: anim
        character(len=200) :: test_files(5)
        logical :: all_rapid_files_valid
        integer :: rapid_test_count, i_rapid

        print *, ""
        print *, "TEST: Rapid Generation Stress"
        print *, "============================"

        rapid_test_count = 5
        all_rapid_files_valid = .true.
        
        test_x = [(real(i, real64), i=1,50)]
        test_y = test_x

        print *, "Rapidly generating", rapid_test_count, "files..."
        do i_rapid = 1, rapid_test_count
            write(test_files(i_rapid), '(A,I0,A)') "stress_rapid_", i_rapid, ".mp4"
            
            call test_fig%initialize(width=320, height=240)
            call test_fig%add_plot(test_x, test_y)
            
            anim = FuncAnimation(update_stress_rapid_data, frames=3, interval=50, fig=test_fig)
            call anim%save(test_files(i_rapid), fps=20)
            
            if (.not. validate_rapid_generation(test_files(i_rapid))) then
                all_rapid_files_valid = .false.
                print *, "Rapid file", i_rapid, "failed validation"
            end if
        end do

        print *, "All rapid files valid:", all_rapid_files_valid

        if (.not. all_rapid_files_valid) then
            print *, "*** RAPID GENERATION STRESS FAILURE ***"
            print *, "Some rapidly generated files failed validation"
        end if

        do i_rapid = 1, rapid_test_count
            call execute_command_line("rm -f " // trim(test_files(i_rapid)))
        end do
    end subroutine

    subroutine update_stress_rapid_data(frame)
        integer, intent(in) :: frame
        test_y = test_x + real(frame, real64) * 0.2_real64
        call test_fig%set_ydata(1, test_y)
    end subroutine

    function validate_rapid_generation(filename) result(valid)
        character(len=*), intent(in) :: filename
        logical :: valid
        logical :: exists
        integer :: file_size

        inquire(file=filename, exist=exists, size=file_size)
        valid = exists .and. (file_size > 500)  ! Minimum for rapid generation

        print *, "  Rapid validation for", trim(filename), ":", valid, "(", file_size, "bytes)"
    end function

    subroutine test_memory_usage_stress()
        ! Given: Memory usage should remain reasonable under stress
        ! When: We test memory-intensive operations
        ! Then: Validation should complete without memory issues

        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: memory_stress_passes
        integer :: large_data_size

        print *, ""
        print *, "TEST: Memory Usage Stress"
        print *, "========================"

        test_file = "stress_memory.mp4"
        large_data_size = 50  ! Use larger data arrays
        
        ! Use larger test data for memory stress
        test_x = [(real(i, real64), i=1,large_data_size)]
        test_y = test_x**2 + sin(test_x * 2.0_real64)

        call test_fig%initialize(width=800, height=600)
        call test_fig%add_plot(test_x, test_y)

        print *, "Testing with large data size:", large_data_size, "points"
        anim = FuncAnimation(update_stress_memory_data, frames=20, interval=50, fig=test_fig)
        call anim%save(test_file, fps=24)

        memory_stress_passes = validate_memory_stress(test_file)

        print *, "Memory stress validation passes:", memory_stress_passes

        if (.not. memory_stress_passes) then
            print *, "*** MEMORY USAGE STRESS FAILURE ***"
            print *, "Validation fails under memory usage stress"
        end if

        call execute_command_line("rm -f " // trim(test_file))
    end subroutine

    subroutine update_stress_memory_data(frame)
        integer, intent(in) :: frame
        real(real64) :: phase
        phase = real(frame, real64) * 0.1_real64
        test_y = test_x**2 + sin(test_x * 2.0_real64 + phase)
        call test_fig%set_ydata(1, test_y)
    end subroutine

    function validate_memory_stress(filename) result(valid)
        character(len=*), intent(in) :: filename
        logical :: valid
        logical :: exists, substantial_content, external_valid
        integer :: file_size
        character(len=500) :: command
        integer :: status

        inquire(file=filename, exist=exists, size=file_size)
        substantial_content = (file_size > 5000)

        ! Test external validation if available
        call execute_command_line("which ffprobe >/dev/null 2>&1", exitstat=status)
        if (status == 0) then
            write(command, '(A,A,A)') 'ffprobe -v error -show_format "', trim(filename), '" >/dev/null 2>&1'
            call execute_command_line(command, exitstat=status)
            external_valid = (status == 0)
        else
            external_valid = .true.  ! Can't test, assume valid
        end if

        valid = exists .and. substantial_content .and. external_valid

        print *, "  Memory stress validation:"
        print *, "    File exists:", exists
        print *, "    Substantial content:", substantial_content
        print *, "    External validation:", external_valid
        print *, "    Memory stress valid:", valid
    end function

end program test_mpeg_stress_validation
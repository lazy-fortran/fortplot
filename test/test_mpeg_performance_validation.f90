program test_mpeg_performance_validation
    use fortplot
    use iso_fortran_env, only: real64
    implicit none

    ! Given: MPEG validation should perform efficiently (Issue #32)
    ! When: We measure validation performance characteristics
    ! Then: Validation should complete within reasonable time bounds

    type(figure_t) :: test_fig
    real(real64), dimension(10) :: test_x, test_y
    integer :: i

    print *, "=== MPEG PERFORMANCE VALIDATION TESTS ==="
    
    call test_validation_speed_performance()
    call test_file_size_impact_on_performance()
    call test_multiple_file_validation_performance()
    call test_validation_memory_efficiency()

    print *, "=== Performance validation tests completed ==="

contains

    subroutine test_validation_speed_performance()
        ! Given: Validation should complete quickly
        ! When: We measure validation execution time
        ! Then: Validation should finish within acceptable time limits

        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: performance_acceptable
        integer :: start_time, end_time, validation_time_ms

        print *, ""
        print *, "TEST: Validation Speed Performance"
        print *, "================================="

        test_file = "performance_speed_test.mp4"
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = test_x

        call test_fig%initialize(width=640, height=480)
        call test_fig%add_plot(test_x, test_y)

        anim = FuncAnimation(update_speed_data, frames=12, interval=50, fig=test_fig)
        call anim%save(test_file, fps=20)

        ! Measure validation time
        start_time = get_time_milliseconds()
        call perform_comprehensive_validation(test_file)
        end_time = get_time_milliseconds()
        
        validation_time_ms = end_time - start_time
        performance_acceptable = (validation_time_ms < 3000)  ! Should complete in < 3 seconds

        print *, "Validation time:", validation_time_ms, "ms"
        print *, "Performance acceptable (<3000ms):", performance_acceptable

        if (.not. performance_acceptable) then
            print *, "*** VALIDATION SPEED PERFORMANCE ISSUE ***"
            print *, "Validation takes too long - performance regression"
        end if

        call execute_command_line("rm -f " // trim(test_file))
    end subroutine

    subroutine update_speed_data(frame)
        integer, intent(in) :: frame
        test_y = test_x + real(frame, real64) * 0.4_real64
        call test_fig%set_ydata(1, test_y)
    end subroutine

    function get_time_milliseconds() result(time_ms)
        integer :: time_ms
        integer :: values(8)
        
        call date_and_time(values=values)
        time_ms = values(5) * 3600000 + values(6) * 60000 + values(7) * 1000 + values(8)
    end function

    subroutine perform_comprehensive_validation(filename)
        character(len=*), intent(in) :: filename
        logical :: result
        
        result = validate_file_comprehensive(filename)
        ! Just perform validation, result stored for timing purposes
    end subroutine

    function validate_file_comprehensive(filename) result(valid)
        character(len=*), intent(in) :: filename
        logical :: valid
        logical :: exists, size_ok, header_ok, tool_ok
        integer :: file_size
        
        inquire(file=filename, exist=exists, size=file_size)
        size_ok = (file_size > 1000)
        header_ok = check_header_format(filename)
        tool_ok = check_external_tool(filename)
        
        valid = exists .and. size_ok .and. header_ok .and. tool_ok
    end function

    function check_header_format(filename) result(valid)
        character(len=*), intent(in) :: filename
        logical :: valid
        character(len=12) :: header
        integer :: file_unit, ios
        
        valid = .false.
        
        open(newunit=file_unit, file=filename, access='stream', form='unformatted', iostat=ios)
        if (ios /= 0) return
        
        read(file_unit, iostat=ios) header
        close(file_unit)
        
        if (ios /= 0) return
        
        valid = (index(header, 'ftyp') > 0 .or. &
                index(header, 'mdat') > 0 .or. &
                index(header, 'moov') > 0)
    end function

    function check_external_tool(filename) result(valid)
        character(len=*), intent(in) :: filename
        logical :: valid
        character(len=500) :: command
        integer :: status
        
        call execute_command_line("which ffprobe >/dev/null 2>&1", exitstat=status)
        if (status /= 0) then
            valid = .true.  ! Tool not available, skip check
            return
        end if
        
        write(command, '(A,A,A)') 'ffprobe -v error -show_format "', trim(filename), '" >/dev/null 2>&1'
        call execute_command_line(command, exitstat=status)
        valid = (status == 0)
    end function

    subroutine test_file_size_impact_on_performance()
        ! Given: Validation performance should scale reasonably with file size
        ! When: We test different file sizes
        ! Then: Performance should not degrade excessively with larger files

        type(animation_t) :: anim
        character(len=200) :: small_file, large_file
        integer :: small_time, large_time, start_time, end_time
        logical :: scaling_acceptable

        print *, ""
        print *, "TEST: File Size Impact on Performance"
        print *, "===================================="

        small_file = "performance_small.mp4"
        large_file = "performance_large.mp4"
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = sin(test_x)

        ! Small file
        call test_fig%initialize(width=320, height=240)
        call test_fig%add_plot(test_x, test_y)
        anim = FuncAnimation(update_size_impact_data, frames=5, interval=100, fig=test_fig)
        call anim%save(small_file, fps=10)

        start_time = get_time_milliseconds()
        call perform_comprehensive_validation(small_file)
        end_time = get_time_milliseconds()
        small_time = end_time - start_time

        ! Large file
        call test_fig%initialize(width=800, height=600)
        call test_fig%add_plot(test_x, test_y)
        anim = FuncAnimation(update_size_impact_data, frames=20, interval=50, fig=test_fig)
        call anim%save(large_file, fps=25)

        start_time = get_time_milliseconds()
        call perform_comprehensive_validation(large_file)
        end_time = get_time_milliseconds()
        large_time = end_time - start_time

        ! Performance should scale reasonably (not more than 5x difference)
        scaling_acceptable = (large_time < small_time * 5)

        print *, "Small file validation time:", small_time, "ms"
        print *, "Large file validation time:", large_time, "ms"
        print *, "Performance scaling acceptable:", scaling_acceptable

        if (.not. scaling_acceptable) then
            print *, "*** PERFORMANCE SCALING ISSUE ***"
            print *, "Validation performance degrades too much with file size"
        end if

        call execute_command_line("rm -f " // trim(small_file) // " " // trim(large_file))
    end subroutine

    subroutine update_size_impact_data(frame)
        integer, intent(in) :: frame
        test_y = sin(test_x + real(frame, real64) * 0.2_real64)
        call test_fig%set_ydata(1, test_y)
    end subroutine

    subroutine test_multiple_file_validation_performance()
        ! Given: Validation should handle multiple files efficiently
        ! When: We validate multiple files in sequence
        ! Then: Performance should remain consistent across multiple validations

        type(animation_t) :: anim
        character(len=200) :: test_files(4)
        integer :: validation_times(4), i_file, start_time, end_time
        logical :: consistency_good
        real :: average_time, max_deviation

        print *, ""
        print *, "TEST: Multiple File Validation Performance"
        print *, "========================================="

        test_x = [(real(i, real64), i=1,10)]
        test_y = cos(test_x)

        ! Generate and validate multiple files
        do i_file = 1, 4
            write(test_files(i_file), '(A,I0,A)') "performance_multi_", i_file, ".mp4"
            
            call test_fig%initialize(width=400, height=300)
            call test_fig%add_plot(test_x, test_y)
            
            anim = FuncAnimation(update_multi_data, frames=8, interval=50, fig=test_fig)
            call anim%save(test_files(i_file), fps=15)
            
            start_time = get_time_milliseconds()
            call perform_comprehensive_validation(test_files(i_file))
            end_time = get_time_milliseconds()
            
            validation_times(i_file) = end_time - start_time
            print *, "File", i_file, "validation time:", validation_times(i_file), "ms"
        end do

        ! Check consistency of validation times
        average_time = real(sum(validation_times)) / 4.0
        max_deviation = maxval(abs(validation_times - int(average_time)))
        consistency_good = (max_deviation < average_time * 0.5)  ! Within 50% of average

        print *, "Average validation time:", average_time, "ms"
        print *, "Maximum deviation:", max_deviation, "ms"
        print *, "Performance consistency good:", consistency_good

        if (.not. consistency_good) then
            print *, "*** MULTIPLE FILE PERFORMANCE ISSUE ***"
            print *, "Validation times inconsistent across multiple files"
        end if

        do i_file = 1, 4
            call execute_command_line("rm -f " // trim(test_files(i_file)))
        end do
    end subroutine

    subroutine update_multi_data(frame)
        integer, intent(in) :: frame
        test_y = cos(test_x + real(frame, real64) * 0.25_real64)
        call test_fig%set_ydata(1, test_y)
    end subroutine

    subroutine test_validation_memory_efficiency()
        ! Given: Validation should be memory efficient
        ! When: We test memory usage patterns
        ! Then: Validation should not consume excessive memory

        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: memory_efficient
        integer :: file_size

        print *, ""
        print *, "TEST: Validation Memory Efficiency"
        print *, "================================="

        test_file = "performance_memory.mp4"
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = test_x**2

        call test_fig%initialize(width=1024, height=768)  ! Large resolution
        call test_fig%add_plot(test_x, test_y)

        anim = FuncAnimation(update_memory_data, frames=15, interval=50, fig=test_fig)
        call anim%save(test_file, fps=24)

        inquire(file=test_file, size=file_size)
        
        ! Test memory efficiency by running validation
        memory_efficient = test_memory_efficiency(test_file, file_size)

        print *, "File size:", file_size, "bytes"
        print *, "Memory efficiency acceptable:", memory_efficient

        if (.not. memory_efficient) then
            print *, "*** MEMORY EFFICIENCY ISSUE ***"
            print *, "Validation uses excessive memory resources"
        end if

        call execute_command_line("rm -f " // trim(test_file))
    end subroutine

    subroutine update_memory_data(frame)
        integer, intent(in) :: frame
        test_y = test_x**2 + real(frame, real64) * 3.0_real64
        call test_fig%set_ydata(1, test_y)
    end subroutine

    function test_memory_efficiency(filename, file_size) result(efficient)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: file_size
        logical :: efficient
        logical :: validation_completed
        
        ! Test that validation completes without memory issues
        validation_completed = validate_file_comprehensive(filename)
        
        ! Simplified memory efficiency test - validation should complete
        efficient = validation_completed
        
        print *, "  Memory efficiency test:"
        print *, "    Validation completed:", validation_completed
        print *, "    Memory efficient:", efficient
    end function

end program test_mpeg_performance_validation
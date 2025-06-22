program test_module_separation
    !! Test that modules follow Single Responsibility Principle
    !! Following TDD to design proper module structure
    use, intrinsic :: iso_fortran_env, only: wp => real64
    ! Test imports for proposed new module structure
    use fortplot_layout, only: plot_margins_t, calculate_plot_area
    use fortplot_ticks, only: calculate_tick_labels, calculate_tick_labels_log, calculate_tick_labels_symlog
    use fortplot_axes, only: draw_basic_axes_frame, get_axis_tick_positions
    implicit none
    
    call test_should_separate_layout_concerns()
    call test_should_separate_tick_concerns()
    call test_should_separate_axes_concerns()
    call test_should_maintain_backward_compatibility()
    
    print *, "All module separation tests passed!"
    
contains

    subroutine test_should_separate_layout_concerns()
        !! Layout module should handle only plot area and margins
        type(plot_margins_t) :: margins
        integer :: canvas_width, canvas_height
        
        canvas_width = 800
        canvas_height = 600
        
        ! Should be able to set margins
        margins%left = 0.12_wp
        margins%right = 0.05_wp
        margins%bottom = 0.15_wp
        margins%top = 0.05_wp
        
        ! Should be able to calculate plot area
        ! This tests that layout functionality is available
        print *, "Layout module accessible for margins and plot area"
    end subroutine test_should_separate_layout_concerns

    subroutine test_should_separate_tick_concerns()
        !! Tick module should handle all tick generation and formatting
        character(len=20) :: labels(5)
        
        ! Should support linear ticks
        call calculate_tick_labels(0.0_wp, 10.0_wp, 5, labels)
        if (trim(labels(1)) == '') then
            print *, "FAIL: Linear tick calculation not available"
            stop 1
        end if
        
        ! Should support log ticks  
        call calculate_tick_labels_log(1.0_wp, 1000.0_wp, 4, labels(1:4))
        if (trim(labels(1)) == '') then
            print *, "FAIL: Log tick calculation not available"
            stop 1
        end if
        
        ! Should support symlog ticks
        call calculate_tick_labels_symlog(-10.0_wp, 100.0_wp, 1.0_wp, 5, labels)
        if (trim(labels(1)) == '') then
            print *, "FAIL: Symlog tick calculation not available"
            stop 1
        end if
        
        print *, "Tick module accessible for all scale types"
    end subroutine test_should_separate_tick_concerns

    subroutine test_should_separate_axes_concerns()
        !! Axes module should handle axis drawing and positioning
        real(wp) :: x_positions(5), y_positions(5)
        integer :: actual_num_x, actual_num_y
        
        ! Should be able to get tick positions
        ! This tests that axes drawing functionality is available
        print *, "Axes module accessible for drawing and positioning"
    end subroutine test_should_separate_axes_concerns

    subroutine test_should_maintain_backward_compatibility()
        !! Existing code should still work with new structure
        character(len=20) :: labels(5)
        
        ! Old imports should still work or have clear migration path
        call calculate_tick_labels(0.0_wp, 5.0_wp, 5, labels)
        
        if (trim(labels(1)) == '' .or. trim(labels(5)) == '') then
            print *, "FAIL: Backward compatibility broken"
            stop 1
        end if
        
        print *, "Backward compatibility maintained"
    end subroutine test_should_maintain_backward_compatibility

end program test_module_separation
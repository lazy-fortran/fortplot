program test_symlog_bounds
    !! Test that symlog scale example plot stays within frame boundaries
    !! Based on the actual scale_examples symlog implementation
    use fortplot
    use fortplot_figure, only: apply_scale_transform
    implicit none
    
    real(wp), dimension(50) :: x_exp, y_symlog
    integer :: i
    type(figure_t) :: test_fig
    real(wp) :: frame_left, frame_right, frame_top, frame_bottom
    real(wp) :: x_trans, y_trans, x_screen, y_screen
    integer :: violations
    logical :: test_passed
    
    print *, "=== Symlog Scale Example Boundary Test ==="
    
    ! Generate the exact same data as the symlog scale example
    x_exp = [(real(i, wp), i=1, 50)]
    y_symlog = x_exp**3 - 50.0_wp * x_exp
    
    ! Create the exact same plot as the scale example using OO API
    call test_fig%initialize(640, 480)
    call test_fig%add_plot(x_exp, y_symlog)
    call test_fig%set_yscale('symlog', 10.0_wp)
    call test_fig%savefig('test_symlog_bounds_check.png')
    
    if (.not. allocated(test_fig%backend)) then
        print *, "❌ FAIL: Backend not allocated"
        stop 1
    end if
    
    ! Calculate expected frame boundaries (same margins as figure)
    frame_left = fig%margin_left * 640.0_wp
    frame_right = (1.0_wp - fig%margin_right) * 640.0_wp
    frame_bottom = fig%margin_bottom * 480.0_wp
    frame_top = (1.0_wp - fig%margin_top) * 480.0_wp
    
    print *, "Expected plot area bounds:"
    print *, "  X range: [", frame_left, ",", frame_right, "]"
    print *, "  Y range: [", frame_bottom, ",", frame_top, "]"
    
    print *, "Backend coordinate system:"
    print *, "  X range: [", fig%backend%x_min, ",", fig%backend%x_max, "]"
    print *, "  Y range: [", fig%backend%y_min, ",", fig%backend%y_max, "]"
    
    ! Check each data point to see if it renders outside plot boundaries
    violations = 0
    do i = 1, size(x_exp)
        ! Transform data coordinates using same method as plotting
        call transform_data_coordinates(fig, x_exp(i), y_symlog(i), x_trans, y_trans)
        
        ! Convert to screen coordinates using backend coordinate system
        call backend_to_screen_coords(fig, x_trans, y_trans, x_screen, y_screen)
        
        ! Check if screen coordinates are outside expected plot area
        if (x_screen < frame_left - 1.0_wp .or. x_screen > frame_right + 1.0_wp .or. &
            y_screen < frame_bottom - 1.0_wp .or. y_screen > frame_top + 1.0_wp) then
            violations = violations + 1
            if (violations == 1) then
                print *, "First boundary violation at point", i, ":"
                print *, "  Data: (", x_exp(i), ",", y_symlog(i), ")"
                print *, "  Transformed: (", x_trans, ",", y_trans, ")"
                print *, "  Screen: (", x_screen, ",", y_screen, ")"
            end if
        end if
    end do
    
    ! Report results
    print *, ""
    if (violations == 0) then
        print *, "✓ PASS: All symlog plot points stay within frame boundaries"
        print *, "  Total points checked:", size(x_exp)
        test_passed = .true.
    else
        print *, "❌ FAIL: Symlog plot extends beyond frame boundaries"
        print *, "  Boundary violations:", violations, "out of", size(x_exp), "points"
        test_passed = .false.
    end if
    
    ! Clean up
    call execute_command_line('rm -f test_symlog_bounds_check.png')
    
    if (.not. test_passed) then
        stop 1
    end if

contains

    subroutine transform_data_coordinates(fig, x_in, y_in, x_out, y_out)
        !! Transform data coordinates according to axis scales
        type(figure_t), intent(in) :: fig
        real(wp), intent(in) :: x_in, y_in
        real(wp), intent(out) :: x_out, y_out
        
        x_out = apply_scale_transform(x_in, fig%xscale, fig%symlog_threshold)
        y_out = apply_scale_transform(y_in, fig%yscale, fig%symlog_threshold)
    end subroutine transform_data_coordinates

    subroutine backend_to_screen_coords(fig, x_backend, y_backend, x_screen, y_screen)
        !! Convert backend coordinates to screen pixel coordinates
        type(figure_t), intent(in) :: fig
        real(wp), intent(in) :: x_backend, y_backend
        real(wp), intent(out) :: x_screen, y_screen
        
        real(wp) :: x_norm, y_norm
        
        ! Normalize coordinates (0-1) based on backend range
        x_norm = (x_backend - fig%backend%x_min) / (fig%backend%x_max - fig%backend%x_min)
        y_norm = (y_backend - fig%backend%y_min) / (fig%backend%y_max - fig%backend%y_min)
        
        ! Convert to screen pixel coordinates
        x_screen = x_norm * 640.0_wp
        y_screen = y_norm * 480.0_wp
    end subroutine backend_to_screen_coords

    function get_current_figure() result(fig)
        !! Get the current global figure instance
        type(figure_t) :: fig
        
        ! Access the global figure from fortplot module
        ! This is a simplified approach - in practice might need module access
        call fig%initialize(640, 480)
        call fig%add_plot(x_exp, y_symlog)
        call fig%set_yscale('symlog', 10.0_wp)
    end function get_current_figure

end program test_symlog_bounds
program test_scaling
    !! Unit tests for coordinate scaling and boundary checking
    !! Tests linear, log, and symlog scaling to ensure plot points stay within canvas boundary
    use fortplot
    use fortplot_figure, only: apply_scale_transform, transform_x_coordinate, transform_y_coordinate
    implicit none
    
    integer :: test_count = 0, passed_count = 0
    logical :: all_tests_passed = .true.
    
    call test_linear_scaling_boundaries()
    call test_log_scaling_boundaries()
    call test_symlog_scaling_boundaries()
    call test_coordinate_transformation_boundaries()
    
    call print_test_summary()
    
contains

    subroutine test_linear_scaling_boundaries()
        !! Test that linear scaling keeps all points within canvas bounds
        type(figure_t) :: fig
        real(wp), parameter :: canvas_width = 640.0_wp, canvas_height = 480.0_wp
        real(wp), dimension(100) :: x_data, y_data
        real(wp), dimension(100) :: x_scaled, y_scaled
        integer :: i
        logical :: all_within_bounds
        
        call test_start("Linear scaling boundary check")
        
        ! Generate test data with various ranges
        x_data = [(real(i, wp), i=1, 100)] * 0.1_wp  ! 0.1 to 10.0
        y_data = sin(x_data) * 1000.0_wp + 500.0_wp  ! Large range: -500 to 1500
        
        call fig%initialize(int(canvas_width), int(canvas_height))
        call fig%set_xlim(minval(x_data), maxval(x_data))
        call fig%set_ylim(minval(y_data), maxval(y_data))
        
        ! Transform coordinates to scaled space
        call transform_test_data(fig, x_data, y_data, x_scaled, y_scaled)
        
        ! Check all points are within canvas boundaries
        all_within_bounds = check_points_within_canvas(x_scaled, y_scaled, canvas_width, canvas_height)
        
        call test_result(all_within_bounds, "All linear scaled points within canvas bounds")
    end subroutine

    subroutine test_log_scaling_boundaries()
        !! Test that log scaling keeps all points within canvas bounds
        type(figure_t) :: fig
        real(wp), parameter :: canvas_width = 640.0_wp, canvas_height = 480.0_wp
        real(wp), dimension(50) :: x_data, y_data
        real(wp), dimension(50) :: x_scaled, y_scaled
        integer :: i
        logical :: all_within_bounds
        
        call test_start("Log scaling boundary check")
        
        ! Generate test data suitable for log scale (positive values)
        x_data = [(10.0_wp**(real(i, wp)/10.0_wp), i=1, 50)]  ! 10^0.1 to 10^5
        y_data = [(real(i, wp)**2, i=1, 50)]  ! 1 to 2500
        
        call fig%initialize(int(canvas_width), int(canvas_height))
        call fig%set_xlim(minval(x_data), maxval(x_data))
        call fig%set_ylim(minval(y_data), maxval(y_data))
        call fig%set_xscale('log')
        call fig%set_yscale('log')
        
        ! Transform coordinates to scaled space
        call transform_test_data(fig, x_data, y_data, x_scaled, y_scaled)
        
        ! Check all points are within canvas boundaries
        all_within_bounds = check_points_within_canvas(x_scaled, y_scaled, canvas_width, canvas_height)
        
        call test_result(all_within_bounds, "All log scaled points within canvas bounds")
    end subroutine

    subroutine test_symlog_scaling_boundaries()
        !! Test that symlog scaling keeps all points within canvas bounds
        type(figure_t) :: fig
        real(wp), parameter :: canvas_width = 640.0_wp, canvas_height = 480.0_wp
        real(wp), dimension(100) :: x_data, y_data
        real(wp), dimension(100) :: x_scaled, y_scaled
        integer :: i
        logical :: all_within_bounds
        
        call test_start("Symlog scaling boundary check")
        
        ! Generate test data with both positive and negative values (symlog specialty)
        x_data = [(real(i-50, wp) * 0.2_wp, i=1, 100)]  ! -9.8 to 9.8
        y_data = x_data**3  ! Cubic function: negative to positive with wide range
        
        call fig%initialize(int(canvas_width), int(canvas_height))
        call fig%set_xlim(minval(x_data), maxval(x_data))
        call fig%set_ylim(minval(y_data), maxval(y_data))
        call fig%set_xscale('symlog')
        call fig%set_yscale('symlog')
        
        ! Transform coordinates to scaled space
        call transform_test_data(fig, x_data, y_data, x_scaled, y_scaled)
        
        ! Check all points are within canvas boundaries
        all_within_bounds = check_points_within_canvas(x_scaled, y_scaled, canvas_width, canvas_height)
        
        call test_result(all_within_bounds, "All symlog scaled points within canvas bounds")
    end subroutine

    subroutine test_coordinate_transformation_boundaries()
        !! Test coordinate transformation with edge cases
        type(figure_t) :: fig
        real(wp), parameter :: canvas_width = 400.0_wp, canvas_height = 300.0_wp
        real(wp), dimension(4) :: x_data, y_data
        real(wp), dimension(4) :: x_scaled, y_scaled
        logical :: all_within_bounds
        
        call test_start("Coordinate transformation edge cases")
        
        ! Test corner points and center
        x_data = [0.0_wp, 1.0_wp, 0.0_wp, 1.0_wp]
        y_data = [0.0_wp, 0.0_wp, 1.0_wp, 1.0_wp]
        
        call fig%initialize(int(canvas_width), int(canvas_height))
        call fig%set_xlim(0.0_wp, 1.0_wp)
        call fig%set_ylim(0.0_wp, 1.0_wp)
        
        ! Transform coordinates
        call transform_test_data(fig, x_data, y_data, x_scaled, y_scaled)
        
        ! Check boundaries
        all_within_bounds = check_points_within_canvas(x_scaled, y_scaled, canvas_width, canvas_height)
        
        call test_result(all_within_bounds, "Edge case coordinates within bounds")
    end subroutine

    subroutine transform_test_data(fig, x_data, y_data, x_scaled, y_scaled)
        !! Transform data coordinates to scaled plot coordinates
        !! This is the core functionality we're testing - extracted from drawing backends
        type(figure_t), intent(in) :: fig
        real(wp), dimension(:), intent(in) :: x_data, y_data
        real(wp), dimension(:), intent(out) :: x_scaled, y_scaled
        integer :: i
        real(wp) :: x_transformed, y_transformed
        
        do i = 1, size(x_data)
            ! Apply scale transformations (linear/log/symlog)
            x_transformed = apply_scale_transform(x_data(i), fig%state%xscale, fig%state%symlog_threshold)
            y_transformed = apply_scale_transform(y_data(i), fig%state%yscale, fig%state%symlog_threshold)
            
            ! Map to plot area coordinates (this should be the extracted scaling logic)
            x_scaled(i) = map_data_to_canvas_x(x_transformed, fig)
            y_scaled(i) = map_data_to_canvas_y(y_transformed, fig)
        end do
    end subroutine

    function map_data_to_canvas_x(x_transformed, fig) result(x_canvas)
        !! Map transformed x-coordinate to canvas space using extracted coordinate transformation
        real(wp), intent(in) :: x_transformed
        type(figure_t), intent(in) :: fig
        real(wp) :: x_canvas
        real(wp) :: x_min_transformed, x_max_transformed
        
        ! Apply same transformation to axis limits
        x_min_transformed = apply_scale_transform(fig%state%x_min, fig%state%xscale, fig%state%symlog_threshold)
        x_max_transformed = apply_scale_transform(fig%state%x_max, fig%state%xscale, fig%state%symlog_threshold)
        
        ! Use extracted coordinate transformation function
        x_canvas = transform_x_coordinate(x_transformed, x_min_transformed, x_max_transformed, fig%state%width)
    end function

    function map_data_to_canvas_y(y_transformed, fig) result(y_canvas)
        !! Map transformed y-coordinate to canvas space using extracted coordinate transformation
        real(wp), intent(in) :: y_transformed
        type(figure_t), intent(in) :: fig
        real(wp) :: y_canvas
        real(wp) :: y_min_transformed, y_max_transformed
        
        ! Apply same transformation to axis limits
        y_min_transformed = apply_scale_transform(fig%state%y_min, fig%state%yscale, fig%state%symlog_threshold)
        y_max_transformed = apply_scale_transform(fig%state%y_max, fig%state%yscale, fig%state%symlog_threshold)
        
        ! Use extracted coordinate transformation function (with Y-axis inversion for screen coords)
        y_canvas = transform_y_coordinate(y_transformed, y_min_transformed, y_max_transformed, fig%state%height, invert=.true.)
    end function

    function check_points_within_canvas(x_scaled, y_scaled, canvas_width, canvas_height) result(all_within)
        !! Check that all scaled points are within the canvas boundaries
        real(wp), dimension(:), intent(in) :: x_scaled, y_scaled
        real(wp), intent(in) :: canvas_width, canvas_height
        logical :: all_within
        integer :: i
        real(wp), parameter :: tolerance = 1.0e-6_wp
        
        all_within = .true.
        
        do i = 1, size(x_scaled)
            ! Check X boundaries with small tolerance for floating point errors
            if (x_scaled(i) < -tolerance .or. x_scaled(i) > canvas_width + tolerance) then
                print *, "Point", i, "X coordinate", x_scaled(i), "outside canvas width", canvas_width
                all_within = .false.
            end if
            
            ! Check Y boundaries
            if (y_scaled(i) < -tolerance .or. y_scaled(i) > canvas_height + tolerance) then
                print *, "Point", i, "Y coordinate", y_scaled(i), "outside canvas height", canvas_height
                all_within = .false.
            end if
        end do
    end function

    ! Test framework utilities
    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A, I0, A, A)') "Test ", test_count, ": ", test_name
    end subroutine

    subroutine test_result(condition, description)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: description
        
        if (condition) then
            write(*, '(A, A)') "  PASS: ", description
            passed_count = passed_count + 1
        else
            write(*, '(A, A)') "  FAIL: ", description
            all_tests_passed = .false.
        end if
    end subroutine

    subroutine print_test_summary()
        write(*, '(/A)') "=== Test Summary ==="
        write(*, '(A, I0, A, I0)') "Passed: ", passed_count, " / ", test_count
        
        if (all_tests_passed) then
            write(*, '(A)') "PASS: All tests PASSED"
        else
            write(*, '(A)') "FAIL: Some tests FAILED"
            error stop 1
        end if
    end subroutine

end program test_scaling
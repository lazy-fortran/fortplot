program test_grid_lines
    !! Test suite for grid lines functionality
    !! Tests major/minor grids, axis selection, and customization
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure
    implicit none
    
    integer :: test_count = 0
    integer :: pass_count = 0
    
    call test_grid_basic()
    call test_grid_major_only()
    call test_grid_minor_only()
    call test_grid_x_axis_only()
    call test_grid_y_axis_only()
    call test_grid_customization()
    call test_grid_toggle()
    
    call print_test_summary()
    
contains

    subroutine test_grid_basic()
        type(figure_t) :: fig
        real(wp) :: x(10), y(10)
        integer :: i
        
        call start_test("Basic grid lines")
        
        call fig%initialize(640, 480)
        
        ! Create test data
        do i = 1, 10
            x(i) = real(i, wp)
            y(i) = sin(real(i, wp) * 0.5_wp)
        end do
        
        call fig%add_plot(x, y)
        call fig%grid(.true.)
        call assert_true(fig%state%grid_enabled, "Grid should be enabled")
        
        call end_test()
    end subroutine test_grid_basic

    subroutine test_grid_major_only()
        type(figure_t) :: fig
        real(wp) :: x(5), y(5)
        integer :: i
        
        call start_test("Major grid lines only")
        
        call fig%initialize(640, 480)
        
        ! Create test data
        do i = 1, 5
            x(i) = real(i, wp)
            y(i) = real(i * 2, wp)
        end do
        
        call fig%add_plot(x, y)
        call fig%grid(which='major')
        call assert_true(fig%state%grid_enabled, "Grid should be enabled")
        call assert_equal_string(fig%state%grid_which, 'major', "Grid which should be major")
        
        call end_test()
    end subroutine test_grid_major_only

    subroutine test_grid_minor_only()
        type(figure_t) :: fig
        real(wp) :: x(5), y(5)
        integer :: i
        
        call start_test("Minor grid lines only")
        
        call fig%initialize(640, 480)
        
        ! Create test data
        do i = 1, 5
            x(i) = real(i, wp)
            y(i) = real(i * i, wp)
        end do
        
        call fig%add_plot(x, y)
        call fig%grid(which='minor')
        call assert_true(fig%state%grid_enabled, "Grid should be enabled")
        call assert_equal_string(fig%state%grid_which, 'minor', "Grid which should be minor")
        
        call end_test()
    end subroutine test_grid_minor_only

    subroutine test_grid_x_axis_only()
        type(figure_t) :: fig
        real(wp) :: x(3), y(3)
        integer :: i
        
        call start_test("X-axis grid lines only")
        
        call fig%initialize(640, 480)
        
        ! Create test data
        do i = 1, 3
            x(i) = real(i, wp)
            y(i) = real(i + 1, wp)
        end do
        
        call fig%add_plot(x, y)
        call fig%grid(axis='x')
        call assert_true(fig%state%grid_enabled, "Grid should be enabled")
        call assert_equal_string(fig%state%grid_axis, 'x', "Grid axis should be x")
        
        call end_test()
    end subroutine test_grid_x_axis_only

    subroutine test_grid_y_axis_only()
        type(figure_t) :: fig
        real(wp) :: x(3), y(3)
        integer :: i
        
        call start_test("Y-axis grid lines only")
        
        call fig%initialize(640, 480)
        
        ! Create test data
        do i = 1, 3
            x(i) = real(i, wp)
            y(i) = real(i * 3, wp)
        end do
        
        call fig%add_plot(x, y)
        call fig%grid(axis='y')
        call assert_true(fig%state%grid_enabled, "Grid should be enabled")
        call assert_equal_string(fig%state%grid_axis, 'y', "Grid axis should be y")
        
        call end_test()
    end subroutine test_grid_y_axis_only

    subroutine test_grid_customization()
        type(figure_t) :: fig
        real(wp) :: x(4), y(4)
        integer :: i
        
        call start_test("Grid customization")
        
        call fig%initialize(640, 480)
        
        ! Create test data
        do i = 1, 4
            x(i) = real(i, wp)
            y(i) = cos(real(i, wp))
        end do
        
        call fig%add_plot(x, y)
        call fig%grid(alpha=0.5_wp, linestyle='--')
        call assert_true(fig%state%grid_enabled, "Grid should be enabled")
        call assert_equal(real(fig%state%grid_alpha, wp), 0.5_wp, "Grid alpha")
        call assert_equal_string(fig%state%grid_linestyle, '--', "Grid linestyle")
        
        call end_test()
    end subroutine test_grid_customization

    subroutine test_grid_toggle()
        type(figure_t) :: fig
        real(wp) :: x(3), y(3)
        integer :: i
        
        call start_test("Grid toggle on/off")
        
        call fig%initialize(640, 480)
        
        ! Create test data
        do i = 1, 3
            x(i) = real(i, wp)
            y(i) = real(i, wp)
        end do
        
        call fig%add_plot(x, y)
        
        ! Turn grid on
        call fig%grid(.true.)
        call assert_true(fig%state%grid_enabled, "Grid should be enabled")
        
        ! Turn grid off
        call fig%grid(.false.)
        call assert_true(.not.(fig%state%grid_enabled), "Grid should be disabled")
        
        call end_test()
    end subroutine test_grid_toggle

    subroutine start_test(test_name)
        character(len=*), intent(in) :: test_name
        write(*, '(A, A)') 'Running test: ', test_name
    end subroutine start_test

    subroutine end_test()
        write(*, '(A)') 'Test completed'
        write(*, *)
    end subroutine end_test

    subroutine assert_true(actual, description)
        logical, intent(in) :: actual
        character(len=*), intent(in) :: description
        
        test_count = test_count + 1
        if (actual) then
            write(*, '(A, A)') '  PASS: ', description
            pass_count = pass_count + 1
        else
            write(*, '(A, A)') '  FAIL: ', description
        end if
    end subroutine assert_true


    subroutine assert_equal(actual, expected, description)
        real(wp), intent(in) :: actual, expected
        character(len=*), intent(in) :: description
        real(wp), parameter :: tolerance = 1.0e-10_wp
        
        test_count = test_count + 1
        if (abs(actual - expected) < tolerance) then
            write(*, '(A, A)') '  PASS: ', description
            pass_count = pass_count + 1
        else
            write(*, '(A, A, F12.6, A, F12.6)') '  FAIL: ', description, actual, ' != ', expected
        end if
    end subroutine assert_equal

    subroutine assert_equal_string(actual, expected, description)
        character(len=*), intent(in) :: actual, expected, description
        
        test_count = test_count + 1
        if (actual == expected) then
            write(*, '(A, A)') '  PASS: ', description
            pass_count = pass_count + 1
        else
            write(*, '(A, A, A, A, A)') '  FAIL: ', description, ' "', trim(actual), '" != "', trim(expected), '"'
        end if
    end subroutine assert_equal_string

    subroutine print_test_summary()
        write(*, '(A)') '============================================'
        write(*, '(A, I0, A, I0, A)') 'Test Summary: ', pass_count, ' of ', test_count, ' tests passed'
        if (pass_count == test_count) then
            write(*, '(A)') 'All tests PASSED!'
        else
            write(*, '(A)') 'Some tests FAILED!'
        end if
    end subroutine print_test_summary

end program test_grid_lines
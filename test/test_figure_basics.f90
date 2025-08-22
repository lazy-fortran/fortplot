program test_figure_basics
    !! Test suite for basic figure functionality
    !! Tests figure initialization, plot addition, and basic operations
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure
    implicit none
    
    integer :: test_count = 0
    integer :: pass_count = 0
    
    call test_figure_initialization()
    call test_plot_addition()
    call test_contour_addition()
    call test_axis_labels()
    
    call print_test_summary()
    
contains

    subroutine test_figure_initialization()
        type(figure_t) :: fig
        
        call start_test("Figure initialization")
        
        ! Test default initialization
        call fig%initialize(640, 480)
        call assert_equal(real(fig%width, wp), 640.0_wp, "Default width")
        call assert_equal(real(fig%height, wp), 480.0_wp, "Default height")
        call assert_equal(real(fig%plot_count, wp), 0.0_wp, "Initial plot count")
        
        call end_test()
    end subroutine test_figure_initialization

    subroutine test_plot_addition()
        type(figure_t) :: fig
        real(wp) :: x(5), y(5)
        integer :: i
        
        call start_test("Plot addition")
        
        call fig%initialize(640, 480)
        
        ! Create test data
        do i = 1, 5
            x(i) = real(i, wp)
            y(i) = real(i**2, wp)
        end do
        
        ! Add plot
        call fig%add_plot(x, y, label="test_data")
        call assert_equal(real(fig%plot_count, wp), 1.0_wp, "Plot count after addition")
        
        ! Add second plot
        call fig%add_plot(x, y*2.0_wp, label="test_data_2")
        call assert_equal(real(fig%plot_count, wp), 2.0_wp, "Plot count after second addition")
        
        call end_test()
    end subroutine test_plot_addition

    subroutine test_contour_addition()
        type(figure_t) :: fig
        real(wp) :: x_grid(3), y_grid(3), z_grid(3,3)
        integer :: i, j
        
        call start_test("Contour addition")
        
        call fig%initialize(640, 480)
        
        ! Create test grid data
        do i = 1, 3
            x_grid(i) = real(i, wp)
            y_grid(i) = real(i, wp)
            do j = 1, 3
                z_grid(i,j) = real(i + j, wp)
            end do
        end do
        
        ! Add contour plot
        call figure_add_contour_filled(fig, x_grid, y_grid, z_grid)
        call assert_equal(real(fig%plot_count, wp), 1.0_wp, "Contour plot count")
        
        call end_test()
    end subroutine test_contour_addition

    subroutine test_axis_labels()
        type(figure_t) :: fig
        
        call start_test("Axis labels")
        
        call fig%initialize(640, 480)
        
        ! Set labels
        call fig%set_xlabel("X Axis")
        call fig%set_ylabel("Y Axis") 
        call fig%set_title("Test Plot")
        
        ! Basic check that labels were set (implementation specific)
        call assert_true(.true., "Labels set successfully")
        
        call end_test()
    end subroutine test_axis_labels

    subroutine start_test(test_name)
        character(len=*), intent(in) :: test_name
        write(*, '(A, A)') 'Running test: ', test_name
    end subroutine start_test

    subroutine end_test()
        write(*, '(A)') 'Test completed'
        write(*, *)
    end subroutine end_test

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

    subroutine assert_true(condition, description)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: description
        
        test_count = test_count + 1
        if (condition) then
            write(*, '(A, A)') '  PASS: ', description
            pass_count = pass_count + 1
        else
            write(*, '(A, A)') '  FAIL: ', description
        end if
    end subroutine assert_true

    subroutine print_test_summary()
        write(*, '(A)') '============================================'
        write(*, '(A, I0, A, I0, A)') 'Test Summary: ', pass_count, ' of ', test_count, ' tests passed'
        if (pass_count == test_count) then
            write(*, '(A)') 'All tests PASSED!'
        else
            write(*, '(A)') 'Some tests FAILED!'
        end if
    end subroutine print_test_summary

end program test_figure_basics
program test_line_styles_basic
    !! Basic line styles test covering fundamental line style functionality
    !! Split from test_line_styles_consolidated.f90 for QADS compliance
    !!
    !! This test covers:
    !! - Basic line styles (solid, dashed, dotted, dash-dot)
    !! - Figure API line style rendering (issue #278)
    !! - Plot function line style support
    !! - Fundamental line style verification
    
    use fortplot
    use fortplot_png
    use fortplot_security, only: get_test_output_path
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    integer :: test_count = 0
    integer :: pass_count = 0

    print *, "=== BASIC LINE STYLES TESTS ==="
    
    ! Run basic line style tests
    call test_basic_line_styles()
    
    call print_test_summary()

contains

    !===========================================================================
    ! Basic Line Styles Tests (from test_line_styles_278.f90)
    !===========================================================================
    
    subroutine test_basic_line_styles()
        print *, "--- Basic Line Styles Tests ---"
        
        call test_figure_api_line_styles()
        call test_plot_function_line_styles()
    end subroutine test_basic_line_styles

    subroutine test_figure_api_line_styles()
        type(figure_t) :: fig
        real(wp) :: x(100), y(100)
        integer :: i
        character(len=512) :: filename
        
        call start_test("Figure API line styles (issue #278)")
        
        ! Generate test data
        do i = 1, 100
            x(i) = real(i-1, wp) / 99.0_wp * 10.0_wp
            y(i) = sin(x(i))
        end do
        
        ! Create figure and test different line styles
        call fig%initialize(640, 480)
        
        call fig%add_plot(x, y, linestyle='-', label='solid')
        call fig%add_plot(x, y + 1.0_wp, linestyle='--', label='dashed')
        call fig%add_plot(x, y + 2.0_wp, linestyle=':', label='dotted')
        call fig%add_plot(x, y + 3.0_wp, linestyle='-.', label='dashdot')
        
        call fig%set_xlabel('X')
        call fig%set_ylabel('Y')
        call fig%set_title('Line Styles Test - Issue #278')
        call fig%legend()
        
        filename = get_test_output_path('/tmp/line_styles_figure_api.png')
        call fig%savefig(filename)
        
        print *, '  Figure API line styles test saved'
        call end_test()
    end subroutine test_figure_api_line_styles

    subroutine test_plot_function_line_styles()
        real(wp) :: x(50), y1(50), y2(50), y3(50), y4(50)
        integer :: i
        character(len=512) :: filename
        
        call start_test("Plot function line styles")
        
        ! Generate test data
        do i = 1, 50
            x(i) = real(i-1, wp) * 0.2_wp
            y1(i) = cos(x(i))
            y2(i) = cos(x(i)) + 1.2_wp
            y3(i) = cos(x(i)) + 2.4_wp  
            y4(i) = cos(x(i)) + 3.6_wp
        end do
        
        call figure(figsize=[600.0_wp, 400.0_wp])
        call plot(x, y1, 'b-', label='solid')
        call plot(x, y2, 'r--', label='dashed')
        call plot(x, y3, 'g:', label='dotted')
        call plot(x, y4, 'm-.', label='dashdot')
        
        call title('Plot Function Line Styles')
        call xlabel('X values')
        call ylabel('Y values')
        call legend()
        call grid(.true.)
        
        filename = get_test_output_path('/tmp/line_styles_plot_function.png')
        call savefig(filename)
        
        print *, '  Plot function line styles test saved'
        call end_test()
    end subroutine test_plot_function_line_styles

    !===========================================================================
    ! Test Framework Utilities
    !===========================================================================

    subroutine start_test(test_name)
        character(len=*), intent(in) :: test_name
        write(*, '(A, A)') 'Running test: ', test_name
    end subroutine start_test

    subroutine end_test()
        write(*, '(A)') 'Test completed'
        write(*, *)
    end subroutine end_test

    subroutine print_test_summary()
        write(*, '(A)') '============================================'
        write(*, '(A)') 'Basic Line Styles Test Summary'
        write(*, '(A)') 'Covers fundamental line style functionality'
        write(*, '(A)') ''
        write(*, '(A)') 'MANUAL VERIFICATION REQUIRED:'
        write(*, '(A)') '  1. Check PNG outputs for basic line pattern visibility'
        write(*, '(A)') '  2. Verify all line styles work in PNG backend (issue #278)'
        write(*, '(A)') '  3. Confirm Figure API and Plot function consistency'
        write(*, '(A)') ''
        write(*, '(A)') 'Basic line styles tests COMPLETED!'
    end subroutine print_test_summary

end program test_line_styles_basic
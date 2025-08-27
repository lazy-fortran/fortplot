program test_line_styles_comprehensive
    !! Comprehensive line styles test consolidating all line style functionality  
    !! Replaces: test_line_styles_278.f90, test_line_styles_comprehensive.f90,
    !!           test_line_patterns_visual_332.f90 (3 files total)
    !!
    !! This test covers:
    !! - All line styles (solid, dashed, dotted, dash-dot)
    !! - Line style rendering in PNG backend (issue #278)
    !! - Line pattern visual appearance (issue #332)
    !! - Pattern clarity and visibility
    !! - Cross-backend compatibility
    !! - Line width and color interactions with styles
    
    use fortplot
    use fortplot_png
    use fortplot_security, only: get_test_output_path
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    integer :: test_count = 0
    integer :: pass_count = 0

    print *, "=== COMPREHENSIVE LINE STYLES TESTS ==="
    
    ! Run all test categories
    call test_basic_line_styles()
    call test_pattern_visibility()
    call test_backend_compatibility()
    call test_style_combinations()
    call test_regression_scenarios()
    
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
        call plot(x, y1, 'b-', label='solid', ! linewidth=2.0_wp)
        call plot(x, y2, 'r--', label='dashed', ! linewidth=2.0_wp)
        call plot(x, y3, 'g:', label='dotted', ! linewidth=2.0_wp)
        call plot(x, y4, 'm-.', label='dashdot', ! linewidth=2.0_wp)
        
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
    ! Pattern Visibility Tests (from test_line_patterns_visual_332.f90)
    !===========================================================================
    
    subroutine test_pattern_visibility()
        print *, "--- Pattern Visibility Tests ---"
        
        call test_dashed_pattern_clarity()
        call test_pattern_comparison_high_level()
    end subroutine test_pattern_visibility

    subroutine test_dashed_pattern_clarity()
        real(wp) :: x_long(100), y_base
        integer :: i
        character(len=512) :: filename
        
        call start_test("Dashed pattern clarity (issue #332)")
        
        ! Create figure for pattern visibility testing
        call figure(figsize=[600.0_wp, 200.0_wp])
        
        ! Create longer line data for pattern visibility
        do i = 1, 100
            x_long(i) = real(i-1, wp) * 0.1_wp
        end do
        
        y_base = 1.0_wp
        ! Test dashed line appearance - should show clear dashes, not dots
        call plot(x_long(1:99), x_long(1:99) * 0.0_wp + y_base, 'r--', &
                 label='Dashed (--) - should show clear dashes')
        
        call title('Dashed Pattern Clarity Test (Issue #332)')
        call xlim([0.0_wp, 10.0_wp])
        call ylim([0.0_wp, 2.0_wp])
        call legend()
        
        filename = get_test_output_path('/tmp/line_dashed_clarity.png')
        call savefig(filename)
        
        print *, '  Dashed pattern clarity test saved'
        call end_test()
    end subroutine test_dashed_pattern_clarity

    subroutine test_pattern_comparison_high_level()
        real(wp) :: x_vals(2), y_vals(4, 2)
        character(len=512) :: filename
        
        call start_test("Pattern comparison high-level")
        
        x_vals = [1.0_wp, 9.0_wp]
        y_vals(1, :) = [3.5_wp, 3.5_wp]  ! Solid
        y_vals(2, :) = [2.5_wp, 2.5_wp]  ! Dashed
        y_vals(3, :) = [1.5_wp, 1.5_wp]  ! Dotted
        y_vals(4, :) = [0.5_wp, 0.5_wp]  ! Dash-dot
        
        call figure(figsize=[600.0_wp, 400.0_wp])
        
        call plot(x_vals, y_vals(1, :), 'k-', ! linewidth=2.0_wp, label='Solid (-)')
        call plot(x_vals, y_vals(2, :), 'r--', ! linewidth=2.0_wp, label='Dashed (--)')
        call plot(x_vals, y_vals(3, :), 'g:', ! linewidth=2.0_wp, label='Dotted (:)')
        call plot(x_vals, y_vals(4, :), 'b-.', ! linewidth=2.0_wp, label='Dash-dot (-.)') 
        
        call title('Line Pattern Comparison')
        call xlim([0.0_wp, 10.0_wp])
        call ylim([0.0_wp, 4.0_wp])
        call legend()
        call text(0.5_wp, 0.2_wp, 'All patterns should be clearly distinguishable')
        
        filename = get_test_output_path('/tmp/line_pattern_comparison.png')
        call savefig(filename)
        
        print *, '  Pattern comparison test saved'
        call end_test()
    end subroutine test_pattern_comparison_high_level



    !===========================================================================
    ! Backend Compatibility Tests
    !===========================================================================
    
    subroutine test_backend_compatibility()
        print *, "--- Backend Compatibility Tests ---"
        
        call test_png_line_styles()
        call test_pdf_line_styles()
        call test_ascii_line_styles()
    end subroutine test_backend_compatibility

    subroutine test_png_line_styles()
        real(wp) :: x(30), y(30)
        integer :: i
        character(len=512) :: filename
        
        call start_test("PNG backend line styles")
        
        do i = 1, 30
            x(i) = real(i-1, wp) * 0.3_wp
            y(i) = sin(x(i)) * exp(-x(i) * 0.1_wp)
        end do
        
        call figure(figsize=[500.0_wp, 350.0_wp])
        call plot(x, y, 'b-', ! linewidth=2.0_wp, label='solid')
        call plot(x, y * 0.7_wp, 'r--', ! linewidth=2.0_wp, label='dashed')
        call plot(x, y * 0.4_wp, 'g:', ! linewidth=2.0_wp, label='dotted')
        call plot(x, y * 0.1_wp, 'm-.', ! linewidth=2.0_wp, label='dashdot')
        
        call title('PNG Backend Line Styles')
        call xlabel('X')
        call ylabel('Y')
        call legend()
        
        filename = get_test_output_path('/tmp/line_styles_png_backend.png')
        call savefig(filename)
        
        print *, '  PNG backend line styles test saved'
        call end_test()
    end subroutine test_png_line_styles

    subroutine test_pdf_line_styles()
        real(wp) :: x(25), y(25)
        integer :: i
        character(len=512) :: filename
        
        call start_test("PDF backend line styles")
        
        do i = 1, 25
            x(i) = real(i-1, wp) * 0.4_wp
            y(i) = exp(-x(i)) * cos(x(i) * 3.0_wp)
        end do
        
        call figure(figsize=[500.0_wp, 350.0_wp])
        call plot(x, y, 'k-', ! linewidth=1.5_wp, label='solid')
        call plot(x, y * 0.8_wp, 'r--', ! linewidth=1.5_wp, label='dashed')
        call plot(x, y * 0.6_wp, 'g:', ! linewidth=1.5_wp, label='dotted')  
        call plot(x, y * 0.4_wp, 'b-.', ! linewidth=1.5_wp, label='dashdot')
        
        call title('PDF Backend Line Styles')
        call xlabel('Position')
        call ylabel('Amplitude')
        call legend()
        call grid(.true.)
        
        filename = get_test_output_path('/tmp/line_styles_pdf_backend.pdf')
        call savefig(filename)
        
        print *, '  PDF backend line styles test saved'
        call end_test()
    end subroutine test_pdf_line_styles

    subroutine test_ascii_line_styles()
        real(wp) :: x(15), y(15)
        integer :: i
        character(len=512) :: filename
        
        call start_test("ASCII backend line styles")
        
        do i = 1, 15
            x(i) = real(i-1, wp) * 0.5_wp
            y(i) = real(i, wp) * 0.3_wp
        end do
        
        call figure(figsize=[60.0_wp, 20.0_wp])
        call plot(x, y, '-', label='solid')
        call plot(x, y * 0.7_wp, '--', label='dashed')
        call plot(x, y * 0.4_wp, ':', label='dotted')
        call plot(x, y * 0.1_wp, '-.', label='dashdot')
        
        call title('ASCII Backend Line Styles')
        call xlabel('X')
        call ylabel('Y')
        
        filename = get_test_output_path('/tmp/line_styles_ascii_backend.txt')
        call savefig(filename)
        
        print *, '  ASCII backend line styles test saved'
        call end_test()
    end subroutine test_ascii_line_styles

    !===========================================================================
    ! Style Combinations Tests
    !===========================================================================
    
    subroutine test_style_combinations()
        print *, "--- Style Combinations Tests ---"
        
        call test_width_style_combinations()
        call test_color_style_combinations()
        call test_marker_style_combinations()
    end subroutine test_style_combinations

    subroutine test_width_style_combinations()
        real(wp) :: x(20), y(20)
        integer :: i
        character(len=512) :: filename
        
        call start_test("Line width and style combinations")
        
        do i = 1, 20
            x(i) = real(i-1, wp) * 0.5_wp
            y(i) = sin(x(i))
        end do
        
        call figure(figsize=[600.0_wp, 400.0_wp])
        
        ! Test different widths with same style
        call plot(x, y, 'b--', ! linewidth=1.0_wp, label='thin dashed')
        call plot(x, y + 0.5_wp, 'r--', ! linewidth=3.0_wp, label='thick dashed')
        call plot(x, y + 1.0_wp, 'g--', ! linewidth=5.0_wp, label='very thick dashed')
        
        call title('Line Width and Style Combinations')
        call xlabel('X')  
        call ylabel('Y')
        call legend()
        
        filename = get_test_output_path('/tmp/line_width_style_combinations.png')
        call savefig(filename)
        
        print *, '  Width-style combinations test saved'
        call end_test()
    end subroutine test_width_style_combinations

    subroutine test_color_style_combinations()
        real(wp) :: x(15), y(15)
        integer :: i
        character(len=512) :: filename
        
        call start_test("Color and style combinations")
        
        do i = 1, 15
            x(i) = real(i, wp) * 0.6_wp
            y(i) = real(i, wp) ** 0.5_wp
        end do
        
        call figure(figsize=[500.0_wp, 400.0_wp])
        
        call plot(x, y, 'r-', ! linewidth=2.0_wp, label='red solid')
        call plot(x, y * 0.9_wp, 'g--', ! linewidth=2.0_wp, label='green dashed')
        call plot(x, y * 0.8_wp, 'b:', ! linewidth=2.0_wp, label='blue dotted')
        call plot(x, y * 0.7_wp, 'k-.', ! linewidth=2.0_wp, label='black dashdot')
        call plot(x, y * 0.6_wp, 'm-', ! linewidth=2.0_wp, label='magenta solid')
        
        call title('Color and Style Combinations')
        call xlabel('X')
        call ylabel('Y')
        call legend()
        
        filename = get_test_output_path('/tmp/line_color_style_combinations.png')
        call savefig(filename)
        
        print *, '  Color-style combinations test saved'
        call end_test()
    end subroutine test_color_style_combinations

    subroutine test_marker_style_combinations()
        real(wp) :: x(10), y(10)
        integer :: i
        character(len=512) :: filename
        
        call start_test("Marker and line style combinations")
        
        do i = 1, 10
            x(i) = real(i, wp)
            y(i) = real(i, wp) * 0.8_wp
        end do
        
        call figure(figsize=[500.0_wp, 350.0_wp])
        
        call plot(x, y, 'bo-', ! linewidth=2.0_wp, markersize=6.0_wp, label='solid with circles')
        call plot(x, y * 0.8_wp, 'rs--', ! linewidth=2.0_wp, markersize=6.0_wp, label='dashed with squares')
        call plot(x, y * 0.6_wp, 'g^:', ! linewidth=2.0_wp, markersize=6.0_wp, label='dotted with triangles')
        call plot(x, y * 0.4_wp, 'md-.', ! linewidth=2.0_wp, markersize=6.0_wp, label='dashdot with diamonds')
        
        call title('Marker and Line Style Combinations')
        call xlabel('X')
        call ylabel('Y')
        call legend()
        call grid(.true.)
        
        filename = get_test_output_path('/tmp/line_marker_style_combinations.png')
        call savefig(filename)
        
        print *, '  Marker-style combinations test saved'
        call end_test()
    end subroutine test_marker_style_combinations

    !===========================================================================
    ! Regression Tests
    !===========================================================================
    
    subroutine test_regression_scenarios()
        print *, "--- Regression Tests ---"
        
        call test_issue_278_regression()
        call test_issue_332_regression() 
    end subroutine test_regression_scenarios

    subroutine test_issue_278_regression()
        type(figure_t) :: fig
        real(wp) :: x(50), y(50)
        integer :: i
        character(len=512) :: filename
        
        call start_test("Issue #278 regression test")
        
        ! Reproduce the original issue #278 scenario
        do i = 1, 50
            x(i) = real(i-1, wp) / 49.0_wp * 8.0_wp
            y(i) = sin(x(i)) + cos(x(i) * 0.5_wp) * 0.3_wp
        end do
        
        call fig%initialize(640, 480)
        call fig%add_plot(x, y, linestyle='-', color='blue', label='original data')
        call fig%add_plot(x, y * 0.8_wp, linestyle='--', color='red', label='modified data')
        call fig%add_plot(x, y * 0.6_wp, linestyle=':', color='green', label='filtered data')
        call fig%add_plot(x, y * 0.4_wp, linestyle='-.', color='orange', label='processed data')
        
        call fig%set_xlabel('Time (s)')
        call fig%set_ylabel('Signal Amplitude')
        call fig%set_title('Issue #278 Regression Test - Line Styles in PNG')
        call fig%legend()
        
        filename = get_test_output_path('/tmp/line_styles_issue_278_regression.png')
        call fig%savefig(filename)
        
        print *, '  Issue #278 regression test saved - verify all line styles visible'
        call end_test()
    end subroutine test_issue_278_regression

    subroutine test_issue_332_regression()
        real(wp) :: x_data(80)
        integer :: i
        character(len=512) :: filename
        
        call start_test("Issue #332 regression test")
        
        ! Generate data for regression test
        do i = 1, 80
            x_data(i) = real(i-1, wp) * 0.125_wp
        end do
        
        call figure(figsize=[800.0_wp, 300.0_wp])
        
        ! Test the scenario that was showing sparse dots instead of dashes
        call plot(x_data(1:79), x_data(1:79) * 0.0_wp + 2.0_wp, 'r--', &
                 label='Issue #332: Dashes should be clear')
        
        ! Add dash-dot pattern for comparison
        call plot(x_data(1:79), x_data(1:79) * 0.0_wp + 1.0_wp, 'b-.', &
                 label='Dash-dot should show distinct rhythm')
        
        call title('Issue #332 Regression Test - Clear Dash Patterns')
        call xlim([0.0_wp, 10.0_wp])
        call ylim([0.0_wp, 3.0_wp])
        call legend()
        call text(0.5_wp, 2.5_wp, 'Verify: Dashes clear, not sparse dots')
        call text(0.5_wp, 0.5_wp, 'Verify: Dash-dot rhythm distinct')
        
        filename = get_test_output_path('/tmp/line_styles_issue_332_regression.png')
        call savefig(filename)
        
        print *, '  Issue #332 regression test saved - verify clear dash patterns'
        call end_test()
    end subroutine test_issue_332_regression

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
        write(*, '(A)') 'Line Styles Comprehensive Test Summary'
        write(*, '(A)') 'Consolidated 3 line style test files into single comprehensive test'
        write(*, '(A)') ''
        write(*, '(A)') 'MANUAL VERIFICATION REQUIRED:'
        write(*, '(A)') '  1. Check all PNG outputs for clear line pattern visibility'
        write(*, '(A)') '  2. Verify dashed lines show dashes, not sparse dots (issue #332)'
        write(*, '(A)') '  3. Confirm all line styles work in PNG backend (issue #278)'
        write(*, '(A)') '  4. Compare PDF output for pattern consistency'
        write(*, '(A)') '  5. Verify ASCII backend renders styles appropriately'
        write(*, '(A)') ''
        write(*, '(A)') 'All comprehensive line styles tests COMPLETED!'
    end subroutine print_test_summary

end program test_line_styles_comprehensive
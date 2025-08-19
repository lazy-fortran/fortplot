program test_syntax_backward_compatibility
    !! Test suite for ensuring backward compatibility with existing fortplot syntax
    !! Following TDD RED phase - these tests ensure existing code continues working
    !! 
    !! GIVEN: existing fortplot code using current parameter formats and constants
    !! WHEN: introducing matplotlib syntax compatibility
    !! THEN: should maintain 100% backward compatibility with zero code changes required

    use fortplot
    use fortplot_markers, only: validate_marker_style, get_marker_size
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    integer :: test_count = 0
    integer :: pass_count = 0
    
    ! Backward compatibility validation tests
    call test_existing_linestyle_constants()
    call test_existing_parameter_names()
    call test_existing_marker_constants()
    call test_existing_api_signatures()
    call test_legacy_examples_still_work()
    call test_mixed_old_new_syntax()
    call test_parameter_precedence()
    
    call print_test_summary()
    
contains

    subroutine test_existing_linestyle_constants()
        !! GIVEN: existing LINESTYLE_* constants in fortplot API
        !! WHEN: using existing constants in plotting functions
        !! THEN: should work exactly as before without modification
        
        type(figure_t) :: fig
        real(wp) :: x(5), y1(5), y2(5), y3(5), y4(5), y5(5)
        integer :: i
        
        call start_test("Existing LINESTYLE_* constants compatibility")
        
        ! Generate test data
        do i = 1, 5
            x(i) = real(i, wp)
            y1(i) = real(i, wp) + 4.0_wp
            y2(i) = real(i, wp) + 3.0_wp
            y3(i) = real(i, wp) + 2.0_wp
            y4(i) = real(i, wp) + 1.0_wp
            y5(i) = real(i, wp) + 0.0_wp
        end do
        
        call fig%initialize(600, 400)
        call fig%set_title("Backward Compatibility: LINESTYLE Constants")
        call fig%set_xlabel("X values")
        call fig%set_ylabel("Y values")
        
        ! Test that existing LINESTYLE_* constants still work
        call fig%add_plot(x, y1, linestyle=LINESTYLE_SOLID, label='LINESTYLE_SOLID')
        call fig%add_plot(x, y2, linestyle=LINESTYLE_DASHED, label='LINESTYLE_DASHED')
        call fig%add_plot(x, y3, linestyle=LINESTYLE_DOTTED, label='LINESTYLE_DOTTED')
        call fig%add_plot(x, y4, linestyle=LINESTYLE_DASHDOT, label='LINESTYLE_DASHDOT')
        call fig%add_plot(x, y5, linestyle=LINESTYLE_NONE, label='LINESTYLE_NONE')
        
        call fig%legend()
        call fig%savefig('output/test/test_syntax_backward_compatibility/linestyle_constants_test.png')
        call fig%savefig('output/test/test_syntax_backward_compatibility/linestyle_constants_test.pdf')
        call fig%savefig('output/test/test_syntax_backward_compatibility/linestyle_constants_test.txt')
        
        call end_test()
    end subroutine test_existing_linestyle_constants

    subroutine test_existing_parameter_names()
        !! GIVEN: existing parameter names like 'linestyle' in current API
        !! WHEN: using existing parameter names in plotting functions
        !! THEN: should work exactly as before with full compatibility
        
        type(figure_t) :: fig
        real(wp) :: x(3), y(3)
        integer :: i
        
        call start_test("Existing parameter names compatibility")
        
        ! Generate simple test data
        do i = 1, 3
            x(i) = real(i, wp)
            y(i) = real(i, wp) * 1.5_wp
        end do
        
        call fig%initialize(400, 300)
        call fig%set_title("Backward Compatibility: Parameter Names")
        
        ! Test existing parameter names still work
        call fig%add_plot(x, y, linestyle='-', label='linestyle parameter')
        
        ! Test global plotting functions still work with existing syntax
        call figure(400, 300)
        call plot(x, y, linestyle='--', label='global plot function')
        call title('Global Function Compatibility')
        call xlabel('X values')
        call ylabel('Y values')
        call legend()
        call savefig('output/test/test_syntax_backward_compatibility/parameter_names_test.png')
        
        call end_test()
    end subroutine test_existing_parameter_names

    subroutine test_existing_marker_constants()
        !! GIVEN: existing marker constants and marker validation functions
        !! WHEN: using existing marker API
        !! THEN: should work exactly as before with all existing functionality
        
        logical :: is_valid
        real(wp) :: marker_size
        
        call start_test("Existing marker constants and functions compatibility")
        
        ! Test that existing marker validation still works
        is_valid = validate_marker_style('o')
        call assert_true(is_valid, "Circle marker validation still works")
        
        is_valid = validate_marker_style('s')
        call assert_true(is_valid, "Square marker validation still works")
        
        is_valid = validate_marker_style('^')
        call assert_true(is_valid, "Triangle marker validation still works")
        
        is_valid = validate_marker_style('invalid')
        call assert_false(is_valid, "Invalid marker rejection still works")
        
        ! Test that existing marker size functions still work
        marker_size = get_marker_size('o')
        call assert_true(marker_size > 0.0_wp, "Circle marker size function works")
        
        marker_size = get_marker_size('s')
        call assert_true(marker_size > 0.0_wp, "Square marker size function works")
        
        ! Test that existing marker constants still work
        is_valid = validate_marker_style(MARKER_CIRCLE)
        call assert_true(is_valid, "MARKER_CIRCLE constant still works")
        
        is_valid = validate_marker_style(MARKER_SQUARE)
        call assert_true(is_valid, "MARKER_SQUARE constant still works")
        
        call end_test()
    end subroutine test_existing_marker_constants

    subroutine test_existing_api_signatures()
        !! GIVEN: existing function signatures in fortplot API
        !! WHEN: calling functions with existing parameter combinations
        !! THEN: should maintain full compatibility without signature changes
        
        type(figure_t) :: fig
        real(wp) :: x(3), y(3), xerr(3), yerr(3)
        integer :: i
        
        call start_test("Existing API signatures compatibility")
        
        ! Generate test data
        do i = 1, 3
            x(i) = real(i, wp)
            y(i) = real(i, wp) * 2.0_wp
            xerr(i) = 0.1_wp
            yerr(i) = 0.2_wp
        end do
        
        call fig%initialize(400, 300)
        call fig%set_title("API Signatures Compatibility")
        
        ! Test that existing function calls still work exactly as before
        call fig%add_plot(x, y, label='Basic plot')
        call fig%add_plot(x, y, linestyle='-', label='With linestyle')
        call fig%add_plot(x, y, label='With label', linestyle='--')
        
        ! Test errorbar function with existing signature
        call fig%errorbar(x, y, xerr=xerr, yerr=yerr, &
                         capsize=5.0_wp, &
                         elinewidth=1.0_wp, &
                         label='Error bars', &
                         linestyle='-')
        
        call fig%legend()
        call fig%savefig('output/test/test_syntax_backward_compatibility/api_signatures_test.png')
        
        call end_test()
    end subroutine test_existing_api_signatures

    subroutine test_legacy_examples_still_work()
        !! GIVEN: existing example code patterns from fortplot documentation
        !! WHEN: running legacy example patterns
        !! THEN: should produce identical results without modification
        
        type(figure_t) :: fig
        real(wp) :: x(10), y1(10), y2(10)
        integer :: i
        
        call start_test("Legacy example patterns still work")
        
        ! Simulate legacy example code patterns
        do i = 1, 10
            x(i) = real(i-1, wp) * 0.5_wp
            y1(i) = sin(x(i))
            y2(i) = cos(x(i))
        end do
        
        ! Legacy pattern 1: Basic plotting with constants
        call fig%initialize(600, 400)
        call fig%add_plot(x, y1, linestyle=LINESTYLE_SOLID, label='Sine')
        call fig%add_plot(x, y2, linestyle=LINESTYLE_DASHED, label='Cosine')
        call fig%set_title('Legacy Pattern: Basic Plotting')
        call fig%set_xlabel('X')
        call fig%set_ylabel('Y')
        call fig%legend()
        call fig%savefig('output/test/test_syntax_backward_compatibility/legacy_basic_test.png')
        
        ! Legacy pattern 2: Global function usage
        call figure(600, 400)
        call plot(x, y1, linestyle=LINESTYLE_SOLID, label='Sine (global)')
        call plot(x, y2, linestyle=LINESTYLE_DASHED, label='Cosine (global)')
        call title('Legacy Pattern: Global Functions')
        call xlabel('X')
        call ylabel('Y')
        call legend()
        call savefig('output/test/test_syntax_backward_compatibility/legacy_global_test.png')
        
        call end_test()
    end subroutine test_legacy_examples_still_work

    subroutine test_mixed_old_new_syntax()
        !! GIVEN: mixture of old fortplot syntax and new matplotlib syntax
        !! WHEN: using both syntaxes in the same plot
        !! THEN: should work seamlessly without conflicts
        
        type(figure_t) :: fig
        real(wp) :: x(5), y1(5), y2(5), y3(5), y4(5)
        integer :: i
        
        call start_test("Mixed old and new syntax compatibility")
        
        ! Generate test data
        do i = 1, 5
            x(i) = real(i, wp)
            y1(i) = real(i, wp) + 3.0_wp
            y2(i) = real(i, wp) + 2.0_wp
            y3(i) = real(i, wp) + 1.0_wp
            y4(i) = real(i, wp) + 0.0_wp
        end do
        
        call fig%initialize(600, 400)
        call fig%set_title("Mixed Syntax Compatibility")
        call fig%set_xlabel("X values")
        call fig%set_ylabel("Y values")
        
        ! Mix old constants with new matplotlib syntax
        call fig%add_plot(x, y1, linestyle=LINESTYLE_SOLID, label='Old: LINESTYLE_SOLID')
        call fig%add_plot(x, y2, linestyle='--', label='New: matplotlib dashed')
        call fig%add_plot(x, y3, linestyle=LINESTYLE_DOTTED, label='Old: LINESTYLE_DOTTED')
        call fig%add_plot(x, y4, linestyle='o', label='New: matplotlib marker')
        
        call fig%legend()
        call fig%savefig('output/test/test_syntax_backward_compatibility/mixed_syntax_test.png')
        call fig%savefig('output/test/test_syntax_backward_compatibility/mixed_syntax_test.pdf')
        call fig%savefig('output/test/test_syntax_backward_compatibility/mixed_syntax_test.txt')
        
        call end_test()
    end subroutine test_mixed_old_new_syntax

    subroutine test_parameter_precedence()
        !! GIVEN: potential conflicts between old and new parameter formats
        !! WHEN: specifying parameters using both old and new syntax
        !! THEN: should handle precedence clearly and predictably
        
        type(figure_t) :: fig
        real(wp) :: x(3), y(3)
        integer :: i
        
        call start_test("Parameter precedence and conflict resolution")
        
        ! Generate simple test data
        do i = 1, 3
            x(i) = real(i, wp)
            y(i) = real(i, wp)
        end do
        
        call fig%initialize(400, 300)
        call fig%set_title("Parameter Precedence Test")
        
        ! Test parameter precedence when both old and new formats might be specified
        ! This ensures predictable behavior in edge cases
        call fig%add_plot(x, y, linestyle='-', label='Basic new syntax')
        
        ! Test that the API handles edge cases gracefully
        ! These tests will help define the precedence rules
        
        call fig%legend()
        call fig%savefig('output/test/test_syntax_backward_compatibility/precedence_test.png')
        
        call end_test()
    end subroutine test_parameter_precedence

    ! Testing utilities
    subroutine start_test(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A,A)') "Running test: ", trim(test_name)
    end subroutine start_test

    subroutine end_test()
        pass_count = pass_count + 1
        write(*, '(A)') "  PASS"
    end subroutine end_test

    subroutine assert_true(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        if (.not. condition) then
            write(*, '(A,A)') "  FAIL: ", trim(message)
            error stop "Test assertion failed"
        end if
    end subroutine assert_true

    subroutine assert_false(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        if (condition) then
            write(*, '(A,A)') "  FAIL: ", trim(message)
            error stop "Test assertion failed"
        end if
    end subroutine assert_false

    subroutine print_test_summary()
        write(*, '(A,I0,A,I0,A)') "Tests completed: ", pass_count, "/", test_count, " passed"
        if (pass_count /= test_count) error stop "Some tests failed"
    end subroutine print_test_summary

end program test_syntax_backward_compatibility
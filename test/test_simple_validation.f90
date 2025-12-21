program test_simple_validation
    !! Simple validation test to verify refactoring works correctly
    !! This replaces the more complex tests with simple pass/fail logic
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure
    use fortplot_constants, only: EPSILON_COMPARE
    implicit none
    
    logical :: all_tests_passed = .true.
    
    write(*, '(A)') '============================================'
    write(*, '(A)') 'REFACTORING VALIDATION TEST'
    write(*, '(A)') '============================================'
    
    ! Test 1: Basic module imports
    call test_module_imports()
    
    ! Test 2: Figure creation
    call test_figure_creation()
    
    ! Test 3: Scale transformations
    call test_scale_functions()
    
    ! Final result
    write(*, '(A)') '============================================'
    if (all_tests_passed) then
        write(*, '(A)') 'PASS: ALL REFACTORING TESTS PASSED!'
        write(*, '(A)') 'Refactored modules are working correctly.'
    else
        write(*, '(A)') 'FAIL: SOME TESTS FAILED!'
        stop 1
    end if
    
contains

    subroutine test_module_imports()
        write(*, '(A)') 'Test 1: Module imports and basic functionality'
        
        block
            real(wp) :: result
            result = apply_scale_transform(10.0_wp, 'log', 1.0_wp)
            if (result > 0.0_wp) then
                write(*, '(A)') '  PASS: Scale module working'
            else
                write(*, '(A)') '  FAIL: Scale module failed'
                all_tests_passed = .false.
            end if
        end block
        
        write(*, *)
    end subroutine test_module_imports

    subroutine test_figure_creation()
        write(*, '(A)') 'Test 2: Figure creation and plot addition'
        
        block
            type(figure_t) :: fig
            real(wp) :: x(3), y(3)
            integer :: i
            
            do i = 1, 3
                x(i) = real(i, wp)
                y(i) = real(i**2, wp)
            end do
            
            call fig%initialize(80, 24)
            call fig%add_plot(x, y, label="test")
            
            if (fig%plot_count == 1) then
                write(*, '(A)') '  PASS: Figure creation working'
            else
                write(*, '(A)') '  FAIL: Figure creation failed'
                all_tests_passed = .false.
            end if
        end block
        
        write(*, *)
    end subroutine test_figure_creation

    subroutine test_scale_functions()
        write(*, '(A)') 'Test 3: Scale transformation functions'
        
        block
            real(wp) :: linear_result, log_result
            
            linear_result = apply_scale_transform(5.0_wp, 'linear', 1.0_wp)
            log_result = apply_scale_transform(10.0_wp, 'log', 1.0_wp)
            
            if (abs(linear_result - 5.0_wp) < EPSILON_COMPARE .and. abs(log_result - 1.0_wp) < EPSILON_COMPARE) then
                write(*, '(A)') '  PASS: Scale transformations working'
            else
                write(*, '(A)') '  FAIL: Scale transformations failed'
                all_tests_passed = .false.
            end if
        end block
        
        write(*, *)
    end subroutine test_scale_functions

end program test_simple_validation
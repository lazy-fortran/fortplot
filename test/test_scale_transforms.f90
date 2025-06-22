program test_scale_transforms
    !! Test suite for scale transformation functions
    !! Tests linear, log, and symlog transformations before refactoring
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure, only: apply_scale_transform, transform_x_coordinate, transform_y_coordinate
    implicit none
    
    integer :: test_count = 0
    integer :: pass_count = 0
    
    call test_linear_scale_transform()
    call test_log_scale_transform()
    call test_symlog_scale_transform()
    call test_coordinate_transforms()
    
    call print_test_summary()
    
contains

    subroutine test_linear_scale_transform()
        real(wp) :: input, result, expected
        
        call start_test("Linear scale transform")
        
        ! Test linear scale (should be identity)
        input = 5.0_wp
        result = apply_scale_transform(input, 'linear', 1.0_wp)
        expected = 5.0_wp
        call assert_equal(result, expected, "Linear transform identity")
        
        ! Test negative values
        input = -3.0_wp
        result = apply_scale_transform(input, 'linear', 1.0_wp)
        expected = -3.0_wp
        call assert_equal(result, expected, "Linear transform negative")
        
        call end_test()
    end subroutine test_linear_scale_transform

    subroutine test_log_scale_transform()
        real(wp) :: input, result, expected
        
        call start_test("Log scale transform")
        
        ! Test log scale
        input = 10.0_wp
        result = apply_scale_transform(input, 'log', 1.0_wp)
        expected = log10(10.0_wp)
        call assert_equal(result, expected, "Log transform base case")
        
        ! Test powers of 10
        input = 100.0_wp
        result = apply_scale_transform(input, 'log', 1.0_wp)
        expected = 2.0_wp
        call assert_equal(result, expected, "Log transform power of 10")
        
        call end_test()
    end subroutine test_log_scale_transform

    subroutine test_symlog_scale_transform()
        real(wp) :: input, result, threshold
        
        call start_test("Symlog scale transform")
        
        threshold = 1.0_wp
        
        ! Test linear region (within threshold)
        input = 0.5_wp
        result = apply_scale_transform(input, 'symlog', threshold)
        call assert_equal(result, input, "Symlog linear region positive")
        
        input = -0.5_wp
        result = apply_scale_transform(input, 'symlog', threshold)
        call assert_equal(result, input, "Symlog linear region negative")
        
        ! Test log region (outside threshold)
        input = 10.0_wp
        result = apply_scale_transform(input, 'symlog', threshold)
        call assert_true(result > threshold, "Symlog log region positive")
        
        input = -10.0_wp
        result = apply_scale_transform(input, 'symlog', threshold)
        call assert_true(result < -threshold, "Symlog log region negative")
        
        call end_test()
    end subroutine test_symlog_scale_transform

    subroutine test_coordinate_transforms()
        real(wp) :: data_x, data_y, screen_x, screen_y
        real(wp) :: x_min, x_max, y_min, y_max
        integer :: width, height
        
        call start_test("Coordinate transforms")
        
        ! Set up test coordinate system
        x_min = 0.0_wp
        x_max = 10.0_wp
        y_min = 0.0_wp
        y_max = 5.0_wp
        width = 400
        height = 300
        
        ! Test center point
        data_x = 5.0_wp
        data_y = 2.5_wp
        screen_x = transform_x_coordinate(data_x, x_min, x_max, width)
        screen_y = transform_y_coordinate(data_y, y_min, y_max, height)
        
        call assert_equal(screen_x, real(width/2, wp), "X coordinate center")
        call assert_equal(screen_y, real(height/2, wp), "Y coordinate center")
        
        call end_test()
    end subroutine test_coordinate_transforms

    subroutine start_test(test_name)
        character(len=*), intent(in) :: test_name
        write(*, '(A, A)') 'Running test: ', test_name
        test_count = test_count + 1
    end subroutine start_test

    subroutine end_test()
        write(*, '(A)') 'Test completed'
        write(*, *)
    end subroutine end_test

    subroutine assert_equal(actual, expected, description)
        real(wp), intent(in) :: actual, expected
        character(len=*), intent(in) :: description
        real(wp), parameter :: tolerance = 1.0e-10_wp
        
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

end program test_scale_transforms
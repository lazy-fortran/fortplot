program test_scatter_behavior
    !! Test that format strings correctly distinguish between
    !! scatter plots (markers only) and line+marker plots
    use fortplot_figure_core, only: figure_t
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use iso_fortran_env, only: error_unit
    implicit none
    
    call test_marker_only_no_line()
    call test_marker_with_line()
    
    write(error_unit, '(A)') 'All scatter behavior tests passed!'
    
contains

    subroutine test_marker_only_no_line()
        !! Test that 'o' format creates markers without lines
        type(figure_t) :: fig
        real(wp) :: x(3) = [1.0_wp, 2.0_wp, 3.0_wp]
        real(wp) :: y(3) = [1.0_wp, 4.0_wp, 2.0_wp]
        
        call fig%initialize(100, 100)
        
        ! This should create markers WITHOUT lines (scatter plot)
        call fig%add_plot(x, y, label='scatter', linestyle='o')
        
        ! Check that the plot has markers but NO line style
        write(error_unit, '(A)') 'DEBUG: marker="' // trim(fig%plots(1)%marker) // '"'
        write(error_unit, '(A)') 'DEBUG: linestyle="' // trim(fig%plots(1)%linestyle) // '"'
        call assert_equal(trim(fig%plots(1)%marker), 'o', 'Expected circle marker')
        call assert_equal(trim(fig%plots(1)%linestyle), '', 'Expected NO line style for scatter plot')
    end subroutine

    subroutine test_marker_with_line()
        !! Test that 'o-' format creates markers with lines
        type(figure_t) :: fig
        real(wp) :: x(3) = [1.0_wp, 2.0_wp, 3.0_wp]
        real(wp) :: y(3) = [1.0_wp, 4.0_wp, 2.0_wp]
        
        call fig%initialize(100, 100)
        
        ! This should create markers WITH lines
        call fig%add_plot(x, y, label='line+marker', linestyle='o-')
        
        ! Check that the plot has both markers AND line style
        call assert_equal(trim(fig%plots(1)%marker), 'o', 'Expected circle marker')
        call assert_equal(trim(fig%plots(1)%linestyle), '-', 'Expected solid line style')
    end subroutine

    subroutine assert_equal(actual, expected, message)
        character(len=*), intent(in) :: actual, expected, message
        
        if (actual /= expected) then
            write(error_unit, '(A)') 'ASSERTION FAILED: ' // trim(message)
            write(error_unit, '(A)') 'Expected: "' // trim(expected) // '"'
            write(error_unit, '(A)') 'Actual: "' // trim(actual) // '"'
            error stop 1
        end if
    end subroutine

end program
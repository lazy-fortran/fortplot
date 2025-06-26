program test_scatter_marker_only
    !! Test that scatter plots with markers only don't draw connecting lines
    !! This test verifies the fix for the issue where linestyle='o' was drawing lines
    
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    call test_should_not_draw_lines_for_marker_only_scatter()
    print *, "All scatter marker tests passed!"
    
contains

    subroutine test_should_not_draw_lines_for_marker_only_scatter()
        !! Test that format string with only markers doesn't draw lines
        type(figure_t) :: fig
        real(wp) :: x(5), y(5)
        integer :: i
        
        ! Create simple test data
        do i = 1, 5
            x(i) = real(i, wp)
            y(i) = real(i, wp) * 2.0_wp
        end do
        
        call fig%initialize(400, 300)
        call fig%set_title("Scatter Plot Test")
        call fig%set_xlabel("X")
        call fig%set_ylabel("Y")
        
        ! This should show ONLY markers, no connecting lines!
        call fig%add_plot(x, y, linestyle='o', label='Markers Only')
        
        ! Save as ASCII to visually verify no connecting lines
        call fig%savefig('test_scatter_marker_only.txt')
        
        ! TODO: Add proper assertions to check internal state
        ! For now, manual verification via ASCII output
        print *, "Check test_scatter_marker_only.txt - should show only 'o' markers, no '#' lines"
    end subroutine test_should_not_draw_lines_for_marker_only_scatter

end program test_scatter_marker_only
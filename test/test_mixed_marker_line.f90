program test_mixed_marker_line
    !! Test that combined marker+line formats still work correctly after the fix
    
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    call test_should_draw_both_markers_and_lines()
    print *, "Mixed marker+line tests passed!"
    
contains

    subroutine test_should_draw_both_markers_and_lines()
        !! Test that format strings with both markers and lines work correctly
        type(figure_t) :: fig
        real(wp) :: x(4), y(4)
        integer :: i
        
        ! Create simple test data
        do i = 1, 4
            x(i) = real(i, wp)
            y(i) = real(i, wp) * 1.5_wp
        end do
        
        call fig%initialize(400, 300)
        call fig%set_title("Mixed Marker+Line Test")
        call fig%set_xlabel("X")
        call fig%set_ylabel("Y")
        
        ! This should show BOTH markers AND connecting lines
        call fig%add_plot(x, y, linestyle='o-', label='Markers with Lines')
        
        ! Save as ASCII to visually verify both markers and lines
        call fig%savefig('output/test/test_mixed_marker_line/test_mixed_marker_line.txt')
        
        print *, "Check test_mixed_marker_line.txt - should show both 'o' markers and '#' lines"
    end subroutine test_should_draw_both_markers_and_lines

end program test_mixed_marker_line
program debug_legend
    !! Debug legend rendering issue
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    type(figure_t) :: test_fig
    real(wp), dimension(10) :: x, y1, y2
    integer :: i
    
    print *, "=== Debug Legend Rendering ==="
    
    ! Generate simple test data
    x = [(real(i, wp), i=1, 10)]
    y1 = x * 2.0_wp
    y2 = x * 3.0_wp
    
    call test_fig%initialize(640, 480)
    call test_fig%set_title("Debug Legend Test")
    call test_fig%set_xlabel("x")
    call test_fig%set_ylabel("y")
    
    ! Add labeled plots
    call test_fig%add_plot(x, y1, label="Line 1")
    call test_fig%add_plot(x, y2, label="Line 2")
    
    print *, "Added plots with labels"
    
    ! Add legend with specific position
    call test_fig%legend(location="lower left")
    
    print *, "Called legend() method"
    print *, "show_legend =", test_fig%show_legend
    print *, "legend entries allocated =", allocated(test_fig%legend_data%entries)
    if (allocated(test_fig%legend_data%entries)) then
        print *, "Number of legend entries =", test_fig%legend_data%num_entries
        do i = 1, test_fig%legend_data%num_entries
            print *, "Entry", i, ":", trim(test_fig%legend_data%entries(i)%label)
        end do
    end if
    
    ! Save to PNG and ASCII
    call test_fig%savefig('app/debug_legend.png')
    call test_fig%savefig('app/debug_legend.txt')
    
    print *, "Saved debug legend test (PNG and ASCII)"

end program debug_legend
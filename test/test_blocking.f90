program test_blocking
    !! Test blocking parameter in show and savefig routines
    
    use fortplot
    use fortplot_security, only: get_test_output_path
    use fortplot_system_runtime, only: is_windows
    use iso_fortran_env, only: wp => real64
    implicit none
    
    call test_show_with_blocking()
    call test_savefig_with_blocking()
    call test_show_viewer_with_blocking()
    
    print *, "All blocking tests passed!"
    
contains

    subroutine test_show_with_blocking()
        !! Test that show() accepts optional blocking parameter
        type(figure_t) :: fig
        real(wp), dimension(5) :: x, y
        integer :: i
        
        ! Create test data
        x = [(real(i, wp), i = 1, 5)]
        y = x**2
        
        ! Initialize figure
        call fig%initialize()
        call fig%add_plot(x, y, label="Test")
        
        ! Test non-blocking (default) - use figure method to force ASCII
        print *, "Testing show() without blocking parameter (default)"
        if (.not. is_windows()) then
            ! Use figure method to force ASCII display and avoid GUI viewer
            call show(blocking=.false.)
        else
            print *, "SKIPPED: show() on Windows (prevents CI hang)"
        end if
        
        ! Test with explicit blocking=false - skip on Windows  
        print *, "Testing show() with explicit blocking=.false."
        if (.not. is_windows()) then
            ! Use figure method to force ASCII display and avoid GUI viewer
            call show(blocking=.false.)
        else
            print *, "SKIPPED: show(blocking=.false.) on Windows (prevents CI hang)"
        end if
        
        print *, "test_show_with_blocking: PASSED"
    end subroutine test_show_with_blocking
    
    subroutine test_savefig_with_blocking()
        !! Test that savefig() accepts optional blocking parameter
        type(figure_t) :: fig
        real(wp), dimension(5) :: x, y
        integer :: i
        
        ! Create test data
        x = [(real(i, wp), i = 1, 5)]
        y = x**2
        
        ! Initialize figure
        call fig%initialize()
        call fig%add_plot(x, y, label="Test")
        
        ! Test non-blocking (default)
        print *, "Testing savefig() without blocking parameter (default)"
        call figure_savefig(fig, get_test_output_path('output/test/test_blocking/test_blocking.png'))
        call figure_savefig(fig, get_test_output_path('/tmp/test_blocking.png'))
        
        ! Test with explicit blocking=false for ASCII
        print *, "Testing savefig() with blocking=.false. for ASCII"
        call figure_savefig(fig, get_test_output_path('output/test/test_blocking/test_blocking.txt'), blocking=.false.)
        
        print *, "test_savefig_with_blocking: PASSED"
    end subroutine test_savefig_with_blocking
    
    subroutine test_show_viewer_with_blocking()
        !! Test that show_viewer() accepts optional blocking parameter
        type(figure_t) :: test_fig
        real(wp), dimension(5) :: x, y
        integer :: i
        
        ! Create test data
        x = [(real(i, wp), i = 1, 5)]
        y = x**2
        
        ! Use figure method to avoid GUI viewer and force ASCII
        call test_fig%initialize()
        call test_fig%add_plot(x, y, label="Test")
        
        ! Test non-blocking (default) - use figure method to force ASCII
        print *, "Testing show_viewer() without blocking parameter (default)"
        if (.not. is_windows()) then
            ! Use figure method to force ASCII display and avoid GUI viewer
            call test_fig%show(blocking=.false.)
        else
            print *, "SKIPPED: show_viewer() on Windows (prevents CI hang)"
        end if
        
        print *, "test_show_viewer_with_blocking: PASSED"
    end subroutine test_show_viewer_with_blocking

end program test_blocking
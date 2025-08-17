program test_blocking
    !! Test blocking parameter in show and savefig routines
    
    use fortplot
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
        
        ! Test non-blocking (default)
        print *, "Testing show() without blocking parameter (default)"
        call fig%show()
        
        ! Test with explicit blocking=false
        print *, "Testing show() with explicit blocking=.false."
        call fig%show(blocking=.false.)
        
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
        call fig%savefig('output/test/test_blocking/test_blocking.png')
        call fig%savefig('/tmp/test_blocking.png')
        
        ! Test with explicit blocking=false for ASCII
        print *, "Testing savefig() with blocking=.false. for ASCII"
        call fig%savefig('output/test/test_blocking/test_blocking.txt', blocking=.false.)
        
        print *, "test_savefig_with_blocking: PASSED"
    end subroutine test_savefig_with_blocking
    
    subroutine test_show_viewer_with_blocking()
        !! Test that show_viewer() accepts optional blocking parameter
        real(wp), dimension(5) :: x, y
        integer :: i
        
        ! Create test data
        x = [(real(i, wp), i = 1, 5)]
        y = x**2
        
        ! Test global figure API
        call figure()
        call plot(x, y, label="Test")
        
        ! Test non-blocking (default)
        print *, "Testing show_viewer() without blocking parameter (default)"
        call show_viewer()
        
        print *, "test_show_viewer_with_blocking: PASSED"
    end subroutine test_show_viewer_with_blocking

end program test_blocking
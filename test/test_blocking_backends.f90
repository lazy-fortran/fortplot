program test_blocking_backends
    !! Test blocking parameter works with all backends
    
    use fortplot
    use fortplot_security, only: get_test_output_path, safe_create_directory
    use iso_fortran_env, only: wp => real64
    implicit none
    
    logical :: dir_success
    
    ! Create test output directory first
    call safe_create_directory('build/test', dir_success)
    if (.not. dir_success) then
        print *, "WARNING: Could not create build/test directory"
    end if
    
    call test_blocking_with_png()
    call test_blocking_with_pdf()
    call test_blocking_with_ascii()
    
    print *, "All backend blocking tests passed!"
    
contains

    subroutine test_blocking_with_png()
        !! Test blocking with PNG backend
        type(figure_t) :: fig
        real(wp), dimension(5) :: x, y
        integer :: i
        
        ! Create test data
        x = [(real(i, wp), i = 1, 5)]
        y = x**2
        
        ! Initialize figure
        call fig%initialize()
        call fig%add_plot(x, y, label="PNG Test")
        
        ! Test non-blocking save
        print *, "Testing PNG save with blocking=.false."
        call fig%savefig(get_test_output_path('build/test/test_blocking_backend.png'), blocking=.false.)
        
        print *, "test_blocking_with_png: PASSED"
    end subroutine test_blocking_with_png
    
    subroutine test_blocking_with_pdf()
        !! Test blocking with PDF backend
        type(figure_t) :: fig
        real(wp), dimension(5) :: x, y
        integer :: i
        
        ! Create test data
        x = [(real(i, wp), i = 1, 5)]
        y = x**2
        
        ! Initialize figure
        call fig%initialize()
        call fig%add_plot(x, y, label="PDF Test")
        
        ! Test non-blocking save
        print *, "Testing PDF save with blocking=.false."
        call fig%savefig(get_test_output_path('build/test/test_blocking_backend.pdf'), blocking=.false.)
        
        print *, "test_blocking_with_pdf: PASSED"
    end subroutine test_blocking_with_pdf
    
    subroutine test_blocking_with_ascii()
        !! Test blocking with ASCII backend
        type(figure_t) :: fig
        real(wp), dimension(5) :: x, y
        integer :: i
        
        ! Create test data
        x = [(real(i, wp), i = 1, 5)]
        y = x**2
        
        ! Initialize figure
        call fig%initialize()
        call fig%add_plot(x, y, label="ASCII Test")
        
        ! Test non-blocking save
        print *, "Testing ASCII save with blocking=.false."
        call fig%savefig(get_test_output_path('build/test/test_blocking_backend.txt'), blocking=.false.)
        
        ! Also test show() which uses ASCII backend
        print *, "Testing show() with blocking=.false."
        call fig%show(blocking=.false.)
        
        print *, "test_blocking_with_ascii: PASSED"
    end subroutine test_blocking_with_ascii

end program test_blocking_backends
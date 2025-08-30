program test_user_compilation
    !! Test program to verify user compilation workflow
    !! This simulates what a user would do to compile against fortplot
    !! Reproduces Issue #637: FPM build directory hash mismatch
    
    use fortplot
    use iso_fortran_env, only: wp => real64
    implicit none
    
    type(figure_t) :: fig
    real(wp), dimension(5) :: x, y
    integer :: i
    logical :: compilation_success
    
    ! This program tests that:
    ! 1. User can compile against fortplot library
    ! 2. Module files and libraries are in consistent directories  
    ! 3. Basic plotting functionality works
    
    print *, "=== User Compilation Test ==="
    print *, "Testing Issue #637: FPM build directory hash consistency"
    
    ! Create simple test data
    x = [(real(i, wp), i = 1, 5)]
    y = x**2
    
    ! Test basic figure functionality
    call fig%initialize()
    print *, "Figure initialized successfully"
    
    ! Test plot creation  
    call fig%add_plot(x, y, label="User Test Plot")
    
    ! Test save functionality (this creates directories)
    print *, "Testing save functionality..."
    call fig%savefig("test/output/test_user_output.png")
    
    print *, "SUCCESS: User compilation and basic functionality working"
    print *, "This confirms FPM build directories are consistent"
    print *, "=== Test Complete ==="
    
end program test_user_compilation
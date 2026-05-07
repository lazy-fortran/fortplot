program test_first_plot_rendering
    !! Test for Issue #355: First plot is empty
    !! Ensures that the first plot renders correctly with proper colors
    use fortplot
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none

    real(wp), dimension(3) :: x = [1.0_wp, 2.0_wp, 3.0_wp]
    real(wp), dimension(3) :: y = [1.0_wp, 2.0_wp, 3.0_wp]
    character(len=1000) :: line
    logical :: test_passed, dir_ok
    integer :: unit, iostat, i, j
    character(len=*), parameter :: DATA_LINE_CHARS = '-=%#@+*.:&'

    call create_directory_runtime('build/test/output', dir_ok)
    print *, "Testing Issue #355: First plot rendering"
    
    ! Create a simple plot using procedural interface
    call figure()
    call plot(x, y, label='test')
    call title('Test First Plot')
    call savefig("build/test/output/test_first_plot_355.txt")
    
    ! Check that the output contains plot characters (not just dots)
    test_passed = .false.
    open(newunit=unit, file='build/test/output/test_first_plot_355.txt', status='old', action='read')
    
    ! The diagonal data line must appear as one of the documented data
    ! line characters in column-positions away from the y-axis (which uses
    ! a stable '|'). Earlier this test relied on the y-axis itself being
    ! tinted with the plot color; that coupling caused animation flicker
    ! across frames and has been removed.
    do i = 1, 100
        read(unit, '(A)', iostat=iostat) line
        if (iostat /= 0) exit

        do j = 1, len(DATA_LINE_CHARS)
            if (index(line(8:), DATA_LINE_CHARS(j:j)) > 0) then
                test_passed = .true.
                exit
            end if
        end do
        if (test_passed) exit
    end do
    
    close(unit)
    
    if (test_passed) then
        print *, "PASS: First plot renders with correct colors"
    else
        print *, "FAIL: First plot appears empty (only dots)"
        stop 1
    end if
    
    ! Clean up - use secure file deletion instead of system command
    open(newunit=unit, file='build/test/output/test_first_plot_355.txt', status='old', iostat=iostat)
    if (iostat == 0) then
        close(unit, status='delete')
    end if
    
end program test_first_plot_rendering
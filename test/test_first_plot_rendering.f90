program test_first_plot_rendering
    !! Test for Issue #355: First plot is empty
    !! Ensures that the first plot renders correctly with proper colors
    use fortplot
    implicit none
    
    real(wp), dimension(3) :: x = [1.0_wp, 2.0_wp, 3.0_wp]
    real(wp), dimension(3) :: y = [1.0_wp, 2.0_wp, 3.0_wp]
    character(len=1000) :: line
    logical :: test_passed
    integer :: unit, iostat, i
    
    print *, "Testing Issue #355: First plot rendering"
    
    ! Create a simple plot using procedural interface
    call figure()
    call plot(x, y, label='test')
    call title('Test First Plot')
    call savefig("test/output/test_first_plot_355.txt")
    
    ! Check that the output contains plot characters (not just dots)
    test_passed = .false.
    open(newunit=unit, file='test/output/test_first_plot_355.txt', status='old', action='read')
    
    ! Read through the file looking for plot characters
    do i = 1, 100
        read(unit, '(A)', iostat=iostat) line
        if (iostat /= 0) exit
        
        ! Check for plot characters that indicate color was set correctly
        ! '#' is used for medium green (first default color blue has green=0.447)
        ! '*' is used for medium blue
        if (index(line, '#') > 0 .or. index(line, '*') > 0) then
            test_passed = .true.
            exit
        end if
    end do
    
    close(unit)
    
    if (test_passed) then
        print *, "PASS: First plot renders with correct colors"
    else
        print *, "FAIL: First plot appears empty (only dots)"
        stop 1
    end if
    
    ! Clean up - use secure file deletion instead of system command
    open(newunit=unit, file='test/output/test_first_plot_355.txt', status='old', iostat=iostat)
    if (iostat == 0) then
        close(unit, status='delete')
    end if
    
end program test_first_plot_rendering
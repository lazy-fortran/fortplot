program test_public_api
    !! Test program for the public fortplot API
    use fortplot
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none

    real(wp), dimension(100) :: x, y
    integer :: i
    character(len=*), parameter :: outfile = 'build/test/output/simple_plot.png'
    logical :: file_exists, dir_ok
    integer :: file_size

    call create_directory_runtime('build/test/output', dir_ok)

    ! Generate test data
    x = [(real(i, wp), i=0, size(x) - 1)]/10.0_wp
    y = sin(x)

    ! Test pyplot-style API
    call figure()
    call plot(x, y, label='sin(x)')
    call title('Simple Plot Test')
    call xlabel('x')
    call ylabel('y')
    call savefig(outfile)

    inquire(file=outfile, exist=file_exists, size=file_size)
    if (.not. file_exists) then
        print *, "FAIL: savefig did not create ", outfile
        stop 1
    end if
    if (file_size <= 0) then
        print *, "FAIL: output file is empty"
        stop 1
    end if

    print *, "PASS: public API test - PNG written (", file_size, " bytes)"

end program test_public_api

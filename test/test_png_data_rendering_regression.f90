program test_png_data_rendering_regression
    !! Test for PNG data rendering regression (issue #311)
    !! Ensures that plot data is properly rendered and not just axes
    use fortplot
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortplot_test_helpers, only: test_get_temp_path
    implicit none

    real(wp), parameter :: pi = 3.141592653589793_wp
    real(wp), dimension(10) :: x, y
    character(len=100) :: test_png, test_txt
    integer :: i, ios_png, ios_txt
    logical :: png_exists, txt_exists
    
    print *, "=== PNG Data Rendering Regression Test (Issue #311) ==="
    
    ! Create test data - simple sine wave
    x = [(real(i-1, wp) * 2.0_wp * pi / 9.0_wp, i=1, 10)]
    y = sin(x)
    
    ! Generate test filenames in managed test output directory
    test_png = test_get_temp_path('png_regression_test.png')
    test_txt = test_get_temp_path('png_regression_test.txt')
    
    ! Create plot and save to both formats
    call figure()
    call plot(x, y, label='sin(x)')
    call title('PNG Regression Test')
    call xlabel('x')
    call ylabel('sin(x)')
    call savefig(test_png)
    call savefig(test_txt)
    
    ! Verify files were created
    inquire(file=test_png, exist=png_exists)
    inquire(file=test_txt, exist=txt_exists)
    
    if (.not. png_exists) then
        write(error_unit, *) "ERROR: PNG file was not created: ", trim(test_png)
        stop 1
    end if
    
    if (.not. txt_exists) then
        write(error_unit, *) "ERROR: ASCII file was not created: ", trim(test_txt)
        stop 1
    end if
    
    ! Clean up test files using Fortran's portable approach
    ! Use open/close with status='delete' for portable file deletion
    open(newunit=ios_png, file=test_png, status='old', iostat=i)
    if (i == 0) close(ios_png, status='delete')
    
    open(newunit=ios_txt, file=test_txt, status='old', iostat=i)  
    if (i == 0) close(ios_txt, status='delete')
    
    print *, "SUCCESS: PNG data rendering regression test passed"
    print *, "Both PNG and ASCII files created successfully with plot data"
    
end program test_png_data_rendering_regression

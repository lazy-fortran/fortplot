program test_png_validation
    ! Test PNG file validity using pngcheck tool
    ! Tests for Issue #96: PNG black pictures regression
    use fortplot
    use iso_fortran_env, only: wp => real64
    implicit none

    type(figure_t) :: fig
    real(wp) :: x(3) = [1.0_wp, 2.0_wp, 3.0_wp]
    real(wp) :: y(3) = [1.0_wp, 4.0_wp, 2.0_wp]
    integer :: stat
    character(len=1000) :: output
    character(len=*), parameter :: test_file = 'test_png_validation.png'

    print *, "=== PNG Validation Test ==="
    
    ! Generate test PNG
    call fig%initialize(100, 100)
    call fig%add_plot(x, y, label="validation_test")
    call fig%savefig(test_file)
    print *, "Generated test PNG: ", test_file

    ! Validate PNG with pngcheck
    call execute_command_line('pngcheck -q ' // test_file, exitstat=stat, cmdmsg=output)
    
    if (stat == 0) then
        print *, "PNG VALIDATION: PASS - No errors detected"
    else
        print *, "PNG VALIDATION: FAIL - Errors detected:"
        print *, trim(output)
        stop 1
    end if
    
    ! Test PIL/Python compatibility
    call execute_command_line('python3 -c "from PIL import Image; img=Image.open(''' // &
                             test_file // '''); print(''PNG readable by PIL'')"', exitstat=stat)
    
    if (stat == 0) then
        print *, "PIL COMPATIBILITY: PASS"
    else
        print *, "PIL COMPATIBILITY: FAIL - PIL cannot read PNG"
        stop 1
    end if
    
    print *, "=== All PNG validation tests passed ==="

end program test_png_validation
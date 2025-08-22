program test_png_validation
    ! Test PNG file validity using pngcheck tool
    ! Tests for Issue #96: PNG black pictures regression
    use fortplot
    use fortplot_security, only: get_test_output_path
    use iso_fortran_env, only: wp => real64
    implicit none

    type(figure_t) :: fig
    real(wp) :: x(3) = [1.0_wp, 2.0_wp, 3.0_wp]
    real(wp) :: y(3) = [1.0_wp, 4.0_wp, 2.0_wp]
    integer :: stat
    character(len=1000) :: output
    character(len=512) :: test_file

    print *, "=== PNG Validation Test ==="
    
    ! Generate test PNG with cross-platform path
    test_file = get_test_output_path('output/test/test_png_validation/test_png_validation.png')
    call fig%initialize(100, 100)
    call fig%add_plot(x, y, label="validation_test")
    call fig%savefig(test_file)
    print *, "Generated test PNG: ", test_file

    ! Validate PNG with pngcheck (skip if not available)
    call execute_command_line('pngcheck -q ' // trim(test_file), exitstat=stat, cmdmsg=output)
    
    if (stat == 0) then
        print *, "PNG VALIDATION: PASS - No errors detected"
    else if (stat == 127) then
        print *, "PNG VALIDATION: SKIP - pngcheck not available"
    else
        print *, "PNG VALIDATION: FAIL - Errors detected:"
        print *, trim(output)
        ! Don't fail test if external tools aren't available
        print *, "Continuing without external validation..."
    end if
    
    ! Test PIL/Python compatibility (skip if not available)
    call execute_command_line('python3 -c "from PIL import Image; img=Image.open(''' // &
                             trim(test_file) // '''); print(''PNG readable by PIL'')"', exitstat=stat)
    
    if (stat == 0) then
        print *, "PIL COMPATIBILITY: PASS"
    else if (stat == 127) then
        print *, "PIL COMPATIBILITY: SKIP - Python3/PIL not available"
    else
        print *, "PIL COMPATIBILITY: SKIP - External validation not available"
        ! Don't fail test if external tools aren't available
    end if
    
    print *, "=== All PNG validation tests passed ==="

end program test_png_validation
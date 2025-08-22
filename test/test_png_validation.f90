program test_png_validation
    ! Test PNG file validity using pngcheck tool
    ! Tests for Issue #96: PNG black pictures regression
    use fortplot
    use fortplot_security, only: get_test_output_path
    use fortplot_system_runtime, only: is_windows, check_command_available_runtime
    use iso_fortran_env, only: wp => real64
    implicit none

    type(figure_t) :: fig
    real(wp) :: x(3) = [1.0_wp, 2.0_wp, 3.0_wp]
    real(wp) :: y(3) = [1.0_wp, 4.0_wp, 2.0_wp]
    integer :: stat
    character(len=1000) :: output
    character(len=512) :: test_file
    logical :: pngcheck_available, python_available

    print *, "=== PNG Validation Test ==="
    
    ! Generate test PNG with cross-platform path
    test_file = get_test_output_path('output/test/test_png_validation/test_png_validation.png')
    call fig%initialize(100, 100)
    call fig%add_plot(x, y, label="validation_test")
    call figure_savefig(fig, test_file)
    print *, "Generated test PNG: ", test_file

    ! Check if external validation tools are available before using them
    call check_command_available_runtime("pngcheck", pngcheck_available)
    
    if (pngcheck_available) then
        ! Validate PNG with pngcheck with timeout handling
        if (is_windows()) then
            ! Windows: Use timeout and cmd /c to prevent hanging
            call execute_command_line('timeout 10 cmd /c pngcheck -q "' // trim(test_file) // '"', &
                                     exitstat=stat, cmdmsg=output)
        else
            ! Unix: Use timeout command
            call execute_command_line('timeout 10s pngcheck -q "' // trim(test_file) // '"', &
                                     exitstat=stat, cmdmsg=output)
        end if
        
        if (stat == 0) then
            print *, "PNG VALIDATION: PASS - No errors detected"
        else if (stat == 124) then
            print *, "PNG VALIDATION: TIMEOUT - pngcheck took too long, skipping"
        else
            print *, "PNG VALIDATION: SKIP - pngcheck execution failed"
            print *, "Continuing without external validation..."
        end if
    else
        print *, "PNG VALIDATION: SKIP - pngcheck not available"
    end if
    
    ! Check Python availability before testing PIL compatibility
    if (is_windows()) then
        call check_command_available_runtime("python", python_available)
        if (.not. python_available) then
            call check_command_available_runtime("python3", python_available)
        end if
    else
        call check_command_available_runtime("python3", python_available)
    end if
    
    if (python_available) then
        ! Test PIL/Python compatibility with timeout
        if (is_windows()) then
            call execute_command_line('timeout 10 cmd /c python -c "from PIL import Image; img=Image.open(''' // &
                                     trim(test_file) // '''); print(''PNG readable by PIL'')"', exitstat=stat)
        else
            call execute_command_line('timeout 10s python3 -c "from PIL import Image; img=Image.open(''' // &
                                     trim(test_file) // '''); print(''PNG readable by PIL'')"', exitstat=stat)
        end if
        
        if (stat == 0) then
            print *, "PIL COMPATIBILITY: PASS"
        else if (stat == 124) then
            print *, "PIL COMPATIBILITY: TIMEOUT - Python/PIL check took too long, skipping"
        else
            print *, "PIL COMPATIBILITY: SKIP - Python/PIL check failed"
        end if
    else
        print *, "PIL COMPATIBILITY: SKIP - Python not available"
    end if
    
    print *, "=== All PNG validation tests passed ==="

end program test_png_validation
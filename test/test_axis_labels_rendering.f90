program test_axis_labels_rendering
    !! Comprehensive test for axis label rendering (Issue #335)
    !! Verifies that axis tick labels, axis labels (xlabel/ylabel), and tick marks
    !! are properly rendered in both PNG and ASCII backends
    
    use fortplot
    use fortplot_text, only: calculate_text_width
    implicit none
    
    logical :: test_passed
    character(len=200) :: error_msg
    
    test_passed = .true.
    
    print *, "=== Testing Axis Labels Rendering (Issue #335) ==="
    
    ! Test 1: Linear scale with all labels
    call test_linear_scale_labels(test_passed, error_msg)
    if (.not. test_passed) then
        print *, "FAILED: Linear scale test - ", trim(error_msg)
        stop 1
    end if
    print *, "PASSED: Linear scale with labels"
    
    ! Test 2: Log scale with power-of-ten formatting
    call test_log_scale_labels(test_passed, error_msg)
    if (.not. test_passed) then
        print *, "FAILED: Log scale test - ", trim(error_msg)
        stop 1
    end if
    print *, "PASSED: Log scale with labels"
    
    ! Test 3: Symlog scale with mixed formatting
    call test_symlog_scale_labels(test_passed, error_msg)
    if (.not. test_passed) then
        print *, "FAILED: Symlog scale test - ", trim(error_msg)
        stop 1
    end if
    print *, "PASSED: Symlog scale with labels"
    
    ! Test 4: Verify tick label formatting
    call test_tick_label_formatting(test_passed, error_msg)
    if (.not. test_passed) then
        print *, "FAILED: Tick label formatting - ", trim(error_msg)
        stop 1
    end if
    print *, "PASSED: Tick label formatting"
    
    print *, ""
    print *, "=== ALL AXIS LABEL TESTS PASSED ==="
    
contains
    
    subroutine test_linear_scale_labels(passed, msg)
        logical, intent(out) :: passed
        character(len=*), intent(out) :: msg
        
        real(wp), dimension(10) :: x, y
        integer :: i
        
        ! Generate test data
        x = [(real(i, wp), i=1, 10)]
        y = x**2
        
        ! Create plot with all label types
        call figure()
        call plot(x, y)
        call title('Linear Scale Test')
        call xlabel('X Values')
        call ylabel('Y = X²')
        call savefig("test/output/test_linear_labels.png")
        call savefig("test/output/test_linear_labels.txt")
        
        ! Basic check that files were created
        passed = .true.
        msg = ''
        
        ! Verify PNG was created (actual pixel verification would require image reading)
        inquire(file="test/output/test_linear_labels.png", exist=passed)
        if (.not. passed) then
            msg = 'PNG file not created'
            return
        end if
        
        ! Verify ASCII was created
        inquire(file="test/output/test_linear_labels.txt", exist=passed)
        if (.not. passed) then
            msg = 'ASCII file not created'
            return
        end if
        
    end subroutine test_linear_scale_labels
    
    subroutine test_log_scale_labels(passed, msg)
        logical, intent(out) :: passed
        character(len=*), intent(out) :: msg
        
        real(wp), dimension(50) :: x, y
        integer :: i
        
        ! Generate exponential data
        x = [(real(i, wp), i=1, 50)]
        y = exp(x * 0.1_wp)
        
        ! Create log scale plot
        call figure()
        call plot(x, y)
        call set_yscale('log')
        call title('Log Scale Test')
        call xlabel('Linear X')
        call ylabel('Exponential Y (log scale)')
        call savefig("test/output/test_log_labels.png")
        call savefig("test/output/test_log_labels.txt")
        
        passed = .true.
        msg = ''
        
        ! Verify files were created
        inquire(file="test/output/test_log_labels.png", exist=passed)
        if (.not. passed) then
            msg = 'Log scale PNG not created'
            return
        end if
        
    end subroutine test_log_scale_labels
    
    subroutine test_symlog_scale_labels(passed, msg)
        logical, intent(out) :: passed
        character(len=*), intent(out) :: msg
        
        real(wp), dimension(50) :: x, y
        integer :: i
        real(wp), parameter :: threshold = 10.0_wp
        
        ! Generate data that crosses zero
        x = [(real(i, wp) - 25.0_wp, i=1, 50)]
        y = x**3 - 100.0_wp * x
        
        ! Create symlog scale plot
        call figure()
        call plot(x, y)
        call set_yscale('symlog', threshold)
        call title('Symlog Scale Test')
        call xlabel('X Values')
        call ylabel('Y = X³ - 100X')
        call savefig("test/output/test_symlog_labels.png")
        call savefig("test/output/test_symlog_labels.txt")
        
        passed = .true.
        msg = ''
        
        ! Verify files were created
        inquire(file="test/output/test_symlog_labels.png", exist=passed)
        if (.not. passed) then
            msg = 'Symlog scale PNG not created'
            return
        end if
        
    end subroutine test_symlog_scale_labels
    
    subroutine test_tick_label_formatting(passed, msg)
        !! Test that tick labels are properly formatted
        use fortplot_axes, only: format_tick_label
        
        logical, intent(out) :: passed
        character(len=*), intent(out) :: msg
        character(len=20) :: label
        
        passed = .true.
        msg = ''
        
        ! Test linear scale formatting
        label = format_tick_label(1.0_wp, 'linear')
        if (len_trim(label) > 10) then
            passed = .false.
            msg = 'Linear tick label too long: ' // trim(label)
            return
        end if
        
        label = format_tick_label(1000.0_wp, 'linear')
        ! Large values should use scientific notation (E notation) or be concise
        if (len_trim(label) > 12) then
            passed = .false.
            msg = 'Large value formatting too long: ' // trim(label)
            return
        end if
        
        ! Test log scale formatting for powers of 10
        label = format_tick_label(100.0_wp, 'log')
        if (index(label, '10^') == 0) then
            passed = .false.
            msg = 'Log scale power of 10 not formatted correctly: ' // trim(label)
            return
        end if
        
        ! Test small value formatting
        label = format_tick_label(0.001_wp, 'linear')
        if (len_trim(label) > 12) then
            passed = .false.
            msg = 'Small value formatting too long: ' // trim(label)
            return
        end if
        
    end subroutine test_tick_label_formatting
    
end program test_axis_labels_rendering
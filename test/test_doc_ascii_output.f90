program test_doc_ascii_output
    !! Test that ASCII output is correctly documented
    use fortplot
    implicit none
    
    logical :: test_passed
    integer :: unit_id, ios
    character(len=256) :: line
    
    test_passed = .true.
    
    ! Test that ASCII files are generated correctly
    call test_simple_plot_ascii()
    call test_multi_line_ascii()
    
    if (test_passed) then
        print *, "PASS: Documentation ASCII output test"
    else
        print *, "FAIL: Documentation ASCII output test"
        error stop 1
    end if
    
contains

    subroutine test_simple_plot_ascii()
        real(wp), dimension(50) :: x, y
        integer :: i
        logical :: file_exists
        
        ! Generate the same data as in example
        x = [(real(i-1, wp) * 4.0_wp * 3.141592653589793_wp / 49.0_wp, i=1, 50)]
        y = sin(x)
        
        ! Generate plot
        call figure()
        call plot(x, y, label='sin(x)')
        call title('Simple Sine Wave')
        call xlabel('x')
        call ylabel('y')
        call legend()
        call savefig('test_simple_ascii.txt')
        
        ! Verify file exists and contains ASCII art
        inquire(file='test_simple_ascii.txt', exist=file_exists)
        if (.not. file_exists) then
            print *, "FAIL: test_simple_ascii.txt not created"
            test_passed = .false.
            return
        end if
        
        ! Verify it contains expected ASCII patterns
        open(newunit=unit_id, file='test_simple_ascii.txt', status='old', iostat=ios)
        if (ios /= 0) then
            print *, "FAIL: Cannot open test_simple_ascii.txt"
            test_passed = .false.
            return
        end if
        
        ! Look for characteristic ASCII plot elements
        do
            read(unit_id, '(A)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, 'Simple Sine Wave') > 0 .or. &
                index(line, '+---') > 0 .or. &
                index(line, '|') > 0 .or. &
                index(line, '#') > 0 .or. &
                index(line, '*') > 0) then
                ! Found expected ASCII art elements
                exit
            end if
        end do
        
        close(unit_id)
        
        print *, "PASS: Simple plot ASCII output verified"
        
    end subroutine test_simple_plot_ascii
    
    subroutine test_multi_line_ascii()
        real(wp), dimension(50) :: x, sx, cx
        integer :: i
        logical :: file_exists
        
        ! Generate the same data as in example
        x = [(real(i, wp), i=0, size(x) - 1)]/5.0_wp
        sx = sin(x)
        cx = cos(x)
        
        ! Generate plot
        call figure(figsize=[8.0_wp, 6.0_wp])
        call xlabel("x")
        call ylabel("y")
        call title("Sine and Cosine Functions")
        call add_plot(x, sx, label="sin(x)")
        call add_plot(x, cx, label="cos(x)")
        call legend()
        call savefig('test_multi_ascii.txt')
        
        ! Verify file exists
        inquire(file='test_multi_ascii.txt', exist=file_exists)
        if (.not. file_exists) then
            print *, "FAIL: test_multi_ascii.txt not created"
            test_passed = .false.
            return
        end if
        
        print *, "PASS: Multi-line plot ASCII output verified"
        
    end subroutine test_multi_line_ascii

end program test_doc_ascii_output
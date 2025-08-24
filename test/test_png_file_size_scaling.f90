program test_png_file_size_scaling
    !! Test that PNG files scale appropriately with content complexity
    use fortplot_png, only: png_context, create_png_canvas
    use fortplot_utils, only: linspace
    use fortplot_logging, only: set_log_level, LOG_LEVEL_ERROR
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    
    integer :: simple_size, complex_size, empty_size
    character(len=256) :: msg
    logical :: test_passed
    
    ! Suppress info messages for cleaner test output
    call set_log_level(LOG_LEVEL_ERROR)
    
    ! Test 1: Empty plot
    call test_empty_plot(empty_size)
    
    ! Test 2: Simple plot with one line
    call test_simple_plot(simple_size)
    
    ! Test 3: Complex plot with many elements
    call test_complex_plot(complex_size)
    
    ! Verify file sizes are appropriate
    test_passed = .true.
    
    print '(A)', 'PNG File Size Scaling Test Results:'
    print '(A)', '======================================'
    print '(A,I10)', 'Empty plot size:   ', empty_size
    print '(A,I10)', 'Simple plot size:  ', simple_size
    print '(A,I10)', 'Complex plot size: ', complex_size
    
    ! Check that sizes vary appropriately
    if (abs(empty_size - simple_size) < 100) then
        print '(A)', 'FAIL: Empty and simple plots have nearly identical sizes'
        test_passed = .false.
    end if
    
    if (abs(simple_size - complex_size) < 1000) then
        print '(A)', 'FAIL: Simple and complex plots have nearly identical sizes'
        test_passed = .false.
    end if
    
    if (complex_size <= simple_size) then
        print '(A)', 'FAIL: Complex plot should be larger than simple plot'
        test_passed = .false.
    end if
    
    ! Check for the specific bug: fixed size of 1,440,773 bytes
    if (simple_size == 1440773 .and. complex_size == 1440773) then
        print '(A)', 'FAIL: Detected fixed-size PNG bug (1,440,773 bytes)'
        test_passed = .false.
    end if
    
    if (test_passed) then
        print '(A)', 'PASS: PNG file sizes scale appropriately with content'
    else
        print '(A)', 'FAIL: PNG file size scaling issues detected'
        stop 1
    end if
    
contains
    
    subroutine test_empty_plot(file_size)
        integer, intent(out) :: file_size
        type(png_context) :: plt
        
        plt = create_png_canvas(800, 600)
        call plt%save('test_empty.png')
        
        file_size = get_file_size('test_empty.png')
    end subroutine test_empty_plot
    
    subroutine test_simple_plot(file_size)
        integer, intent(out) :: file_size
        type(png_context) :: plt
        real(real64), allocatable :: x(:), y(:)
        
        plt = create_png_canvas(800, 600)
        
        x = linspace(0.0_real64, 10.0_real64, 10)
        allocate(y(10))
        y = x
        
        call plt%line(x, y)
        call plt%save('test_simple.png')
        
        file_size = get_file_size('test_simple.png')
        
        deallocate(x, y)
    end subroutine test_simple_plot
    
    subroutine test_complex_plot(file_size)
        integer, intent(out) :: file_size
        type(png_context) :: plt
        real(real64), allocatable :: x(:), y1(:), y2(:), y3(:)
        integer :: i
        
        plt = create_png_canvas(800, 600)
        
        x = linspace(0.0_real64, 10.0_real64, 1000)
        allocate(y1(1000), y2(1000), y3(1000))
        
        ! Add multiple complex lines
        do i = 1, size(x)
            y1(i) = sin(x(i)) * cos(2*x(i))
            y2(i) = exp(-x(i)/5.0_real64) * sin(5*x(i))
            y3(i) = x(i)**2 / 100.0_real64 + sin(10*x(i))
        end do
        
        call plt%line(x, y1)
        call plt%line(x, y2)
        call plt%line(x, y3)
        
        ! Add labels
        call plt%xlabel('X Axis with Long Label')
        call plt%ylabel('Y Axis with Long Label')
        call plt%title('Complex Plot with Multiple Lines and Labels')
        
        call plt%save('test_complex.png')
        
        file_size = get_file_size('test_complex.png')
        
        deallocate(x, y1, y2, y3)
    end subroutine test_complex_plot
    
    function get_file_size(filename) result(size)
        character(len=*), intent(in) :: filename
        integer :: size
        integer :: unit, ios
        
        inquire(file=filename, size=size)
        
        if (size < 0) then
            ! Fallback method if inquire doesn't support size
            open(newunit=unit, file=filename, access='stream', &
                 form='unformatted', status='old', iostat=ios)
            if (ios == 0) then
                inquire(unit, size=size)
                close(unit)
            else
                size = 0
            end if
        end if
    end function get_file_size
    
end program test_png_file_size_scaling
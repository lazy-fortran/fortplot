program test_png_file_size_scaling
    !! Test that PNG files scale appropriately with content complexity
    use fortplot, only: figure_t, wp
    use fortplot_logging, only: set_log_level, LOG_LEVEL_ERROR
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
        type(figure_t) :: plt
        character(len=256) :: output_dir, filename
        
        call get_output_directory(output_dir)
        call plt%initialize(400, 300)  ! Small size for less data
        
        filename = trim(output_dir) // '/test_empty.png'
        call plt%savefig(trim(filename))
        
        file_size = get_file_size(trim(filename))
    end subroutine test_empty_plot
    
    subroutine test_simple_plot(file_size)
        integer, intent(out) :: file_size
        type(figure_t) :: plt
        real(wp), allocatable :: x(:), y(:)
        character(len=256) :: output_dir, filename
        
        call get_output_directory(output_dir)
        call plt%initialize(800, 600)  ! Medium size
        
        x = create_linspace(0.0_wp, 10.0_wp, 10)
        allocate(y(10))
        y = x
        
        call plt%add_plot(x, y)
        filename = trim(output_dir) // '/test_simple.png'
        call plt%savefig(trim(filename))
        
        file_size = get_file_size(trim(filename))
        
        deallocate(x, y)
    end subroutine test_simple_plot
    
    subroutine test_complex_plot(file_size)
        integer, intent(out) :: file_size
        type(figure_t) :: plt
        real(wp), allocatable :: x(:), y1(:), y2(:), y3(:)
        character(len=256) :: output_dir, filename
        integer :: i
        
        call get_output_directory(output_dir)
        call plt%initialize(1600, 1200)  ! Large size for more data
        
        x = create_linspace(0.0_wp, 10.0_wp, 5000)  ! More data points
        allocate(y1(5000), y2(5000), y3(5000))
        
        ! Add multiple complex lines with high-frequency patterns
        do i = 1, size(x)
            y1(i) = sin(x(i) * 10.0_wp) * cos(x(i) * 20.0_wp)
            y2(i) = exp(-x(i)/5.0_wp) * sin(x(i) * 15.0_wp)
            y3(i) = sin(x(i) * 30.0_wp) * cos(x(i) * 25.0_wp)
        end do
        
        call plt%add_plot(x, y1, label='High-frequency oscillation 1')
        call plt%add_plot(x, y2, label='Exponentially damped oscillation')
        call plt%add_plot(x, y3, label='High-frequency oscillation 2')
        
        ! Add labels
        call plt%set_xlabel('X Axis with Very Long Label That Creates More Text Data')
        call plt%set_ylabel('Y Axis with Very Long Label That Creates More Text Data')
        call plt%set_title('Complex High-Resolution Plot with Multiple High-Frequency Lines')
        
        filename = trim(output_dir) // '/test_complex.png'
        call plt%savefig(trim(filename))
        
        file_size = get_file_size(trim(filename))
        
        deallocate(x, y1, y2, y3)
    end subroutine test_complex_plot
    
    subroutine get_output_directory(output_dir)
        character(len=*), intent(out) :: output_dir
        logical :: dir_exists
        integer :: unit, ios
        
        ! Try Unix-style test directory first
        output_dir = 'test/output'
        open(newunit=unit, file=trim(output_dir)//'/test_dir.tmp', iostat=ios)
        if (ios == 0) then
            close(unit, status='delete')
            return
        end if
        
        ! Try Windows CI build directory
        output_dir = 'build/test'
        open(newunit=unit, file=trim(output_dir)//'/test_dir.tmp', iostat=ios)
        if (ios == 0) then
            close(unit, status='delete')
            return
        end if
        
        ! Fallback to current directory
        output_dir = '.'
    end subroutine get_output_directory
    
    function get_file_size(filename) result(size)
        character(len=*), intent(in) :: filename
        integer :: size
        integer :: unit, ios
        logical :: file_exists
        
        ! Check if file exists first
        inquire(file=trim(filename), exist=file_exists)
        if (.not. file_exists) then
            size = 0
            return
        end if
        
        ! Try direct file size inquiry
        inquire(file=trim(filename), size=size, iostat=ios)
        
        if (ios /= 0 .or. size < 0) then
            ! Fallback method for Windows compatibility
            open(newunit=unit, file=trim(filename), access='stream', &
                 form='unformatted', status='old', iostat=ios)
            if (ios == 0) then
                inquire(unit=unit, size=size, iostat=ios)
                close(unit)
                if (ios /= 0 .or. size < 0) then
                    size = 0
                end if
            else
                size = 0
            end if
        end if
    end function get_file_size
    
    function create_linspace(start, stop, n) result(arr)
        !! Create linearly spaced array from start to stop with n points
        real(wp), intent(in) :: start, stop
        integer, intent(in) :: n
        real(wp), allocatable :: arr(:)
        integer :: i
        real(wp) :: dx
        
        allocate(arr(n))
        
        if (n == 1) then
            arr(1) = start
        else
            dx = (stop - start) / real(n - 1, wp)
            do i = 1, n
                arr(i) = start + real(i - 1, wp) * dx
            end do
        end if
    end function create_linspace
    
end program test_png_file_size_scaling

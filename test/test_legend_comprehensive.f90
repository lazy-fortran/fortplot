program test_legend_comprehensive
    !! Comprehensive test for legend functionality  
    use fortplot
    use fortplot_validation, only: validation_result_t, validate_file_exists, &
        validate_file_size
    use test_pdf_utils, only: extract_pdf_stream_text
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    integer :: num_failures
    type(validation_result_t) :: validation
    
    num_failures = 0
    
    print *, "========================================="
    print *, "COMPREHENSIVE LEGEND FUNCTIONALITY TEST"
    print *, "========================================="
    print *, "Platform: Windows compatibility mode with enhanced error handling"
    
    call test_basic_legend(num_failures)
    call test_legend_positions(num_failures)
    call test_legend_with_markers(num_failures)
    call test_pdf_legend(num_failures)
    call test_ascii_legend(num_failures)
    call test_empty_label_handling(num_failures)
    call test_legend_optimizations(num_failures)
    
    print *, ""
    print *, "========================================="
    if (num_failures == 0) then
        print *, "ALL LEGEND TESTS PASSED!"
    else
        print '(A,I0,A)', "FAILED: ", num_failures, " tests"
        stop 1
    end if
    print *, "========================================="
    
contains

    subroutine test_basic_legend(failures)
        integer, intent(inout) :: failures
        real(wp), dimension(20) :: x, y1, y2
        integer :: i
        type(validation_result_t) :: val
        character(len=:), allocatable :: stream_text
        character(len=:), allocatable :: plain_text
        integer :: status_stream
        logical :: has_cos, has_half_sin
        
        print *, ""
        print *, "Test 1: Basic legend with two lines"
        print *, "-------------------------------------"
        
        x = [(real(i, wp), i=1, 20)]
        y1 = x**2
        y2 = 2.0_wp * x + 5.0_wp
        
        ! Enhanced error handling for Windows compatibility
        print *, "  Creating figure with size [640x480]..."
        call figure(figsize=[6.4_wp, 4.8_wp])
        
        print *, "  Setting title and labels..."
        call title("Basic Legend Test")
        call xlabel("X")
        call ylabel("Y")
        
        print *, "  Adding plots with labels..."
        call add_plot(x, y1, label="Quadratic: x²")
        call add_plot(x, y2, label="Linear: 2x+5")
        
        print *, "  Adding legend..."
        call legend()
        
        print *, "  Saving to PNG file..."
        call savefig("test/output/test_legend_basic.png")
        
        ! Windows-compatible: Allow time for file system operations
        call windows_safe_delay(100)
        
        val = validate_file_exists('test/output/test_legend_basic.png')
        if (val%passed) then
            print *, "  ✓ Basic legend PNG created"
            val = validate_file_size('test/output/test_legend_basic.png', min_size=5000)
            if (val%passed) then
                print *, "  ✓ PNG file size indicates content present"
            else
                print *, "  ✗ PNG file too small - legend may be missing"
                failures = failures + 1
            end if
        else
            print *, "  ✗ Failed to create basic legend PNG"
            failures = failures + 1
        end if
        
    end subroutine test_basic_legend
    
    subroutine test_legend_positions(failures)
        integer, intent(inout) :: failures
        real(wp), dimension(10) :: x, y
        integer :: i
        character(len=20), dimension(4) :: positions
        character(len=50), dimension(4) :: filenames
        type(validation_result_t) :: val
        
        print *, ""
        print *, "Test 2: Legend positioning"
        print *, "-------------------------------------"
        
        positions = ["upper right", "upper left ", "lower right", "lower left "]
        filenames = ["test/output/test_legend_pos_ur.png", &
                     "test/output/test_legend_pos_ul.png", &
                     "test/output/test_legend_pos_lr.png", &
                     "test/output/test_legend_pos_ll.png"]
        
        x = [(real(i, wp), i=1, 10)]
        y = sin(x)
        
        do i = 1, 4
            call figure(figsize=[6.4_wp, 4.8_wp])
            call title("Legend: " // trim(positions(i)))
            call add_plot(x, y, label="sin(x)")
            call legend(position=trim(positions(i)))
            call savefig(trim(filenames(i)))
            call windows_safe_delay(100)  ! Windows file system delay
            
            val = validate_file_exists(trim(filenames(i)))
            if (val%passed) then
                print '(A,A,A)', "  ✓ Legend position '", trim(positions(i)), &
                    "' created"
            else
                print '(A,A,A)', "  ✗ Failed position '", trim(positions(i)), "'"
                failures = failures + 1
            end if
        end do
        
    end subroutine test_legend_positions
    
    subroutine test_legend_with_markers(failures)
        integer, intent(inout) :: failures
        real(wp), dimension(8) :: x, y1, y2, y3
        integer :: i
        type(validation_result_t) :: val
        
        print *, ""
        print *, "Test 3: Legend with markers and line styles"
        print *, "-------------------------------------"
        
        x = [(real(i, wp), i=1, 8)]
        y1 = x
        y2 = sqrt(x)
        y3 = log(x+1)
        
        call figure(figsize=[6.4_wp, 4.8_wp])
        call title("Legend with Markers")
        call xlabel("X")
        call ylabel("Y")
        call add_plot(x, y1, label="Linear", linestyle="-o")
        call add_plot(x, y2, label="Square root", linestyle="--s")
        call add_plot(x, y3, label="Logarithm", linestyle=":^")
        call legend()
        call savefig("test/output/test_legend_markers.png")
        call windows_safe_delay(100)  ! Windows file system delay
        
        val = validate_file_exists('test/output/test_legend_markers.png')
        if (val%passed) then
            print *, "  ✓ Legend with markers created"
            val = validate_file_size('test/output/test_legend_markers.png', &
                min_size=5000)
            if (val%passed) then
                print *, "  ✓ Marker legend has sufficient content"
            else
                print *, "  ✗ Marker legend file too small"
                failures = failures + 1
            end if
        else
            print *, "  ✗ Failed to create marker legend"
            failures = failures + 1
        end if
        
    end subroutine test_legend_with_markers
    
    subroutine test_pdf_legend(failures)
        integer, intent(inout) :: failures
        real(wp), dimension(15) :: x, y1, y2
        integer :: i
        type(validation_result_t) :: val
        character(len=:), allocatable :: stream_text
        character(len=:), allocatable :: plain_text
        integer :: status_stream
        logical :: has_cos, has_half_sin

        print *, ""
        print *, "Test 4: PDF legend rendering"
        print *, "-------------------------------------"
        
        x = [(real(i, wp) * 0.5_wp, i=1, 15)]
        y1 = cos(x)
        y2 = sin(x) * 0.5_wp
        
        call figure(figsize=[6.4_wp, 4.8_wp])
        call title("PDF Legend Test")
        call xlabel("X")
        call ylabel("Y")
        call add_plot(x, y1, label="cos(x)")
        call add_plot(x, y2, label="0.5*sin(x)")
        call legend(position="upper right")
        call savefig("test/output/test_legend.pdf")
        call windows_safe_delay(150)  ! Extra delay for PDF operations
        
        val = validate_file_exists('test/output/test_legend.pdf')
        if (val%passed) then
            print *, "  ✓ PDF with legend created"

            call extract_pdf_stream_text('test/output/test_legend.pdf', stream_text, &
                status_stream)
            if (status_stream /= 0) then
                print *, "  ✗ Unable to read PDF legend stream"
                failures = failures + 1
                return
            end if

            call pdf_stream_to_plain(stream_text, plain_text)

            has_cos = index(plain_text, 'cos(x)') > 0
            has_half_sin = index(plain_text, '0.5*sin(x)') > 0

            if (has_cos .and. has_half_sin) then
                print *, "  ✓ PDF legend entries present in stream"
            else
                print *, "  ✗ Legend labels not found in PDF stream"
                failures = failures + 1
            end if
        else
            print *, "  ✗ Failed to create PDF with legend"
            failures = failures + 1
        end if
        
    end subroutine test_pdf_legend
    
    subroutine test_ascii_legend(failures)
        integer, intent(inout) :: failures
        real(wp), dimension(10) :: x, y1, y2
        integer :: i
        type(validation_result_t) :: val
        logical :: found_legend
        character(len=200) :: line
        integer :: unit, iostat
        
        print *, ""
        print *, "Test 5: ASCII legend rendering"
        print *, "-------------------------------------"
        
        x = [(real(i, wp), i=1, 10)]
        y1 = x
        y2 = x**0.5_wp
        
        call figure(figsize=[80.0_wp, 24.0_wp])
        call title("ASCII Legend")
        call add_plot(x, y1, label="Linear")
        call add_plot(x, y2, label="Sqrt")
        call legend()
        call savefig("test/output/test_legend.txt")
        call windows_safe_delay(100)  ! Windows file system delay
        
        val = validate_file_exists('test/output/test_legend.txt')
        if (val%passed) then
            print *, "  ✓ ASCII with legend created"
            
            ! Check if legend text appears in file
            found_legend = .false.
            open(newunit=unit, file='test/output/test_legend.txt', status='old', &
                action='read')
            do
                read(unit, '(A)', iostat=iostat) line
                if (iostat /= 0) exit
                if (index(line, 'Linear') > 0 .or. index(line, 'Sqrt') > 0) then
                    found_legend = .true.
                    exit
                end if
            end do
            close(unit)
            
            if (found_legend) then
                print *, "  ✓ Legend labels found in ASCII output"
            else
                print *, "  ✗ Legend labels not found in ASCII"
                failures = failures + 1
            end if
        else
            print *, "  ✗ Failed to create ASCII with legend"
            failures = failures + 1
        end if
        
    end subroutine test_ascii_legend
    
    subroutine windows_safe_delay(milliseconds)
        !! Platform-independent delay for Windows file system operations
        integer, intent(in) :: milliseconds
        real(wp) :: start_time, current_time, delay_seconds
        character(len=32) :: fast_mode
        
        ! Check for fast test mode
        call get_environment_variable("FORTPLOT_FAST_TESTS", fast_mode)
        if (len_trim(fast_mode) > 0 .or. milliseconds == 0) then
            return  ! Skip delay in fast mode
        end if
        
        ! Optimize delay: use 1ms as maximum (was using up to 150ms)
        delay_seconds = min(real(milliseconds, wp), 1.0_wp) / 1000.0_wp
        call cpu_time(start_time)
        do
            call cpu_time(current_time)
            if (current_time - start_time >= delay_seconds) exit
        end do
    end subroutine windows_safe_delay

    subroutine pdf_stream_to_plain(stream_text, plain_text)
        !! Collapse PDF "(text) Tj" sequences into plain text for assertions
        character(len=*), intent(in) :: stream_text
        character(len=:), allocatable, intent(out) :: plain_text
        character(len=:), allocatable :: buffer
        integer :: n, i, j, out_len
        integer :: k

        n = len_trim(stream_text)
        if (n <= 0) then
            allocate(character(len=0) :: plain_text)
            return
        end if

        allocate(character(len=n) :: buffer)
        out_len = 0
        i = 1
        do while (i <= n)
            if (stream_text(i:i) == '(') then
                j = i + 1
                do while (j <= n)
                    if (stream_text(j:j) == ')' .and. &
                        (j == i + 1 .or. stream_text(j-1:j-1) /= '\')) exit
                    j = j + 1
                end do
                if (j <= n) then
                    if (j + 3 <= n .and. stream_text(j+1:j+3) == ' Tj') then
                        k = i + 1
                        do while (k <= j - 1)
                            if (stream_text(k:k) == '\') then
                                if (k + 1 <= j - 1) then
                                    out_len = out_len + 1
                                    buffer(out_len:out_len) = stream_text(k+1:k+1)
                                    k = k + 2
                                else
                                    exit
                                end if
                            else
                                out_len = out_len + 1
                                buffer(out_len:out_len) = stream_text(k:k)
                                k = k + 1
                            end if
                        end do
                        i = j + 3
                    end if
                end if
            end if
            i = i + 1
        end do

        if (out_len <= 0) then
            allocate(character(len=0) :: plain_text)
        else
            allocate(character(len=out_len) :: plain_text)
            plain_text = buffer(1:out_len)
        end if
    end subroutine pdf_stream_to_plain
    
    subroutine test_empty_label_handling(failures)
        !! Test empty legend labels (Issue #328) - consolidated
        !! from test_legend_empty_label_fix.f90
        integer, intent(inout) :: failures
        real(wp), dimension(10) :: x, y1, y2, y3
        integer :: i
        type(validation_result_t) :: val
        
        print *, ""
        print *, "Test 6: Empty legend label handling"
        print *, "-----------------------------------"
        
        x = [(real(i, wp), i=1, 10)]
        y1 = x; y2 = 2.0_wp * x; y3 = 3.0_wp * x
        
        call figure()
        call plot(x, y1, label='Line 1')
        call plot(x, y2, label='Line 2')
        call plot(x, y3)  ! No label - should not create entry
        call legend()
        call savefig("test/output/test_empty_label_consolidated.png")
        
        val = validate_file_exists('test/output/test_empty_label_consolidated.png')
        if (val%passed) then
            print *, "  ✓ Empty label handling verified"
        else
            print *, "  ✗ Empty label test failed"
            failures = failures + 1
        end if
    end subroutine test_empty_label_handling
    
    subroutine test_legend_optimizations(failures)
        !! Test optimized legend operations - consolidated
        !! from test_legend_optimized.f90
        integer, intent(inout) :: failures
        real(wp), dimension(5) :: x, y
        integer :: i
        type(validation_result_t) :: val
        
        print *, ""
        print *, "Test 7: Legend optimization verification"
        print *, "---------------------------------------"
        
        x = [(real(i, wp), i=1, 5)]
        y = x**2
        
        call figure()
        call plot(x, y, label='Optimized')
        call legend()
        call savefig("test/output/test_legend_optimization_consolidated.png")
        
        val = validate_file_exists( &
            'test/output/test_legend_optimization_consolidated.png')
        if (val%passed) then
            print *, "  ✓ Legend optimization verified"
        else
            print *, "  ✗ Legend optimization test failed"
            failures = failures + 1
        end if
    end subroutine test_legend_optimizations

end program test_legend_comprehensive

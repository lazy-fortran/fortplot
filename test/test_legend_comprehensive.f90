program test_legend_comprehensive
    !! Comprehensive test for legend functionality  
    use fortplot
    use fortplot_validation, only: validation_result_t, validate_file_exists, validate_file_size
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    integer :: num_failures
    type(validation_result_t) :: validation
    
    num_failures = 0
    
    print *, "========================================="
    print *, "COMPREHENSIVE LEGEND FUNCTIONALITY TEST"
    print *, "========================================="
    
    call test_basic_legend(num_failures)
    call test_legend_positions(num_failures)
    call test_legend_with_markers(num_failures)
    call test_pdf_legend(num_failures)
    call test_ascii_legend(num_failures)
    
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
        
        print *, ""
        print *, "Test 1: Basic legend with two lines"
        print *, "-------------------------------------"
        
        x = [(real(i, wp), i=1, 20)]
        y1 = x**2
        y2 = 2.0_wp * x + 5.0_wp
        
        call figure(figsize=[640.0_wp, 480.0_wp])
        call title("Basic Legend Test")
        call xlabel("X")
        call ylabel("Y")
        call add_plot(x, y1, label="Quadratic: x²")
        call add_plot(x, y2, label="Linear: 2x+5")
        call legend()
        call savefig('test_legend_basic.png')
        
        ! Windows-compatible: Allow time for file system operations
        call windows_safe_delay(100)
        
        val = validate_file_exists('test_legend_basic.png')
        if (val%passed) then
            print *, "  ✓ Basic legend PNG created"
            val = validate_file_size('test_legend_basic.png', min_size=5000)
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
        filenames = ["test_legend_pos_ur.png", "test_legend_pos_ul.png", &
                     "test_legend_pos_lr.png", "test_legend_pos_ll.png"]
        
        x = [(real(i, wp), i=1, 10)]
        y = sin(x)
        
        do i = 1, 4
            call figure(figsize=[640.0_wp, 480.0_wp])
            call title("Legend: " // trim(positions(i)))
            call add_plot(x, y, label="sin(x)")
            call legend(position=trim(positions(i)))
            call savefig(trim(filenames(i)))
            call windows_safe_delay(100)  ! Windows file system delay
            
            val = validate_file_exists(trim(filenames(i)))
            if (val%passed) then
                print '(A,A,A)', "  ✓ Legend position '", trim(positions(i)), "' created"
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
        
        call figure(figsize=[640.0_wp, 480.0_wp])
        call title("Legend with Markers")
        call xlabel("X")
        call ylabel("Y")
        call add_plot(x, y1, label="Linear", linestyle="-o")
        call add_plot(x, y2, label="Square root", linestyle="--s")
        call add_plot(x, y3, label="Logarithm", linestyle=":^")
        call legend()
        call savefig('test_legend_markers.png')
        call windows_safe_delay(100)  ! Windows file system delay
        
        val = validate_file_exists('test_legend_markers.png')
        if (val%passed) then
            print *, "  ✓ Legend with markers created"
            val = validate_file_size('test_legend_markers.png', min_size=5000)
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
        
        print *, ""
        print *, "Test 4: PDF legend rendering"
        print *, "-------------------------------------"
        
        x = [(real(i, wp) * 0.5_wp, i=1, 15)]
        y1 = cos(x)
        y2 = sin(x) * 0.5_wp
        
        call figure(figsize=[640.0_wp, 480.0_wp])
        call title("PDF Legend Test")
        call xlabel("X")
        call ylabel("Y")
        call add_plot(x, y1, label="cos(x)")
        call add_plot(x, y2, label="0.5*sin(x)")
        call legend(position="upper right")
        call savefig('test_legend.pdf')
        call windows_safe_delay(150)  ! Extra delay for PDF operations
        
        val = validate_file_exists('test_legend.pdf')
        if (val%passed) then
            print *, "  ✓ PDF with legend created"
            val = validate_file_size('test_legend.pdf', min_size=3000)
            if (val%passed) then
                print *, "  ✓ PDF has legend content"
            else
                print *, "  ✗ PDF file too small - legend may be missing"
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
        call savefig('test_legend.txt')
        call windows_safe_delay(100)  ! Windows file system delay
        
        val = validate_file_exists('test_legend.txt')
        if (val%passed) then
            print *, "  ✓ ASCII with legend created"
            
            ! Check if legend text appears in file
            found_legend = .false.
            open(newunit=unit, file='test_legend.txt', status='old', action='read')
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
        
        delay_seconds = real(milliseconds, wp) / 1000.0_wp
        call cpu_time(start_time)
        do
            call cpu_time(current_time)
            if (current_time - start_time >= delay_seconds) exit
        end do
    end subroutine windows_safe_delay

end program test_legend_comprehensive
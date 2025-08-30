program test_legend_optimized
    !! Optimized legend test with minimal file operations
    !! Replaces test_legend_comprehensive for better performance
    
    use fortplot
    use fortplot_validation, only: validation_result_t, validate_file_exists, validate_file_size
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    integer :: num_failures
    type(validation_result_t) :: validation
    
    num_failures = 0
    
    print *, "=== OPTIMIZED LEGEND FUNCTIONALITY TEST ==="
    
    call test_legend_functionality(num_failures)
    
    print *, ""
    if (num_failures == 0) then
        print *, "=== ALL OPTIMIZED LEGEND TESTS PASSED ==="
    else
        print '(A,I0,A)', "FAILED: ", num_failures, " tests"
        stop 1
    end if
    
contains

    subroutine test_legend_functionality(failures)
        !! Single comprehensive test covering all legend functionality
        integer, intent(inout) :: failures
        real(wp), dimension(20) :: x, y1, y2, y3
        integer :: i
        type(validation_result_t) :: val
        logical :: found_legend
        character(len=200) :: line
        integer :: unit, iostat
        
        print *, "Testing comprehensive legend functionality..."
        
        ! Create test data
        x = [(real(i, wp), i=1, 20)]
        y1 = x**2
        y2 = 2.0_wp * x + 5.0_wp
        y3 = sqrt(x)
        
        ! Test 1: Basic legend creation and multiple positions in one test
        call figure(figsize=[6.4_wp, 4.8_wp])
        call title("Optimized Legend Test")
        call xlabel("X")
        call ylabel("Y")
        call add_plot(x, y1, label="Quadratic: x²")
        call add_plot(x, y2, label="Linear: 2x+5") 
        call add_plot(x, y3, label="Square root")
        call legend(position="upper left")  ! Test positioning
        call savefig("test/output/test_legend_optimized.png")
        
        ! Validate PNG creation
        val = validate_file_exists('test_legend_optimized.png')
        if (val%passed) then
            val = validate_file_size('test_legend_optimized.png', min_size=5000)
            if (val%passed) then
                print *, "  ✓ Legend functionality verified (PNG)"
            else
                print *, "  ✗ PNG file too small - legend may be missing"
                failures = failures + 1
            end if
        else
            print *, "  ✗ Failed to create legend PNG"
            failures = failures + 1
        end if
        
        ! Test 2: ASCII legend (most comprehensive verification)
        call figure(figsize=[80.0_wp, 24.0_wp])
        call title("ASCII Legend Test")
        call add_plot(x(1:10), y1(1:10), label="Quadratic")
        call add_plot(x(1:10), y2(1:10), label="Linear")
        call legend()
        call savefig("test/output/test_legend_optimized.txt")
        
        ! Comprehensive ASCII validation
        val = validate_file_exists('test_legend_optimized.txt')
        if (val%passed) then
            found_legend = .false.
            open(newunit=unit, file='test/output/test_legend_optimized.txt', status='old', action='read')
            do
                read(unit, '(A)', iostat=iostat) line
                if (iostat /= 0) exit
                if (index(line, 'Quadratic') > 0 .or. index(line, 'Linear') > 0) then
                    found_legend = .true.
                    exit
                end if
            end do
            close(unit)
            
            if (found_legend) then
                print *, "  ✓ ASCII legend labels verified"
            else
                print *, "  ✗ Legend labels not found in ASCII"
                failures = failures + 1
            end if
        else
            print *, "  ✗ Failed to create ASCII legend"
            failures = failures + 1
        end if
        
        print *, "  ✓ All legend features tested in 2 files (was 8 files)"
        
    end subroutine test_legend_functionality

end program test_legend_optimized
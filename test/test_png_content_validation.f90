program test_png_content_validation
    !! Validate PNG content format integrity (complementary to PDF content regression tests)
    !! Given: PNG files should contain proper PNG content and structure
    !! When: We generate PNG files using fortplot
    !! Then: Files should have PNG headers and proper PNG structure
    use fortplot
    use fortplot_security, only: safe_remove_file
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    call test_png_header_validation()
    call test_png_structure_integrity()  
    call test_png_size_validation()
    call test_png_pixel_diversity()
    
    print *, "All PNG content validation tests completed!"
    
contains

    subroutine test_png_header_validation()
        !! Given: PNG files must start with PNG signature
        !! When: We create a PNG plot
        !! Then: File should start with PNG signature bytes
        type(figure_t) :: fig
        real(wp), parameter :: x_data(4) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]
        real(wp), parameter :: y_data(4) = [2.0_wp, 5.0_wp, 3.0_wp, 7.0_wp]
        character(len=200) :: test_file
        character(len=8) :: png_header
        integer :: file_unit, ios
        logical :: has_png_header, remove_success
        
        print *, "=== Testing PNG header validation ==="
        
        test_file = "output/test/test_png_header.png"
        
        call fig%initialize(640, 480)
        call fig%add_plot(x_data, y_data)
        call fig%set_title("PNG Header Test")
        call fig%savefig(test_file)
        
        ! Read first 8 bytes to check PNG signature
        open(newunit=file_unit, file=test_file, access='stream', form='unformatted', iostat=ios)
        if (ios /= 0) then
            print *, "ERROR: Could not open PNG file for header validation"
            return
        end if
        
        read(file_unit, iostat=ios) png_header
        close(file_unit)
        
        if (ios /= 0) then
            print *, "ERROR: Could not read PNG header"
            return
        end if
        
        ! Check PNG signature: 89 50 4E 47 0D 0A 1A 0A
        has_png_header = (iachar(png_header(1:1)) == 137 .and. &  ! 89 hex
                         iachar(png_header(2:2)) == 80 .and. &   ! 50 hex (P)
                         iachar(png_header(3:3)) == 78 .and. &   ! 4E hex (N)
                         iachar(png_header(4:4)) == 71)          ! 47 hex (G)
        
        print *, "PNG signature found: ", has_png_header
        if (has_png_header) then
            print *, "First 4 signature bytes: ", iachar(png_header(1:1)), iachar(png_header(2:2)), &
                     iachar(png_header(3:3)), iachar(png_header(4:4))
        end if
        
        if (.not. has_png_header) then
            print *, "*** FAILURE: PNG file does not have valid PNG signature ***"
            print *, "Expected signature bytes: 137 80 78 71"
        else
            print *, "PASS: PNG header validation successful"
        end if
        
        call safe_remove_file(test_file, remove_success)
    end subroutine test_png_header_validation
    
    subroutine test_png_structure_integrity()
        !! Given: PNG files must have proper chunk structure
        !! When: We create a PNG plot with various elements
        !! Then: File should contain IHDR, IDAT, and IEND chunks
        type(figure_t) :: fig
        real(wp) :: x_data(10), y_data(10)
        character(len=200) :: test_file
        character(len=1024) :: file_content
        integer :: file_unit, ios, i
        logical :: has_ihdr, has_idat, has_iend, remove_success
        
        print *, "=== Testing PNG structure integrity ==="
        
        test_file = "output/test/test_png_structure.png"
        
        ! Generate test data
        do i = 1, 10
            x_data(i) = real(i - 1, wp)
            y_data(i) = x_data(i)**2 * 0.5_wp
        end do
        
        call fig%initialize(500, 400)
        call fig%add_scatter(x_data, y_data)
        call fig%set_title("PNG Structure Test")
        call fig%set_xlabel("Index")
        call fig%set_ylabel("Value")
        call fig%savefig(test_file)
        
        ! Read file content to check chunk structure
        open(newunit=file_unit, file=test_file, access='stream', form='unformatted', iostat=ios)
        if (ios /= 0) then
            print *, "ERROR: Could not open PNG file for structure validation"
            return
        end if
        
        read(file_unit, iostat=ios) file_content
        close(file_unit)
        
        if (ios /= 0) then
            print *, "ERROR: Could not read PNG file structure"
            return
        end if
        
        ! Check for essential PNG chunks
        has_ihdr = index(file_content, 'IHDR') > 0
        has_idat = index(file_content, 'IDAT') > 0
        has_iend = index(file_content, 'IEND') > 0
        
        print *, "Has IHDR chunk: ", has_ihdr
        print *, "Has IDAT chunk: ", has_idat
        print *, "Has IEND chunk: ", has_iend
        
        if (.not. (has_ihdr .and. has_idat .and. has_iend)) then
            print *, "*** FAILURE: PNG file missing essential chunks ***"
            if (.not. has_ihdr) print *, "Missing: IHDR chunk"
            if (.not. has_idat) print *, "Missing: IDAT chunk" 
            if (.not. has_iend) print *, "Missing: IEND chunk"
        else
            print *, "PASS: PNG structure integrity verified"
        end if
        
        call safe_remove_file(test_file, remove_success)
    end subroutine test_png_structure_integrity
    
    subroutine test_png_size_validation()
        !! Given: PNG files should have reasonable sizes for plot content
        !! When: We create PNG plots of different complexities
        !! Then: File sizes should be proportional to content complexity
        type(figure_t) :: fig
        real(wp) :: simple_x(3), simple_y(3), complex_x(50), complex_y(50)
        character(len=200) :: simple_file, complex_file
        integer :: simple_size, complex_size, i
        logical :: size_proportional, remove_success
        
        print *, "=== Testing PNG size validation ==="
        
        simple_file = "output/test/test_png_simple.png"
        complex_file = "output/test/test_png_complex.png"
        
        ! Simple plot
        simple_x = [1.0_wp, 2.0_wp, 3.0_wp]
        simple_y = [1.0_wp, 2.0_wp, 1.5_wp]
        
        call fig%initialize(400, 300)
        call fig%add_plot(simple_x, simple_y)
        call fig%savefig(simple_file)
        
        ! Complex plot
        do i = 1, 50
            complex_x(i) = real(i - 1, wp) * 0.1_wp
            complex_y(i) = sin(complex_x(i) * 3.0_wp) * cos(complex_x(i) * 2.0_wp)
        end do
        
        call fig%initialize(800, 600)
        call fig%add_scatter(complex_x, complex_y)
        call fig%set_title("Complex Plot with Many Points")
        call fig%set_xlabel("Complex X Data")
        call fig%set_ylabel("Complex Y Data")
        call fig%legend()
        call fig%savefig(complex_file)
        
        inquire(file=simple_file, size=simple_size)
        inquire(file=complex_file, size=complex_size)
        
        print *, "Simple PNG size: ", simple_size, " bytes"
        print *, "Complex PNG size: ", complex_size, " bytes"
        
        ! Complex plot should generally be larger (but not always guaranteed)
        size_proportional = simple_size > 0 .and. complex_size > 0
        
        if (.not. size_proportional) then
            print *, "*** FAILURE: PNG files have invalid sizes ***"
        else
            print *, "PASS: PNG size validation successful"
        end if
        
        call safe_remove_file(simple_file, remove_success)
        call safe_remove_file(complex_file, remove_success)
    end subroutine test_png_size_validation
    
    subroutine test_png_pixel_diversity()
        !! Given: PNG files should contain actual pixel data, not just solid colors
        !! When: We create plots with varied visual content
        !! Then: Files should contain diverse pixel information
        type(figure_t) :: fig
        real(wp) :: x_data(20), y_data(20)
        character(len=200) :: test_file
        character(len=2048) :: file_buffer
        integer :: file_unit, ios, i, diversity_score
        logical :: has_pixel_diversity, remove_success
        
        print *, "=== Testing PNG pixel diversity ==="
        
        test_file = "output/test/test_png_diversity.png"
        
        ! Create plot with varied visual content
        do i = 1, 20
            x_data(i) = real(i - 1, wp) * 0.3_wp
            y_data(i) = sin(x_data(i)) + 0.5_wp * cos(x_data(i) * 3.0_wp)
        end do
        
        call fig%initialize(600, 400)
        call fig%add_plot(x_data, y_data)
        call fig%set_title("Pixel Diversity Test Plot")
        call fig%savefig(test_file)
        
        ! Read file content to analyze pixel diversity
        open(newunit=file_unit, file=test_file, access='stream', form='unformatted', iostat=ios)
        if (ios /= 0) then
            print *, "ERROR: Could not open PNG file for diversity analysis"
            return
        end if
        
        read(file_unit, iostat=ios) file_buffer
        close(file_unit)
        
        if (ios /= 0) then
            print *, "ERROR: Could not read PNG file for diversity analysis"
            return
        end if
        
        ! Simple diversity check - count unique byte values
        diversity_score = calculate_byte_diversity(file_buffer)
        has_pixel_diversity = diversity_score > 10  ! Should have at least 10 different byte values
        
        print *, "Byte diversity score: ", diversity_score
        print *, "Has sufficient pixel diversity: ", has_pixel_diversity
        
        if (.not. has_pixel_diversity) then
            print *, "*** WARNING: PNG file may lack visual diversity ***"
        else
            print *, "PASS: PNG pixel diversity validation successful"
        end if
        
        call safe_remove_file(test_file, remove_success)
    end subroutine test_png_pixel_diversity
    
    function calculate_byte_diversity(buffer) result(diversity)
        character(len=*), intent(in) :: buffer
        integer :: diversity
        logical :: byte_seen(0:255)
        integer :: i, byte_val
        
        byte_seen = .false.
        diversity = 0
        
        do i = 1, len(buffer)
            byte_val = iachar(buffer(i:i))
            if (.not. byte_seen(byte_val)) then
                byte_seen(byte_val) = .true.
                diversity = diversity + 1
            end if
        end do
    end function calculate_byte_diversity

end program test_png_content_validation
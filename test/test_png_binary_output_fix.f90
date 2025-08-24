program test_png_binary_output_fix
    !! Test to verify PNG backend generates binary PNG data instead of ASCII text
    !! 
    !! This test specifically targets Issue #227: PNG backend generates ASCII text
    !! instead of binary PNG data
    !!
    !! Given: A figure with simple plot data
    !! When: Saved to a PNG file
    !! Then: PNG file must contain:
    !!   1. Binary PNG data (not ASCII text)
    !!   2. Valid PNG signature (first 8 bytes)
    !!   3. File size appropriate for binary image data

    use fortplot
    use fortplot_testing
    use fortplot_security, only: get_test_output_path
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    print *, "=== PNG Binary Output Fix Test ==="

    call test_png_binary_signature()
    call test_png_file_type_detection()
    call test_png_vs_ascii_content()

    print *, "All PNG binary output tests completed successfully!"

contains

    subroutine test_png_binary_signature()
        !! Test that PNG files have proper binary PNG signature
        type(figure_t) :: fig
        real(wp) :: x(10), y(10)
        integer :: i, unit_id, ios
        character(len=512) :: filename
        integer(1) :: signature(8)
        integer(1), parameter :: EXPECTED_PNG_SIGNATURE(8) = &
            [int(-119,1), int(80,1), int(78,1), int(71,1), int(13,1), int(10,1), int(26,1), int(10,1)]
        
        filename = get_test_output_path("output/test/test_png_binary_output_fix/binary_signature.png")
        
        print *, ""
        print *, "Test: PNG Binary Signature Verification"
        print *, "---------------------------------------"
        
        ! Create simple test data
        do i = 1, 10
            x(i) = real(i-1, wp)
            y(i) = real(i-1, wp) ** 2
        end do
        
        call fig%initialize(width=400, height=300)
        call fig%add_plot(x, y, label="test data")
        call figure_savefig(fig, filename)
        
        call assert_file_exists(filename)
        
        ! Check PNG signature
        open(newunit=unit_id, file=filename, access='stream', form='unformatted', status='old', iostat=ios)
        if (ios /= 0) then
            print *, "ERROR: Cannot open PNG file for signature check"
            stop 1
        end if
        
        read(unit_id, iostat=ios) signature
        close(unit_id)
        
        if (ios /= 0) then
            print *, "ERROR: Cannot read PNG signature"
            stop 1
        end if
        
        if (any(signature /= EXPECTED_PNG_SIGNATURE)) then
            print *, "ERROR: Invalid PNG signature"
            print *, "Expected:", EXPECTED_PNG_SIGNATURE
            print *, "Got:     ", signature
            stop 1
        end if
        
        print *, "✅ PNG file has correct binary signature"
        
    end subroutine test_png_binary_signature

    subroutine test_png_file_type_detection()
        !! Test that file command recognizes PNG as PNG image data
        type(figure_t) :: fig
        real(wp) :: x(5), y(5)
        character(len=512) :: filename, file_type_output
        integer :: unit_id, ios
        
        filename = get_test_output_path("output/test/test_png_binary_output_fix/file_type_test.png")
        
        print *, ""
        print *, "Test: PNG File Type Detection"
        print *, "-----------------------------"
        
        x = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        y = [1.0_wp, 4.0_wp, 9.0_wp, 16.0_wp, 25.0_wp]
        
        call fig%initialize(width=300, height=200)
        call fig%add_plot(x, y)
        call figure_savefig(fig, filename)
        
        call assert_file_exists(filename)
        
        ! Use file command to check type (basic validation)
        ! We can't easily execute file command in test, so just check file size
        call validate_binary_png_size(filename)
        
        print *, "✅ PNG file passes binary content validation"
        
    end subroutine test_png_file_type_detection

    subroutine test_png_vs_ascii_content()
        !! Test that PNG content is different from ASCII output
        type(figure_t) :: fig_png, fig_ascii
        real(wp) :: x(6), y(6)
        integer :: i
        character(len=512) :: png_filename, ascii_filename
        integer :: png_size, ascii_size
        
        png_filename = get_test_output_path("output/test/test_png_binary_output_fix/content_test.png")
        ascii_filename = get_test_output_path("output/test/test_png_binary_output_fix/content_test.txt")
        
        print *, ""
        print *, "Test: PNG vs ASCII Content Comparison"
        print *, "-------------------------------------"
        
        ! Create test data
        do i = 1, 6
            x(i) = real(i-1, wp) * 0.5_wp
            y(i) = sin(x(i)) * 2.0_wp
        end do
        
        ! Create PNG output
        call fig_png%initialize(width=500, height=400)
        call fig_png%add_plot(x, y, label="sine wave")
        call fig_png%set_title("PNG Binary Test")
        call figure_savefig(fig_png, png_filename)
        
        ! Create ASCII output for comparison
        call fig_ascii%initialize(width=80, height=24, backend='ascii')
        call fig_ascii%add_plot(x, y, label="sine wave")
        call fig_ascii%set_title("ASCII Test")
        call figure_savefig(fig_ascii, ascii_filename)
        
        call assert_file_exists(png_filename)
        call assert_file_exists(ascii_filename)
        
        ! Check file sizes - PNG should be significantly larger than ASCII
        inquire(file=png_filename, size=png_size)
        inquire(file=ascii_filename, size=ascii_size)
        
        if (png_size <= ascii_size * 2) then
            print *, "ERROR: PNG file size too small compared to ASCII:", png_size, "vs", ascii_size
            stop 1
        end if
        
        ! Validate PNG has binary signature while ASCII doesn't
        call validate_binary_vs_text_content(png_filename, ascii_filename)
        
        print *, "✅ PNG and ASCII outputs are correctly differentiated"
        
    end subroutine test_png_vs_ascii_content

    subroutine validate_binary_png_size(filename)
        !! Validate PNG file has reasonable binary size
        character(len=*), intent(in) :: filename
        integer :: file_size
        
        inquire(file=filename, size=file_size)
        
        ! Binary PNG should be at least 200 bytes (header + minimal data)
        if (file_size < 200) then
            print *, "ERROR: PNG file too small for binary content:", file_size, "bytes"
            stop 1
        end if
        
        ! Should not be excessively large either
        if (file_size > 100000) then
            print *, "ERROR: PNG file unexpectedly large:", file_size, "bytes"
            stop 1
        end if
        
    end subroutine validate_binary_png_size

    subroutine validate_binary_vs_text_content(png_filename, ascii_filename)
        !! Validate that PNG contains binary data while ASCII contains text
        character(len=*), intent(in) :: png_filename, ascii_filename
        integer :: unit_id, ios
        integer(1) :: png_bytes(50), ascii_bytes(50)
        logical :: png_has_binary, ascii_is_text
        integer :: i
        
        ! Read first 50 bytes of PNG file
        open(newunit=unit_id, file=png_filename, access='stream', form='unformatted', status='old')
        read(unit_id) png_bytes
        close(unit_id)
        
        ! Read first 50 bytes of ASCII file
        open(newunit=unit_id, file=ascii_filename, access='stream', form='unformatted', status='old')
        read(unit_id) ascii_bytes
        close(unit_id)
        
        ! Check PNG has binary data (non-printable characters)
        png_has_binary = .false.
        do i = 1, 50
            if (png_bytes(i) < 32 .and. png_bytes(i) /= 10 .and. png_bytes(i) /= 13) then
                png_has_binary = .true.
                exit
            end if
        end do
        
        ! Check ASCII file contains mostly printable characters
        ascii_is_text = .true.
        do i = 1, min(50, size(ascii_bytes))
            if (ascii_bytes(i) < 9 .or. ascii_bytes(i) > 126) then
                if (ascii_bytes(i) /= 10 .and. ascii_bytes(i) /= 13) then
                    ascii_is_text = .false.
                    exit
                end if
            end if
        end do
        
        if (.not. png_has_binary) then
            print *, "ERROR: PNG file appears to contain only text data"
            stop 1
        end if
        
        if (.not. ascii_is_text) then
            print *, "WARNING: ASCII file contains non-text data (may be acceptable)"
        end if
        
    end subroutine validate_binary_vs_text_content

end program test_png_binary_output_fix
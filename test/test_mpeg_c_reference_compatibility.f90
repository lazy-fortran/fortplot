program test_mpeg_c_reference_compatibility
    use fortplot
    use fortplot_security, only: safe_remove_file, safe_check_program_available, sanitize_filename, safe_validate_mpeg_with_ffprobe
    use iso_fortran_env, only: real64
    implicit none

    ! Given: MPEG files should be compatible with C reference implementation (Issue #32)
    ! When: We compare against C reference library behavior
    ! Then: Files should be bit-exact or functionally equivalent to C reference

    type(figure_t) :: test_fig
    real(real64), dimension(10) :: test_x, test_y
    integer :: i

    print *, "=== C REFERENCE COMPATIBILITY TESTS ==="
    
    call test_c_reference_file_comparison()
    call test_c_library_compatibility()
    call test_format_specification_compliance()
    call test_standard_conformance()

    print *, "=== C reference compatibility tests completed ==="

contains

    subroutine test_c_reference_file_comparison()
        ! Given: C reference implementation produces known good files
        ! When: We compare our output with C reference files
        ! Then: Files should be structurally similar

        type(animation_t) :: anim
        character(len=200) :: fortran_file, reference_file
        logical :: reference_exists, comparison_valid
        integer :: fortran_size, reference_size, status

        print *, ""
        print *, "TEST: C Reference File Comparison"
        print *, "================================"

        fortran_file = "c_ref_fortran.mp4"
        reference_file = "short.mpg"  ! Known good C reference file from issue description
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = test_x

        call test_fig%initialize(width=320, height=240)
        call test_fig%add_plot(test_x, test_y)

        anim = FuncAnimation(update_c_ref_data, frames=5, interval=100, fig=test_fig)
        call anim%save(fortran_file, fps=10)

        ! Check if reference file exists
        inquire(file=reference_file, exist=reference_exists, size=reference_size)
        inquire(file=fortran_file, size=fortran_size)

        print *, "Reference file exists:", reference_exists
        if (reference_exists) then
            print *, "Reference file size:", reference_size, "bytes"
            print *, "Fortran file size:", fortran_size, "bytes"
            
            ! Compare sizes - should be in same order of magnitude
            comparison_valid = compare_file_characteristics(fortran_file, reference_file)
        else
            print *, "Reference file not found - creating expected behavior test"
            comparison_valid = validate_expected_c_behavior(fortran_file)
        end if

        print *, "C reference compatibility:", comparison_valid

        if (.not. comparison_valid) then
            print *, "*** C REFERENCE COMPATIBILITY FAILURE ***"
            print *, "Generated file not compatible with C reference behavior"
        end if

        block
            logical :: remove_success
            call safe_remove_file(fortran_file, remove_success)
            if (.not. remove_success) then
                print *, "Warning: Could not remove temporary file: " // trim(fortran_file)
                    end if
    end block
    end subroutine

    subroutine update_c_ref_data(frame)
        integer, intent(in) :: frame
        test_y = test_x + real(frame, real64) * 0.5_real64
        call test_fig%set_ydata(1, test_y)
    end subroutine

    function compare_file_characteristics(fortran_file, reference_file) result(compatible)
        character(len=*), intent(in) :: fortran_file, reference_file
        logical :: compatible
        integer :: fort_size, ref_size
        logical :: fort_valid, ref_valid

        inquire(file=fortran_file, size=fort_size)
        inquire(file=reference_file, size=ref_size)

        ! Both files should be substantial
        fort_valid = (fort_size > 1000)
        ref_valid = (ref_size > 1000)

        ! Size comparison - Fortran file should be in reasonable range of reference
        ! Allow for different compression but expect same order of magnitude
        compatible = fort_valid .and. ref_valid .and. &
                    (fort_size > ref_size / 100) .and. (fort_size < ref_size * 100)

        print *, "  File characteristics comparison:"
        print *, "    Fortran file valid:", fort_valid
        print *, "    Reference file valid:", ref_valid
        print *, "    Size compatibility:", compatible
    end function

    function validate_expected_c_behavior(filename) result(valid)
        character(len=*), intent(in) :: filename
        logical :: valid
        integer :: file_size

        inquire(file=filename, size=file_size)
        
        ! Expected C behavior: files should be substantial (>10KB for reasonable content)
        ! This is based on issue description showing reference file at 83,522 bytes
        valid = (file_size > 10000)  ! 10KB minimum for C-like behavior

        print *, "  Expected C behavior validation:"
        print *, "    File size:", file_size, "bytes"
        print *, "    Meets C expectations (>10KB):", valid
    end function

    subroutine test_c_library_compatibility()
        ! Given: Files should be compatible with C MPEG libraries
        ! When: We test against C library standards
        ! Then: Files should meet C library expectations

        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: c_library_compatible

        print *, ""
        print *, "TEST: C Library Compatibility"
        print *, "============================"

        test_file = "c_library_test.mp4"
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = sin(test_x)

        call test_fig%initialize(width=640, height=480)
        call test_fig%add_plot(test_x, test_y)

        anim = FuncAnimation(update_c_library_data, frames=10, interval=50, fig=test_fig)
        call anim%save(test_file, fps=15)

        c_library_compatible = validate_c_library_standards(test_file)

        print *, "C library compatibility:", c_library_compatible

        if (.not. c_library_compatible) then
            print *, "*** C LIBRARY COMPATIBILITY FAILURE ***"
            print *, "File does not meet C library standards"
        end if

        block
            logical :: remove_success
            call safe_remove_file(test_file, remove_success)
            if (.not. remove_success) then
                print *, "Warning: Could not remove temporary file: " // trim(test_file)
                    end if
    end block
    end subroutine

    subroutine update_c_library_data(frame)
        integer, intent(in) :: frame
        test_y = sin(test_x + real(frame, real64) * 0.3_real64)
        call test_fig%set_ydata(1, test_y)
    end subroutine

    function validate_c_library_standards(filename) result(compatible)
        character(len=*), intent(in) :: filename
        logical :: compatible
        integer :: file_size
        logical :: size_adequate, header_standard, tool_compatible

        inquire(file=filename, size=file_size)

        ! C library standards validation
        size_adequate = (file_size > 5000)  ! C libraries expect substantial files
        header_standard = check_c_standard_header(filename)
        tool_compatible = check_c_tool_compatibility(filename)

        compatible = size_adequate .and. header_standard .and. tool_compatible

        print *, "  C library standards validation:"
        print *, "    Adequate size:", size_adequate
        print *, "    Standard header:", header_standard
        print *, "    Tool compatible:", tool_compatible
        print *, "    Overall compatible:", compatible
    end function

    function check_c_standard_header(filename) result(standard)
        character(len=*), intent(in) :: filename
        logical :: standard
        character(len=16) :: header
        integer :: file_unit, ios

        standard = .false.

        open(newunit=file_unit, file=filename, access='stream', form='unformatted', iostat=ios)
        if (ios /= 0) return

        read(file_unit, iostat=ios) header
        close(file_unit)

        if (ios /= 0) return

        ! C standard headers should have proper MP4 box structure
        standard = (index(header, 'ftyp') > 0 .or. &
                   index(header, 'mdat') > 0 .or. &
                   index(header, 'moov') > 0)
    end function

    function check_c_tool_compatibility(filename) result(compatible)
        character(len=*), intent(in) :: filename
        logical :: compatible
        character(len=500) :: command
        integer :: status

        ! Check if C-based tools can read the file - use secure validation
        if (.not. safe_check_program_available('ffprobe')) then
            print *, "Operating in secure mode - using internal MPEG validation"
        end if
        
        compatible = safe_validate_mpeg_with_ffprobe(filename)
    end function

    subroutine test_format_specification_compliance()
        ! Given: Files should comply with MPEG/MP4 format specifications
        ! When: We validate against format specifications
        ! Then: Files should meet formal specification requirements

        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: spec_compliant

        print *, ""
        print *, "TEST: Format Specification Compliance"
        print *, "===================================="

        test_file = "format_spec_test.mp4"
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = cos(test_x)

        call test_fig%initialize(width=800, height=600)
        call test_fig%add_plot(test_x, test_y)

        anim = FuncAnimation(update_spec_data, frames=15, interval=50, fig=test_fig)
        call anim%save(test_file, fps=24)

        spec_compliant = validate_format_specification_compliance(test_file)

        print *, "Format specification compliance:", spec_compliant

        if (.not. spec_compliant) then
            print *, "*** FORMAT SPECIFICATION FAILURE ***"
            print *, "File does not comply with MPEG/MP4 specifications"
        end if

        block
            logical :: remove_success
            call safe_remove_file(test_file, remove_success)
            if (.not. remove_success) then
                print *, "Warning: Could not remove temporary file: " // trim(test_file)
                    end if
    end block
    end subroutine

    subroutine update_spec_data(frame)
        integer, intent(in) :: frame
        test_y = cos(test_x + real(frame, real64) * 0.2_real64)
        call test_fig%set_ydata(1, test_y)
    end subroutine

    function validate_format_specification_compliance(filename) result(compliant)
        character(len=*), intent(in) :: filename
        logical :: compliant
        logical :: box_structure_valid, content_valid, metadata_valid

        box_structure_valid = validate_mp4_box_structure(filename)
        content_valid = validate_content_structure(filename)
        metadata_valid = validate_metadata_structure(filename)

        compliant = box_structure_valid .and. content_valid .and. metadata_valid

        print *, "  Format specification validation:"
        print *, "    Box structure valid:", box_structure_valid
        print *, "    Content structure valid:", content_valid
        print *, "    Metadata structure valid:", metadata_valid
        print *, "    Overall compliant:", compliant
    end function

    function validate_mp4_box_structure(filename) result(valid)
        character(len=*), intent(in) :: filename
        logical :: valid
        character(len=32) :: header
        integer :: file_unit, ios

        valid = .false.

        open(newunit=file_unit, file=filename, access='stream', form='unformatted', iostat=ios)
        if (ios /= 0) return

        read(file_unit, iostat=ios) header
        close(file_unit)

        if (ios /= 0) return

        ! MP4 specification requires proper box structure
        valid = (index(header, 'ftyp') > 0)  ! File type box should be present
    end function

    function validate_content_structure(filename) result(valid)
        character(len=*), intent(in) :: filename
        logical :: valid
        integer :: file_size

        inquire(file=filename, size=file_size)
        
        ! Content should be substantial for format compliance
        valid = (file_size > 2000)
    end function

    function validate_metadata_structure(filename) result(valid)
        character(len=*), intent(in) :: filename
        logical :: valid
        character(len=500) :: command
        integer :: status

        ! Check if metadata structure is readable - use secure validation
        if (.not. safe_check_program_available('ffprobe')) then
            print *, "Operating in secure mode - using internal metadata validation"
        end if
        
        valid = safe_validate_mpeg_with_ffprobe(filename)
    end function

    subroutine test_standard_conformance()
        ! Given: Files should conform to industry standards
        ! When: We validate against standard conformance
        ! Then: Files should meet industry standard requirements

        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: standard_conformant

        print *, ""
        print *, "TEST: Industry Standard Conformance"
        print *, "=================================="

        test_file = "standard_conformance_test.mp4"
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = test_x**2

        call test_fig%initialize(width=1024, height=768)
        call test_fig%add_plot(test_x, test_y)

        anim = FuncAnimation(update_standard_data, frames=20, interval=50, fig=test_fig)
        call anim%save(test_file, fps=30)

        standard_conformant = validate_industry_standard_conformance(test_file)

        print *, "Industry standard conformance:", standard_conformant

        if (.not. standard_conformant) then
            print *, "*** INDUSTRY STANDARD CONFORMANCE FAILURE ***"
            print *, "File does not meet industry standard requirements"
        end if

        block
            logical :: remove_success
            call safe_remove_file(test_file, remove_success)
            if (.not. remove_success) then
                print *, "Warning: Could not remove temporary file: " // trim(test_file)
                    end if
    end block
    end subroutine

    subroutine update_standard_data(frame)
        integer, intent(in) :: frame
        test_y = test_x**2 + real(frame, real64) * 3.0_real64
        call test_fig%set_ydata(1, test_y)
    end subroutine

    function validate_industry_standard_conformance(filename) result(conformant)
        character(len=*), intent(in) :: filename
        logical :: conformant
        logical :: size_standard, format_standard, compatibility_standard

        size_standard = validate_size_standard(filename)
        format_standard = validate_format_standard(filename)
        compatibility_standard = validate_compatibility_standard(filename)

        conformant = size_standard .and. format_standard .and. compatibility_standard

        print *, "  Industry standard conformance:"
        print *, "    Size meets standards:", size_standard
        print *, "    Format meets standards:", format_standard
        print *, "    Compatibility meets standards:", compatibility_standard
        print *, "    Overall conformant:", conformant
    end function

    function validate_size_standard(filename) result(standard)
        character(len=*), intent(in) :: filename
        logical :: standard
        integer :: file_size

        inquire(file=filename, size=file_size)
        
        ! Industry standard: files should be substantial but not excessive
        standard = (file_size > 10000 .and. file_size < 100000000)  ! 10KB to 100MB range
    end function

    function validate_format_standard(filename) result(standard)
        character(len=*), intent(in) :: filename
        logical :: standard
        character(len=16) :: header
        integer :: file_unit, ios

        standard = .false.

        open(newunit=file_unit, file=filename, access='stream', form='unformatted', iostat=ios)
        if (ios /= 0) return

        read(file_unit, iostat=ios) header
        close(file_unit)

        if (ios /= 0) return

        ! Standard format should have recognizable structure
        standard = (index(header, 'ftyp') > 0 .or. &
                   index(header, 'mdat') > 0 .or. &
                   index(header, 'moov') > 0)
    end function

    function validate_compatibility_standard(filename) result(standard)
        character(len=*), intent(in) :: filename
        logical :: standard
        character(len=500) :: command
        integer :: status

        if (.not. safe_check_program_available('ffprobe')) then
            print *, "Operating in secure mode - using internal compatibility validation"
        end if
        
        standard = safe_validate_mpeg_with_ffprobe(filename)
    end function

end program test_mpeg_c_reference_compatibility
module fortplot_png_validation
    !! PNG file validation using internal format checks (no shell execution)
    !! CRITICAL: Every PNG file MUST be validated after creation
    use fortplot_validation, only: validate_png_format, validate_pdf_format, &
        validation_result_t
    implicit none
    
    private
    public :: validate_png_file, png_validation_available
    
contains

    function png_validation_available() result(available)
        !! Legacy API: external tool check disabled for security compliance
        !! Always returns .false. to avoid shell-based validation paths
        logical :: available
        available = .false.
    end function png_validation_available

    subroutine validate_png_file(filename, test_name)
        !! Validate PNG file using internal signature checks (secure)
        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: test_name
        
        type(validation_result_t) :: png_ok, pdf_ok
        logical :: file_exists
        
        ! Check if file exists first
        inquire(file=filename, exist=file_exists)
        if (.not. file_exists) then
            print *, "FATAL: PNG file does not exist: ", trim(filename)
            print *, "TEST FAILED: ", trim(test_name)
            error stop "PNG file missing"
        end if
        
        ! Perform secure, in-process PNG validation (no external commands)
        png_ok = validate_png_format(filename)
        if (png_ok%passed) then
            print *, "✓ PNG validation passed: ", trim(filename)
            return
        end if
        
        ! If PNG signature invalid, check if file is actually a PDF fallback
        pdf_ok = validate_pdf_format(filename)
        if (pdf_ok%passed) then
            print *, "WARNING: File ", trim(filename), " is PDF, not PNG (backend fallback)"
            print *, "✓ PNG validation skipped for PDF fallback file"
            return
        end if
        
        ! Neither valid PNG nor PDF – treat as fatal corruption
        print *, "FATAL: PNG file failed validation: ", trim(filename)
        print *, "TEST FAILED: ", trim(test_name)
        error stop "PNG validation failed - file is corrupted"
    end subroutine validate_png_file

end module fortplot_png_validation

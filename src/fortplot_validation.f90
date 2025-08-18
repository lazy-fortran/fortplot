! fortplot_validation.f90 - Functional validation utilities for output verification
module fortplot_validation
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    public :: validate_file_exists
    public :: validate_file_size
    public :: validate_png_format
    public :: validate_pdf_format  
    public :: validate_ascii_format
    public :: compare_with_baseline
    public :: validation_result_t
    
    ! Validation result type for structured reporting
    type :: validation_result_t
        logical :: passed
        character(len=256) :: message
        real(wp) :: metric_value
    end type
    
    ! Minimum file size thresholds (bytes)
    integer, parameter :: MIN_PNG_SIZE = 100
    integer, parameter :: MIN_PDF_SIZE = 200
    integer, parameter :: MIN_ASCII_SIZE = 50
    
contains
    
    ! Given: A file path to validate
    ! When: Checking if the file exists
    ! Then: Return validation result with existence status
    function validate_file_exists(file_path) result(validation)
        character(len=*), intent(in) :: file_path
        type(validation_result_t) :: validation
        
        logical :: file_exists
        
        inquire(file=file_path, exist=file_exists)
        
        validation%passed = file_exists
        if (file_exists) then
            validation%message = "File exists: " // trim(file_path)
        else
            validation%message = "File missing: " // trim(file_path)
        end if
        validation%metric_value = 0.0_wp
    end function
    
    ! Given: A file path to validate
    ! When: Checking file size against minimum thresholds
    ! Then: Return validation result with size information
    function validate_file_size(file_path, min_size) result(validation)
        character(len=*), intent(in) :: file_path
        integer, intent(in) :: min_size
        type(validation_result_t) :: validation
        
        integer :: file_size, ios
        
        open(unit=99, file=file_path, access='stream', iostat=ios)
        if (ios /= 0) then
            validation%passed = .false.
            validation%message = "Cannot open file for size check: " // trim(file_path)
            validation%metric_value = 0.0_wp
            return
        end if
        
        inquire(unit=99, size=file_size)
        close(99)
        
        validation%passed = file_size >= min_size
        validation%metric_value = real(file_size, wp)
        
        if (validation%passed) then
            write(validation%message, '(a,i0,a,i0,a)') &
                "File size valid: ", file_size, " bytes (min: ", min_size, ")"
        else
            write(validation%message, '(a,i0,a,i0,a)') &
                "File too small: ", file_size, " bytes (min: ", min_size, ")"
        end if
    end function
    
    ! Given: A PNG file path
    ! When: Validating PNG format signature
    ! Then: Return validation result for PNG format compliance
    function validate_png_format(file_path) result(validation)
        character(len=*), intent(in) :: file_path
        type(validation_result_t) :: validation
        
        integer, parameter :: PNG_SIGNATURE_SIZE = 8
        character(len=1), dimension(PNG_SIGNATURE_SIZE) :: signature
        character(len=1), dimension(PNG_SIGNATURE_SIZE), parameter :: &
            PNG_SIGNATURE = [achar(137), 'P', 'N', 'G', achar(13), achar(10), achar(26), achar(10)]
        integer :: i, ios
        
        open(unit=98, file=file_path, access='stream', form='unformatted', iostat=ios)
        if (ios /= 0) then
            validation%passed = .false.
            validation%message = "Cannot open PNG file: " // trim(file_path)
            validation%metric_value = 0.0_wp
            return
        end if
        
        read(98, iostat=ios) signature
        close(98)
        
        if (ios /= 0) then
            validation%passed = .false.
            validation%message = "Cannot read PNG signature"
            validation%metric_value = 0.0_wp
            return
        end if
        
        validation%passed = .true.
        do i = 1, PNG_SIGNATURE_SIZE
            if (signature(i) /= PNG_SIGNATURE(i)) then
                validation%passed = .false.
                exit
            end if
        end do
        
        if (validation%passed) then
            validation%message = "Valid PNG format signature"
        else
            validation%message = "Invalid PNG format signature"
        end if
        validation%metric_value = 1.0_wp
    end function
    
    ! Given: A PDF file path
    ! When: Validating PDF format signature
    ! Then: Return validation result for PDF format compliance
    function validate_pdf_format(file_path) result(validation)
        character(len=*), intent(in) :: file_path
        type(validation_result_t) :: validation
        
        character(len=5) :: pdf_header
        integer :: ios
        
        open(unit=97, file=file_path, iostat=ios)
        if (ios /= 0) then
            validation%passed = .false.
            validation%message = "Cannot open PDF file: " // trim(file_path)
            validation%metric_value = 0.0_wp
            return
        end if
        
        read(97, '(a5)', iostat=ios) pdf_header
        close(97)
        
        if (ios /= 0) then
            validation%passed = .false.
            validation%message = "Cannot read PDF header"
            validation%metric_value = 0.0_wp
            return
        end if
        
        validation%passed = (pdf_header == '%PDF-')
        if (validation%passed) then
            validation%message = "Valid PDF format header"
        else
            validation%message = "Invalid PDF format header: " // pdf_header
        end if
        validation%metric_value = 1.0_wp
    end function
    
    ! Given: An ASCII file path  
    ! When: Validating ASCII format content
    ! Then: Return validation result for ASCII format compliance
    function validate_ascii_format(file_path) result(validation)
        character(len=*), intent(in) :: file_path
        type(validation_result_t) :: validation
        
        character(len=256) :: line
        integer :: ios, line_count
        
        open(unit=96, file=file_path, iostat=ios)
        if (ios /= 0) then
            validation%passed = .false.
            validation%message = "Cannot open ASCII file: " // trim(file_path)
            validation%metric_value = 0.0_wp
            return
        end if
        
        line_count = 0
        do
            read(96, '(a)', iostat=ios) line
            if (ios /= 0) exit
            line_count = line_count + 1
        end do
        close(96)
        
        validation%passed = line_count > 0
        validation%metric_value = real(line_count, wp)
        
        if (validation%passed) then
            write(validation%message, '(a,i0,a)') "Valid ASCII content (", line_count, " lines)"
        else
            validation%message = "Empty or invalid ASCII content"
        end if
    end function
    
    ! Given: Current and baseline file paths
    ! When: Comparing files for regression detection
    ! Then: Return validation result for comparison
    function compare_with_baseline(current_file, baseline_file) result(validation)
        character(len=*), intent(in) :: current_file, baseline_file
        type(validation_result_t) :: validation
        
        integer :: current_size, baseline_size, ios
        
        ! Check if baseline exists
        inquire(file=baseline_file, exist=validation%passed, size=baseline_size)
        if (.not. validation%passed) then
            validation%message = "Baseline file missing: " // trim(baseline_file)
            validation%metric_value = 0.0_wp
            return
        end if
        
        ! Get current file size
        inquire(file=current_file, exist=validation%passed, size=current_size)
        if (.not. validation%passed) then
            validation%message = "Current file missing: " // trim(current_file)
            validation%metric_value = 0.0_wp
            return
        end if
        
        ! Compare sizes (simple regression check)
        validation%metric_value = abs(real(current_size - baseline_size, wp) / real(baseline_size, wp))
        validation%passed = validation%metric_value < 0.1_wp  ! 10% size difference threshold
        
        if (validation%passed) then
            write(validation%message, '(a,f6.2,a)') "Size difference within tolerance (", &
                validation%metric_value * 100.0_wp, "%)"
        else
            write(validation%message, '(a,f6.2,a)') "Size difference exceeds tolerance (", &
                validation%metric_value * 100.0_wp, "%)"
        end if
    end function
    
end module fortplot_validation
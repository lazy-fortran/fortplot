program test_pdf_content_regression
    !! Test PDF content integrity and format validation for Issue #97
    !! Given: PDF files should contain proper PDF content structure
    !! When: We generate PDF files using fortplot
    !! Then: Files should contain PDF headers and NOT PNG data
    use fortplot
    use fortplot_security, only: safe_remove_file, get_test_output_path
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    call test_pdf_header_validation()
    call test_pdf_not_png_content()
    call test_pdf_structure_integrity()
    call test_pdf_content_format_various_scenarios()
    
    print *, "All PDF content regression tests completed!"
    
contains

    subroutine test_pdf_header_validation()
        !! Given: PDF files must start with valid PDF header
        !! When: We create a simple PDF plot
        !! Then: File should start with "%PDF-" header
        type(figure_t) :: fig
        real(wp), parameter :: x_data(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        real(wp), parameter :: y_data(5) = [1.0_wp, 4.0_wp, 2.0_wp, 8.0_wp, 5.0_wp]
        character(len=200) :: test_file
        character(len=8) :: pdf_header
        integer :: file_unit, ios
        logical :: has_pdf_header, remove_success
        
        print *, "=== Testing PDF header validation ==="
        
        test_file = get_test_output_path("output/test/test_pdf_header_validation.pdf")
        
        call fig%initialize(640, 480)
        call fig%add_plot(x_data, y_data)
        call fig%set_title("PDF Header Test")
        call figure_savefig(fig, test_file)
        
        ! Read first 8 bytes to check PDF header
        open(newunit=file_unit, file=test_file, access='stream', form='unformatted', iostat=ios)
        if (ios /= 0) then
            print *, "ERROR: Could not open PDF file for header validation"
            return
        end if
        
        read(file_unit, iostat=ios) pdf_header
        close(file_unit)
        
        if (ios /= 0) then
            print *, "ERROR: Could not read PDF header"
            return
        end if
        
        ! Check if header starts with "%PDF-"
        has_pdf_header = (pdf_header(1:5) == '%PDF-')
        
        print *, "PDF header found: ", pdf_header(1:8)
        print *, "Valid PDF header: ", has_pdf_header
        
        if (.not. has_pdf_header) then
            print *, "*** CRITICAL FAILURE: PDF file does not have valid PDF header ***"
            print *, "Expected: %PDF-, Found: ", pdf_header(1:5)
        end if
        
        call safe_remove_file(test_file, remove_success)
    end subroutine test_pdf_header_validation
    
    subroutine test_pdf_not_png_content()
        !! Given: PDF files should not contain PNG data
        !! When: We create a PDF plot  
        !! Then: File should NOT contain PNG signature bytes
        type(figure_t) :: fig
        real(wp), parameter :: x_data(3) = [0.0_wp, 1.0_wp, 2.0_wp]
        real(wp), parameter :: y_data(3) = [0.0_wp, 1.0_wp, 0.5_wp]
        character(len=200) :: test_file
        character(len=512) :: file_buffer
        integer :: file_unit, ios, file_size, i
        logical :: contains_png_signature, remove_success
        
        print *, "=== Testing PDF does not contain PNG data ==="
        
        test_file = get_test_output_path("output/test/test_pdf_not_png.pdf")
        
        call fig%initialize(800, 600)
        call fig%add_plot(x_data, y_data, label="Test Data")
        call figure_legend(fig, )
        call figure_savefig(fig, test_file)
        
        ! Check file size first
        inquire(file=test_file, size=file_size)
        print *, "Generated PDF file size: ", file_size, " bytes"
        
        ! Read beginning of file to check for PNG signature
        open(newunit=file_unit, file=test_file, access='stream', form='unformatted', iostat=ios)
        if (ios /= 0) then
            print *, "ERROR: Could not open PDF file for PNG signature check"
            return
        end if
        
        read(file_unit, iostat=ios) file_buffer
        close(file_unit)
        
        if (ios /= 0) then
            print *, "ERROR: Could not read PDF file content"
            return
        end if
        
        ! Check for PNG signature: 89 50 4E 47 (decimal: 137 80 78 71) which is \211PNG
        contains_png_signature = .false.
        do i = 1, len(file_buffer) - 3
            if (iachar(file_buffer(i:i)) == 137 .and. &     ! 89 hex = 137 decimal
                iachar(file_buffer(i+1:i+1)) == 80 .and. &  ! 50 hex = 80 decimal  
                iachar(file_buffer(i+2:i+2)) == 78 .and. &  ! 4E hex = 78 decimal
                iachar(file_buffer(i+3:i+3)) == 71) then    ! 47 hex = 71 decimal
                contains_png_signature = .true.
                exit
            end if
        end do
        
        print *, "Contains PNG signature: ", contains_png_signature
        
        if (contains_png_signature) then
            print *, "*** CRITICAL FAILURE: PDF file contains PNG data ***"
            print *, "This indicates Issue #97 regression - PNG output in PDF files"
        else
            print *, "PASS: PDF file does not contain PNG data"
        end if
        
        call safe_remove_file(test_file, remove_success)
    end subroutine test_pdf_not_png_content
    
    subroutine test_pdf_structure_integrity()
        !! Given: PDF files must have proper internal structure
        !! When: We create a PDF with various elements
        !! Then: File should contain PDF objects, xref table, and trailer
        type(figure_t) :: fig
        real(wp), parameter :: n = 20
        real(wp) :: x_data(20), y_data(20)
        character(len=200) :: test_file
        character(len=20000) :: file_content
        integer :: file_unit, ios, i, file_size
        logical :: has_pdf_objects, has_xref_table, has_trailer, remove_success
        
        print *, "=== Testing PDF structure integrity ==="
        
        test_file = get_test_output_path("output/test/test_pdf_structure.pdf")
        
        ! Generate test data
        do i = 1, 20
            x_data(i) = real(i - 1, wp) * 0.1_wp
            y_data(i) = sin(x_data(i) * 6.28_wp) * exp(-x_data(i))
        end do
        
        call fig%initialize(640, 480)
        call fig%add_plot(x_data, y_data)
        call fig%set_title("PDF Structure Test")
        call fig%set_xlabel("Time (s)")
        call fig%set_ylabel("Amplitude")
        call figure_savefig(fig, test_file)
        
        ! Read file content to check internal structure
        inquire(file=test_file, size=file_size)
        open(newunit=file_unit, file=test_file, access='stream', form='unformatted', iostat=ios)
        if (ios /= 0) then
            print *, "ERROR: Could not open PDF file for structure validation"
            return
        end if
        
        if (file_size > len(file_content)) then
            print *, "WARNING: PDF file too large for buffer, reading first ", len(file_content), " bytes"
        end if
        
        read(file_unit, iostat=ios) file_content(1:min(file_size, len(file_content)))
        close(file_unit)
        
        if (ios /= 0) then
            print *, "ERROR: Could not read PDF file structure"
            return
        end if
        
        ! Check for essential PDF structure elements
        has_pdf_objects = index(file_content, ' 0 obj') > 0
        has_xref_table = index(file_content, 'xref') > 0
        has_trailer = index(file_content, 'trailer') > 0
        
        print *, "Has PDF objects: ", has_pdf_objects
        print *, "Has xref table: ", has_xref_table  
        print *, "Has trailer: ", has_trailer
        
        if (.not. (has_pdf_objects .and. has_xref_table .and. has_trailer)) then
            print *, "*** CRITICAL FAILURE: PDF file missing essential structure elements ***"
            if (.not. has_pdf_objects) print *, "Missing: PDF objects"
            if (.not. has_xref_table) print *, "Missing: xref table"
            if (.not. has_trailer) print *, "Missing: trailer"
        else
            print *, "PASS: PDF structure integrity verified"
        end if
        
        call safe_remove_file(test_file, remove_success)
    end subroutine test_pdf_structure_integrity
    
    subroutine test_pdf_content_format_various_scenarios()
        !! Given: PDF content should be consistent across different plot types
        !! When: We create various PDF plots (scatter, line, contour)
        !! Then: All should have valid PDF format, not PNG content
        use fortplot_validation, only: validation_result_t
        character(len=200) :: scatter_file, line_file, contour_file
        type(validation_result_t) :: scatter_validation, line_validation, contour_validation
        logical :: all_valid_pdfs, remove_success
        
        print *, "=== Testing PDF format across various scenarios ==="
        
        scatter_file = get_test_output_path("output/test/test_pdf_scatter.pdf")
        line_file = get_test_output_path("output/test/test_pdf_line.pdf")
        contour_file = get_test_output_path("output/test/test_pdf_contour.pdf")
        
        ! Create scatter plot PDF
        call create_test_scatter_pdf(scatter_file)
        
        ! Create line plot PDF
        call create_test_line_pdf(line_file)
        
        ! Create contour plot PDF  
        call create_test_contour_pdf(contour_file)
        
        ! Validate all files
        scatter_validation = validate_pdf_format(scatter_file)
        line_validation = validate_pdf_format(line_file)
        contour_validation = validate_pdf_format(contour_file)
        
        all_valid_pdfs = scatter_validation%passed .and. &
                        line_validation%passed .and. &
                        contour_validation%passed
        
        print *, "All PDFs have valid format: ", all_valid_pdfs
        
        if (.not. all_valid_pdfs) then
            print *, "*** CRITICAL FAILURE: One or more PDF files have invalid format ***"
        else
            print *, "PASS: All PDF scenarios have valid format"
        end if
        
        ! Cleanup
        call safe_remove_file(scatter_file, remove_success)
        call safe_remove_file(line_file, remove_success)
        call safe_remove_file(contour_file, remove_success)
    end subroutine test_pdf_content_format_various_scenarios
    
    subroutine create_test_scatter_pdf(filename)
        character(len=*), intent(in) :: filename
        type(figure_t) :: fig
        real(wp) :: x_data(15), y_data(15)
        integer :: i
        
        do i = 1, 15
            x_data(i) = real(i, wp)
            y_data(i) = x_data(i)**2 + real(i, wp) * 0.5_wp
        end do
        
        call fig%initialize(600, 450)
        call fig%add_scatter(x_data, y_data, marker='o')
        call fig%set_title("PDF Scatter Test")
        call figure_savefig(fig, filename)
    end subroutine create_test_scatter_pdf
    
    subroutine create_test_line_pdf(filename)
        character(len=*), intent(in) :: filename
        type(figure_t) :: fig
        real(wp) :: x_data(25), y_data(25)
        integer :: i
        
        do i = 1, 25
            x_data(i) = real(i - 1, wp) * 0.2_wp
            y_data(i) = cos(x_data(i) * 2.0_wp) + 0.5_wp * sin(x_data(i) * 5.0_wp)
        end do
        
        call fig%initialize(800, 600)
        call fig%add_plot(x_data, y_data, linestyle='-')
        call fig%set_title("PDF Line Test")
        call figure_savefig(fig, filename)
    end subroutine create_test_line_pdf
    
    subroutine create_test_contour_pdf(filename)
        character(len=*), intent(in) :: filename
        type(figure_t) :: fig
        integer, parameter :: nx = 10, ny = 10
        real(wp) :: x_data(nx), y_data(ny), z_data(nx, ny)
        integer :: i, j
        
        do i = 1, nx
            x_data(i) = real(i - 1, wp) * 0.1_wp
        end do
        
        do j = 1, ny
            y_data(j) = real(j - 1, wp) * 0.1_wp
        end do
        
        do i = 1, nx
            do j = 1, ny
                z_data(i, j) = exp(-(x_data(i)**2 + y_data(j)**2))
            end do
        end do
        
        call fig%initialize(700, 500)
        call figure_add_contour_filled(fig, x_data, y_data, z_data)
        call fig%set_title("PDF Contour Test")
        call figure_savefig(fig, filename)
    end subroutine create_test_contour_pdf

end program test_pdf_content_regression
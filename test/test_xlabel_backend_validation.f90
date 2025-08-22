program test_xlabel_backend_validation
    !! Backend-specific xlabel rendering validation - Issue #190
    !! 
    !! GIVEN: xlabel set on figure
    !! WHEN: Rendered with different backends (ASCII, PNG, PDF)
    !! THEN: xlabel should appear in format-appropriate output
    !!
    !! This test validates xlabel rendering across all supported backends

    use, intrinsic :: iso_fortran_env, only: wp => real64, output_unit, error_unit
    use fortplot_figure_core
    implicit none

    character(len=*), parameter :: test_xlabel = "Multi-Backend X-axis Label"
    real(wp), parameter :: x_test(4) = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
    real(wp), parameter :: y_test(4) = [0.0_wp, 1.0_wp, 4.0_wp, 9.0_wp]

    write(output_unit, '(A)') "=== Testing xlabel rendering across backends ==="

    ! Test ASCII backend xlabel rendering
    call test_ascii_xlabel_content()

    ! Test PNG backend xlabel embedding
    call test_png_xlabel_validation()

    ! Test PDF backend xlabel integration
    call test_pdf_xlabel_verification()

    ! Test backend switching with xlabel persistence
    call test_backend_switching_xlabel()

    write(output_unit, '(A)') "Backend validation tests completed"

contains

    subroutine test_ascii_xlabel_content()
        !! GIVEN: Figure with xlabel for ASCII output
        !! WHEN: Saved as .txt file
        !! THEN: xlabel text should be present in ASCII content
        
        type(figure_t) :: ascii_fig
        character(len=*), parameter :: ascii_file = "test_ascii_xlabel_backend.txt"
        character(len=*), parameter :: ascii_xlabel = "ASCII Backend X-axis"
        logical :: ascii_xlabel_found, has_plot_content
        integer :: ascii_unit, io_stat, content_lines
        character(len=1000) :: buffer
        
        write(output_unit, '(A)') "--- Testing ASCII backend xlabel rendering ---"
        
        call ascii_fig%initialize(400, 300)
        call ascii_fig%add_plot(x_test, y_test, label="ascii_data")
        call ascii_fig%set_xlabel(ascii_xlabel)
        
        write(output_unit, '(A)') "GIVEN: ASCII figure with xlabel = '" // ascii_xlabel // "'"
        
        call ascii_fig%save(ascii_file)
        write(output_unit, '(A)') "WHEN: Saved to " // ascii_file
        
        ascii_xlabel_found = .false.
        has_plot_content = .false.
        content_lines = 0
        
        open(newunit=ascii_unit, file=ascii_file, status='old', action='read', iostat=io_stat)
        
        if (io_stat /= 0) then
            write(error_unit, '(A,I0)') "CRITICAL: Cannot read ASCII output file. iostat = ", io_stat
            call exit(1)
        end if
        
        do
            read(ascii_unit, '(A)', iostat=io_stat) buffer
            if (io_stat /= 0) exit
            
            content_lines = content_lines + 1
            
            if (index(buffer, ascii_xlabel) > 0) then
                ascii_xlabel_found = .true.
                write(output_unit, '(A)') "Found xlabel in ASCII: " // trim(buffer)
            end if
            
            ! Check for plot content (axes, data points)
            if (index(buffer, '|') > 0 .or. index(buffer, '-') > 0 .or. index(buffer, '+') > 0) then
                has_plot_content = .true.
            end if
        end do
        
        close(ascii_unit)
        
        write(output_unit, '(A,I0)') "ASCII content lines: ", content_lines
        
        if (.not. has_plot_content) then
            write(error_unit, '(A)') "CRITICAL: No ASCII plot content generated"
            call exit(1)
        end if
        
        if (.not. ascii_xlabel_found) then
            write(error_unit, '(A)') "FAIL: xlabel missing from ASCII output"
            write(error_unit, '(A)') "This confirms render_axis_labels() stub issue in ASCII backend"
            call exit(1)
        end if
        
        write(output_unit, '(A)') "PASS: ASCII xlabel rendering validated"
    end subroutine test_ascii_xlabel_content

    subroutine test_png_xlabel_validation()
        !! GIVEN: Figure with xlabel for PNG output
        !! WHEN: Saved as .png file
        !! THEN: PNG file should be created (content validation requires image parsing)
        
        type(figure_t) :: png_fig
        character(len=*), parameter :: png_file = "test_png_xlabel_backend.png"
        character(len=*), parameter :: png_xlabel = "PNG Backend X-axis"
        logical :: png_exists
        integer :: png_size
        
        write(output_unit, '(A)') "--- Testing PNG backend xlabel rendering ---"
        
        call png_fig%initialize(400, 300)
        call png_fig%add_plot(x_test, y_test, label="png_data")
        call png_fig%set_xlabel(png_xlabel)
        
        write(output_unit, '(A)') "GIVEN: PNG figure with xlabel = '" // png_xlabel // "'"
        
        call png_fig%save(png_file)
        write(output_unit, '(A)') "WHEN: Saved to " // png_file
        
        inquire(file=png_file, exist=png_exists, size=png_size)
        
        if (.not. png_exists) then
            write(error_unit, '(A)') "CRITICAL: PNG file not created"
            call exit(1)
        end if
        
        if (png_size < 1000) then
            write(error_unit, '(A,I0)') "SUSPICIOUS: PNG file too small (bytes): ", png_size
            write(error_unit, '(A)') "This may indicate incomplete rendering"
        end if
        
        write(output_unit, '(A,I0)') "PNG file size: ", png_size, " bytes"
        
        ! Note: Actual xlabel text validation in PNG requires image parsing
        ! This test confirms PNG backend activation and file generation
        write(output_unit, '(A)') "PASS: PNG file created (xlabel content requires image analysis)"
        
        ! Test PNG backend file header validation
        call validate_png_header(png_file)
    end subroutine test_png_xlabel_validation

    subroutine test_pdf_xlabel_verification()
        !! GIVEN: Figure with xlabel for PDF output
        !! WHEN: Saved as .pdf file
        !! THEN: PDF should contain xlabel text objects
        
        type(figure_t) :: pdf_fig
        character(len=*), parameter :: pdf_file = "test_pdf_xlabel_backend.pdf"
        character(len=*), parameter :: pdf_xlabel = "PDF Backend X-axis Label"
        logical :: pdf_exists, xlabel_in_pdf
        integer :: pdf_size, pdf_unit, io_stat
        character(len=1000) :: buffer
        
        write(output_unit, '(A)') "--- Testing PDF backend xlabel rendering ---"
        
        call pdf_fig%initialize(400, 300)
        call pdf_fig%add_plot(x_test, y_test, label="pdf_data")
        call pdf_fig%set_xlabel(pdf_xlabel)
        
        write(output_unit, '(A)') "GIVEN: PDF figure with xlabel = '" // pdf_xlabel // "'"
        
        call pdf_fig%save(pdf_file)
        write(output_unit, '(A)') "WHEN: Saved to " // pdf_file
        
        inquire(file=pdf_file, exist=pdf_exists, size=pdf_size)
        
        if (.not. pdf_exists) then
            write(error_unit, '(A)') "CRITICAL: PDF file not created"
            call exit(1)
        end if
        
        write(output_unit, '(A,I0)') "PDF file size: ", pdf_size, " bytes"
        
        ! Search PDF content for xlabel text
        xlabel_in_pdf = .false.
        open(newunit=pdf_unit, file=pdf_file, status='old', action='read', iostat=io_stat)
        
        if (io_stat == 0) then
            do
                read(pdf_unit, '(A)', iostat=io_stat) buffer
                if (io_stat /= 0) exit
                
                ! Look for xlabel text in PDF content
                if (index(buffer, pdf_xlabel) > 0) then
                    xlabel_in_pdf = .true.
                    write(output_unit, '(A)') "Found xlabel in PDF content"
                    exit
                end if
            end do
            close(pdf_unit)
        end if
        
        if (.not. xlabel_in_pdf) then
            write(error_unit, '(A)') "FAIL: xlabel not found in PDF content"
            write(error_unit, '(A)') "This confirms render_axis_labels() stub issue affects PDF backend"
            call exit(1)
        end if
        
        write(output_unit, '(A)') "PASS: PDF xlabel rendering validated"
    end subroutine test_pdf_xlabel_verification

    subroutine test_backend_switching_xlabel()
        !! GIVEN: Figure with xlabel
        !! WHEN: Saved to different format files (backend switching)
        !! THEN: xlabel should persist across backend switches
        
        type(figure_t) :: switch_fig
        character(len=*), parameter :: switch_xlabel = "Backend Switch Label"
        character(len=*), parameter :: switch_files(3) = [ &
            "test_switch_1.txt", &
            "test_switch_2.png", &
            "test_switch_3.pdf"  &
        ]
        integer :: i
        logical :: file_exists
        
        write(output_unit, '(A)') "--- Testing backend switching with xlabel ---"
        
        call switch_fig%initialize(400, 300)
        call switch_fig%add_plot(x_test, y_test, label="switch_data")
        call switch_fig%set_xlabel(switch_xlabel)
        
        write(output_unit, '(A)') "GIVEN: Figure with xlabel = '" // switch_xlabel // "'"
        
        ! Test saving to different formats (backend switching)
        do i = 1, size(switch_files)
            call switch_fig%save(switch_files(i))
            
            inquire(file=switch_files(i), exist=file_exists)
            
            if (.not. file_exists) then
                write(error_unit, '(A)') "FAIL: Backend switch failed for " // switch_files(i)
                call exit(1)
            end if
            
            write(output_unit, '(A)') "Backend switch successful: " // switch_files(i)
        end do
        
        ! Validate xlabel persistence by checking ASCII output
        call validate_xlabel_persistence_ascii(switch_files(1), switch_xlabel)
        
        write(output_unit, '(A)') "PASS: Backend switching with xlabel persistence"
    end subroutine test_backend_switching_xlabel

    subroutine validate_png_header(png_filename)
        !! Validate PNG file has proper header
        character(len=*), intent(in) :: png_filename
        integer :: png_unit, io_stat
        character(len=8) :: png_header
        character(len=8), parameter :: expected_png = char(137) // "PNG" // char(13) // char(10) // char(26) // char(10)
        
        open(newunit=png_unit, file=png_filename, status='old', action='read', &
             access='stream', iostat=io_stat)
        
        if (io_stat == 0) then
            read(png_unit, iostat=io_stat) png_header
            close(png_unit)
            
            if (png_header == expected_png) then
                write(output_unit, '(A)') "PNG header validation: PASS"
            else
                write(error_unit, '(A)') "PNG header validation: FAIL (invalid format)"
            end if
        end if
    end subroutine validate_png_header

    subroutine validate_xlabel_persistence_ascii(ascii_filename, expected_xlabel)
        !! Validate xlabel persists in ASCII format after backend switching
        character(len=*), intent(in) :: ascii_filename, expected_xlabel
        logical :: xlabel_persists
        integer :: ascii_unit, io_stat
        character(len=1000) :: buffer
        
        xlabel_persists = .false.
        open(newunit=ascii_unit, file=ascii_filename, status='old', action='read', iostat=io_stat)
        
        if (io_stat == 0) then
            do
                read(ascii_unit, '(A)', iostat=io_stat) buffer
                if (io_stat /= 0) exit
                
                if (index(buffer, expected_xlabel) > 0) then
                    xlabel_persists = .true.
                    exit
                end if
            end do
            close(ascii_unit)
        end if
        
        if (.not. xlabel_persists) then
            write(error_unit, '(A)') "FAIL: xlabel lost during backend switching"
            call exit(1)
        end if
        
        write(output_unit, '(A)') "xlabel persistence after backend switching: PASS"
    end subroutine validate_xlabel_persistence_ascii

end program test_xlabel_backend_validation
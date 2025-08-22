program test_unicode_png_pdf_rendering
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_security, only: get_test_output_path
    implicit none
    
    call test_unicode_png_rendering()
    call test_unicode_pdf_rendering()
    call test_comprehensive_unicode_backends()
    
    print *, "All Unicode PNG/PDF rendering tests passed!"
    
contains

    subroutine test_unicode_png_rendering()
        type(figure_t) :: fig
        character(len=100) :: test_filename
        logical :: file_created
        
        ! Create figure with PNG backend
        call fig%initialize(640, 480)
        
        ! Add plot with Unicode in various places
        call fig%set_title("Plot of \alpha vs \beta")
        call fig%set_xlabel("Angle \theta (rad)")
        call fig%set_ylabel("Energy \epsilon (J)")
        call fig%add_plot([1.0_wp, 2.0_wp, 3.0_wp], [1.0_wp, 4.0_wp, 9.0_wp], label="\gamma values")
        call fig%legend("upper right")
        
        ! Save to PNG file
        test_filename = get_test_output_path("/tmp/test_unicode_png.png")
        call fig%savefig(test_filename)
        
        ! Check that file was created
        inquire(file=test_filename, exist=file_created)
        
        if (.not. file_created) then
            print *, "ERROR: PNG file not created"
            stop 1
        end if
        
        print *, "test_unicode_png_rendering: PASSED"
    end subroutine
    
    subroutine test_unicode_pdf_rendering()
        type(figure_t) :: fig
        character(len=100) :: test_filename
        logical :: file_created
        
        ! Create figure with PDF backend
        call fig%initialize(640, 480)
        
        ! Add plot with Unicode in various places
        call fig%set_title("Graph: \Omega = 2\pi f")
        call fig%set_xlabel("Frequency \nu (Hz)")
        call fig%set_ylabel("Amplitude \psi")
        call fig%add_plot([1.0_wp, 2.0_wp, 3.0_wp], [2.0_wp, 3.0_wp, 1.0_wp], label="\lambda wave")
        call fig%legend("upper left")
        
        ! Save to PDF file
        test_filename = get_test_output_path("/tmp/test_unicode_pdf.pdf")
        call fig%savefig(test_filename)
        
        ! Check that file was created
        inquire(file=test_filename, exist=file_created)
        
        if (.not. file_created) then
            print *, "ERROR: PDF file not created"
            stop 1
        end if
        
        print *, "test_unicode_pdf_rendering: PASSED"
    end subroutine
    
    subroutine test_comprehensive_unicode_backends()
        type(figure_t) :: fig
        character(len=100) :: png_file, pdf_file, ascii_file
        logical :: png_exists, pdf_exists, ascii_exists
        
        ! Test the same content across all backends
        png_file = get_test_output_path("/tmp/test_all_backends.png")
        pdf_file = get_test_output_path("/tmp/test_all_backends.pdf")
        ascii_file = get_test_output_path("/tmp/test_all_backends.txt")
        
        ! Create figure with comprehensive Unicode content
        call fig%initialize(800, 600)
        call fig%set_title("Mathematical Functions: \alpha\beta\gamma\delta")
        call fig%set_xlabel("Variable \xi")
        call fig%set_ylabel("Function \zeta(\xi)")
        
        ! Add multiple plots with Greek letters
        call fig%add_plot([1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp], [1.0_wp, 2.0_wp, 4.0_wp, 8.0_wp], label="\alpha(x)")
        call fig%add_plot([1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp], [2.0_wp, 3.0_wp, 5.0_wp, 9.0_wp], label="\beta(x)")
        call fig%add_plot([1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp], [0.5_wp, 1.5_wp, 3.5_wp, 7.5_wp], label="\gamma(x)")
        call fig%legend("upper left")
        
        ! Save to all three backends
        call fig%savefig(png_file)
        call fig%savefig(pdf_file)
        call fig%savefig(ascii_file)
        
        ! Verify all files were created
        inquire(file=png_file, exist=png_exists)
        inquire(file=pdf_file, exist=pdf_exists)
        inquire(file=ascii_file, exist=ascii_exists)
        
        if (.not. png_exists) then
            print *, "ERROR: Comprehensive PNG file not created"
            stop 1
        end if
        
        if (.not. pdf_exists) then
            print *, "ERROR: Comprehensive PDF file not created"
            stop 1
        end if
        
        if (.not. ascii_exists) then
            print *, "ERROR: Comprehensive ASCII file not created"
            stop 1
        end if
        
        print *, "test_comprehensive_unicode_backends: PASSED"
    end subroutine

end program test_unicode_png_pdf_rendering
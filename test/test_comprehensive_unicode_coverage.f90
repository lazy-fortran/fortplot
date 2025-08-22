program test_comprehensive_unicode_coverage
    use fortplot
    use fortplot_latex_parser
    use fortplot_unicode
    use fortplot_security, only: get_test_output_path
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    call test_all_greek_letters_mapping()
    call test_edge_case_latex_commands()
    call test_mixed_latex_ascii_unicode()
    call test_invalid_latex_handling()
    call test_unicode_in_all_text_elements()
    call test_escaped_characters()
    call test_complex_mathematical_expressions()
    
    print *, "All comprehensive Unicode coverage tests passed!"
    
contains

    subroutine test_all_greek_letters_mapping()
        character(len=20), parameter :: lowercase_letters(24) = [ &
            "alpha   ", "beta    ", "gamma   ", "delta   ", &
            "epsilon ", "zeta    ", "eta     ", "theta   ", &
            "iota    ", "kappa   ", "lambda  ", "mu      ", &
            "nu      ", "xi      ", "omicron ", "pi      ", &
            "rho     ", "sigma   ", "tau     ", "upsilon ", &
            "phi     ", "chi     ", "psi     ", "omega   " ]
        character(len=20), parameter :: uppercase_letters(24) = [ &
            "Alpha   ", "Beta    ", "Gamma   ", "Delta   ", &
            "Epsilon ", "Zeta    ", "Eta     ", "Theta   ", &
            "Iota    ", "Kappa   ", "Lambda  ", "Mu      ", &
            "Nu      ", "Xi      ", "Omicron ", "Pi      ", &
            "Rho     ", "Sigma   ", "Tau     ", "Upsilon ", &
            "Phi     ", "Chi     ", "Psi     ", "Omega   " ]
        character(len=20) :: unicode_char
        logical :: success
        integer :: i
        
        ! Test all lowercase Greek letters
        do i = 1, 24
            call latex_to_unicode(trim(lowercase_letters(i)), unicode_char, success)
            if (.not. success) then
                print *, "ERROR: Failed to map lowercase", trim(lowercase_letters(i))
                stop 1
            end if
        end do
        
        ! Test all uppercase Greek letters
        do i = 1, 24
            call latex_to_unicode(trim(uppercase_letters(i)), unicode_char, success)
            if (.not. success) then
                print *, "ERROR: Failed to map uppercase", trim(uppercase_letters(i))
                stop 1
            end if
        end do
        
        print *, "test_all_greek_letters_mapping: PASSED"
    end subroutine
    
    subroutine test_edge_case_latex_commands()
        character(len=200) :: result_text
        integer :: result_len
        
        ! Empty string
        call process_latex_in_text("", result_text, result_len)
        if (result_len /= 0) then
            print *, "ERROR: Empty string should produce empty result"
            stop 1
        end if
        
        ! Single backslash
        call process_latex_in_text("\", result_text, result_len)
        if (result_len /= 1 .or. result_text(1:1) /= "\") then
            print *, "ERROR: Single backslash not handled correctly"
            stop 1
        end if
        
        ! Incomplete command
        call process_latex_in_text("\alph", result_text, result_len)
        if (result_text(1:result_len) /= "\alph") then
            print *, "ERROR: Incomplete command should remain unchanged"
            stop 1
        end if
        
        ! Command at end of string
        call process_latex_in_text("End with \omega", result_text, result_len)
        if (index(result_text(1:result_len), "ω") == 0) then
            print *, "ERROR: Command at end not processed"
            stop 1
        end if
        
        print *, "test_edge_case_latex_commands: PASSED"
    end subroutine
    
    subroutine test_mixed_latex_ascii_unicode()
        character(len=200) :: result_text
        integer :: result_len
        logical :: has_unicode, has_ascii
        
        ! Mix LaTeX, ASCII, and existing Unicode
        call process_latex_in_text("Mix: ASCII \alpha existing α more \beta", result_text, result_len)
        
        has_unicode = contains_unicode(result_text(1:result_len))
        has_ascii = index(result_text(1:result_len), "ASCII") > 0
        
        if (.not. has_unicode) then
            print *, "ERROR: Mixed text should contain Unicode"
            stop 1
        end if
        
        if (.not. has_ascii) then
            print *, "ERROR: ASCII text should be preserved"
            stop 1
        end if
        
        print *, "test_mixed_latex_ascii_unicode: PASSED"
    end subroutine
    
    subroutine test_invalid_latex_handling()
        character(len=200) :: result_text
        integer :: result_len
        
        ! Invalid command should remain unchanged
        call process_latex_in_text("Invalid \notgreek command", result_text, result_len)
        if (index(result_text(1:result_len), "\notgreek") == 0) then
            print *, "ERROR: Invalid command should remain in output"
            stop 1
        end if
        
        ! Mixed valid and invalid
        call process_latex_in_text("\alpha \invalid \beta", result_text, result_len)
        if (index(result_text(1:result_len), "\invalid") == 0) then
            print *, "ERROR: Invalid command should be preserved"
            stop 1
        end if
        if (index(result_text(1:result_len), "α") == 0 .or. index(result_text(1:result_len), "β") == 0) then
            print *, "ERROR: Valid commands should be processed"
            stop 1
        end if
        
        print *, "test_invalid_latex_handling: PASSED"
    end subroutine
    
    subroutine test_unicode_in_all_text_elements()
        type(figure_t) :: fig
        character(len=100) :: test_filename
        logical :: file_exists
        
        ! Test Unicode in every possible text element
        call fig%initialize(640, 480)
        
        ! Title with complex mathematical expression
        call fig%set_title("Schrödinger: i\hbar\partial\psi/\partial t = H\psi")
        
        ! Axis labels with Greek letters
        call fig%set_xlabel("Wavelength \lambda (nm)")
        call fig%set_ylabel("Intensity \Phi (\mu W/cm²)")
        
        ! Multiple plots with Greek letter labels
        call fig%add_plot([1.0_wp, 2.0_wp, 3.0_wp], [1.0_wp, 2.0_wp, 3.0_wp], label="\alpha radiation")
        call fig%add_plot([1.0_wp, 2.0_wp, 3.0_wp], [1.5_wp, 2.5_wp, 3.5_wp], label="\beta decay")
        call fig%add_plot([1.0_wp, 2.0_wp, 3.0_wp], [0.5_wp, 1.5_wp, 2.5_wp], label="\gamma emission")
        
        call fig%legend("upper right")
        
        ! Test in all backends
        test_filename = get_test_output_path("/tmp/test_all_elements_unicode.png")
        call fig%savefig(test_filename)
        inquire(file=test_filename, exist=file_exists)
        if (.not. file_exists) then
            print *, "ERROR: PNG with all Unicode elements not created"
            stop 1
        end if
        
        test_filename = get_test_output_path("/tmp/test_all_elements_unicode.pdf")
        call fig%savefig(test_filename)
        inquire(file=test_filename, exist=file_exists)
        if (.not. file_exists) then
            print *, "ERROR: PDF with all Unicode elements not created"
            stop 1
        end if
        
        test_filename = get_test_output_path("/tmp/test_all_elements_unicode.txt")
        call fig%savefig(test_filename)
        inquire(file=test_filename, exist=file_exists)
        if (.not. file_exists) then
            print *, "ERROR: ASCII with all Unicode elements not created"
            stop 1
        end if
        
        print *, "test_unicode_in_all_text_elements: PASSED"
    end subroutine
    
    subroutine test_escaped_characters()
        character(len=200) :: result_text
        integer :: result_len
        
        ! Note: In Fortran strings, \ needs to be escaped as \\
        ! But for LaTeX processing, we're looking for literal backslash followed by dollar
        
        ! Test that processing doesn't interfere with regular text
        call process_latex_in_text("Regular text with symbols: $ and \alpha", result_text, result_len)
        if (index(result_text(1:result_len), "α") == 0) then
            print *, "ERROR: Alpha should be converted"
            stop 1
        end if
        if (index(result_text(1:result_len), "$") == 0) then
            print *, "ERROR: Dollar sign should be preserved"
            stop 1
        end if
        
        print *, "test_escaped_characters: PASSED"
    end subroutine
    
    subroutine test_complex_mathematical_expressions()
        character(len=300) :: complex_expr, result_text
        integer :: result_len
        logical :: all_converted
        
        ! Complex mathematical expression with multiple Greek letters
        complex_expr = "Maxwell equations: \nabla \cdot E = \rho/\epsilon, " // &
                      "\nabla \times B - \mu\epsilon \partial E/\partial t = \mu J"
        
        call process_latex_in_text(complex_expr, result_text, result_len)
        
        ! Check that recognized Greek letters are converted
        all_converted = index(result_text(1:result_len), "ρ") > 0 .and. &
                       index(result_text(1:result_len), "ε") > 0 .and. &
                       index(result_text(1:result_len), "μ") > 0
        
        if (.not. all_converted) then
            print *, "ERROR: Not all Greek letters converted in complex expression"
            stop 1
        end if
        
        ! Verify unrecognized commands remain unchanged
        if (index(result_text(1:result_len), "\nabla") == 0 .or. &
            index(result_text(1:result_len), "\cdot") == 0 .or. &
            index(result_text(1:result_len), "\times") == 0 .or. &
            index(result_text(1:result_len), "\partial") == 0) then
            print *, "ERROR: Unrecognized commands should remain unchanged"
            stop 1
        end if
        
        print *, "test_complex_mathematical_expressions: PASSED"
    end subroutine

end program test_comprehensive_unicode_coverage
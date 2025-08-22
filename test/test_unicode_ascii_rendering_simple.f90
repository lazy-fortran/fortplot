program test_unicode_ascii_rendering_simple
    use fortplot
    use fortplot_security, only: get_test_output_path
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    call test_basic_unicode_rendering()
    call test_title_with_unicode()
    call test_mixed_text_rendering()
    
    print *, "All simplified Unicode ASCII rendering tests passed!"
    
contains

    subroutine test_basic_unicode_rendering()
        type(figure_t) :: fig
        character(len=100) :: test_filename
        logical :: contains_unicode
        
        ! Create figure with ASCII backend
        call fig%initialize(80, 24, backend="ascii")
        
        ! Add plot with Unicode in label
        call fig%add_plot([1.0_wp, 2.0_wp, 3.0_wp], [1.0_wp, 4.0_wp, 9.0_wp], label="\alpha values")
        call figure_legend(fig, "upper right")
        
        ! Save to temporary file and read it back
        test_filename = get_test_output_path("/tmp/test_basic_unicode.txt")
        call figure_savefig(fig, test_filename)
        
        ! Check file content for Unicode rendering
        contains_unicode = check_file_contains_unicode(test_filename)
        
        if (.not. contains_unicode) then
            print *, "ERROR: Alpha character not found in ASCII output"
            stop 1
        end if
        
        print *, "test_basic_unicode_rendering: PASSED"
    end subroutine
    
    subroutine test_title_with_unicode()
        type(figure_t) :: fig
        character(len=100) :: test_filename
        logical :: contains_unicode
        
        call fig%initialize(80, 24, backend="ascii")
        call fig%set_title("Graph of \alpha vs \beta")
        call fig%add_plot([1.0_wp, 2.0_wp], [1.0_wp, 2.0_wp])
        
        test_filename = get_test_output_path("/tmp/test_title_unicode.txt")
        call figure_savefig(fig, test_filename)
        
        contains_unicode = check_file_contains_unicode(test_filename)
        
        if (.not. contains_unicode) then
            print *, "ERROR: Unicode not found in title"
            stop 1
        end if
        
        print *, "test_title_with_unicode: PASSED"
    end subroutine
    
    subroutine test_mixed_text_rendering()
        type(figure_t) :: fig
        character(len=100) :: test_filename
        logical :: contains_unicode
        
        call fig%initialize(80, 24, backend="ascii")
        call fig%set_xlabel("Angle \theta (rad)")
        call fig%set_ylabel("Energy \epsilon (J)")
        call fig%add_plot([1.0_wp, 2.0_wp], [1.0_wp, 2.0_wp], label="Data \gamma")
        call figure_legend(fig, "upper right")
        
        test_filename = get_test_output_path("/tmp/test_mixed_unicode.txt")
        call figure_savefig(fig, test_filename)
        
        contains_unicode = check_file_contains_unicode(test_filename)
        
        if (.not. contains_unicode) then
            print *, "ERROR: Unicode not found in mixed text"
            stop 1
        end if
        
        print *, "test_mixed_text_rendering: PASSED"
    end subroutine
    
    logical function check_file_contains_unicode(filename)
        character(len=*), intent(in) :: filename
        character(len=1000) :: line
        integer :: unit, ios
        logical :: found_unicode
        
        found_unicode = .false.
        open(newunit=unit, file=filename, status='old', iostat=ios)
        
        if (ios == 0) then
            do
                read(unit, '(A)', iostat=ios) line
                if (ios /= 0) exit
                
                ! Check for Unicode characters
                if (index(line, "α") > 0 .or. index(line, "β") > 0 .or. &
                    index(line, "γ") > 0 .or. index(line, "θ") > 0 .or. &
                    index(line, "π") > 0 .or. index(line, "ε") > 0) then
                    found_unicode = .true.
                    exit
                end if
            end do
            close(unit)
        end if
        
        check_file_contains_unicode = found_unicode
    end function check_file_contains_unicode

end program test_unicode_ascii_rendering_simple
module fortplot_pdf_text
    !! PDF text rendering operations
    !! Handles text output, font management, and special character support
    
    use iso_fortran_env, only: wp => real64
    use fortplot_pdf_core, only: pdf_context_core, PDF_FONT_SIZE, PDF_LABEL_SIZE, &
                                PDF_TICK_LABEL_SIZE, PDF_TITLE_SIZE
    implicit none
    private
    
    ! Public procedures
    public :: draw_pdf_text
    public :: draw_pdf_text_direct
    public :: draw_pdf_text_bold
    public :: draw_mixed_font_text
    public :: draw_rotated_mixed_font_text
    public :: escape_pdf_string
    public :: unicode_to_symbol_char
    public :: unicode_codepoint_to_pdf_escape
    
    ! Symbol font mappings
    integer, parameter :: SYMBOL_ALPHA = 97      ! α (hex 61)
    integer, parameter :: SYMBOL_BETA = 98       ! β (hex 62)
    integer, parameter :: SYMBOL_GAMMA = 103     ! γ (hex 67)
    integer, parameter :: SYMBOL_DELTA = 100     ! δ (hex 64)
    integer, parameter :: SYMBOL_EPSILON = 101   ! ε (hex 65)
    integer, parameter :: SYMBOL_ZETA = 122      ! ζ (hex 7A)
    integer, parameter :: SYMBOL_ETA = 104       ! η (hex 68)
    integer, parameter :: SYMBOL_THETA = 113     ! θ (hex 71)
    integer, parameter :: SYMBOL_IOTA = 105      ! ι (hex 69)
    integer, parameter :: SYMBOL_KAPPA = 107     ! κ (hex 6B)
    integer, parameter :: SYMBOL_LAMBDA = 108    ! λ (hex 6C)
    integer, parameter :: SYMBOL_MU = 109        ! μ (hex 6D)
    integer, parameter :: SYMBOL_NU = 110        ! ν (hex 6E)
    integer, parameter :: SYMBOL_XI = 120        ! ξ (hex 78)
    integer, parameter :: SYMBOL_OMICRON = 111   ! ο (hex 6F)
    integer, parameter :: SYMBOL_PI = 112        ! π (hex 70)
    integer, parameter :: SYMBOL_RHO = 114       ! ρ (hex 72)
    integer, parameter :: SYMBOL_SIGMA = 115     ! σ (hex 73)
    integer, parameter :: SYMBOL_TAU = 116       ! τ (hex 74)
    integer, parameter :: SYMBOL_UPSILON = 117   ! υ (hex 75)
    integer, parameter :: SYMBOL_PHI = 102       ! φ (hex 66)
    integer, parameter :: SYMBOL_CHI = 99        ! χ (hex 63)
    integer, parameter :: SYMBOL_PSI = 121       ! ψ (hex 79)
    integer, parameter :: SYMBOL_OMEGA = 119     ! ω (hex 77)

contains

    subroutine draw_pdf_text(this, x, y, text)
        class(pdf_context_core), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: escaped_text
        character(len=256) :: text_cmd
        integer :: escaped_len
        
        allocate(character(len=len(text)*6) :: escaped_text)
        call escape_pdf_string(text, escaped_text, escaped_len)
        
        ! Begin text object
        this%stream_data = this%stream_data // "BT" // new_line('a')
        
        ! Set font (Helvetica)
        write(text_cmd, '("/F", I0, 1X, F0.1, " Tf")') &
            this%fonts%get_helvetica_obj(), PDF_FONT_SIZE
        this%stream_data = this%stream_data // trim(adjustl(text_cmd)) // new_line('a')
        
        ! Position text using absolute positioning matrix
        ! Use identity transform with translation (x,y)
        write(text_cmd, '("1 0 0 1 ", F0.3, 1X, F0.3, " Tm")') x, y
        this%stream_data = this%stream_data // trim(adjustl(text_cmd)) // new_line('a')
        
        ! Show text
        this%stream_data = this%stream_data // "(" // escaped_text(1:escaped_len) // ") Tj" // new_line('a')
        
        ! End text object
        this%stream_data = this%stream_data // "ET" // new_line('a')
    end subroutine draw_pdf_text

    subroutine draw_mixed_font_text(this, x, y, text)
        !! Draw text with mixed fonts (Helvetica and Symbol)
        class(pdf_context_core), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        character(len=1024) :: text_cmd
        logical :: in_symbol_font
        integer :: i, text_pos, segment_pos
        character(len=256) :: current_segment
        
        in_symbol_font = .false.
        text_pos = 1
        segment_pos = 0
        current_segment = ""
        
        ! Begin text object
        this%stream_data = this%stream_data // "BT" // new_line('a')
        
        ! Select default font (Helvetica) for label-sized text
        write(text_cmd, '("/F", I0, 1X, F0.1, " Tf")') &
            this%fonts%get_helvetica_obj(), PDF_LABEL_SIZE
        this%stream_data = this%stream_data // trim(adjustl(text_cmd)) // new_line('a')

        ! Set initial absolute position
        write(text_cmd, '("1 0 0 1 ", F0.3, 1X, F0.3, " Tm")') x, y
        this%stream_data = this%stream_data // trim(adjustl(text_cmd)) // new_line('a')
        
        ! Process text character by character
        call process_text_segments(this, text, in_symbol_font)
        
        ! End text object
        this%stream_data = this%stream_data // "ET" // new_line('a')
    end subroutine draw_mixed_font_text

    subroutine draw_rotated_mixed_font_text(this, x, y, text)
        !! Draw rotated text with mixed fonts (for Y-axis labels)
        class(pdf_context_core), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        character(len=256) :: font_cmd
        
        ! Begin text object
        this%stream_data = this%stream_data // "BT" // new_line('a')
        
        ! Select default font (Helvetica) for label-sized text
        write(font_cmd, '("/F", I0, 1X, F0.1, " Tf")') &
            this%fonts%get_helvetica_obj(), PDF_LABEL_SIZE
        this%stream_data = this%stream_data // trim(adjustl(font_cmd)) // new_line('a')

        ! Set up rotation matrix
        call setup_rotated_text_matrix(this, x, y)
        
        ! Process rotated text segments
        call process_rotated_text_segments(this, text)
        
        ! End text object
        this%stream_data = this%stream_data // "ET" // new_line('a')
    end subroutine draw_rotated_mixed_font_text

    subroutine setup_rotated_text_matrix(this, x, y)
        class(pdf_context_core), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=256) :: matrix_cmd
        
        ! Set text matrix for 90-degree rotation
        ! Matrix: [cos(90) sin(90) -sin(90) cos(90) x y] = [0 1 -1 0 x y]
        write(matrix_cmd, '("0 1 -1 0 ", F0.3, 1X, F0.3, " Tm")') x, y
        this%stream_data = this%stream_data // trim(adjustl(matrix_cmd)) // new_line('a')
    end subroutine setup_rotated_text_matrix

    subroutine process_text_segments(this, text, in_symbol_font)
        !! Process text segments for mixed font rendering
        class(pdf_context_core), intent(inout) :: this
        character(len=*), intent(in) :: text
        logical, intent(inout) :: in_symbol_font
        integer :: i, codepoint
        character(len=4) :: utf_char
        character(len=8) :: symbol_char
        
        do i = 1, len_trim(text)
            ! Get Unicode codepoint for character
            codepoint = iachar(text(i:i))
            
            ! Check if character needs Symbol font
            call unicode_to_symbol_char(codepoint, symbol_char)
            
            if (len_trim(symbol_char) > 0) then
                ! Switch to Symbol font if needed
                if (.not. in_symbol_font) then
                    call switch_to_symbol_font(this)
                    in_symbol_font = .true.
                end if
                ! Output symbol character
                this%stream_data = this%stream_data // "(" // trim(symbol_char) // ") Tj" // new_line('a')
            else
                ! Switch to Helvetica if needed
                if (in_symbol_font) then
                    call switch_to_helvetica_font(this)
                    in_symbol_font = .false.
                end if
                ! Output regular character
                this%stream_data = this%stream_data // "(" // text(i:i) // ") Tj" // new_line('a')
            end if
        end do
    end subroutine process_text_segments

    subroutine process_rotated_text_segments(this, text)
        !! Process text segments for rotated mixed font rendering
        class(pdf_context_core), intent(inout) :: this
        character(len=*), intent(in) :: text
        logical :: in_symbol_font
        
        in_symbol_font = .false.
        
        ! Process the text similarly to non-rotated version
        call process_text_segments(this, text, in_symbol_font)
    end subroutine process_rotated_text_segments

    subroutine switch_to_symbol_font(this)
        class(pdf_context_core), intent(inout) :: this
        character(len=64) :: font_cmd
        
        write(font_cmd, '("/F", I0, 1X, F0.1, " Tf")') &
            this%fonts%get_symbol_obj(), PDF_FONT_SIZE
        this%stream_data = this%stream_data // trim(adjustl(font_cmd)) // new_line('a')
    end subroutine switch_to_symbol_font

    subroutine switch_to_helvetica_font(this)
        class(pdf_context_core), intent(inout) :: this
        character(len=64) :: font_cmd
        
        write(font_cmd, '("/F", I0, 1X, F0.1, " Tf")') &
            this%fonts%get_helvetica_obj(), PDF_FONT_SIZE
        this%stream_data = this%stream_data // trim(adjustl(font_cmd)) // new_line('a')
    end subroutine switch_to_helvetica_font

    subroutine draw_pdf_text_direct(this, x, y, text)
        !! Draw text directly without escaping
        class(pdf_context_core), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        character(len=256) :: text_cmd
        
        ! Begin text object
        this%stream_data = this%stream_data // "BT" // new_line('a')
        
        ! Set font
        write(text_cmd, '("/F", I0, 1X, F0.1, " Tf")') &
            this%fonts%get_helvetica_obj(), PDF_FONT_SIZE
        this%stream_data = this%stream_data // trim(adjustl(text_cmd)) // new_line('a')
        
        ! Position and show text using absolute positioning
        write(text_cmd, '("1 0 0 1 ", F0.3, 1X, F0.3, " Tm")') x, y
        this%stream_data = this%stream_data // trim(adjustl(text_cmd)) // new_line('a')
        this%stream_data = this%stream_data // "(" // text // ") Tj" // new_line('a')
        
        ! End text object
        this%stream_data = this%stream_data // "ET" // new_line('a')
    end subroutine draw_pdf_text_direct

    subroutine draw_pdf_text_bold(this, x, y, text)
        !! Draw bold text (using line width trick)
        class(pdf_context_core), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        character(len=256) :: text_cmd
        
        ! Begin text object
        this%stream_data = this%stream_data // "BT" // new_line('a')
        
        ! Set text rendering mode to fill and stroke (bold effect)
        this%stream_data = this%stream_data // "2 Tr" // new_line('a')
        this%stream_data = this%stream_data // "0.3 w" // new_line('a')  ! Stroke width for bold
        
        ! Set font
        write(text_cmd, '("/F", I0, 1X, F0.1, " Tf")') &
            this%fonts%get_helvetica_obj(), PDF_TITLE_SIZE
        this%stream_data = this%stream_data // trim(adjustl(text_cmd)) // new_line('a')
        
        ! Position and show text using absolute positioning
        write(text_cmd, '("1 0 0 1 ", F0.3, 1X, F0.3, " Tm")') x, y
        this%stream_data = this%stream_data // trim(adjustl(text_cmd)) // new_line('a')
        this%stream_data = this%stream_data // "(" // text // ") Tj" // new_line('a')
        
        ! Reset text rendering mode
        this%stream_data = this%stream_data // "0 Tr" // new_line('a')
        
        ! End text object
        this%stream_data = this%stream_data // "ET" // new_line('a')
    end subroutine draw_pdf_text_bold

    subroutine escape_pdf_string(input, output, output_len)
        !! Escape special characters in PDF strings
        character(len=*), intent(in) :: input
        character(len=*), intent(out) :: output
        integer, intent(out) :: output_len
        integer :: i, j
        character :: ch
        
        j = 0
        do i = 1, len_trim(input)
            ch = input(i:i)
            
            ! Escape special PDF characters
            if (ch == '(' .or. ch == ')' .or. ch == '\') then
                j = j + 1
                if (j <= len(output)) output(j:j) = '\'
                j = j + 1
                if (j <= len(output)) output(j:j) = ch
            else
                j = j + 1
                if (j <= len(output)) output(j:j) = ch
            end if
        end do
        
        output_len = j
        if (j < len(output)) output(j+1:) = ' '
    end subroutine escape_pdf_string

    subroutine unicode_to_symbol_char(unicode_codepoint, symbol_char)
        !! Convert Unicode codepoint to Symbol font character
        integer, intent(in) :: unicode_codepoint
        character(len=*), intent(out) :: symbol_char
        
        symbol_char = ""
        
        ! Greek lowercase letters
        select case(unicode_codepoint)
        case(945)  ! α (alpha)
            symbol_char = "\141"
        case(946)  ! β (beta)
            symbol_char = "\142"
        case(947)  ! γ (gamma)
            symbol_char = "\147"
        case(948)  ! δ (delta)
            symbol_char = "\144"
        case(949)  ! ε (epsilon)
            symbol_char = "\145"
        case(950)  ! ζ (zeta)
            symbol_char = "\172"
        case(951)  ! η (eta)
            symbol_char = "\150"
        case(952)  ! θ (theta)
            symbol_char = "\161"
        case(953)  ! ι (iota)
            symbol_char = "\151"
        case(954)  ! κ (kappa)
            symbol_char = "\153"
        case(955)  ! λ (lambda)
            symbol_char = "\154"
        case(956)  ! μ (mu)
            symbol_char = "\155"
        case(957)  ! ν (nu)
            symbol_char = "\156"
        case(958)  ! ξ (xi)
            symbol_char = "\170"
        case(959)  ! ο (omicron)
            symbol_char = "\157"
        case(960)  ! π (pi)
            symbol_char = "\160"
        case(961)  ! ρ (rho)
            symbol_char = "\162"
        case(963)  ! σ (sigma)
            symbol_char = "\163"
        case(964)  ! τ (tau)
            symbol_char = "\164"
        case(965)  ! υ (upsilon)
            symbol_char = "\165"
        case(966)  ! φ (phi)
            symbol_char = "\146"
        case(967)  ! χ (chi)
            symbol_char = "\143"
        case(968)  ! ψ (psi)
            symbol_char = "\171"
        case(969)  ! ω (omega)
            symbol_char = "\167"
        end select
    end subroutine unicode_to_symbol_char

    subroutine unicode_codepoint_to_pdf_escape(codepoint, escape_seq)
        !! Convert Unicode codepoint to PDF escape sequence
        integer, intent(in) :: codepoint
        character(len=*), intent(out) :: escape_seq
        logical :: found
        
        escape_seq = ""
        found = .false.
        
        ! Try lowercase Greek
        if (.not. found) then
            call lookup_lowercase_greek(codepoint, escape_seq, found)
        end if
        
        ! Try uppercase Greek
        if (.not. found) then
            call lookup_uppercase_greek(codepoint, escape_seq, found)
        end if
        
        ! If not found, return empty
        if (.not. found) then
            escape_seq = ""
        end if
    end subroutine unicode_codepoint_to_pdf_escape

    subroutine lookup_lowercase_greek(codepoint, escape_seq, found)
        integer, intent(in) :: codepoint
        character(len=*), intent(out) :: escape_seq
        logical, intent(out) :: found
        
        found = .true.
        
        select case(codepoint)
        case(945)  ! α
            escape_seq = "\141"
        case(946)  ! β
            escape_seq = "\142"
        case(947)  ! γ
            escape_seq = "\147"
        case(948)  ! δ
            escape_seq = "\144"
        case(949)  ! ε
            escape_seq = "\145"
        case(950)  ! ζ
            escape_seq = "\172"
        case(951)  ! η
            escape_seq = "\150"
        case(952)  ! θ
            escape_seq = "\161"
        case(953)  ! ι
            escape_seq = "\151"
        case(954)  ! κ
            escape_seq = "\153"
        case(955)  ! λ
            escape_seq = "\154"
        case(956)  ! μ
            escape_seq = "\155"
        case(957)  ! ν
            escape_seq = "\156"
        case(958)  ! ξ
            escape_seq = "\170"
        case(959)  ! ο
            escape_seq = "\157"
        case(960)  ! π
            escape_seq = "\160"
        case(961)  ! ρ
            escape_seq = "\162"
        case(963)  ! σ
            escape_seq = "\163"
        case(964)  ! τ
            escape_seq = "\164"
        case(965)  ! υ
            escape_seq = "\165"
        case(966)  ! φ
            escape_seq = "\146"
        case(967)  ! χ
            escape_seq = "\143"
        case(968)  ! ψ
            escape_seq = "\171"
        case(969)  ! ω
            escape_seq = "\167"
        case default
            found = .false.
        end select
    end subroutine lookup_lowercase_greek

    subroutine lookup_uppercase_greek(codepoint, escape_seq, found)
        integer, intent(in) :: codepoint
        character(len=*), intent(out) :: escape_seq
        logical, intent(out) :: found
        
        found = .true.
        
        select case(codepoint)
        case(913)  ! Α
            escape_seq = "\101"
        case(914)  ! Β
            escape_seq = "\102"
        case(915)  ! Γ
            escape_seq = "\107"
        case(916)  ! Δ
            escape_seq = "\104"
        case(917)  ! Ε
            escape_seq = "\105"
        case(918)  ! Ζ
            escape_seq = "\132"
        case(919)  ! Η
            escape_seq = "\110"
        case(920)  ! Θ
            escape_seq = "\121"
        case(921)  ! Ι
            escape_seq = "\111"
        case(922)  ! Κ
            escape_seq = "\113"
        case(923)  ! Λ
            escape_seq = "\114"
        case(924)  ! Μ
            escape_seq = "\115"
        case(925)  ! Ν
            escape_seq = "\116"
        case(926)  ! Ξ
            escape_seq = "\130"
        case(927)  ! Ο
            escape_seq = "\117"
        case(928)  ! Π
            escape_seq = "\120"
        case(929)  ! Ρ
            escape_seq = "\122"
        case(931)  ! Σ
            escape_seq = "\123"
        case(932)  ! Τ
            escape_seq = "\124"
        case(933)  ! Υ
            escape_seq = "\125"
        case(934)  ! Φ
            escape_seq = "\106"
        case(935)  ! Χ
            escape_seq = "\103"
        case(936)  ! Ψ
            escape_seq = "\131"
        case(937)  ! Ω
            escape_seq = "\127"
        case default
            found = .false.
        end select
    end subroutine lookup_uppercase_greek

end module fortplot_pdf_text

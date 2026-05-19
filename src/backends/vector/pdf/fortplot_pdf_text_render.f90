module fortplot_pdf_text_render
    !! High level PDF text drawing routines

    use iso_fortran_env, only: wp => real64
    use fortplot_pdf_core, only: pdf_context_core, PDF_FONT_SIZE, &
                                 PDF_LABEL_SIZE, PDF_TITLE_SIZE
    use fortplot_pdf_text_escape, only: escape_pdf_string
    use fortplot_pdf_text_segments, only: process_text_segments, &
                                          process_rotated_text_segments, &
                                          render_mixed_font_at_position
    implicit none
    private

    public :: draw_pdf_text
    public :: draw_mixed_font_text
    public :: draw_rotated_mixed_font_text
    public :: draw_pdf_text_direct
    public :: draw_pdf_text_bold

contains

    subroutine draw_pdf_text(this, x, y, text)
        class(pdf_context_core), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: escaped_text
        character(len=1024) :: text_cmd
        integer :: escaped_len

        allocate (character(len=len(text)*6) :: escaped_text)
        call escape_pdf_string(text, escaped_text, escaped_len)

        this%stream_data = this%stream_data//'BT'//new_line('a')

        write (text_cmd, '("/F", I0, 1X, F0.1, " Tf")') &
            this%fonts%get_helvetica_obj(), PDF_FONT_SIZE
        this%stream_data = this%stream_data//trim(adjustl(text_cmd))//new_line('a')

        write (text_cmd, '("1 0 0 1 ", F0.3, 1X, F0.3, " Tm")') &
            x, y
        this%stream_data = this%stream_data//trim(adjustl(text_cmd))//new_line('a')

        this%stream_data = this%stream_data//'('//escaped_text(1:escaped_len)// &
                           ') Tj'//new_line('a')

        this%stream_data = this%stream_data//'ET'//new_line('a')
    end subroutine draw_pdf_text

    subroutine draw_mixed_font_text(this, x, y, text, font_size)
        class(pdf_context_core), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        real(wp), intent(in), optional :: font_size
        character(len=1024) :: text_cmd
        logical :: in_symbol_font
        real(wp) :: fs

        in_symbol_font = .false.
        fs = PDF_LABEL_SIZE
        if (present(font_size)) fs = font_size

        this%stream_data = this%stream_data//'BT'//new_line('a')

        write (text_cmd, '("/F", I0, 1X, F0.1, " Tf")') &
            this%fonts%get_helvetica_obj(), fs
        this%stream_data = this%stream_data//trim(adjustl(text_cmd))//new_line('a')

        write (text_cmd, '("1 0 0 1 ", F0.3, 1X, F0.3, " Tm")') &
            x, y
        this%stream_data = this%stream_data//trim(adjustl(text_cmd))//new_line('a')

        call process_text_segments(this, text, in_symbol_font, fs)

        this%stream_data = this%stream_data//'ET'//new_line('a')
    end subroutine draw_mixed_font_text

    subroutine draw_rotated_mixed_font_text(this, x, y, text, font_size, angle_deg)
        class(pdf_context_core), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        real(wp), intent(in), optional :: font_size
        real(wp), intent(in), optional :: angle_deg
        character(len=1024) :: font_cmd
        real(wp) :: fs
        real(wp) :: angle

        fs = PDF_LABEL_SIZE
        if (present(font_size)) fs = font_size
        angle = 90.0_wp
        if (present(angle_deg)) angle = angle_deg

        this%stream_data = this%stream_data//'BT'//new_line('a')

        write (font_cmd, '("/F", I0, 1X, F0.1, " Tf")') &
            this%fonts%get_helvetica_obj(), fs
        this%stream_data = this%stream_data//trim(adjustl(font_cmd))//new_line('a')

        call setup_rotated_text_matrix(this, x, y, angle)

        call process_rotated_text_segments(this, text, fs)

        this%stream_data = this%stream_data//'ET'//new_line('a')
    end subroutine draw_rotated_mixed_font_text

    subroutine draw_pdf_text_direct(this, x, y, text)
        class(pdf_context_core), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        character(len=1024) :: text_cmd

        this%stream_data = this%stream_data//'BT'//new_line('a')

        write (text_cmd, '("/F", I0, 1X, F0.1, " Tf")') &
            this%fonts%get_helvetica_obj(), PDF_FONT_SIZE
        this%stream_data = this%stream_data//trim(adjustl(text_cmd))//new_line('a')

        write (text_cmd, '("1 0 0 1 ", F0.3, 1X, F0.3, " Tm")') &
            x, y
        this%stream_data = this%stream_data//trim(adjustl(text_cmd))//new_line('a')
        this%stream_data = this%stream_data//'('//text//') Tj'//new_line('a')

        this%stream_data = this%stream_data//'ET'//new_line('a')
    end subroutine draw_pdf_text_direct

    subroutine draw_pdf_text_bold(this, x, y, text)
        class(pdf_context_core), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        character(len=1024) :: text_cmd
        character(len=:), allocatable :: escaped_text
        integer :: escaped_len

        this%stream_data = this%stream_data//'BT'//new_line('a')
        this%stream_data = this%stream_data//'2 Tr'//new_line('a')
        this%stream_data = this%stream_data//'0.3 w'//new_line('a')

        write (text_cmd, '("/F", I0, 1X, F0.1, " Tf")') &
            this%fonts%get_helvetica_obj(), PDF_TITLE_SIZE
        this%stream_data = this%stream_data//trim(adjustl(text_cmd))//new_line('a')

        allocate (character(len=len(text)*6) :: escaped_text)
        call escape_pdf_string(text, escaped_text, escaped_len)

        write (text_cmd, '("1 0 0 1 ", F0.3, 1X, F0.3, " Tm")') &
            x, y
        this%stream_data = this%stream_data//trim(adjustl(text_cmd))// &
                           new_line('a')
        this%stream_data = this%stream_data//'('// &
                           escaped_text(1:escaped_len)//') Tj'//new_line('a')

        this%stream_data = this%stream_data//'0 Tr'//new_line('a')
        this%stream_data = this%stream_data//'ET'//new_line('a')
    end subroutine draw_pdf_text_bold

    subroutine setup_rotated_text_matrix(this, x, y, angle_deg)
        class(pdf_context_core), intent(inout) :: this
        real(wp), intent(in) :: x, y
        real(wp), intent(in) :: angle_deg
        character(len=256) :: matrix_cmd
        real(wp) :: a, b, c, d, theta

        theta = angle_deg*acos(-1.0_wp)/180.0_wp
        a = cos(theta)
        b = sin(theta)
        c = -sin(theta)
        d = cos(theta)

        write (matrix_cmd, '(4(F0.6,1X),F0.3,1X,F0.3," Tm")') &
            a, b, c, d, x, y
        this%stream_data = this%stream_data//trim(adjustl(matrix_cmd))// &
                           new_line('a')
    end subroutine setup_rotated_text_matrix

end module fortplot_pdf_text_render

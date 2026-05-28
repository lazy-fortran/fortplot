module fortplot_pdf_axes_text
    !! PDF axis text rendering module
    !!
    !! Handles title, axis labels, and mixed-font text rendering with mathtext support.

    use iso_fortran_env, only: wp => real64
    use fortplot_pdf_core, only: pdf_context_core, PDF_LABEL_SIZE, PDF_TITLE_SIZE, &
                                 PDF_TICK_LABEL_SIZE
    use fortplot_constants, only: AXIS_LABEL_PAD_PT
    use fortplot_pdf_text, only: draw_pdf_text, draw_pdf_text_bold, &
                                 draw_mixed_font_text, draw_rotated_mixed_font_text, &
                                 draw_pdf_mathtext, estimate_pdf_text_width
    use fortplot_text_helpers, only: prepare_mathtext_if_needed
    use fortplot_text_layout, only: has_mathtext, preprocess_math_text
    use fortplot_latex_parser, only: process_latex_in_text
    use fortplot_mathtext, only: mathtext_element_t, parse_mathtext
    use fortplot_pdf_mathtext_render, only: render_mathtext_element_pdf
    use fortplot_unicode, only: utf8_char_length, utf8_to_codepoint
    implicit none
    private

    ! Public procedures
    public :: draw_pdf_title_and_labels
    public :: render_mixed_text
    public :: render_rotated_mixed_text

contains

    subroutine draw_pdf_title_and_labels(ctx, title, xlabel, ylabel, &
                                         plot_area_left, plot_area_bottom, &
                                         plot_area_width, plot_area_height, &
                                         y_tick_label_max_width)
        !! Draw plot title and axis labels
        type(pdf_context_core), intent(inout) :: ctx
        character(len=*), intent(in), optional :: title, xlabel, ylabel
        real(wp), intent(in) :: plot_area_left, plot_area_bottom, plot_area_width, &
                                plot_area_height
        real(wp), intent(in), optional :: y_tick_label_max_width

        real(wp) :: title_x, title_y
        real(wp) :: xlabel_x, xlabel_y
        real(wp) :: ylabel_x, ylabel_y
        real(wp) :: width_pt
        real(wp) :: y_tick_w
        character(len=512) :: processed_title, processed_xlabel, processed_ylabel
        integer :: processed_len
        real(wp), parameter :: TITLE_GAP = 6.0_wp
        real(wp), parameter :: Y_TICK_GAP_LOCAL = 1.0_wp
        real(wp), parameter :: YLABEL_PAD = 1.0_wp
        real(wp), parameter :: LABEL_THICKNESS = 1.2_wp*PDF_LABEL_SIZE
        ! X tick-label baseline gap below the axis (matches X_TICK_GAP in
        ! fortplot_pdf_axes_drawing).
        real(wp), parameter :: X_TICK_GAP = 15.0_wp
        ! Helvetica vertical metrics as fractions of the font size: ascent
        ! reaches roughly the cap line, descent drops below the baseline.
        real(wp), parameter :: FONT_ASCENT_FRAC = 0.718_wp
        real(wp), parameter :: FONT_DESCENT_FRAC = 0.207_wp
        real(wp) :: tick_label_bottom

        ! Draw title (centered at top)
        if (present(title)) then
            if (len_trim(title) > 0) then
                ! Process LaTeX commands for accurate width calculation
                call process_latex_in_text(trim(title), processed_title, processed_len)

                width_pt = estimate_pdf_text_width(processed_title(1:processed_len), &
                                                   PDF_TITLE_SIZE)
                title_x = plot_area_left + 0.5_wp*plot_area_width - 0.5_wp*width_pt
                ! Lift the title above the twiny top-axis block (tick labels and
                ! top axis label) so it does not overlap them. ctx%twiny_top_offset
                ! is zero without a twiny, keeping the default position.
                title_y = plot_area_bottom + plot_area_height + &
                          real(ctx%twiny_top_offset, wp) + TITLE_GAP
                ! Process LaTeX commands to Unicode and render with mixed fonts
                ! Use mathtext rendering for title to handle superscripts properly
                call draw_pdf_mathtext(ctx, title_x, title_y, trim(title), &
                                       PDF_TITLE_SIZE)
            end if
        end if

        ! Draw X-axis label (centered at bottom).
        if (present(xlabel)) then
            if (len_trim(xlabel) > 0) then
                ! Process LaTeX commands for accurate width calculation
                call process_latex_in_text(trim(xlabel), processed_xlabel, &
                                           processed_len)

                width_pt = estimate_pdf_text_width(processed_xlabel(1:processed_len), &
                                                   PDF_LABEL_SIZE)
                xlabel_x = plot_area_left + 0.5_wp*plot_area_width - 0.5_wp*width_pt
                ! Outer (lower) edge of the x tick labels: baseline sits
                ! X_TICK_GAP below the axis, descenders reach below that.
                tick_label_bottom = plot_area_bottom - X_TICK_GAP - &
                                    FONT_DESCENT_FRAC*PDF_TICK_LABEL_SIZE
                ! Place the xlabel baseline a labelpad below the tick-label edge,
                ! leaving room for the xlabel's own ascent so the visible gap
                ! equals the pad (mirrors the y-label treatment below).
                xlabel_y = tick_label_bottom - real(AXIS_LABEL_PAD_PT, wp) - &
                           FONT_ASCENT_FRAC*PDF_LABEL_SIZE
                call render_mixed_text(ctx, xlabel_x, xlabel_y, trim(xlabel))
            end if
        end if

        ! Draw Y-axis label (rotated on left) - anchor point is the right edge of the
        ! rotated glyphs (text extends to the left in device X).
        if (present(ylabel)) then
            if (len_trim(ylabel) > 0) then
                ! Process LaTeX commands for accurate width calculation
                call process_latex_in_text(trim(ylabel), processed_ylabel, &
                                           processed_len)

                y_tick_w = 0.0_wp
                if (present(y_tick_label_max_width)) y_tick_w = y_tick_label_max_width

                ! Place y-label block left of the y-tick label block by a fixed padding.
                ! Account for rotated text matrix: glyphs extend left of (ylabel_x).
                ylabel_x = plot_area_left - Y_TICK_GAP_LOCAL - y_tick_w - YLABEL_PAD

                ! Vertically center rotated label: its extent along Y equals the
                ! unrotated text width in points.
                width_pt = estimate_pdf_text_width(processed_ylabel(1:processed_len), &
                                                   PDF_LABEL_SIZE)
                ylabel_y = plot_area_bottom + 0.5_wp*plot_area_height - 0.5_wp*width_pt
                call render_rotated_mixed_text(ctx, ylabel_x, ylabel_y, trim(ylabel))
            end if
        end if
    end subroutine draw_pdf_title_and_labels

    subroutine render_mixed_text(ctx, x, y, text, font_size)
        !! Process LaTeX and render mixed-font text with mathtext support.
        type(pdf_context_core), intent(inout) :: ctx
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        real(wp), intent(in), optional :: font_size
        character(len=512) :: processed
        integer :: plen
        character(len=600) :: math_ready
        integer :: mlen

        ! Mathtext renders Unicode superscripts properly
        call process_latex_in_text(text, processed, plen)
        ! Mathtext engages only when callers supply explicit $...$ delimiters
        call prepare_mathtext_if_needed(processed(1:plen), math_ready, mlen)

        ! Route through mathtext renderer only when math segments are present
        if (has_mathtext(math_ready(1:mlen))) then
            if (present(font_size)) then
                call draw_pdf_mathtext(ctx, x, y, math_ready(1:mlen), font_size)
            else
                call draw_pdf_mathtext(ctx, x, y, math_ready(1:mlen))
            end if
        else
            if (present(font_size)) then
                call draw_mixed_font_text(ctx, x, y, processed(1:plen), font_size)
            else
                call draw_mixed_font_text(ctx, x, y, processed(1:plen))
            end if
        end if
    end subroutine render_mixed_text

    subroutine render_rotated_mixed_text(ctx, x, y, text)
        !! Process LaTeX and render rotated mixed-font ylabel with mathtext support.
        type(pdf_context_core), intent(inout) :: ctx
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        character(len=512) :: processed
        integer :: plen
        character(len=600) :: math_ready
        integer :: mlen

        ! Process LaTeX commands
        call process_latex_in_text(text, processed, plen)

        call prepare_mathtext_if_needed(processed(1:plen), math_ready, mlen)

        if (has_mathtext(math_ready(1:mlen))) then
            ! draw_pdf_mathtext doesn't support rotation; use text matrix approach
            call draw_rotated_pdf_mathtext(ctx, x, y, math_ready(1:mlen))
        else
            call draw_rotated_mixed_font_text(ctx, x, y, processed(1:plen))
        end if
    end subroutine render_rotated_mixed_text

    subroutine draw_rotated_pdf_mathtext(ctx, x, y, text)
        !! Draw rotated mathtext for ylabel using rotation matrix with manual text positioning
        use fortplot_pdf_text_segments, only: process_text_segments
        type(pdf_context_core), intent(inout) :: ctx
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        character(len=1024) :: matrix_cmd, td_cmd
        character(len=2048) :: preprocessed_text
        integer :: processed_len
        character(len=4096) :: math_ready
        integer :: mlen
        type(mathtext_element_t), allocatable :: elements(:)
        integer :: i
        real(wp) :: elem_font_size, elem_y_offset
        real(wp) :: char_width
        integer :: j, codepoint, char_len, text_len
        logical :: in_symbol_font

        ! Process text for mathtext
        call process_latex_in_text(text, preprocessed_text, processed_len)
        call preprocess_math_text(preprocessed_text(1:processed_len), math_ready, mlen)

        ! Parse mathtext elements
        elements = parse_mathtext(math_ready(1:mlen))

        ! Begin text object with rotation matrix (90 degrees counterclockwise)
        ctx%stream_data = ctx%stream_data//'BT'//new_line('a')

        ! Set rotation matrix: [0 1 -1 0 x y] for 90-degree rotation
        write (matrix_cmd, '("0 1 -1 0 ", F0.3, 1X, F0.3, " Tm")') x, y
        ctx%stream_data = ctx%stream_data//trim(adjustl(matrix_cmd))//new_line('a')

        ! Render each mathtext element with proper font size and vertical offset
        in_symbol_font = .false.
        do i = 1, size(elements)
            if (len_trim(elements(i)%text) > 0) then
                ! Calculate element font size and vertical offset
                elem_font_size = PDF_LABEL_SIZE*elements(i)%font_size_ratio
                elem_y_offset = elements(i)%vertical_offset*PDF_LABEL_SIZE

                ! Move to position for this element using Td (relative positioning)
                ! The rotation matrix transforms these:
                ! x->forward along text, y->perpendicular.
                if (i > 1) then
                    ! Move horizontally by previous element width, vertically by
                    ! offset difference.
                    write (td_cmd, '(F0.3, 1X, F0.3, " Td")') char_width, &
                        elem_y_offset - (elements(i - 1)%vertical_offset*PDF_LABEL_SIZE)
                    ctx%stream_data = &
                        ctx%stream_data//trim(adjustl(td_cmd))//new_line('a')
                else if (abs(elem_y_offset) > 0.01_wp) then
                    ! First element with non-zero offset
                    write (td_cmd, '("0 ", F0.3, " Td")') elem_y_offset
                    ctx%stream_data = &
                        ctx%stream_data//trim(adjustl(td_cmd))//new_line('a')
                end if

                ! Set font size for this element
                write (matrix_cmd, '("/F", I0, 1X, F0.1, " Tf")') &
                    ctx%fonts%get_helvetica_obj(), elem_font_size
                ctx%stream_data = &
                    ctx%stream_data//trim(adjustl(matrix_cmd))//new_line('a')

                ! Render text segments
                call process_text_segments(ctx, elements(i)%text, in_symbol_font, &
                                           elem_font_size)

                ! Calculate width for next element positioning
                char_width = 0.0_wp
                j = 1
                text_len = len_trim(elements(i)%text)
                do while (text_len < len(elements(i)%text))
                    if (elements(i)%text(text_len + 1:text_len + 1) == ' ') then
                        text_len = text_len + 1
                    else
                        exit
                    end if
                end do

                do while (j <= text_len)
                    char_len = utf8_char_length(elements(i)%text(j:j))
                    if (char_len == 0) then
                        codepoint = iachar(elements(i)%text(j:j))
                        char_len = 1
                    else
                        codepoint = utf8_to_codepoint(elements(i)%text, j)
                    end if

                    if (codepoint >= 48 .and. codepoint <= 57) then
                        char_width = char_width + elem_font_size*0.55_wp
                    else if (codepoint >= 65 .and. codepoint <= 90) then
                        char_width = char_width + elem_font_size*0.65_wp
                    else if (codepoint >= 97 .and. codepoint <= 122) then
                        char_width = char_width + elem_font_size*0.5_wp
                    else if (codepoint == 32) then
                        char_width = char_width + elem_font_size*0.3_wp
                    else
                        char_width = char_width + elem_font_size*0.5_wp
                    end if

                    j = j + char_len
                end do
            end if
        end do

        ctx%stream_data = ctx%stream_data//'ET'//new_line('a')
    end subroutine draw_rotated_pdf_mathtext

end module fortplot_pdf_axes_text

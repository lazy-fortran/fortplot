module fortplot_ascii_rendering
    !! ASCII terminal plotting backend - Core Rendering Logic
    !!
    !! This module contains the core rendering functionality for ASCII plotting,
    !! including canvas output, file writing, and terminal display.
    !!
    !! Author: fortplot contributors
    
    use fortplot_logging, only: log_info, log_error
    use fortplot_ascii_utils, only: text_element_t, render_text_elements_to_canvas
    use fortplot_ascii_utils, only: print_centered_title, write_centered_title
    use fortplot_ascii_axis_policy, only: put_cell, LAYER_DATA
    use fortplot_ascii_drawing, only: text_charset_t, unicode_glyphs, &
                                      normalize_text_charset, charset_is_unicode, &
                                      text_frame_line, translate_text_cell
    use fortplot_unicode, only: escape_unicode_for_ascii
    use fortplot_braille, only: braille_char
    use fortplot_text_color, only: resolve_text_color_mode_file, &
                                   resolve_text_color_mode_terminal, &
                                   sgr_foreground, sgr_reset, unpack_rgb, COLOR_NONE
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: output_to_terminal, output_to_file
    public :: ascii_finalize, ascii_get_output
    public :: braille_finalize, braille_output_to_file
    
contains

    subroutine ascii_finalize(canvas, text_elements, num_text_elements, &
                             arrow_elements, num_arrow_elements, &
                             plot_width, plot_height, title_text, xlabel_text, ylabel_text, &
                             legend_lines, num_legend_lines, filename, text_charset, &
                             color_mode, canvas_color)
        character(len=1), intent(inout) :: canvas(:,:)
        type(text_element_t), intent(inout) :: text_elements(:)
        integer, intent(in) :: num_text_elements
        type(text_element_t), intent(inout) :: arrow_elements(:)
        integer, intent(in) :: num_arrow_elements
        integer, intent(in) :: plot_width, plot_height
        character(len=:), allocatable, intent(in) :: title_text, xlabel_text, ylabel_text
        character(len=*), intent(in) :: legend_lines(:)
        integer, intent(in) :: num_legend_lines
        character(len=*), intent(in) :: filename
        character(len=*), intent(in), optional :: text_charset
        !! Raw color mode ('never'..'auto'); resolved per destination here so
        !! file saves stay reproducible and terminal output honors 'auto'.
        character(len=*), intent(in), optional :: color_mode
        integer, intent(in), optional :: canvas_color(:,:)

        integer :: unit, ios
        character(len=512) :: error_msg
        character(len=:), allocatable :: mode, raw_color, term_color, file_color

        mode = 'ascii'
        if (present(text_charset)) mode = normalize_text_charset(text_charset)

        raw_color = 'never'
        if (present(color_mode)) raw_color = trim(color_mode)
        term_color = resolve_text_color_mode_terminal(raw_color)
        file_color = resolve_text_color_mode_file(raw_color)

        if (len_trim(filename) == 0 .or. trim(filename) == "terminal") then
            call output_to_terminal(canvas, text_elements, num_text_elements, &
                                  arrow_elements, num_arrow_elements, &
                                  plot_width, plot_height, title_text, xlabel_text, ylabel_text, &
                                  legend_lines, num_legend_lines, mode, &
                                  term_color, canvas_color)
        else
            open(newunit=unit, file=filename, status='replace', iostat=ios, iomsg=error_msg)

            if (ios /= 0) then
                call log_error("Cannot save ASCII file '" // trim(filename) // "': " // trim(error_msg))
                ! Fall back to terminal output
                call log_info("Falling back to terminal output due to file error")
                call output_to_terminal(canvas, text_elements, num_text_elements, &
                                      arrow_elements, num_arrow_elements, &
                                      plot_width, plot_height, title_text, xlabel_text, ylabel_text, &
                                      legend_lines, num_legend_lines, mode, &
                                      term_color, canvas_color)
                return
            end if

            call output_to_file(canvas, text_elements, num_text_elements, &
                              arrow_elements, num_arrow_elements, &
                              plot_width, plot_height, title_text, xlabel_text, ylabel_text, &
                              legend_lines, num_legend_lines, unit, mode, &
                              file_color, canvas_color)
            close(unit)
            call log_info("Unicode plot saved to '" // trim(filename) // "'")
        end if
    end subroutine ascii_finalize
    
    subroutine output_to_terminal(canvas, text_elements, num_text_elements, &
                                 arrow_elements, num_arrow_elements, &
                                 plot_width, plot_height, title_text, xlabel_text, ylabel_text, &
                                 legend_lines, num_legend_lines, text_charset, &
                                 color_mode, canvas_color)
        character(len=1), intent(inout) :: canvas(:,:)
        type(text_element_t), intent(in) :: text_elements(:)
        integer, intent(in) :: num_text_elements
        type(text_element_t), intent(in) :: arrow_elements(:)
        integer, intent(in) :: num_arrow_elements
        integer, intent(in) :: plot_width, plot_height
        character(len=:), allocatable, intent(in) :: title_text, xlabel_text, ylabel_text
        character(len=*), intent(in) :: legend_lines(:)
        integer, intent(in) :: num_legend_lines
        character(len=*), intent(in), optional :: text_charset
        !! Resolved color mode ('never' disables all escapes) and the per-cell
        !! packed-RGB buffer used to wrap colored data glyphs in SGR spans.
        character(len=*), intent(in), optional :: color_mode
        integer, intent(in), optional :: canvas_color(:,:)
        integer :: i, j, legend_idx
        logical :: unicode_mode, use_color
        character(len=:), allocatable :: resolved_color
        type(text_charset_t) :: glyphs

        unicode_mode = .false.
        if (present(text_charset)) unicode_mode = charset_is_unicode(text_charset)
        call resolve_output_color(color_mode, canvas_color, resolved_color, use_color)

        ! Render text elements to canvas before output
        call render_text_elements_to_canvas(canvas, text_elements, &
                                            num_text_elements, &
                                            plot_width, plot_height)
        call render_overlay_elements_to_canvas(canvas, arrow_elements, &
                                               num_arrow_elements, &
                                               plot_width, plot_height)

        if (allocated(title_text)) then
            print '(A)', ''  ! Empty line before title
            call print_centered_title(title_text, plot_width, raw_text=unicode_mode)
        end if

        if (unicode_mode) then
            glyphs = unicode_glyphs()
            print '(A)', text_frame_line(plot_width, glyphs, .true.)
            do i = 1, plot_height
                print '(A)', build_unicode_row(canvas(i, :), plot_width, glyphs)
            end do
            print '(A)', text_frame_line(plot_width, glyphs, .false.)
        else
            print '(A)', '+' // repeat('-', plot_width) // '+'
            if (use_color) then
                do i = 1, plot_height
                    print '(A)', build_ascii_color_row(canvas(i, :), &
                        canvas_color(i, :), plot_width, resolved_color)
                end do
            else
                block
                    character(len=:), allocatable :: row_buffer
                    allocate(character(len=plot_width + 2) :: row_buffer)
                    do i = 1, plot_height
                        row_buffer(1:1) = '|'
                        do j = 1, plot_width
                            row_buffer(j + 1:j + 1) = canvas(i, j)
                        end do
                        row_buffer(plot_width + 2:plot_width + 2) = '|'
                        print '(A)', row_buffer
                    end do
                end block
            end if
            print '(A)', '+' // repeat('-', plot_width) // '+'
        end if

        ! Print xlabel below the plot if present
        if (allocated(xlabel_text)) then
            call print_centered_title(xlabel_text, plot_width, raw_text=unicode_mode)
        end if

        ! Print ylabel with Unicode-to-ASCII conversion
        if (allocated(ylabel_text)) then
            if (unicode_mode) then
                print '(A)', trim(ylabel_text)
            else
                block
                    character(len=500) :: ascii_ylabel
                    call escape_unicode_for_ascii(ylabel_text, ascii_ylabel)
                    print '(A)', trim(ascii_ylabel)
                end block
            end if
        end if

        if (num_legend_lines > 0) then
            print '(A)', ''
            do legend_idx = 1, min(num_legend_lines, size(legend_lines))
                print '(A)', trim(legend_lines(legend_idx))
            end do
        end if
    end subroutine output_to_terminal

    subroutine output_to_file(canvas, text_elements, num_text_elements, &
                            arrow_elements, num_arrow_elements, &
                            plot_width, plot_height, title_text, xlabel_text, ylabel_text, &
                            legend_lines, num_legend_lines, unit, text_charset, &
                            color_mode, canvas_color)
        character(len=1), intent(inout) :: canvas(:,:)
        type(text_element_t), intent(in) :: text_elements(:)
        integer, intent(in) :: num_text_elements
        type(text_element_t), intent(in) :: arrow_elements(:)
        integer, intent(in) :: num_arrow_elements
        integer, intent(in) :: plot_width, plot_height
        character(len=:), allocatable, intent(in) :: title_text, xlabel_text, ylabel_text
        character(len=*), intent(in) :: legend_lines(:)
        integer, intent(in) :: num_legend_lines
        integer, intent(in) :: unit
        character(len=*), intent(in), optional :: text_charset
        !! Resolved color mode ('never' disables all escapes) and the per-cell
        !! packed-RGB buffer used to wrap colored data glyphs in SGR spans.
        character(len=*), intent(in), optional :: color_mode
        integer, intent(in), optional :: canvas_color(:,:)
        integer :: i, j, legend_idx
        character(len=:), allocatable :: row_buffer, resolved_color
        logical :: unicode_mode, use_color
        type(text_charset_t) :: glyphs

        unicode_mode = .false.
        if (present(text_charset)) unicode_mode = charset_is_unicode(text_charset)
        call resolve_output_color(color_mode, canvas_color, resolved_color, use_color)

        ! Render text elements to canvas before output
        call render_text_elements_to_canvas(canvas, text_elements, &
                                            num_text_elements, &
                                            plot_width, plot_height)
        call render_overlay_elements_to_canvas(canvas, arrow_elements, &
                                               num_arrow_elements, &
                                               plot_width, plot_height)

        if (allocated(title_text)) then
            write(unit, '(A)') ''  ! Empty line before title
            call write_centered_title(unit, title_text, plot_width, raw_text=unicode_mode)
        end if

        if (unicode_mode) then
            glyphs = unicode_glyphs()
            write(unit, '(A)') text_frame_line(plot_width, glyphs, .true.)
            do i = 1, plot_height
                write(unit, '(A)') build_unicode_row(canvas(i, :), plot_width, glyphs)
            end do
            write(unit, '(A)') text_frame_line(plot_width, glyphs, .false.)
        else
            write(unit, '(A)') '+' // repeat('-', plot_width) // '+'
            if (use_color) then
                do i = 1, plot_height
                    write(unit, '(A)') build_ascii_color_row(canvas(i, :), &
                        canvas_color(i, :), plot_width, resolved_color)
                end do
            else
                ! Buffer each row in a single string before writing. Per-character
                ! `advance='no'` writes are pathologically slow when streaming many
                ! frames (e.g. ASCII animation save), so build the row first.
                allocate(character(len=plot_width + 2) :: row_buffer)
                do i = 1, plot_height
                    row_buffer(1:1) = '|'
                    do j = 1, plot_width
                        row_buffer(j + 1:j + 1) = canvas(i, j)
                    end do
                    row_buffer(plot_width + 2:plot_width + 2) = '|'
                    write(unit, '(A)') row_buffer
                end do
            end if
            write(unit, '(A)') '+' // repeat('-', plot_width) // '+'
        end if

        ! Write xlabel below the plot if present
        if (allocated(xlabel_text)) then
            call write_centered_title(unit, xlabel_text, plot_width, raw_text=unicode_mode)
        end if

        ! Write ylabel with Unicode-to-ASCII conversion
        if (allocated(ylabel_text)) then
            if (unicode_mode) then
                write(unit, '(A)') trim(ylabel_text)
            else
                block
                    character(len=500) :: ascii_ylabel
                    call escape_unicode_for_ascii(ylabel_text, ascii_ylabel)
                    write(unit, '(A)') trim(ascii_ylabel)
                end block
            end if
        end if

        if (num_legend_lines > 0) then
            write(unit, '(A)') ''
            do legend_idx = 1, min(num_legend_lines, size(legend_lines))
                write(unit, '(A)') trim(legend_lines(legend_idx))
            end do
        end if
    end subroutine output_to_file

    function ascii_get_output(canvas, width, height) result(output)
        !! Get the complete ASCII canvas as a string
        character(len=1), intent(in) :: canvas(:,:)
        integer, intent(in) :: width, height
        character(len=:), allocatable :: output
        character(len=1000) :: line_buffer
        integer :: i, j, total_len, line_len
        
        ! Calculate total length needed
        total_len = height * (width + 1)  ! +1 for newline per row
        allocate(character(len=total_len) :: output)
        
        output = ""
        do i = 1, height
            line_buffer = ""
            do j = 1, width
                line_buffer(j:j) = canvas(i, j)
            end do
            line_len = len_trim(line_buffer(:width))
            if (line_len == 0) line_len = 1  ! Ensure at least one character
            output = output // line_buffer(1:line_len) // new_line('a')
        end do
    end function ascii_get_output

    subroutine resolve_output_color(color_mode, canvas_color, resolved_color, use_color)
        !! Decide whether the ASCII output path should emit ANSI color. Color is
        !! used only when a concrete (non-'never') mode is supplied together with
        !! a per-cell color buffer.
        character(len=*), intent(in), optional :: color_mode
        integer, intent(in), optional :: canvas_color(:,:)
        character(len=:), allocatable, intent(out) :: resolved_color
        logical, intent(out) :: use_color

        resolved_color = 'never'
        if (present(color_mode)) resolved_color = trim(color_mode)
        use_color = present(canvas_color) .and. resolved_color /= 'never'
    end subroutine resolve_output_color

    function build_ascii_color_row(canvas_row, color_row, plot_width, &
                                   color_mode) result(row)
        !! Assemble one bordered ASCII row, wrapping each colored data glyph in an
        !! SGR foreground span. A span is opened when the packed color changes and
        !! always reset before a new color, before a plain cell, and before the
        !! trailing border so every line ends reset (issue #2062).
        character(len=1), intent(in) :: canvas_row(:)
        integer, intent(in) :: color_row(:)
        integer, intent(in) :: plot_width
        character(len=*), intent(in) :: color_mode
        character(len=:), allocatable :: row
        integer :: j, active
        real(wp) :: r, g, b
        logical :: colored

        row = '|'
        active = COLOR_NONE
        do j = 1, plot_width
            colored = color_row(j) /= COLOR_NONE
            if (colored) colored = canvas_row(j) /= ' '
            if (colored) then
                if (color_row(j) /= active) then
                    if (active /= COLOR_NONE) row = row//sgr_reset()
                    call unpack_rgb(color_row(j), r, g, b)
                    row = row//sgr_foreground(color_mode, r, g, b)
                    active = color_row(j)
                end if
                row = row//canvas_row(j)
            else
                if (active /= COLOR_NONE) then
                    row = row//sgr_reset()
                    active = COLOR_NONE
                end if
                row = row//canvas_row(j)
            end if
        end do
        if (active /= COLOR_NONE) row = row//sgr_reset()
        row = row//'|'
    end function build_ascii_color_row

    function build_unicode_row(canvas_row, plot_width, glyphs) result(row)
        !! Assemble one bordered output row for the Unicode charset, translating
        !! each interior canvas byte through the glyph table. Multibyte glyphs
        !! force variable-length concatenation, so this path is kept separate
        !! from the fixed-width ASCII fast path.
        character(len=1), intent(in) :: canvas_row(:)
        integer, intent(in) :: plot_width
        type(text_charset_t), intent(in) :: glyphs
        character(len=:), allocatable :: row
        integer :: j

        row = glyphs%vline
        do j = 1, plot_width
            row = row//translate_text_cell(canvas_row(j), glyphs)
        end do
        row = row//glyphs%vline
    end function build_unicode_row

    subroutine render_overlay_elements_to_canvas(canvas, overlay_elements, num_overlay_elements, &
                                                 plot_width, plot_height)
        character(len=1), intent(inout) :: canvas(:,:)
        type(text_element_t), intent(in) :: overlay_elements(:)
        integer, intent(in) :: num_overlay_elements, plot_width, plot_height
        integer :: i, x, y
        character(len=500) :: ascii_text

        do i = 1, num_overlay_elements
            call escape_unicode_for_ascii(overlay_elements(i)%text, ascii_text)
            if (len_trim(ascii_text) == 0) cycle
            x = max(1, min(overlay_elements(i)%x, plot_width - 1))
            y = max(1, min(overlay_elements(i)%y, plot_height))
            ! Overlay arrows are plot data: keep them from clobbering axis,
            ! tick, or label glyphs that share the cell (issue #2069).
            call put_cell(canvas, y, x, ascii_text(1:1), LAYER_DATA)
        end do
    end subroutine render_overlay_elements_to_canvas

    subroutine braille_finalize(canvas, mask, text_elements, num_text_elements, &
                                arrow_elements, num_arrow_elements, &
                                plot_width, plot_height, &
                                title_text, xlabel_text, ylabel_text, &
                                legend_lines, num_legend_lines, filename)
        !! Braille counterpart of ascii_finalize: writes the composited braille
        !! canvas to a file, falling back to terminal output on error.
        character(len=1), intent(inout) :: canvas(:,:)
        integer, intent(in) :: mask(:,:)
        type(text_element_t), intent(inout) :: text_elements(:)
        integer, intent(in) :: num_text_elements
        type(text_element_t), intent(inout) :: arrow_elements(:)
        integer, intent(in) :: num_arrow_elements
        integer, intent(in) :: plot_width, plot_height
        character(len=:), allocatable, intent(in) :: title_text, xlabel_text, ylabel_text
        character(len=*), intent(in) :: legend_lines(:)
        integer, intent(in) :: num_legend_lines
        character(len=*), intent(in) :: filename

        integer :: unit, ios
        character(len=512) :: error_msg

        if (len_trim(filename) == 0 .or. trim(filename) == "terminal") then
            call braille_output_to_terminal(canvas, mask, text_elements, &
                num_text_elements, arrow_elements, num_arrow_elements, &
                plot_width, plot_height, title_text, xlabel_text, ylabel_text, &
                legend_lines, num_legend_lines)
            return
        end if

        open(newunit=unit, file=filename, status='replace', iostat=ios, iomsg=error_msg)
        if (ios /= 0) then
            call log_error("Cannot save braille file '" // trim(filename) // &
                           "': " // trim(error_msg))
            call log_info("Falling back to terminal output due to file error")
            call braille_output_to_terminal(canvas, mask, text_elements, &
                num_text_elements, arrow_elements, num_arrow_elements, &
                plot_width, plot_height, title_text, xlabel_text, ylabel_text, &
                legend_lines, num_legend_lines)
            return
        end if
        call braille_output_to_file(canvas, mask, text_elements, num_text_elements, &
            arrow_elements, num_arrow_elements, plot_width, plot_height, &
            title_text, xlabel_text, ylabel_text, legend_lines, num_legend_lines, unit)
        close(unit)
        call log_info("Braille plot saved to '" // trim(filename) // "'")
    end subroutine braille_finalize

    subroutine braille_output_to_file(canvas, mask, text_elements, num_text_elements, &
                                      arrow_elements, num_arrow_elements, &
                                      plot_width, plot_height, &
                                      title_text, xlabel_text, ylabel_text, &
                                      legend_lines, num_legend_lines, unit)
        !! Compose the ASCII text canvas and the braille subpixel mask into UTF-8
        !! rows: cells with a set braille mask emit the braille glyph, all other
        !! cells keep their ASCII character (frame, axes, ticks, labels).
        character(len=1), intent(inout) :: canvas(:,:)
        integer, intent(in) :: mask(:,:)
        type(text_element_t), intent(in) :: text_elements(:)
        integer, intent(in) :: num_text_elements
        type(text_element_t), intent(in) :: arrow_elements(:)
        integer, intent(in) :: num_arrow_elements
        integer, intent(in) :: plot_width, plot_height
        character(len=:), allocatable, intent(in) :: title_text, xlabel_text, ylabel_text
        character(len=*), intent(in) :: legend_lines(:)
        integer, intent(in) :: num_legend_lines
        integer, intent(in) :: unit
        integer :: i, legend_idx

        call render_text_elements_to_canvas(canvas, text_elements, &
                                            num_text_elements, plot_width, plot_height)
        call render_overlay_elements_to_canvas(canvas, arrow_elements, &
                                               num_arrow_elements, plot_width, plot_height)

        if (allocated(title_text)) then
            write(unit, '(A)') ''
            call write_centered_title(unit, title_text, plot_width)
        end if

        write(unit, '(A)') '+' // repeat('-', plot_width) // '+'
        do i = 1, plot_height
            write(unit, '(A)') '|' // braille_row_utf8(canvas, mask, i, plot_width) // '|'
        end do
        write(unit, '(A)') '+' // repeat('-', plot_width) // '+'

        if (allocated(xlabel_text)) then
            call write_centered_title(unit, xlabel_text, plot_width)
        end if
        if (allocated(ylabel_text)) then
            block
                character(len=500) :: ascii_ylabel
                call escape_unicode_for_ascii(ylabel_text, ascii_ylabel)
                write(unit, '(A)') trim(ascii_ylabel)
            end block
        end if

        if (num_legend_lines > 0) then
            write(unit, '(A)') ''
            do legend_idx = 1, min(num_legend_lines, size(legend_lines))
                write(unit, '(A)') trim(legend_lines(legend_idx))
            end do
        end if
    end subroutine braille_output_to_file

    subroutine braille_output_to_terminal(canvas, mask, text_elements, num_text_elements, &
                                          arrow_elements, num_arrow_elements, &
                                          plot_width, plot_height, &
                                          title_text, xlabel_text, ylabel_text, &
                                          legend_lines, num_legend_lines)
        character(len=1), intent(inout) :: canvas(:,:)
        integer, intent(in) :: mask(:,:)
        type(text_element_t), intent(in) :: text_elements(:)
        integer, intent(in) :: num_text_elements
        type(text_element_t), intent(in) :: arrow_elements(:)
        integer, intent(in) :: num_arrow_elements
        integer, intent(in) :: plot_width, plot_height
        character(len=:), allocatable, intent(in) :: title_text, xlabel_text, ylabel_text
        character(len=*), intent(in) :: legend_lines(:)
        integer, intent(in) :: num_legend_lines
        integer :: i, legend_idx

        call render_text_elements_to_canvas(canvas, text_elements, &
                                            num_text_elements, plot_width, plot_height)
        call render_overlay_elements_to_canvas(canvas, arrow_elements, &
                                               num_arrow_elements, plot_width, plot_height)

        if (allocated(title_text)) then
            print '(A)', ''
            call print_centered_title(title_text, plot_width)
        end if

        print '(A)', '+' // repeat('-', plot_width) // '+'
        do i = 1, plot_height
            print '(A)', '|' // braille_row_utf8(canvas, mask, i, plot_width) // '|'
        end do
        print '(A)', '+' // repeat('-', plot_width) // '+'

        if (allocated(xlabel_text)) then
            call print_centered_title(xlabel_text, plot_width)
        end if
        if (allocated(ylabel_text)) then
            block
                character(len=500) :: ascii_ylabel
                call escape_unicode_for_ascii(ylabel_text, ascii_ylabel)
                print '(A)', trim(ascii_ylabel)
            end block
        end if

        if (num_legend_lines > 0) then
            print '(A)', ''
            do legend_idx = 1, min(num_legend_lines, size(legend_lines))
                print '(A)', trim(legend_lines(legend_idx))
            end do
        end if
    end subroutine braille_output_to_terminal

    function braille_row_utf8(canvas, mask, row, plot_width) result(line)
        !! Build one UTF-8 output row: braille glyph where the mask is set,
        !! otherwise the ASCII canvas character for that cell.
        character(len=1), intent(in) :: canvas(:,:)
        integer, intent(in) :: mask(:,:)
        integer, intent(in) :: row, plot_width
        character(len=:), allocatable :: line
        integer :: j

        line = ''
        do j = 1, plot_width
            if (mask(row, j) /= 0) then
                line = line // braille_char(mask(row, j))
            else
                line = line // canvas(row, j)
            end if
        end do
    end function braille_row_utf8

end module fortplot_ascii_rendering

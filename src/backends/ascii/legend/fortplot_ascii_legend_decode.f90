submodule (fortplot_ascii_legend) fortplot_ascii_legend_decode

    !! ASCII legend line decoding and render implementation
    !!
    !! Single Responsibility: Decode raw legend lines and implement
    !! the ASCII legend render interface.

    use fortplot_ascii_utils, only: is_legend_entry_text, is_registered_legend_label, is_autopct_text
    use fortplot_ascii_mathtext, only: sanitize_ascii_text
    implicit none

contains

    module subroutine decode_ascii_legend_line(raw_text, formatted_line, entry_label)
        !! Decode a raw legend line into formatted output and entry label
        character(len=*), intent(in) :: raw_text
        character(len=96), intent(out) :: formatted_line
        character(len=64), intent(out) :: entry_label

        character(len=:), allocatable :: trimmed_text
        character(len=256) :: sanitized
        integer :: sanitized_len, first_space

        formatted_line = ''
        entry_label = ''

        trimmed_text = trim(adjustl(raw_text))
        if (len_trim(trimmed_text) == 0) return

        if (len(trimmed_text) >= 3 .and. trimmed_text(1:3) == '-- ') then
            if (len(trimmed_text) > 3) then
                call sanitize_ascii_text(trim(adjustl(trimmed_text(4:))), sanitized, sanitized_len)
                entry_label = trim(sanitized(1:sanitized_len))
            else
                entry_label = ''
            end if
            formatted_line = '  - '//trim(entry_label)
        else
            formatted_line = '  '//trim(trimmed_text)
            first_space = index(trimmed_text, ' ')
            if (first_space > 0 .and. first_space < len(trimmed_text)) then
                call sanitize_ascii_text(trim(adjustl(trimmed_text(first_space + 1:))), sanitized, sanitized_len)
                entry_label = trim(sanitized(1:sanitized_len))
            else
                call sanitize_ascii_text(trim(trimmed_text), sanitized, sanitized_len)
                entry_label = trim(sanitized(1:sanitized_len))
            end if
        end if
    end subroutine decode_ascii_legend_line

    module subroutine ascii_render_legend_impl(legend, legend_lines, num_legend_lines, raw_labels)
        type(legend_t), intent(in) :: legend
        character(len=96), allocatable, intent(inout) :: legend_lines(:)
        integer, intent(inout) :: num_legend_lines
        logical, intent(in), optional :: raw_labels

        integer :: i, label_len
        character(len=96) :: line_buffer
        character(len=256) :: sanitized_label
        character(len=:), allocatable :: label_text
        character(len=1) :: marker_char
        logical :: keep_raw

        call reset_ascii_legend_lines_helper(legend_lines, num_legend_lines)

        if (legend%num_entries <= 0) return

        call append_ascii_legend_line_helper(legend_lines, num_legend_lines, 'Legend:')
        keep_raw = .false.
        if (present(raw_labels)) keep_raw = raw_labels

        do i = 1, legend%num_entries
            if (allocated(legend%entries(i)%label)) then
                if (keep_raw) then
                    label_text = trim(legend%entries(i)%label)
                else
                    call sanitize_ascii_text(legend%entries(i)%label, sanitized_label, label_len)
                    label_text = sanitized_label(1:label_len)
                end if
            else
                label_text = ''
            end if

            if (len_trim(label_text) == 0) then
                write (line_buffer, '("Series ",I0)') i
                label_text = trim(line_buffer)
            end if

            marker_char = '*'
            if (allocated(legend%entries(i)%marker)) then
                if (trim(legend%entries(i)%marker) /= 'None' .and. &
                    len_trim(legend%entries(i)%marker) > 0) then
                    marker_char = trim(legend%entries(i)%marker)
                end if
            end if

            line_buffer = '  '//marker_char//' '//label_text
            call append_ascii_legend_line_helper(legend_lines, num_legend_lines, trim(line_buffer))
        end do
    end subroutine ascii_render_legend_impl

end submodule fortplot_ascii_legend_decode

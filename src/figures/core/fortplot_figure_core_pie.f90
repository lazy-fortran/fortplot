submodule (fortplot_figure_core) fortplot_figure_core_pie
    use fortplot_annotations, only: create_text_annotation, validate_annotation, &
        COORD_DATA
    use fortplot_ascii, only: ascii_context

    implicit none

contains

    module subroutine add_pie(self, values, labels, autopct, startangle, colors, &
                              explode)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: values(:)
        character(len=*), intent(in), optional :: labels(:)
        character(len=*), intent(in), optional :: autopct
        real(wp), intent(in), optional :: startangle
        character(len=*), intent(in), optional :: colors(:)
        real(wp), intent(in), optional :: explode(:)

        call core_add_pie(self%plots, self%state, values, labels=labels, &
                          autopct=autopct, startangle=startangle, colors=colors, &
                          explode=explode, plot_count=self%plot_count)

        self%plot_count = self%state%plot_count
        if (self%plot_count <= 0) return

        call add_pie_annotations(self, self%plots(self%plot_count))
    end subroutine add_pie

    module subroutine add_pie_annotations(self, pie_plot)
        class(figure_t), intent(inout) :: self
        type(plot_data_t), intent(in) :: pie_plot

        if (pie_plot%pie_slice_count <= 0) return

        select type (backend => self%state%backend)
        type is (ascii_context)
            call add_ascii_pie_entries(backend, pie_plot)
            return
        class default
        end select

        call add_autopct_annotations(self, pie_plot)
        call add_label_annotations(self, pie_plot)
    end subroutine add_pie_annotations
    
    module subroutine add_ascii_pie_entries(backend, pie_plot)
        use fortplot_plot_data, only: plot_data_t
        class(ascii_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: pie_plot

        integer :: i
        real(wp) :: total
        character(len=64) :: label_buffer
        character(len=:), allocatable :: auto_text
        logical :: warned

        if (pie_plot%pie_slice_count <= 0) return

        call backend%clear_pie_legend_entries()

        total = sum(pie_plot%pie_values(1:pie_plot%pie_slice_count))
        warned = .false.

        do i = 1, pie_plot%pie_slice_count
            label_buffer = ''
            if (allocated(pie_plot%pie_labels)) then
                if (i <= size(pie_plot%pie_labels)) then
                    label_buffer = trim(pie_plot%pie_labels(i))
                end if
            end if
            if (len_trim(label_buffer) == 0) then
                write(label_buffer, '("Slice ",I0)') i
            end if

            auto_text = ''
            if (total > 0.0_wp .and. allocated(pie_plot%pie_autopct)) then
                if (len_trim(pie_plot%pie_autopct) > 0) then
                    call format_autopct_value(pie_plot%pie_values(i), total, &
                                              pie_plot%pie_autopct, auto_text, warned)
                    if (len_trim(auto_text) > 0) then
                        auto_text = trim(adjustl(auto_text))
                    end if
                end if
            end if

            call backend%register_pie_legend_entry(label_buffer, auto_text)
        end do
    end subroutine add_ascii_pie_entries

    module subroutine add_autopct_annotations(self, pie_plot)
        class(figure_t), intent(inout) :: self
        type(plot_data_t), intent(in) :: pie_plot

        integer :: i
        real(wp) :: total
        character(len=:), allocatable :: text
        logical :: warned

        if (.not. allocated(pie_plot%pie_autopct)) return
        if (pie_plot%pie_slice_count <= 0) return

        total = sum(pie_plot%pie_values(1:pie_plot%pie_slice_count))
        if (total <= 0.0_wp) return

        warned = .false.
        do i = 1, pie_plot%pie_slice_count
            call format_autopct_value(pie_plot%pie_values(i), total, &
                                      pie_plot%pie_autopct, text, warned)
            if (len_trim(text) == 0) cycle
            call append_figure_annotation(self, pie_plot%pie_label_pos(1, i), &
                                          pie_plot%pie_label_pos(2, i), text, &
                                          'center', 'center')
        end do
    end subroutine add_autopct_annotations

    module subroutine add_label_annotations(self, pie_plot)
        class(figure_t), intent(inout) :: self
        type(plot_data_t), intent(in) :: pie_plot

        integer :: i
        real(wp) :: mid_angle, offset_value, center_x, center_y, outer_radius
        character(len=8) :: ha_value

        if (.not. allocated(pie_plot%pie_labels)) return
        if (pie_plot%pie_slice_count <= 0) return

        outer_radius = pie_plot%pie_radius * 1.15_wp
        do i = 1, pie_plot%pie_slice_count
            if (len_trim(pie_plot%pie_labels(i)) == 0) cycle
            mid_angle = 0.5_wp * (pie_plot%pie_start(i) + pie_plot%pie_end(i))
            offset_value = 0.0_wp
            if (allocated(pie_plot%pie_offsets)) then
                if (i <= size(pie_plot%pie_offsets)) then
                    offset_value = pie_plot%pie_offsets(i)
                end if
            end if
            center_x = pie_plot%pie_center(1) + offset_value * cos(mid_angle)
            center_y = pie_plot%pie_center(2) + offset_value * sin(mid_angle)
            ha_value = determine_alignment(mid_angle)
            call append_figure_annotation(self, center_x + outer_radius * &
                                          cos(mid_angle), center_y + outer_radius * &
                                          sin(mid_angle), &
                                          trim(pie_plot%pie_labels(i)), ha_value, &
                                          'center')
        end do
    end subroutine add_label_annotations

    module subroutine append_figure_annotation(self, x, y, text, ha_value, va_value)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: ha_value, va_value
        type(text_annotation_t) :: annotation
        logical :: valid
        character(len=256) :: error_message

        if (len_trim(text) == 0) return
        if (.not. allocated(self%annotations)) then
            allocate(self%annotations(self%max_annotations))
        end if
        if (self%annotation_count >= self%max_annotations) then
            call log_warning('pie: maximum annotation capacity reached; skipping label')
            return
        end if

        annotation = create_text_annotation(text=trim(text), x=x, y=y, &
                                            coord_type=COORD_DATA)
        annotation%ha = trim(ha_value)
        annotation%alignment = trim(ha_value)
        annotation%va = trim(va_value)
        annotation%font_size = 12.0_wp
        call validate_annotation(annotation, valid, error_message)
        annotation%validated = .true.
        annotation%valid = valid
        if (valid) then
            self%annotation_count = self%annotation_count + 1
            self%annotations(self%annotation_count) = annotation
        else
            call log_warning('Skipping invalid annotation: ' // trim(error_message))
        end if
    end subroutine append_figure_annotation

    module subroutine format_autopct_value(value, total_value, fmt, text, warned)
        real(wp), intent(in) :: value, total_value
        character(len=*), intent(in) :: fmt
        character(len=:), allocatable, intent(out) :: text
        logical, intent(inout) :: warned

        integer :: fmt_len, idx, spec_end, spec_start
        integer :: width, precision
        real(wp) :: percent
        logical :: ok, plus_flag, space_flag, left_flag, zero_flag
        character(len=:), allocatable :: chunk
        character(len=:), allocatable :: trimmed_fmt
        character(len=:), allocatable :: literal
        integer :: auto_precision
        integer :: next_percent, literal_len

        text = ''
        if (total_value <= 0.0_wp) return
        if (len_trim(fmt) == 0) return

        fmt_len = len(fmt)
        percent = 100.0_wp * value / max(total_value, tiny(1.0_wp))

        trimmed_fmt = trim(fmt)
        if (trimmed_fmt == 'auto' .or. trimmed_fmt == 'Auto' .or. &
            trimmed_fmt == 'AUTO') then
            if (abs(percent - nint(percent)) < 0.05_wp) then
                auto_precision = 0
            else
                auto_precision = 1
            end if
            call build_autopct_chunk(percent, 0, auto_precision, .false., .false., &
                                     .false., .false., chunk)
            text = chunk // '%'
            return
        end if

        idx = 1
        do
            if (idx > fmt_len) exit

            next_percent = index(fmt(idx:), '%')
            if (next_percent == 0) then
                text = text // fmt(idx:fmt_len)
                exit
            end if

            if (next_percent > 1) then
                literal_len = next_percent - 1
                literal = fmt(idx:idx + literal_len - 1)
                text = text // literal
            end if

            spec_start = idx + next_percent - 1
            if (spec_start >= fmt_len) then
                if (.not. warned) then
                    call log_warning('pie: unsupported autopct format, ' // &
                                     'skipping percentage labels')
                    warned = .true.
                end if
                text = ''
                return
            end if

            if (fmt(spec_start + 1:spec_start + 1) == '%') then
                text = text // '%'
                idx = spec_start + 2
                cycle
            end if

            call parse_autopct_spec(fmt, spec_start, spec_end, width, precision, &
                                    plus_flag, space_flag, left_flag, zero_flag, &
                                    ok)
            if (.not. ok) then
                if (.not. warned) then
                    call log_warning('pie: unsupported autopct format, ' // &
                                     'skipping percentage labels')
                    warned = .true.
                end if
                text = ''
                return
            end if

            call build_autopct_chunk(percent, width, precision, plus_flag, &
                                     space_flag, left_flag, zero_flag, chunk)
            text = text // chunk
            idx = spec_end + 1
        end do
    end subroutine format_autopct_value

    module subroutine parse_autopct_spec(fmt, start_pos, spec_end, width, &
                                         precision, plus_flag, space_flag, &
                                         left_flag, zero_flag, ok)
        character(len=*), intent(in) :: fmt
        integer, intent(in) :: start_pos
        integer, intent(out) :: spec_end
        integer, intent(out) :: width, precision
        logical, intent(out) :: plus_flag, space_flag, left_flag, zero_flag
        logical, intent(out) :: ok

        character(len=:), allocatable :: spec_body
        integer :: fmt_len, body_len, idx
        integer :: width_start, precision_start
        integer :: ios

        fmt_len = len(fmt)
        spec_end = 0
        do idx = start_pos + 1, fmt_len
            if (fmt(idx:idx) == 'f' .or. fmt(idx:idx) == 'F') then
                spec_end = idx
                exit
            end if
            if (fmt(idx:idx) == '%') then
                ok = .false.
                return
            end if
        end do

        if (spec_end <= start_pos) then
            ok = .false.
            return
        end if

        if (spec_end > start_pos + 1) then
            spec_body = fmt(start_pos + 1:spec_end - 1)
        else
            spec_body = ''
        end if

        plus_flag = .false.
        space_flag = .false.
        left_flag = .false.
        zero_flag = .false.
        width = 0
        precision = -1
        ios = 0

        body_len = len(spec_body)
        idx = 1
        do while (idx <= body_len)
            select case (spec_body(idx:idx))
            case ('+')
                plus_flag = .true.
                idx = idx + 1
            case (' ')
                space_flag = .true.
                idx = idx + 1
            case ('-')
                left_flag = .true.
                idx = idx + 1
            case ('0')
                zero_flag = .true.
                idx = idx + 1
            case ('#')
                idx = idx + 1
            case default
                exit
            end select
        end do

        if (left_flag) zero_flag = .false.

        width_start = idx
        do while (idx <= body_len)
            if (spec_body(idx:idx) < '0' .or. spec_body(idx:idx) > '9') exit
            idx = idx + 1
        end do
        if (idx > width_start) then
            read(spec_body(width_start:idx - 1), *, iostat=ios) width
            if (ios /= 0) width = 0
        end if

        if (idx <= body_len) then
            if (spec_body(idx:idx) == '.') then
                idx = idx + 1
                precision_start = idx
                do while (idx <= body_len)
                    if (spec_body(idx:idx) < '0' .or. spec_body(idx:idx) > '9') exit
                    idx = idx + 1
                end do
                if (idx > precision_start) then
                    read(spec_body(precision_start:idx - 1), *, iostat=ios) precision
                    if (ios /= 0) precision = -1
                else
                    precision = 0
                end if
            end if
        end if

        if (idx <= body_len) then
            ok = .false.
            return
        end if

        if (precision < 0) precision = 6
        ok = .true.
    end subroutine parse_autopct_spec

    module subroutine build_autopct_chunk(percent, width, precision, plus_flag, &
                                          space_flag, left_flag, zero_flag, chunk)
        real(wp), intent(in) :: percent
        integer, intent(in) :: width, precision
        logical, intent(in) :: plus_flag, space_flag, left_flag, zero_flag
        character(len=:), allocatable, intent(out) :: chunk

        character(len=64) :: buffer
        character(len=32) :: fmt_spec
        character(len=:), allocatable :: base
        integer :: pad_len
        logical :: negative
        character(len=1) :: sign_char

        write(fmt_spec, '(A,I0,A,I0,A)') '(f', max(precision + 8, 24), '.', &
                                         precision, ')'
        write(buffer, fmt_spec) percent
        base = trim(adjustl(buffer))
        if (len(base) == 0) base = '0'
        if (precision == 0) then
            if (len(base) > 0) then
                if (base(len(base):len(base)) == '.') then
                    if (len(base) > 1) then
                        base = base(1:len(base) - 1)
                    else
                        base = '0'
                    end if
                end if
            end if
        end if

        negative = (base(1:1) == '-')
        if (.not. negative) then
            if (plus_flag) then
                base = '+' // base
            else if (space_flag) then
                base = ' ' // base
            end if
        end if

        chunk = base
        if (width > len(chunk)) then
            pad_len = width - len(chunk)
            if (left_flag) then
                chunk = chunk // repeat(' ', pad_len)
            else if (zero_flag) then
                if (len(chunk) > 0) then
                    sign_char = chunk(1:1)
                    select case (sign_char)
                    case ('+', '-', ' ')
                        chunk = sign_char // repeat('0', pad_len) // chunk(2:)
                    case default
                        chunk = repeat('0', pad_len) // chunk
                    end select
                else
                    chunk = repeat('0', pad_len)
                end if
            else
                chunk = repeat(' ', pad_len) // chunk
            end if
        end if
    end subroutine build_autopct_chunk

    pure module function determine_alignment(angle) result(alignment)
        real(wp), intent(in) :: angle
        character(len=8) :: alignment
        real(wp) :: cos_val

        cos_val = cos(angle)
        if (cos_val < -0.3_wp) then
            alignment = 'right'
        else if (cos_val > 0.3_wp) then
            alignment = 'left'
        else
            alignment = 'center'
        end if
    end function determine_alignment

end submodule fortplot_figure_core_pie

submodule (fortplot_figure_core) fortplot_figure_core_pie
    use fortplot_annotations, only: create_text_annotation, validate_annotation, &
        COORD_DATA

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
        call add_autopct_annotations(self, pie_plot)
        call add_label_annotations(self, pie_plot)
    end subroutine add_pie_annotations

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
                                          sin(mid_angle), trim(pie_plot%pie_labels(i)), &
                                          ha_value, 'center')
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

        character(len=:), allocatable :: fmt_trimmed
        character(len=:), allocatable :: prefix_raw, suffix_raw
        character(len=:), allocatable :: prefix, suffix
        character(len=32) :: fmt_spec
        character(len=64) :: buffer
        integer :: fmt_len, spec_start, spec_end
        integer :: pos_dot, precision, ios
        integer :: i
        real(wp) :: percent

        text = ''
        if (total_value <= 0.0_wp) return
        if (len_trim(fmt) == 0) return

        fmt_trimmed = trim(fmt)
        fmt_len = len(fmt_trimmed)

        spec_start = 0
        spec_end = 0
        i = 1
        do while (i <= fmt_len)
            if (fmt_trimmed(i:i) == '%') then
                if (i < fmt_len .and. fmt_trimmed(i + 1:i + 1) == '%') then
                    i = i + 2
                else
                    spec_start = i
                    exit
                end if
            else
                i = i + 1
            end if
        end do

        if (spec_start <= 0) then
            if (.not. warned) then
                call log_warning('pie: unsupported autopct format, ' // &
                                 'skipping percentage labels')
                warned = .true.
            end if
            return
        end if

        i = spec_start + 1
        do while (i <= fmt_len)
            if (fmt_trimmed(i:i) == 'f' .or. fmt_trimmed(i:i) == 'F') then
                spec_end = i
                exit
            end if
            if (fmt_trimmed(i:i) == '%') exit
            i = i + 1
        end do

        if (spec_end <= spec_start) then
            if (.not. warned) then
                call log_warning('pie: unsupported autopct format, ' // &
                                 'skipping percentage labels')
                warned = .true.
            end if
            return
        end if

        if (spec_start > 1) then
            prefix_raw = fmt_trimmed(1:spec_start - 1)
        else
            prefix_raw = ''
        end if
        if (spec_end < fmt_len) then
            suffix_raw = fmt_trimmed(spec_end + 1:fmt_len)
        else
            suffix_raw = ''
        end if

        prefix = collapse_percent_literals(prefix_raw)
        suffix = collapse_percent_literals(suffix_raw)

        ios = 0
        pos_dot = index(fmt_trimmed(spec_start:spec_end), '.')
        if (pos_dot > 0) then
            pos_dot = spec_start + pos_dot - 1
            if (pos_dot + 1 <= spec_end - 1) then
                read(fmt_trimmed(pos_dot + 1:spec_end - 1), *, iostat=ios) precision
            else
                precision = 1
            end if
        else
            precision = 1
        end if
        if (ios /= 0) precision = 1
        if (precision < 0) precision = 0
        write(fmt_spec, '(A,I0,A)') '(f0.', precision, ')'

        percent = 100.0_wp * value / max(total_value, tiny(1.0_wp))
        write(buffer, fmt_spec) percent

        text = prefix // trim(buffer) // suffix
    end subroutine format_autopct_value

    pure module function collapse_percent_literals(raw) result(text_out)
        character(len=*), intent(in) :: raw
        character(len=:), allocatable :: text_out
        integer :: raw_len, i

        raw_len = len(raw)
        text_out = ''
        if (raw_len <= 0) return

        i = 1
        do while (i <= raw_len)
            if (i < raw_len) then
                if (raw(i:i) == '%' .and. raw(i + 1:i + 1) == '%') then
                    text_out = text_out // '%'
                    i = i + 2
                    cycle
                end if
            end if
            text_out = text_out // raw(i:i)
            i = i + 1
        end do
    end function collapse_percent_literals

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

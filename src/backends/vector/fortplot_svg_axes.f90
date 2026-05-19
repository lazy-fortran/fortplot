module fortplot_svg_axes
    !! Standalone SVG axes and label rendering

    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: svg_render_ylabel_impl, svg_draw_axes_labels_impl

contains

    subroutine svg_render_ylabel_impl(pa_left, pa_bottom, pa_height, ylabel, svg_elem)
        real(wp), intent(in) :: pa_left, pa_bottom, pa_height
        character(len=*), intent(in) :: ylabel
        character(len=:), allocatable, intent(out) :: svg_elem
        real(wp) :: x, y
        character(len=1024) :: local_elem

        x = pa_left - 35.0_wp
        y = pa_bottom + pa_height*0.5_wp

        write (local_elem, '(A,F0.3,A,F0.3,A,A,A)') &
            '<text x="', x, '" y="', y, &
            '" font-family="sans-serif" font-size="12" '// &
            'text-anchor="middle" transform="rotate(-90 ', x, ' ', y, ')">', &
            trim(ylabel), '</text>'
        svg_elem = trim(local_elem)
    end subroutine svg_render_ylabel_impl

    subroutine svg_draw_axes_labels_impl(svg_content, pa_left, pa_right, pa_bottom, &
                                          pa_top, xscale, yscale, symlog_threshold, &
                                          x_min, x_max, y_min, y_max, &
                                          title, xlabel, ylabel, &
                                          x_date_format, y_date_format, &
                                          z_min, z_max, has_3d_plots)
        character(len=:), allocatable, intent(inout) :: svg_content
        real(wp), intent(in) :: pa_left, pa_right, pa_bottom, pa_top
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=:), allocatable, intent(in), optional :: title, xlabel, ylabel
        character(len=*), intent(in), optional :: x_date_format, y_date_format
        real(wp), intent(in), optional :: z_min, z_max
        logical, intent(in) :: has_3d_plots
        character(len=1024) :: elem
        real(wp) :: mid_x, mid_y
        integer :: i
        real(wp) :: tick_x, tick_y, val
        character(len=32) :: val_str

        associate (xs => xscale, ys => yscale, st => symlog_threshold, &
                   zmi => z_min, zma => z_max, h3d => has_3d_plots)
        end associate
        if (present(x_date_format)) then
            associate (unused_xfmt => len_trim(x_date_format)); end associate
        end if
        if (present(y_date_format)) then
            associate (unused_yfmt => len_trim(y_date_format)); end associate
        end if

        write (elem, '(A,F0.3,A,F0.3,A,F0.3,A,F0.3,A)') &
            '<rect x="', pa_left, '" y="', pa_top, '" width="', pa_right - pa_left, &
            '" height="', pa_bottom - pa_top, '" fill="none" stroke="black"/>'
        svg_content = svg_content // elem // new_line('a')

        do i = 0, 4
            tick_x = pa_left + real(i, wp)/4.0_wp*(pa_right - pa_left)
            val = x_min + real(i, wp)/4.0_wp*(x_max - x_min)
            write (val_str, '(G10.3)') val
            write (elem, '(A,F0.3,A,F0.3,A,F0.3,A,F0.3,A)') &
                '<line x1="', tick_x, '" y1="', pa_bottom, &
                '" x2="', tick_x, '" y2="', pa_bottom + 5.0_wp, '" stroke="black"/>'
            svg_content = svg_content // elem // new_line('a')
            write (elem, '(A,F0.3,A,F0.3,A,A,A)') &
                '<text x="', tick_x, '" y="', pa_bottom + 18.0_wp, &
                '" font-family="sans-serif" font-size="10" text-anchor="middle">', &
                trim(adjustl(val_str)), '</text>'
            svg_content = svg_content // elem // new_line('a')
        end do

        do i = 0, 4
            tick_y = pa_top + real(i, wp)/4.0_wp*(pa_bottom - pa_top)
            val = y_max - real(i, wp)/4.0_wp*(y_max - y_min)
            write (val_str, '(G10.3)') val
            write (elem, '(A,F0.3,A,F0.3,A,F0.3,A,F0.3,A)') &
                '<line x1="', pa_left - 5.0_wp, '" y1="', tick_y, &
                '" x2="', pa_left, '" y2="', tick_y, '" stroke="black"/>'
            svg_content = svg_content // elem // new_line('a')
            write (elem, '(A,F0.3,A,F0.3,A,A,A)') &
                '<text x="', pa_left - 8.0_wp, '" y="', tick_y + 4.0_wp, &
                '" font-family="sans-serif" font-size="10" text-anchor="end">', &
                trim(adjustl(val_str)), '</text>'
            svg_content = svg_content // elem // new_line('a')
        end do

        mid_x = (pa_left + pa_right)/2.0_wp
        mid_y = (pa_top + pa_bottom)/2.0_wp

        if (present(title)) then
            if (len_trim(title) > 0) then
                write (elem, '(A,F0.3,A,F0.3,A,A,A)') &
                    '<text x="', mid_x, '" y="', pa_top - 10.0_wp, &
                    '" font-family="sans-serif" font-size="14" '// &
                    'font-weight="bold" text-anchor="middle">', &
                    trim(title), '</text>'
                svg_content = svg_content // elem // new_line('a')
            end if
        end if

        if (present(xlabel)) then
            if (len_trim(xlabel) > 0) then
                write (elem, '(A,F0.3,A,F0.3,A,A,A)') &
                    '<text x="', mid_x, '" y="', pa_bottom + 35.0_wp, &
                    '" font-family="sans-serif" font-size="12" text-anchor="middle">', &
                    trim(xlabel), '</text>'
                svg_content = svg_content // elem // new_line('a')
            end if
        end if

        if (present(ylabel)) then
            if (len_trim(ylabel) > 0) then
                write (elem, '(A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,A,A)') &
                    '<text x="', pa_left - 45.0_wp, '" y="', mid_y, &
                    '" font-family="sans-serif" font-size="12" '// &
                    'text-anchor="middle" transform="rotate(-90 ', &
                    pa_left - 45.0_wp, ' ', mid_y, ')">', trim(ylabel), '</text>'
                svg_content = svg_content // elem // new_line('a')
            end if
        end if
    end subroutine svg_draw_axes_labels_impl

end module fortplot_svg_axes

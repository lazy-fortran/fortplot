module fortplot_svg_legend
    !! Standalone SVG legend rendering and layout

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_legend, only: legend_entry_t
    implicit none

    private
    public :: svg_render_legend_impl, svg_calc_legend_dims_impl, &
              svg_set_legend_border_impl, svg_calc_legend_pos_impl

contains

    subroutine svg_render_legend_impl(svg_content, entries, x, y, width, height)
        character(len=:), allocatable, intent(inout) :: svg_content
        type(legend_entry_t), dimension(:), intent(in) :: entries
        real(wp), intent(in) :: x, y, width, height
        integer :: i, n
        real(wp) :: entry_h, lx, ly
        character(len=1024) :: elem

        n = size(entries)
        if (n == 0) return
        entry_h = height/real(n, wp)

        write (elem, '(A,F0.3,A,F0.3,A,F0.3,A,F0.3,A)') &
            '<rect x="', x, '" y="', y, '" width="', width, &
            '" height="', height, '" fill="white" stroke="black"/>'
        svg_content = svg_content // elem // new_line('a')

        do i = 1, n
            ly = y + real(i - 1, wp)*entry_h + entry_h*0.5_wp
            lx = x + 5.0_wp
            write (elem, '(A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,F0.1,A,F0.1,A,F0.1,A)') &
                '<line x1="', lx, '" y1="', ly, '" x2="', lx + 20.0_wp, &
                '" y2="', ly, '" stroke="rgb(', &
                entries(i)%color(1)*255.0_wp, ',', &
                entries(i)%color(2)*255.0_wp, ',', &
                entries(i)%color(3)*255.0_wp, ')" stroke-width="2"/>'
            svg_content = svg_content // elem // new_line('a')

            if (allocated(entries(i)%label)) then
                write (elem, '(A,F0.3,A,F0.3,A,A,A)') &
                    '<text x="', lx + 25.0_wp, '" y="', ly + 4.0_wp, &
                    '" font-family="sans-serif" font-size="10">', &
                    trim(entries(i)%label), '</text>'
                svg_content = svg_content // elem // new_line('a')
            end if
        end do
    end subroutine svg_render_legend_impl

    subroutine svg_calc_legend_dims_impl(entries, width, height)
        type(legend_entry_t), dimension(:), intent(in) :: entries
        real(wp), intent(out) :: width, height
        integer :: n, i, max_len

        n = size(entries)
        max_len = 0
        do i = 1, n
            if (allocated(entries(i)%label)) then
                max_len = max(max_len, len_trim(entries(i)%label))
            end if
        end do
        width = 30.0_wp + real(max_len, wp)*7.0_wp
        height = real(n, wp)*18.0_wp + 10.0_wp
    end subroutine svg_calc_legend_dims_impl

    subroutine svg_set_legend_border_impl(width)
        real(wp), intent(in) :: width
        associate (w => width); end associate
    end subroutine svg_set_legend_border_impl

    subroutine svg_calc_legend_pos_impl(pa_left, pa_bottom, pa_width, pa_height, loc, x, y)
        real(wp), intent(in) :: pa_left, pa_bottom, pa_width, pa_height
        character(len=*), intent(in) :: loc
        real(wp), intent(out) :: x, y

        select case (trim(loc))
        case ('upper left')
            x = pa_left + 10.0_wp
            y = pa_bottom + 10.0_wp
        case ('upper right')
            x = pa_left + pa_width - 110.0_wp
            y = pa_bottom + 10.0_wp
        case ('lower left')
            x = pa_left + 10.0_wp
            y = pa_bottom + pa_height - 60.0_wp
        case ('lower right')
            x = pa_left + pa_width - 110.0_wp
            y = pa_bottom + pa_height - 60.0_wp
        case ('east')
            x = pa_left + pa_width + 10.0_wp
            y = pa_bottom + pa_height*0.5_wp - 30.0_wp
        case default
            x = pa_left + pa_width - 110.0_wp
            y = pa_bottom + 10.0_wp
        end select
    end subroutine svg_calc_legend_pos_impl

end module fortplot_svg_legend

module fortplot_svg_markers
    !! Standalone SVG marker element generation

    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: svg_draw_marker_impl

contains

    subroutine svg_draw_marker_impl(sx, sy, style, fill_color, edge_color, &
                                    fill_opacity, stroke_opacity, stroke_width, &
                                    svg_elem, scale)
        real(wp), intent(in) :: sx, sy
        character(len=*), intent(in) :: style
        character(len=*), intent(in) :: fill_color, edge_color
        character(len=*), intent(in) :: fill_opacity, stroke_opacity, stroke_width
        character(len=:), allocatable, intent(out) :: svg_elem
        real(wp), intent(in), optional :: scale
        real(wp) :: r, half
        character(len=512) :: local_elem

        r = 4.0_wp
        if (present(scale)) r = r*scale

        select case (trim(style))
        case ('o', 'circle')
            write (local_elem, '(A,F0.3,A,F0.3,A,F0.3,A,A,A,A,A,A,A,A,A)') &
                '<circle cx="', sx, '" cy="', sy, '" r="', r, &
                '" fill="', trim(fill_color), '" stroke="', trim(edge_color), &
                trim(fill_opacity), trim(stroke_opacity), trim(stroke_width), '/>'
        case ('s', 'square')
            half = r
            write (local_elem, '(A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,A,A,A,A,A,A,A,A)') &
                '<rect x="', sx - half, '" y="', sy - half, &
                '" width="', 2.0_wp*half, '" height="', 2.0_wp*half, &
                '" fill="', trim(fill_color), '" stroke="', trim(edge_color), &
                trim(fill_opacity), trim(stroke_opacity), trim(stroke_width), '/>'
        case ('^', 'triangle_up')
            write (local_elem, '(A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,A,A,A,A, &
&               A,A,A,A)') &
                '<polygon points="', sx, ',', sy - r, ' ', sx - r, ',', sy + r, &
                ' ', sx + r, ',', sy + r, &
                '" fill="', trim(fill_color), '" stroke="', trim(edge_color), &
                trim(fill_opacity), trim(stroke_opacity), trim(stroke_width), '/>'
        case ('v', 'triangle_down')
            write (local_elem, '(A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,A,A,A,A, &
&               A,A,A,A)') &
                '<polygon points="', sx, ',', sy + r, ' ', sx - r, ',', sy - r, &
                ' ', sx + r, ',', sy - r, &
                '" fill="', trim(fill_color), '" stroke="', trim(edge_color), &
                trim(fill_opacity), trim(stroke_opacity), trim(stroke_width), '/>'
        case ('D', 'diamond')
            write (local_elem, &
                '(A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,F0.3, &
&               A,A,A,A,A,A,A,A,A)') &
                '<polygon points="', sx, ',', sy - r, ' ', sx + r, ',', sy, &
                ' ', sx, ',', sy + r, ' ', sx - r, ',', sy, &
                '" fill="', trim(fill_color), '" stroke="', trim(edge_color), &
                trim(fill_opacity), trim(stroke_opacity), trim(stroke_width), '/>'
        case ('+', 'plus')
            write (local_elem, '(A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,A,A,A,A,A,A,A,A, &
&               F0.3,A,F0.3,A,F0.3,A,F0.3,A,A,A,A,A,A)') &
                '<line x1="', sx - r, '" y1="', sy, '" x2="', sx + r, &
                    '" y2="', sy, &
                '" stroke="', trim(edge_color), trim(stroke_opacity), &
                trim(stroke_width), '/><line x1="', sx, '" y1="', sy - r, &
                '" x2="', sx, '" y2="', sy + r, '" stroke="', &
                trim(edge_color), trim(stroke_opacity), trim(stroke_width), '/>'
        case ('x', 'cross')
            write (local_elem, '(A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,A,A,A,A,A,A,A,A, &
&               F0.3,A,F0.3,A,F0.3,A,F0.3,A,A,A,A,A,A)') &
                '<line x1="', sx - r, '" y1="', sy - r, '" x2="', sx + r, &
                '" y2="', sy + r, '" stroke="', trim(edge_color), &
                trim(stroke_opacity), trim(stroke_width), '/><line x1="', sx - r, &
                '" y1="', sy + r, '" x2="', sx + r, '" y2="', sy - r, &
                '" stroke="', trim(edge_color), trim(stroke_opacity), trim(stroke_width), '/>'
        case ('.', 'point')
            write (local_elem, '(A,F0.3,A,F0.3,A,A,A,A,A,A)') &
                '<circle cx="', sx, '" cy="', sy, '" r="2" fill="', &
                trim(fill_color), trim(fill_opacity), '/>'
        case default
            write (local_elem, '(A,F0.3,A,F0.3,A,F0.3,A,A,A,A,A,A,A,A,A)') &
                '<circle cx="', sx, '" cy="', sy, '" r="', r, &
                '" fill="', trim(fill_color), '" stroke="', trim(edge_color), &
                trim(fill_opacity), trim(stroke_opacity), trim(stroke_width), '/>'
        end select
        svg_elem = trim(local_elem)
    end subroutine svg_draw_marker_impl

end module fortplot_svg_markers

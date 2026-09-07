module fortplot_marker_paths
    !! Unit marker paths, centered at the data point, in point-size units.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    implicit none
    private
    public :: marker_vertices, MAX_MARKER_VERTICES
    integer, parameter :: MAX_MARKER_VERTICES = 32

contains

    pure subroutine marker_vertices(style, x, y, n, closed)
        character(len=*), intent(in) :: style
        real(wp), intent(out) :: x(:), y(:)
        integer, intent(out) :: n
        logical, intent(out) :: closed
        real(wp), parameter :: pi = acos(-1.0_wp)
        real(wp) :: angle, radius
        integer :: i

        x = 0.0_wp
        y = 0.0_wp
        n = 4
        closed = .true.
        select case (trim(style))
        case ('s', 'square')
            x(:4) = [-0.5_wp, 0.5_wp, 0.5_wp, -0.5_wp]
            y(:4) = [-0.5_wp, -0.5_wp, 0.5_wp, 0.5_wp]
        case ('D', 'd', 'diamond')
            x(:4) = [0.0_wp, 1.0_wp, 0.0_wp, -1.0_wp]/sqrt(2.0_wp)
            y(:4) = [-1.0_wp, 0.0_wp, 1.0_wp, 0.0_wp]/sqrt(2.0_wp)
            if (trim(style) == 'd') x = 0.6_wp*x
        case ('+', 'plus')
            closed = .false.
            x(:4) = [-0.5_wp, 0.5_wp, 0.0_wp, 0.0_wp]
            y(:4) = [0.0_wp, 0.0_wp, -0.5_wp, 0.5_wp]
        case ('x', 'cross')
            closed = .false.
            x(:4) = [-0.5_wp, 0.5_wp, -0.5_wp, 0.5_wp]
            y(:4) = [-0.5_wp, 0.5_wp, 0.5_wp, -0.5_wp]
        case ('^', 'v', '<', '>')
            n = 3
            x(:3) = [0.0_wp, -0.5_wp, 0.5_wp]
            y(:3) = [0.5_wp, -0.5_wp, -0.5_wp]
            if (trim(style) == 'v') y = -y
            if (trim(style) == '<' .or. trim(style) == '>') then
                x(:3) = [-0.5_wp, 0.5_wp, 0.5_wp]
                y(:3) = [0.0_wp, -0.5_wp, 0.5_wp]
                if (trim(style) == '>') x = -x
            end if
        case ('p', 'h', '*')
            n = 5
            if (trim(style) == 'h') n = 6
            if (trim(style) == '*') n = 10
            do i = 1, n
                angle = pi/2.0_wp + real(i - 1, wp)*2.0_wp*pi/real(n, wp)
                radius = 0.5_wp
                if (trim(style) == '*') then
                    if (mod(i, 2) == 0) radius = (3.0_wp - sqrt(5.0_wp))/4.0_wp
                end if
                x(i) = radius*cos(angle)
                y(i) = radius*sin(angle)
            end do
        case default
            n = 0
            if (len_trim(style) > 0) then
                if (style(1:1) == 'M') call svg_marker_vertices(style, x, y, n, closed)
            end if
        end select
    end subroutine marker_vertices

    pure subroutine svg_marker_vertices(path, x, y, n, closed)
        !! Supported SVG subset: one closed M/L polygon or independent M/L lines.
        !! Vega SVG coordinates scale by sqrt(size)/2 and point downward.
        character(len=*), intent(in) :: path
        real(wp), intent(out) :: x(:), y(:)
        integer, intent(out) :: n
        logical, intent(out) :: closed
        integer :: pos, count, subpaths, segment_count
        character :: command
        logical :: ok

        n = 0
        closed = .false.
        count = 0
        subpaths = 0
        segment_count = 0
        pos = 1
        do
            call skip_svg_separators(path, pos)
            if (pos > len_trim(path)) exit
            command = path(pos:pos)
            pos = pos + 1
            if (command == 'Z') then
                call skip_svg_separators(path, pos)
                if (pos <= len_trim(path)) return
                if (subpaths /= 1 .or. count < 3) return
                closed = .true.
                exit
            end if
            if (command /= 'M' .and. command /= 'L') return
            if (command == 'M') then
                if (subpaths > 0 .and. segment_count /= 2) return
                subpaths = subpaths + 1
                segment_count = 0
            end if
            if (subpaths == 0) return
            if (count >= min(size(x), size(y), MAX_MARKER_VERTICES)) return
            count = count + 1
            call read_svg_coordinate(path, pos, x(count), ok)
            if (.not. ok) return
            call read_svg_coordinate(path, pos, y(count), ok)
            if (.not. ok) return
            segment_count = segment_count + 1
        end do
        if (.not. closed .and. segment_count /= 2) return
        x(:count) = 0.5_wp*x(:count)
        y(:count) = -0.5_wp*y(:count)
        n = count
    end subroutine svg_marker_vertices

    pure subroutine skip_svg_separators(path, pos)
        character(len=*), intent(in) :: path
        integer, intent(inout) :: pos
        do while (pos <= len_trim(path))
            if (index(' ,'//achar(9)//achar(10)//achar(13), path(pos:pos)) == 0) exit
            pos = pos + 1
        end do
    end subroutine skip_svg_separators

    pure subroutine read_svg_coordinate(path, pos, value, ok)
        character(len=*), intent(in) :: path
        integer, intent(inout) :: pos
        real(wp), intent(out) :: value
        logical, intent(out) :: ok
        integer :: first, status

        ok = .false.
        call skip_svg_separators(path, pos)
        first = pos
        do while (pos <= len_trim(path))
            if (index('0123456789+-.eE', path(pos:pos)) == 0) exit
            pos = pos + 1
        end do
        if (pos == first) return
        read (path(first:pos - 1), *, iostat=status) value
        if (status /= 0) return
        ok = ieee_is_finite(value)
    end subroutine read_svg_coordinate

end module fortplot_marker_paths

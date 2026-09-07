module fortplot_spec_marker_shapes
    !! Vega symbols use bounding-box size and their own geometry, unlike MPL paths.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_marker_paths, only: marker_vertices, MAX_MARKER_VERTICES
    implicit none
    private
    public :: spec_marker_shape

contains

    subroutine spec_marker_shape(shape, angle, marker, status)
        character(len=*), intent(in) :: shape
        real(wp), intent(in) :: angle
        character(len=:), allocatable, intent(out) :: marker
        integer, intent(out) :: status
        real(wp) :: x(MAX_MARKER_VERTICES), y(MAX_MARKER_VERTICES)
        real(wp) :: theta, xr, yr
        integer :: n, i
        logical :: closed
        character(len=80) :: coordinates
        character :: command

        status = 0
        if (shape == 'circle') then
            marker = 'o'
            return
        end if
        call vega_vertices(shape, x, y, n, closed)
        if (n == 0) then
            status = 2
            return
        end if
        marker = ''
        theta = angle*acos(-1.0_wp)/180.0_wp
        do i = 1, n
            xr = cos(theta)*x(i) - sin(theta)*y(i)
            yr = sin(theta)*x(i) + cos(theta)*y(i)
            command = 'L'
            if (i == 1) command = 'M'
            if (.not. closed) then
                if (mod(i, 2) == 1) command = 'M'
            end if
            write (coordinates, '(a,f0.9,a,f0.9)') command, xr, ',', yr
            marker = marker//trim(coordinates)
        end do
        if (closed) marker = marker//'Z'
    end subroutine spec_marker_shape

    subroutine vega_vertices(shape, x, y, n, closed)
        character(len=*), intent(in) :: shape
        real(wp), intent(out) :: x(:), y(:)
        integer, intent(out) :: n
        logical, intent(out) :: closed
        real(wp), parameter :: h = sqrt(3.0_wp)/2.0_wp

        x = 0.0_wp
        y = 0.0_wp
        n = 4
        closed = .true.
        select case (shape)
        case ('square')
            x(:4) = [-1.0_wp, 1.0_wp, 1.0_wp, -1.0_wp]
            y(:4) = [-1.0_wp, -1.0_wp, 1.0_wp, 1.0_wp]
        case ('diamond')
            x(:4) = [-1.0_wp, 0.0_wp, 1.0_wp, 0.0_wp]
            y(:4) = [0.0_wp, -1.0_wp, 0.0_wp, 1.0_wp]
        case ('cross')
            n = 12
            x(:12) = [-1.0_wp, -1.0_wp, -0.4_wp, -0.4_wp, 0.4_wp, 0.4_wp, &
                1.0_wp, 1.0_wp, 0.4_wp, 0.4_wp, -0.4_wp, -0.4_wp]
            y(:12) = [-0.4_wp, 0.4_wp, 0.4_wp, 1.0_wp, 1.0_wp, 0.4_wp, &
                0.4_wp, -0.4_wp, -0.4_wp, -1.0_wp, -1.0_wp, -0.4_wp]
        case ('triangle', 'triangle-up', 'triangle-down', &
                'triangle-left', 'triangle-right')
            n = 3
            x(:3) = [0.0_wp, -1.0_wp, 1.0_wp]
            y(:3) = [-h, h, h]
            if (shape == 'triangle') y(:3) = y(:3) - h/3.0_wp
            if (shape == 'triangle-down') y(:3) = -y(:3)
            if (shape == 'triangle-left' .or. shape == 'triangle-right') then
                x(:3) = [-h, h, h]
                y(:3) = [0.0_wp, -1.0_wp, 1.0_wp]
                if (shape == 'triangle-right') x(:3) = -x(:3)
            end if
        case ('stroke')
            n = 2
            closed = .false.
            x(:2) = [-1.0_wp, 1.0_wp]
        case default
            call marker_vertices(shape, x, y, n, closed)
            if (len_trim(shape) == 0) n = 0
            if (len_trim(shape) > 0) then
                if (shape(1:1) /= 'M') n = 0
            end if
            x = 2.0_wp*x
            y = -2.0_wp*y
        end select
    end subroutine vega_vertices

end module fortplot_spec_marker_shapes

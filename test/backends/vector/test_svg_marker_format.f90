program test_svg_marker_format
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_svg_markers, only: svg_draw_marker_impl
    implicit none

    character(len=:), allocatable :: elem
    character(len=*), parameter :: opacity = ' stroke-opacity="0.500"'
    character(len=*), parameter :: width = ' stroke-width="2.000"'
    character(len=*), parameter :: stroke = &
        '" stroke="black"'//opacity//width//'/>'
    character(len=13), parameter :: styles(*) = [character(len=13) :: &
        'o', 'circle', 's', 'square', '^', 'triangle_up', 'v', &
        'triangle_down', 'D', 'diamond', '+', 'plus', 'x', 'cross', &
        '.', 'point', 'unknown']
    integer :: i

    call draw('x')
    call expect('<line x1="6.000" y1="16.000" x2="14.000" y2="24.000' &
        //stroke//'<line x1="6.000" y1="24.000" x2="14.000"' &
        //' y2="16.000'//stroke)
    call draw('plus')
    call expect('<line x1="6.000" y1="20.000" x2="14.000" y2="20.000' &
        //stroke//'<line x1="10.000" y1="16.000" x2="10.000"' &
        //' y2="24.000'//stroke)

    do i = 1, size(styles)
        call draw(trim(styles(i)))
        if (trim(styles(i)) /= '.' .and. trim(styles(i)) /= 'point') then
            if (index(elem, 'stroke="black"') == 0) then
                print *, 'FAIL: invalid stroke attributes for ', trim(styles(i))
                error stop 1
            end if
        end if
        if (index(elem, 'fill=') > 0) then
            if (index(elem, 'fill="red"') == 0) then
                print *, 'FAIL: invalid fill attribute for ', trim(styles(i))
                error stop 1
            end if
        end if
    end do
    print *, 'PASS: SVG marker coordinates and quoted attributes'

contains

    subroutine draw(style)
        character(len=*), intent(in) :: style
        call svg_draw_marker_impl(10.0_wp, 20.0_wp, style, 'red', 'black', &
            ' fill-opacity="0.250"', opacity, width, elem)
    end subroutine draw

    subroutine expect(expected)
        character(len=*), intent(in) :: expected
        if (elem /= expected) then
            print *, 'FAIL: SVG marker geometry or attributes'
            print *, 'Expected: ', expected
            print *, 'Actual:   ', elem
            error stop 1
        end if
    end subroutine expect

end program test_svg_marker_format

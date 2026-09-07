program test_marker_profile
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_core, only: figure_t
    use fortplot_test_helpers, only: test_get_temp_path, &
        test_initialize_environment
    implicit none
    integer, parameter :: width = 780, height = 300
    character(len=1), parameter :: styles(15) = &
        ['.', 'o', 's', 'D', 'd', 'x', '+', '*', '^', 'v', '<', '>', 'p', 'h', 'o']
    real(wp), parameter :: blue(3) = [31.0_wp, 119.0_wp, 180.0_wp]/255.0_wp
    type(figure_t) :: fig
    real(wp) :: rgb(width, height, 3), area(1), x(1), y(1)
    integer :: i, row, cx, cy, colored
    character(len=:), allocatable :: path

    call test_initialize_environment('marker_profile')
    call fig%initialize(width, height)
    call fig%set_xlim(0.0_wp, 16.0_wp)
    call fig%set_ylim(0.0_wp, 3.0_wp)
    do row = 1, 2
        do i = 1, size(styles)
            x = real(i, wp)
            y = real(row, wp)
            area = 36.0_wp*real(row*row, wp)
            if (i == 15) area = 0.0_wp
            call fig%scatter(x, y, marker=styles(i), s=area, &
                color=blue, linewidth=1.0_wp)
        end do
    end do
    call fig%extract_rgb_data_for_animation(rgb)
    do row = 1, 2
        cy = nint(height*(0.89_wp - 0.77_wp*real(row, wp)/3.0_wp))
        do i = 1, size(styles)
            cx = nint(width*(0.125_wp + 0.775_wp*real(i, wp)/16.0_wp))
            colored = count(rgb(cx - 17:cx + 17, cy - 17:cy + 17, 3) - &
                rgb(cx - 17:cx + 17, cy - 17:cy + 17, 1) > 0.2_wp)
            if (i == 15) then
                if (colored /= 0) error stop 'zero scatter area must be invisible'
            else
                if (colored < 3) error stop 'supported marker left no visible pixels'
            end if
        end do
    end do
    path = test_get_temp_path('marker_profile.png')
    call fig%savefig(path)
    print '(a)', 'Marker PNG: '//path
    path = test_get_temp_path('marker_profile.pdf')
    call fig%savefig(path)
    print '(a)', 'Marker PDF: '//path
    print '(a)', 'PASS: all supported marker paths render and zero area stays empty'
end program test_marker_profile

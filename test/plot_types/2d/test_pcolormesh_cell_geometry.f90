program test_pcolormesh_cell_geometry
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_core, only: figure_t
    use fortplot_test_helpers, only: test_initialize_environment, test_get_temp_path
    implicit none

    type(figure_t) :: fig
    real(wp) :: values(2, 3)
    real(wp), allocatable :: rgb(:, :, :)
    integer :: i
    ! Matplotlib's default viridis at Normalize(0,5)([0,1,2,3,4,5]).
    integer, parameter :: colors(3, 6) = reshape([68, 1, 84, 65, 68, 135, &
        42, 120, 142, 34, 168, 132, 122, 209, 81, 253, 231, 37], [3, 6])

    call test_initialize_environment('pcolormesh_cell_geometry')
    allocate (rgb(640, 480, 3))
    values = reshape([0.0_wp, 3.0_wp, 1.0_wp, 4.0_wp, 2.0_wp, 5.0_wp], [2, 3])
    call render_case('asymmetric', [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp], &
        [0.0_wp, 1.0_wp, 2.0_wp], values)
    do i = 1, 6
        call require_color(colors(:, i))
    end do
    call render_case('nonuniform', [0.0_wp, 0.5_wp, 2.0_wp, 3.0_wp], &
        [0.0_wp, 0.5_wp, 2.0_wp], values)
    call render_case('descending', [3.0_wp, 2.0_wp, 1.0_wp, 0.0_wp], &
        [2.0_wp, 1.0_wp, 0.0_wp], values)
    call render_case('singleton', [0.0_wp, 3.0_wp], [0.0_wp, 2.0_wp], &
        reshape([7.0_wp], [1, 1]))
    call require_color(colors(:, 1))
    print *, 'ARTIFACTS: ', test_get_temp_path('pcolormesh_asymmetric.png')
    print *, 'PASS: every mesh cell is rendered, including a constant singleton'

contains

    subroutine render_case(name, x, y, c)
        character(len=*), intent(in) :: name
        real(wp), contiguous, intent(in) :: x(:), y(:), c(:, :)

        call fig%initialize(width=640, height=480, backend='png')
        call fig%add_pcolormesh(x, y, c)
        call fig%set_xlim(0.0_wp, 3.0_wp)
        call fig%set_ylim(0.0_wp, 2.0_wp)
        call fig%extract_rgb_data_for_animation(rgb)
        call fig%savefig(test_get_temp_path('pcolormesh_'//name//'.png'))
        call fig%savefig(test_get_temp_path('pcolormesh_'//name//'.pdf'))
    end subroutine render_case

    subroutine require_color(color)
        integer, intent(in) :: color(3)
        logical :: matching(640, 480)
        integer :: channel

        matching = .true.
        do channel = 1, 3
            matching = matching .and. &
                abs(255.0_wp*rgb(:, :, channel) - real(color(channel), wp)) <= 2.0_wp
        end do
        if (count(matching) < 100) error stop 'A mesh cell color is missing'
    end subroutine require_color
end program test_pcolormesh_cell_geometry

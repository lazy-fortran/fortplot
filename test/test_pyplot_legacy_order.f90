program test_pyplot_legacy_order
    !! Ensure stateful pyplot wrappers preserve legacy positional ordering
    use fortplot, only: figure, get_global_figure, step, stem, fill
    use fortplot, only: fill_between, polar, pie, imshow
    use fortplot_figure_core, only: figure_t, plot_data_t
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    call test_step()
    call test_stem()
    call test_fill()
    call test_fill_between()
    call test_polar()
    call test_pie()
    call test_imshow()

    print *, '✓ Pyplot positional wrappers remain OO-compatible'

contains

    subroutine test_step()
        real(wp) :: x(4), y(4)
        class(figure_t), pointer :: fig
        integer :: i

        call reset_global(fig)
        do i = 1, size(x)
            x(i) = real(i, wp)
            y(i) = sin(real(i, wp))
        end do

        call step(x, y, 'pre', 'legacy-step', '--', 'blue', 2.0_wp)
        call assert_label_anywhere(fig, 'legacy-step')
    end subroutine test_step

    subroutine test_stem()
        real(wp) :: x(5), y(5)
        class(figure_t), pointer :: fig
        integer :: i

        call reset_global(fig)
        do i = 1, size(x)
            x(i) = real(i - 1, wp)
            y(i) = 0.5_wp * real(i, wp)
        end do

        call stem(x, y, 'r-', 'ro', 'g-', 'legacy-stem', 0.0_wp)
        call assert_label_anywhere(fig, 'legacy-stem')
    end subroutine test_stem

    subroutine test_fill()
        real(wp) :: x(6), y(6)
        class(figure_t), pointer :: fig
        integer :: i

        call reset_global(fig)
        do i = 1, size(x)
            x(i) = real(i - 1, wp)
            y(i) = 0.1_wp * real(i * i, wp)
        end do

        call fill(x, y, 'orange', 0.4_wp)
        call assert_no_labels(fig)
    end subroutine test_fill

    subroutine test_fill_between()
        real(wp) :: x(6), y1(6), y2(6)
        logical :: mask(6)
        class(figure_t), pointer :: fig
        integer :: i

        call reset_global(fig)
        do i = 1, size(x)
            x(i) = real(i - 1, wp)
            y1(i) = sin(real(i, wp))
            y2(i) = 0.5_wp * y1(i)
        end do
        mask = .true.

        call fill_between(x, y1, y2, mask, 'green', 0.3_wp, .false.)
        call assert_no_labels(fig)
    end subroutine test_fill_between

    subroutine test_polar()
        real(wp) :: theta(5), r(5)
        class(figure_t), pointer :: fig
        integer :: i
        real(wp), parameter :: PI = acos(-1.0_wp)

        call reset_global(fig)
        do i = 1, size(theta)
            theta(i) = 2.0_wp * PI * real(i - 1, wp) / real(size(theta), wp)
            r(i) = 1.0_wp + 0.2_wp * real(i, wp)
        end do

        call polar(theta, r, 'r-', 'legacy-polar', '--', 'o', 'red')
        call assert_label_anywhere(fig, 'legacy-polar')
    end subroutine test_polar

    subroutine test_pie()
        real(wp) :: values(3)
        character(len=1) :: labels(3)
        character(len=5) :: colors(3)
        real(wp) :: explode(3)
        class(figure_t), pointer :: fig

        call reset_global(fig)
        values = [0.5_wp, 0.3_wp, 0.2_wp]
        labels = ['A', 'B', 'C']
        colors = ['red  ', 'green', 'blue ']
        explode = [0.0_wp, 0.1_wp, 0.0_wp]

        call pie(values, labels, colors, explode, '%.1f', 45.0_wp)
        call assert_label_anywhere(fig, 'A')
    end subroutine test_pie

    subroutine test_imshow()
        real(wp) :: z(3, 3)
        real(wp) :: extent_vals(4)
        class(figure_t), pointer :: fig
        integer :: i

        call reset_global(fig)
        z = reshape([(real(i, wp), i = 1, size(z))], shape(z))
        extent_vals = [-1.0_wp, 1.0_wp, 0.0_wp, 2.0_wp]

        call imshow(z, 'viridis', 0.5_wp, 0.0_wp, 1.0_wp, 'lower', extent_vals, &
                    'nearest', 'equal')
        call assert_plot_count(fig, 1)
    end subroutine test_imshow

    subroutine reset_global(fig)
        class(figure_t), pointer :: fig

        call figure()
        fig => get_global_figure()
        if (.not. associated(fig)) then
            error stop 'reset_global: global figure not associated'
        end if
    end subroutine reset_global

    subroutine assert_label_anywhere(fig, expected)
        class(figure_t), pointer, intent(in) :: fig
        character(len=*), intent(in) :: expected
        type(plot_data_t), pointer :: plots(:)
        integer :: count, i

        count = fig%get_plot_count()
        if (count <= 0) then
            error stop 'assert_label_anywhere: expected plots to exist'
        end if

        plots => fig%get_plots()
        if (.not. associated(plots)) then
            error stop 'assert_label_anywhere: plots pointer not associated'
        end if

        do i = 1, count
            if (allocated(plots(i)%label)) then
                if (trim(plots(i)%label) == trim(expected)) return
            end if
        end do

        error stop 'assert_label_anywhere: expected label not found'
    end subroutine assert_label_anywhere

    subroutine assert_no_labels(fig)
        class(figure_t), pointer, intent(in) :: fig
        type(plot_data_t), pointer :: plots(:)
        integer :: count, i

        count = fig%get_plot_count()
        if (count <= 0) then
            error stop 'assert_no_labels: expected plots to exist'
        end if

        plots => fig%get_plots()
        if (.not. associated(plots)) then
            error stop 'assert_no_labels: plots pointer not associated'
        end if

        do i = 1, count
            if (allocated(plots(i)%label)) then
                if (len_trim(plots(i)%label) /= 0) then
                    error stop 'assert_no_labels: unexpected label discovered'
                end if
            end if
        end do
    end subroutine assert_no_labels

    subroutine assert_plot_count(fig, min_expected)
        class(figure_t), pointer, intent(in) :: fig
        integer, intent(in) :: min_expected
        integer :: count

        count = fig%get_plot_count()
        if (count < min_expected) then
            error stop 'assert_plot_count: not enough plots'
        end if
    end subroutine assert_plot_count

end program test_pyplot_legacy_order

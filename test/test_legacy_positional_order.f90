program test_legacy_positional_order
    !! Ensure figure_t add_* helpers accept legacy pyplot positional ordering
    use fortplot, only: figure_t
    use fortplot_figure_core, only: PLOT_TYPE_FILL, plot_data_t
    use iso_fortran_env, only: wp => real64
    implicit none

    call test_step()
    call test_stem()
    call test_fill()
    call test_fill_between()
    call test_polar()
    call test_pie()
    call test_imshow()

    print *, 'PASS: Legacy positional ordering works for figure_t add_* helpers'

contains

    subroutine test_step()
        type(figure_t) :: fig
        type(plot_data_t), pointer :: plots(:)
        real(wp) :: x(5), y(5)
        integer :: n, i

        call fig%initialize()
        x = [(real(i, wp), i = 1, size(x))]
        y = sin(x)

        call fig%add_step(x, y, 'legacy-step')
        call fetch_plots(fig, plots, n)
        call assert_label(plots(n), 'legacy-step')
    end subroutine test_step

    subroutine test_stem()
        type(figure_t) :: fig
        type(plot_data_t), pointer :: plots(:)
        real(wp) :: x(4), y(4)
        integer :: n, i

        call fig%initialize()
        x = [(real(i, wp), i = 1, size(x))]
        y = x * 0.5_wp

        call fig%add_stem(x, y, 'legacy-stem')
        call fetch_plots(fig, plots, n)
        call assert_any_label(plots, n, 'legacy-stem')
    end subroutine test_stem

    subroutine test_fill()
        type(figure_t) :: fig
        type(plot_data_t), pointer :: plots(:)
        real(wp) :: x(6), y(6)
        integer :: n, i

        call fig%initialize()
        x = [(real(i, wp), i = 1, size(x))]
        y = sin(x)

        call fig%add_fill(x, y)
        call fetch_plots(fig, plots, n)
        call assert_no_labels(plots, n)
    end subroutine test_fill

    subroutine test_fill_between()
        type(figure_t) :: fig
        type(plot_data_t), pointer :: plots(:)
        real(wp) :: x(6), y1(6), y2(6)
        logical :: mask(6)
        integer :: n, i

        call fig%initialize()
        x = [(real(i, wp), i = 1, size(x))]
        y1 = sin(x)
        y2 = y1 * 0.5_wp
        mask = .true.

        call fig%add_fill_between(x, y1, y2, mask, 'red', 0.25_wp)
        call fetch_plots(fig, plots, n)
        call assert_no_labels(plots, n)
        call assert_fill_between_storage(plots(n), x, y1, y2, mask)
    end subroutine test_fill_between

    subroutine test_polar()
        type(figure_t) :: fig
        type(plot_data_t), pointer :: plots(:)
        real(wp) :: theta(5), r(5)
        integer :: n, i

        call fig%initialize()
        theta = [(2.0_wp * acos(-1.0_wp) * real(i - 1, wp) / real(size(theta), wp), &
                 i = 1, size(theta))]
        r = 1.0_wp + 0.1_wp * [(real(i, wp), i = 1, size(r))]

        call fig%add_polar(theta, r, 'legacy-polar')
        call fetch_plots(fig, plots, n)
        call assert_label(plots(n), 'legacy-polar')
    end subroutine test_polar

    subroutine test_pie()
        type(figure_t) :: fig
        type(plot_data_t), pointer :: plots(:)
        real(wp) :: values(3)
        integer :: n

        call fig%initialize()
        values = [0.5_wp, 0.3_wp, 0.2_wp]

        call fig%add_pie(values, ['A', 'B', 'C'], '%.1f', 45.0_wp)
        call fetch_plots(fig, plots, n)
        call assert_any_label(plots, n, 'A')
    end subroutine test_pie

    subroutine test_imshow()
        type(figure_t) :: fig
        real(wp) :: z(4, 4)
        integer :: i

        call fig%initialize()
        z = reshape([(real(i, wp), i = 1, size(z))], shape(z))

        call fig%add_imshow(z, xlim=[-1.0_wp, 1.0_wp], ylim=[0.0_wp, 2.0_wp])
        if (fig%get_plot_count() <= 0) then
            error stop 'test_imshow: expected plot after legacy xlim/ylim call'
        end if
    end subroutine test_imshow

    subroutine fetch_plots(fig, plots, count)
        class(figure_t), intent(inout) :: fig
        type(plot_data_t), pointer :: plots(:)
        integer, intent(out) :: count

        count = fig%get_plot_count()
        if (count <= 0) then
            error stop 'fetch_plots: expected at least one plot'
        end if
        plots => fig%get_plots()
        if (.not. associated(plots)) then
            error stop 'fetch_plots: plots pointer not associated'
        end if
    end subroutine fetch_plots

    subroutine assert_label(plot, expected)
        type(plot_data_t), intent(in) :: plot
        character(len=*), intent(in) :: expected
        character(len=:), allocatable :: label_value

        if (allocated(plot%label)) then
            label_value = trim(plot%label)
        else
            label_value = ''
        end if

        if (trim(expected) /= trim(label_value)) then
            error stop 'assert_label: label mismatch'
        end if
    end subroutine assert_label

    subroutine assert_any_label(plots, count, expected)
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: count
        character(len=*), intent(in) :: expected
        integer :: i

        do i = 1, count
            if (allocated(plots(i)%label)) then
                if (trim(plots(i)%label) == trim(expected)) return
            end if
        end do
        error stop 'assert_any_label: expected label not found'
    end subroutine assert_any_label

    subroutine assert_fill_between_storage(plot, x, y1, y2, mask)
        type(plot_data_t), intent(in) :: plot
        real(wp), intent(in) :: x(:), y1(:), y2(:)
        logical, intent(in) :: mask(:)

        if (plot%plot_type /= PLOT_TYPE_FILL) then
            error stop 'assert_fill_between_storage: expected fill plot'
        end if
        if (.not. allocated(plot%fill_between_data%x) .or. &
            .not. allocated(plot%fill_between_data%upper) .or. &
            .not. allocated(plot%fill_between_data%lower) .or. &
            .not. allocated(plot%fill_between_data%mask)) then
            error stop 'assert_fill_between_storage: missing arrays'
        end if
        if (.not. plot%fill_between_data%has_mask) then
            error stop 'assert_fill_between_storage: mask not stored'
        end if
        if (any(abs(plot%fill_between_data%x - x) > 1.0e-12_wp) .or. &
            any(abs(plot%fill_between_data%upper - y1) > 1.0e-12_wp) .or. &
            any(abs(plot%fill_between_data%lower - y2) > 1.0e-12_wp) .or. &
            any(plot%fill_between_data%mask .neqv. mask)) then
            error stop 'assert_fill_between_storage: data mismatch'
        end if
        if (abs(plot%fill_alpha - 0.25_wp) > 1.0e-12_wp) then
            error stop 'assert_fill_between_storage: alpha mismatch'
        end if
        if (any(abs(plot%color - [1.0_wp, 0.0_wp, 0.0_wp]) > 1.0e-12_wp)) then
            error stop 'assert_fill_between_storage: color mismatch'
        end if
    end subroutine assert_fill_between_storage

    subroutine assert_no_labels(plots, count)
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: count
        integer :: i

        do i = 1, count
            if (allocated(plots(i)%label)) then
                if (len_trim(plots(i)%label) /= 0) then
                    error stop 'assert_no_labels: unexpected label assigned'
                end if
            end if
        end do
    end subroutine assert_no_labels

end program test_legacy_positional_order

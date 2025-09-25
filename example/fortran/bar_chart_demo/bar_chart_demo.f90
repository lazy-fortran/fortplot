program bar_chart_demo
    !! Demonstrates vertical and horizontal bar charts using fortplot
    !! Covers both pyplot-style and figure_t object workflows

    use, intrinsic :: iso_fortran_env, only: dp => real64
    use fortplot, only: figure, bar, barh, xlabel, ylabel, title, legend
    use fortplot, only: savefig_with_status, figure_t
    use fortplot_plotting_advanced, only: bar_impl
    use fortplot_errors, only: SUCCESS
    implicit none

    call demo_stateful_grouped()
    call demo_stateful_horizontal()
    call demo_object_grouped()

contains

    subroutine demo_stateful_grouped()
        real(dp) :: centers(4)
        real(dp) :: positions_a(4)
        real(dp) :: positions_b(4)
        real(dp) :: product_a(4)
        real(dp) :: product_b(4)
        real(dp) :: offset
        integer :: status
        logical :: ok

        centers = [1.0d0, 2.0d0, 3.0d0, 4.0d0]
        product_a = [4.5d0, 5.8d0, 6.1d0, 6.7d0]
        product_b = [3.2d0, 4.6d0, 5.4d0, 6.0d0]
        offset = 0.2d0
        positions_a = centers - offset
        positions_b = centers + offset

        call figure(figsize=[7.5d0, 5.0d0])
        call bar(positions_a, product_a, width=0.4d0, label='Product A')
        call bar(positions_b, product_b, width=0.4d0, label='Product B')
        call xlabel('Quarter')
        call ylabel('Revenue (million $)')
        call title('Stateful API - grouped bar chart')
        call legend()

        ok = .true.
        call savefig_with_status('output/example/fortran/bar_chart_demo/' // &
                                 'stateful_grouped.png', status)
        if (status /= SUCCESS) ok = .false.
        call savefig_with_status('output/example/fortran/bar_chart_demo/' // &
                                 'stateful_grouped.pdf', status)
        if (status /= SUCCESS) ok = .false.
        call savefig_with_status('output/example/fortran/bar_chart_demo/' // &
                                 'stateful_grouped.txt', status)
        if (status /= SUCCESS) ok = .false.
        if (.not. ok) then
            print *, 'WARNING: failed to save stateful grouped bar outputs'
        end if
    end subroutine demo_stateful_grouped

    subroutine demo_stateful_horizontal()
        real(dp) :: employee_levels(4)
        real(dp) :: proficiency(4)
        integer :: status
        logical :: ok

        employee_levels = [1.0d0, 2.0d0, 3.0d0, 4.0d0]
        proficiency = [72.0d0, 65.0d0, 84.0d0, 90.0d0]

        call figure(figsize=[6.8d0, 4.6d0])
        call barh(employee_levels, proficiency, height=0.6d0, &
                  label='Certification completion')
        call xlabel('Completion (%)')
        call ylabel('Training module')
        call title('Stateful API - horizontal bar chart')
        call legend()

        ok = .true.
        call savefig_with_status('output/example/fortran/bar_chart_demo/' // &
                                 'stateful_horizontal.png', status)
        if (status /= SUCCESS) ok = .false.
        call savefig_with_status('output/example/fortran/bar_chart_demo/' // &
                                 'stateful_horizontal.pdf', status)
        if (status /= SUCCESS) ok = .false.
        call savefig_with_status('output/example/fortran/bar_chart_demo/' // &
                                 'stateful_horizontal.txt', status)
        if (status /= SUCCESS) ok = .false.
        if (.not. ok) then
            print *, 'WARNING: failed to save stateful horizontal bar outputs'
        end if
    end subroutine demo_stateful_horizontal

    subroutine demo_object_grouped()
        type(figure_t) :: fig
        real(dp) :: centers(3)
        real(dp) :: baseline(3)
        real(dp) :: projected(3)
        real(dp) :: offset
        real(dp) :: positions_baseline(3)
        real(dp) :: positions_projected(3)
        integer :: status
        logical :: ok

        call fig%initialize()

        centers = [1.0d0, 2.0d0, 3.0d0]
        baseline = [2.8d0, 3.4d0, 3.9d0]
        projected = [3.5d0, 3.8d0, 4.6d0]
        offset = 0.18d0
        positions_baseline = centers - offset
        positions_projected = centers + offset

        call fig%set_title('Object API - grouped budget comparison')
        call fig%set_xlabel('Team')
        call fig%set_ylabel('Quarterly budget (M$)')
        call bar_impl(fig, positions_baseline, baseline, width=0.32d0, &
                      label='Baseline')
        call bar_impl(fig, positions_projected, projected, width=0.32d0, &
                      label='Projected')
        call fig%legend()

        ok = .true.
        call fig%savefig_with_status('output/example/fortran/bar_chart_demo/' // &
                                     'oo_grouped.png', status)
        if (status /= SUCCESS) ok = .false.
        call fig%savefig_with_status('output/example/fortran/bar_chart_demo/' // &
                                     'oo_grouped.pdf', status)
        if (status /= SUCCESS) ok = .false.
        call fig%savefig_with_status('output/example/fortran/bar_chart_demo/' // &
                                     'oo_grouped.txt', status)
        if (status /= SUCCESS) ok = .false.
        if (.not. ok) then
            print *, 'WARNING: failed to save OO grouped bar outputs'
        end if
    end subroutine demo_object_grouped

end program bar_chart_demo

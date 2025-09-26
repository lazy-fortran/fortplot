program pie_chart_demo
    !! Demonstrates pie charts with exploded wedges and autopct labels

    use, intrinsic :: iso_fortran_env, only: dp => real64
    use fortplot, only: figure, pie, title, legend, savefig_with_status, figure_t
    use fortplot_errors, only: SUCCESS
    implicit none

    call demo_stateful_sales()
    call demo_object_energy()

contains

    subroutine demo_stateful_sales()
        real(dp) :: sales(5)
        real(dp) :: explode_vals(5)
        character(len=16) :: labels(5)
        integer :: status
        logical :: ok

        sales = [30.0_dp, 22.0_dp, 18.0_dp, 15.0_dp, 15.0_dp]
        explode_vals = [0.0_dp, 0.05_dp, 0.0_dp, 0.08_dp, 0.0_dp]
        labels(1) = 'North'
        labels(2) = 'East'
        labels(3) = 'South'
        labels(4) = 'West'
        labels(5) = 'Online'

        call figure(figsize=[6.0_dp, 6.0_dp])
        call pie(sales, labels=labels, autopct='%.1f%%', explode=explode_vals, startangle=90.0_dp)
        call title('Regional revenue share')
        call legend('east')

        ok = .true.
        call savefig_with_status('output/example/fortran/pie_chart_demo/stateful_sales.png', status)
        if (status /= SUCCESS) ok = .false.
        call savefig_with_status('output/example/fortran/pie_chart_demo/stateful_sales.pdf', status)
        if (status /= SUCCESS) ok = .false.
        call savefig_with_status('output/example/fortran/pie_chart_demo/stateful_sales.txt', status)
        if (status /= SUCCESS) ok = .false.
        if (.not. ok) then
            print *, 'WARNING: failed to save stateful sales outputs'
        end if
    end subroutine demo_stateful_sales

    subroutine demo_object_energy()
        type(figure_t) :: fig
        real(dp) :: sources(4)
        real(dp) :: explode_vals(4)
        character(len=20) :: labels(4)
        integer :: status
        logical :: ok

        call fig%initialize(width=640, height=640)

        sources = [42.0_dp, 28.0_dp, 18.0_dp, 12.0_dp]
        explode_vals = [0.1_dp, 0.0_dp, 0.0_dp, 0.05_dp]
        labels(1) = 'Solar'
        labels(2) = 'Wind'
        labels(3) = 'Hydro'
        labels(4) = 'Storage'

        call fig%set_title('Clean energy capacity mix')
        call fig%add_pie(sources, labels=labels, autopct='%.0f%%', explode=explode_vals, startangle=75.0_dp)
        call fig%legend(location='east')

        ok = .true.
        call fig%savefig_with_status('output/example/fortran/pie_chart_demo/oo_energy.png', status)
        if (status /= SUCCESS) ok = .false.
        call fig%savefig_with_status('output/example/fortran/pie_chart_demo/oo_energy.pdf', status)
        if (status /= SUCCESS) ok = .false.
        call fig%savefig_with_status('output/example/fortran/pie_chart_demo/oo_energy.txt', status)
        if (status /= SUCCESS) ok = .false.
        if (.not. ok) then
            print *, 'WARNING: failed to save OO energy outputs'
        end if
    end subroutine demo_object_energy

end program pie_chart_demo

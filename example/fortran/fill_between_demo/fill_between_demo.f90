program fill_between_demo
    !! Demonstrates stateful and object-oriented fill_between usage
    use iso_fortran_env, only: wp => real64
    use fortplot, only: figure, plot, fill_between, xlabel, ylabel, title, legend
    use fortplot, only: savefig_with_status, figure_t
    use fortplot_errors, only: SUCCESS
    implicit none

    call demo_stateful()
    call demo_object()

contains

    subroutine demo_stateful()
        real(wp) :: x(200), upper(200), lower(200)
        logical :: mask(200)
        integer :: i, status
        logical :: ok

        do i = 1, size(x)
            x(i) = 2.0_wp * acos(-1.0_wp) * real(i - 1, wp) / &
                   real(size(x) - 1, wp)
            upper(i) = sin(x(i))
            lower(i) = 0.5_wp * sin(x(i))
        end do
        mask = x <= acos(-1.0_wp)

        call figure()
        call plot(x, upper, label='sin(x)')
        call fill_between(x, upper, lower, mask)
        call title('Stateful fill_between with mask')
        call xlabel('angle (rad)')
        call ylabel('amplitude')
        call legend()

        ok = .true.
        call savefig_with_status('output/example/fortran/fill_between_demo/' // &
                                 'stateful_fill_between.png', status)
        if (status /= SUCCESS) ok = .false.
        call savefig_with_status('output/example/fortran/fill_between_demo/' // &
                                 'stateful_fill_between.txt', status)
        if (status /= SUCCESS) ok = .false.
        call savefig_with_status('output/example/fortran/fill_between_demo/' // &
                                 'stateful_fill_between.pdf', status)
        if (status /= SUCCESS) ok = .false.
        if (.not. ok) then
            print *, 'WARNING: Failed to write stateful fill_between outputs'
        end if
    end subroutine demo_stateful

    subroutine demo_object()
        type(figure_t) :: fig
        real(wp) :: x(120), profile(120)
        integer :: i, status
        logical :: ok

        call fig%initialize()
        do i = 1, size(x)
            x(i) = real(i - 1, wp) / 20.0_wp
            profile(i) = exp(-0.1_wp * x(i)) * sin(0.8_wp * x(i))
        end do

        call fig%set_title('Object API fill_between baseline')
        call fig%set_xlabel('time')
        call fig%set_ylabel('response')
        call fig%add_plot(x, profile)
        call fig%add_fill_between(x, y1=profile)

        ok = .true.
        call fig%savefig_with_status('output/example/fortran/fill_between_demo/' // &
                                     'oo_fill_between.png', status)
        if (status /= SUCCESS) ok = .false.
        call fig%savefig_with_status('output/example/fortran/fill_between_demo/' // &
                                     'oo_fill_between.txt', status)
        if (status /= SUCCESS) ok = .false.
        call fig%savefig_with_status('output/example/fortran/fill_between_demo/' // &
                                     'oo_fill_between.pdf', status)
        if (status /= SUCCESS) ok = .false.
        if (.not. ok) then
            print *, 'WARNING: Failed to write OO fill_between outputs'
        end if
    end subroutine demo_object

end program fill_between_demo

program test_clear_uninitialized
    !! Regression test for #1638: clear() and common mutating operations on
    !! a figure_t that was never explicitly initialized must not crash. This
    !! locks in the lazy-init contract added to figure_t operations.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure_t
    implicit none

    real(wp) :: x(3), y(3)

    x = [1.0_wp, 2.0_wp, 3.0_wp]
    y = [1.0_wp, 4.0_wp, 9.0_wp]

    call case_bare_clear()
    call case_add_plot_then_clear(x, y)
    call case_clear_then_plot(x, y)
    call case_savefig_without_init()
    call case_scatter_without_init(x, y)
    call case_reflines_without_init()

    print *, "PASS: clear()/mutators on uninitialized figure are safe (#1638)"

contains

    subroutine case_bare_clear()
        type(figure_t) :: fig
        call fig%clear()
        call fig%clear()
    end subroutine case_bare_clear

    subroutine case_add_plot_then_clear(xv, yv)
        real(wp), intent(in) :: xv(:), yv(:)
        type(figure_t) :: fig
        call fig%add_plot(xv, yv)
        call fig%clear()
        if (fig%get_plot_count() /= 0) then
            print *, "FAIL: clear did not reset plot_count"
            stop 1
        end if
    end subroutine case_add_plot_then_clear

    subroutine case_clear_then_plot(xv, yv)
        real(wp), intent(in) :: xv(:), yv(:)
        type(figure_t) :: fig
        integer :: status
        call fig%clear()
        call fig%add_plot(xv, yv)
        call fig%savefig_with_status("build/test/output/clear_uninit.png", &
                                     status)
        if (status /= 0) then
            print *, "FAIL: savefig after clear returned", status
            stop 1
        end if
    end subroutine case_clear_then_plot

    subroutine case_savefig_without_init()
        type(figure_t) :: fig
        integer :: status
        call fig%savefig_with_status("build/test/output/empty_uninit.png", &
                                     status)
        if (status /= 0) then
            print *, "FAIL: savefig on bare figure returned", status
            stop 1
        end if
    end subroutine case_savefig_without_init

    subroutine case_scatter_without_init(xv, yv)
        real(wp), intent(in) :: xv(:), yv(:)
        type(figure_t) :: fig
        call fig%scatter(xv, yv)
        if (fig%get_plot_count() /= 1) then
            print *, "FAIL: scatter on bare figure did not add plot"
            stop 1
        end if
    end subroutine case_scatter_without_init

    subroutine case_reflines_without_init()
        type(figure_t) :: fig
        call fig%axhline(0.5_wp)
        call fig%axvline(0.5_wp)
        if (fig%get_plot_count() /= 2) then
            print *, "FAIL: axhline/axvline on bare figure did not add plots"
            stop 1
        end if
    end subroutine case_reflines_without_init

end program test_clear_uninitialized

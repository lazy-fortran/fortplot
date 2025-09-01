program test_matplotlib_figure_dimensions
    !! Verify matplotlib figure() sets exact pixel dimensions
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_matplotlib, only: figure, get_global_figure
    use fortplot_figure_core, only: figure_t
    implicit none

    type(figure_t), pointer :: f
    integer :: w, h

    call figure(figsize=[8.0_wp, 6.0_wp], dpi=100)
    f => get_global_figure()
    w = f%get_width()
    h = f%get_height()

    if (w /= 800 .or. h /= 600) then
        print *, "FAIL: Expected 800x600 got", w, "x", h
        stop 1
    end if

    print *, "PASS: Matplotlib figure dimensions set correctly"
end program test_matplotlib_figure_dimensions


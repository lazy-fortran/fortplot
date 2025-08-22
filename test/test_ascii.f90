program test_ascii
    !! Test program for ASCII terminal plotting functionality
    !!
    !! This program tests the ASCII backend by creating sine and cosine
    !! plots and displaying them in the terminal using character graphics.
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure
    use fortplot_system_runtime, only: is_windows
    implicit none

    real(wp), dimension(100) :: x, sx, cx
    type(figure_t) :: ascii_fig
    integer :: i

    x = [(real(i, wp), i=0, size(x) - 1)]/5.0_wp
    sx = sin(x)
    cx = cos(x)

    call ascii_fig%initialize(80, 24)
    call ascii_fig%set_title("Sine and Cosine Functions (ASCII)")

    call ascii_fig%add_plot(x, sx, label="sin(x)")
    call ascii_fig%add_plot(x, cx, label="cos(x)")

    ! Skip show() on Windows to prevent MS Paint hanging in CI
    ! Use blocking=.false. to avoid CI hang on all platforms
    if (.not. is_windows()) then
        call ascii_fig%show(blocking=.false.)
    else
        print *, "SKIPPED: show() on Windows (prevents CI hang)"
        ! Test that we can save to file instead
        call ascii_fig%savefig("test_ascii_output.txt")
    end if

    print *, ""
    print *, "ascii terminal plot created successfully!"

end program test_ascii

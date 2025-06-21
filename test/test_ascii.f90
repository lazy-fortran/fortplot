program test_ascii
    !! Test program for ASCII terminal plotting functionality
    !!
    !! This program tests the ASCII backend by creating sine and cosine
    !! plots and displaying them in the terminal using character graphics.
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure
    implicit none

    real(wp), dimension(100) :: x, sx, cx
    type(figure_t) :: ascii_fig
    integer :: i

    ! Generate some data:
    x = [(real(i, wp), i=0, size(x) - 1)]/5.0_wp
    sx = sin(x)
    cx = cos(x)

    ! Create and configure ascii figure
    call ascii_fig%initialize(80, 24)
    call ascii_fig%set_title("Sine and Cosine Functions (ASCII)")

    ! Plot data
    call ascii_fig%add_plot(x, sx, label="sin(x)")
    call ascii_fig%add_plot(x, cx, label="cos(x)")

    ! Display to terminal
    call ascii_fig%show()

    print *, ""
    print *, "ascii terminal plot created successfully!"

end program test_ascii

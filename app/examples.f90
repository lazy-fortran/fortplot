program main
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure
    implicit none

    real(wp), dimension(100) :: x, sx, cx
    type(figure_t) :: png_fig, pdf_fig
    integer :: i

    ! Generate some data:
    x = [(real(i, wp), i=0, size(x) - 1)]/5.0_wp
    sx = sin(x)
    cx = cos(x)

    ! Create and configure PNG figure
    call png_fig%initialize(640, 480, "png")
    call png_fig%set_xlabel("x")
    call png_fig%set_ylabel("y")
    call png_fig%set_title("Sine and Cosine Functions")

    ! Plot data - sine first since it has range [-1,1], cosine has range [-1,1]
    ! Both should have same range, but let's plot sine first to establish coordinate system
    call png_fig%add_plot(x, sx)
    call png_fig%add_plot(x, cx)

    ! Save PNG
    call png_fig%savefig('output.png')

    ! Show ASCII version
    call png_fig%show_ascii(x, sx, size(x), 'sin(x) ASCII plot')

    ! Create and configure PDF figure
    call pdf_fig%initialize(640, 480, "pdf")
    call pdf_fig%set_xlabel("x")
    call pdf_fig%set_ylabel("y")
    call pdf_fig%set_title("Sine and Cosine Functions")

    call pdf_fig%add_plot(x, sx)
    call pdf_fig%add_plot(x, cx)
    call pdf_fig%savefig('output.pdf')

    print *, "PNG and PDF plots created successfully!"

end program main

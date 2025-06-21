program main
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure
    implicit none

    real(wp), dimension(100) :: x, sx, cx
    type(figure_t) :: fig
    integer :: i

    ! Generate some data:
    x = [(real(i, wp), i=0, size(x) - 1)]/5.0_wp
    sx = sin(x)
    cx = cos(x)
    
    ! Create and configure figure (no backend specified - will auto-detect from filename)
    call fig%initialize(640, 480)  ! Remove explicit backend specification
    call fig%set_xlabel("x")
    call fig%set_ylabel("y")
    call fig%set_title("Sine and Cosine Functions")

    ! Add plot data (same data will be used for all output formats)
    call fig%add_plot(x, sx)
    call fig%add_plot(x, cx)

    ! Save to multiple formats - backend auto-detected from file extension
    call fig%savefig('output.png')   ! Automatically uses PNG backend
    call fig%savefig('output.pdf')   ! Automatically uses PDF backend  
    call fig%savefig('output.txt')   ! Automatically uses ASCII backend
    
    ! Show on terminal using new unified interface
    call fig%show()  ! Displays ASCII version directly
    
    print *, "PNG, PDF, and ASCII plots created successfully!"
    print *, "Same plot data rendered with multiple backends!"

end program main

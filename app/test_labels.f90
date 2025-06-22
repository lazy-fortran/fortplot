program test_labels
    use fortplot, only: figure_t
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    type(figure_t) :: fig
    real(wp) :: x(100), y(100)
    integer :: i
    
    ! Generate sample data
    do i = 1, 100
        x(i) = real(i - 1, wp) * 0.1_wp
        y(i) = sin(x(i))
    end do
    
    ! Create figure with title and labels
    call fig%initialize(800, 600)
    call fig%set_title("Sine Wave Example")
    call fig%set_xlabel("X Axis (radians)")
    call fig%set_ylabel("Y Axis (amplitude)")
    call fig%add_plot(x, y, label="sin(x)")
    call fig%savefig("test_labels.png")
    call fig%savefig("test_labels.pdf")
    
    print *, "Created test_labels.png and test_labels.pdf with title and axis labels"
end program test_labels
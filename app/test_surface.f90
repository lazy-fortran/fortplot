program test_surface
    !! Test case to reproduce and verify README surface plot example
    !! This replicates the exact code from README to verify it compiles
    
    use iso_fortran_env, only: wp => real64
    use fortplot
    implicit none

    type(figure_t) :: fig
    integer :: i, j
    real(wp), dimension(21) :: x, y
    real(wp), dimension(21,21) :: z  ! Must match: size(z,1)=size(x), size(z,2)=size(y)

    ! Create coordinate arrays
    do i = 1, 21
        x(i) = (i-1) * 0.2_wp
        y(i) = (i-1) * 0.2_wp
    end do

    ! Calculate surface values
    do i = 1, 21
        do j = 1, 21  
            z(i,j) = x(i)**2 + y(j)**2  ! Paraboloid
        end do
    end do

    call fig%initialize(800, 600)
    call fig%add_surface(x, y, z, label="Paraboloid")
    call fig%savefig("surface.png")
    
    print *, "Surface plot example compiled and executed successfully"

end program test_surface
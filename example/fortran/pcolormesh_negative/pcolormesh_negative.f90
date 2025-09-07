program pcolormesh_negative
    !! pcolormesh with negative coordinates and values to exercise edge cases
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure, title, xlabel, ylabel, pcolormesh, savefig
    implicit none

    integer, parameter :: nx = 31, ny = 21
    real(wp) :: x(nx), y(ny)
    real(wp) :: c(nx-1, ny-1)
    integer :: i, j
    real(wp), parameter :: pi = 3.141592653589793_wp
    real(wp) :: xc, yc

    ! Coordinates spanning negative and positive ranges
    do i = 1, nx
        x(i) = -2.0_wp + 3.0_wp * real(i-1, wp) / real(nx-1, wp)
    end do
    do j = 1, ny
        y(j) = -1.0_wp + 3.0_wp * real(j-1, wp) / real(ny-1, wp)
    end do

    ! Cell-centered values with both negative and positive values
    do i = 1, nx-1
        do j = 1, ny-1
            xc = 0.5_wp * (x(i) + x(i+1))
            yc = 0.5_wp * (y(j) + y(j+1))
            c(i, j) = sin(2.0_wp*pi*xc/3.0_wp) * cos(2.0_wp*pi*yc/3.0_wp) - 0.2_wp
        end do
    end do

    call figure()
    call title('Pcolormesh with Negative Coordinates and Values')
    call xlabel('X')
    call ylabel('Y')
    call pcolormesh(x, y, c, colormap='coolwarm')

    call savefig('output/example/fortran/pcolormesh_negative/pcolormesh_negative.png')
    call savefig('output/example/fortran/pcolormesh_negative/pcolormesh_negative.pdf')
    call savefig('output/example/fortran/pcolormesh_negative/pcolormesh_negative.txt')
end program pcolormesh_negative


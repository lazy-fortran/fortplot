program test_3d_png_output
    use fortplot
    use iso_fortran_env, only: wp => real64
    implicit none
    
    type(figure_t) :: fig
    real(wp), allocatable :: x(:), y(:), z(:)
    integer :: i
    
    ! Create a simple 3D helix
    allocate(x(100), y(100), z(100))
    do i = 1, 100
        x(i) = cos(real(i-1, wp) * 0.2_wp)
        y(i) = sin(real(i-1, wp) * 0.2_wp)
        z(i) = real(i-1, wp) * 0.05_wp
    end do
    
    call fig%initialize(width=800, height=600)
    call fig%add_3d_plot(x, y, z, label="3D Helix")
    call fig%set_title("3D Plot Projected to PNG")
    call fig%set_xlabel("X")
    call fig%set_ylabel("Y")
    call fig%savefig("3d_helix.png")
    
    print *, "Created 3D helix plot as PNG using default projection:"
    print *, "  - Azimuth: -60 degrees"
    print *, "  - Elevation: 30 degrees" 
    print *, "  - Distance: 10 units"
    print *, "File saved as: 3d_helix.png"
    
end program test_3d_png_output
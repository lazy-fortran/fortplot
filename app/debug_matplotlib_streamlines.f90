program debug_matplotlib_streamlines
    !! Debug the matplotlib streamline implementation
    use fortplot_streamplot_matplotlib
    use fortplot_streamline_placement
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    
    integer, parameter :: nx = 20, ny = 20
    real(real64), dimension(nx) :: x
    real(real64), dimension(ny) :: y
    real(real64), dimension(nx, ny) :: u, v
    integer :: i, j
    real, allocatable :: trajectories(:,:,:)
    integer :: n_trajectories
    integer, allocatable :: trajectory_lengths(:)
    
    ! Create same grid as streamplot_demo
    do i = 1, nx
        x(i) = -2.0_real64 + 4.0_real64 * real(i-1, real64) / real(nx-1, real64)
    end do
    
    do j = 1, ny
        y(j) = -2.0_real64 + 4.0_real64 * real(j-1, real64) / real(ny-1, real64)
    end do
    
    ! Create circular flow field
    do j = 1, ny
        do i = 1, nx
            u(i,j) = -y(j)
            v(i,j) = x(i)
        end do
    end do
    
    print *, "=== DEBUGGING MATPLOTLIB STREAMLINES ==="
    print *, "Grid: x =", x(1), "to", x(nx), ", y =", y(1), "to", y(ny)
    print *, "Flow field sample at center: u(10,10)=", u(10,10), ", v(10,10)=", v(10,10)
    
    ! Test the matplotlib implementation directly
    call streamplot_matplotlib(x, y, u, v, 1.0_real64, trajectories, n_trajectories, trajectory_lengths)
    
    print *, "Number of trajectories generated:", n_trajectories
    
    if (n_trajectories > 0) then
        print *, "Trajectory array shape:", shape(trajectories)
        
        ! Show details of first few trajectories
        do i = 1, min(5, n_trajectories)
            j = trajectory_lengths(i)
            
            print *, "Trajectory", i, "has", j, "points"
            if (j > 0) then
                print *, "  Start:", trajectories(i, 1, 1), trajectories(i, 1, 2)
                print *, "  End  :", trajectories(i, j, 1), trajectories(i, j, 2)
                
                ! Show a few middle points to check progression
                if (j > 10) then
                    print *, "  Mid1 :", trajectories(i, j/4, 1), trajectories(i, j/4, 2)
                    print *, "  Mid2 :", trajectories(i, j/2, 1), trajectories(i, j/2, 2)
                    print *, "  Mid3 :", trajectories(i, 3*j/4, 1), trajectories(i, 3*j/4, 2)
                end if
            end if
        end do
    else
        print *, "ERROR: No trajectories generated!"
    end if
    
end program debug_matplotlib_streamlines
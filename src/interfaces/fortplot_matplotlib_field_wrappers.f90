module fortplot_matplotlib_field_wrappers
    !! Contour, field, and vector visualisation wrappers for matplotlib facade

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_global, only: fig => global_figure
    use fortplot_figure_core, only: figure_t
    use fortplot_logging, only: log_error
    use fortplot_matplotlib_session, only: ensure_fig_init

    implicit none
    private

    public :: contour
    public :: contour_filled
    public :: pcolormesh
    public :: streamplot
    public :: add_contour
    public :: add_contour_filled
    public :: add_pcolormesh
    public :: add_surface

contains

    subroutine contour(x, y, z, levels, label)
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in) :: z(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: label

        call ensure_fig_init()
        call fig%add_contour(x, y, z, levels=levels, label=label)
    end subroutine contour

    subroutine contour_filled(x, y, z, levels, colormap, show_colorbar, label)
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in) :: z(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: colormap, label
        logical, intent(in), optional :: show_colorbar

        real(wp), allocatable :: wp_x(:), wp_y(:), wp_levels(:)
        real(wp), allocatable :: wp_z(:,:)

        call ensure_fig_init()
        call convert_contour_arrays(x, y, z, levels, wp_x, wp_y, wp_z, wp_levels)
        call forward_contour_filled_params(fig, wp_x, wp_y, wp_z, wp_levels, &
                                             colormap, show_colorbar, label)
    end subroutine contour_filled

    subroutine pcolormesh(x, y, z, shading, colormap, show_colorbar, label, &
                              edgecolors, linewidths, vmin, vmax)
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in) :: z(:,:)
        character(len=*), intent(in), optional :: shading, colormap, label
        logical, intent(in), optional :: show_colorbar
        real(wp), intent(in), optional :: edgecolors(3)
        real(wp), intent(in), optional :: linewidths
        real(wp), intent(in), optional :: vmin, vmax

        character(len=32) :: shading_local, colormap_local, label_local
        logical :: show_colorbar_local
        real(wp) :: vmin_local, vmax_local, linewidths_local
        integer :: nx, ny

        call ensure_fig_init()

        nx = size(x)
        ny = size(y)
        if (.not. (size(z, 1) == ny - 1 .and. size(z, 2) == nx - 1) .and. &
            .not. (size(z, 1) == ny .and. size(z, 2) == nx) .and. &
            .not. (size(z, 1) == nx - 1 .and. size(z, 2) == ny - 1) .and. &
            .not. (size(z, 1) == nx .and. size(z, 2) == ny)) then
            call log_error( &
                "pcolormesh: z dimensions incompatible with x,y grid. " // &
                "Expected one of: z(ny-1,nx-1), z(ny,nx), z(nx-1,ny-1), or z(nx,ny)")
            return
        end if

        shading_local = 'flat'
        if (present(shading)) shading_local = shading

        colormap_local = 'viridis'
        if (present(colormap)) colormap_local = colormap

        show_colorbar_local = .false.
        if (present(show_colorbar)) show_colorbar_local = show_colorbar

        label_local = ''
        if (present(label)) label_local = label

        linewidths_local = 1.0_wp
        if (present(linewidths)) linewidths_local = linewidths

        if (present(vmin)) then
            vmin_local = vmin
        else
            vmin_local = minval(z)
        end if

        if (present(vmax)) then
            vmax_local = vmax
        else
            vmax_local = maxval(z)
        end if

        call fig%add_pcolormesh(x, y, z, colormap=colormap_local, vmin=vmin_local, &
                                vmax=vmax_local, linewidths=linewidths_local)
    end subroutine pcolormesh

    subroutine streamplot(x, y, u, v, density, linewidth_scale, arrow_scale, &
                             colormap, label, arrowsize, arrowstyle)
        use fortplot_streamplot_matplotlib, only: streamplot_matplotlib

        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in) :: u(:,:), v(:,:)
        real(wp), intent(in), optional :: density, linewidth_scale, arrow_scale
        character(len=*), intent(in), optional :: colormap, label
        real(wp), intent(in), optional :: arrowsize
        character(len=*), intent(in), optional :: arrowstyle

        real(wp) :: density_local
        real(wp) :: line_color(3)
        real(wp), allocatable :: traj_x(:), traj_y(:)
        real(wp) :: xp, yp, x2, y2
        real, allocatable :: trajectories(:,:,:)
        integer, allocatable :: traj_lengths(:)
        integer :: n_traj, i, j
        integer :: nx, ny
        real(wp) :: asize
        character(len=4) :: astyle

        call ensure_fig_init()

        nx = size(x)
        ny = size(y)
        if (size(u, 1) /= nx .or. size(u, 2) /= ny) then
            call log_error("streamplot: u dimensions must match x and y")
            return
        end if
        if (size(v, 1) /= nx .or. size(v, 2) /= ny) then
            call log_error("streamplot: v dimensions must match x and y")
            return
        end if

        density_local = 1.0_wp
        if (present(density)) density_local = density

        line_color = [0.0_wp, 0.447_wp, 0.698_wp]

        call streamplot_matplotlib(x, y, u, v, density_local, trajectories, &
                                     n_traj, traj_lengths)

        do i = 1, n_traj
            if (traj_lengths(i) <= 1) cycle
            allocate(traj_x(traj_lengths(i)), traj_y(traj_lengths(i)))
            do j = 1, traj_lengths(i)
                traj_x(j) = x(1) + real(trajectories(i, j, 1), wp) * (x(nx) - x(1)) / &
                    real(nx - 1, wp)
                traj_y(j) = y(1) + real(trajectories(i, j, 2), wp) * (y(ny) - y(1)) / &
                    real(ny - 1, wp)
            end do
            call fig%add_plot(traj_x, traj_y, color=line_color)
            deallocate(traj_x, traj_y)
        end do

        ! Optional: draw arrows along streamlines when requested
        if (present(arrow_scale) .or. present(arrowsize) .or. present(arrowstyle)) then
            asize = 1.0_wp
            if (present(arrowsize)) asize = arrowsize
            astyle = 'filled'
            if (present(arrowstyle)) astyle = arrowstyle
            do i = 1, n_traj
                if (traj_lengths(i) < 5) cycle
                ! Place ~3 arrows per trajectory, similar to core
                j = max(1, traj_lengths(i) / 3)
                do while (j < traj_lengths(i))
                    ! Convert trajectory point to data coordinates
                    xp = x(1) + real(trajectories(i, j, 1), wp) * (x(nx) - x(1)) / real(nx - 1, wp)
                    yp = y(1) + real(trajectories(i, j, 2), wp) * (y(ny) - y(1)) / real(ny - 1, wp)
                    ! Direction using next step when available
                    if (j+1 <= traj_lengths(i)) then
                        x2 = x(1) + real(trajectories(i, j+1, 1), wp) * (x(nx) - x(1)) / real(nx - 1, wp)
                        y2 = y(1) + real(trajectories(i, j+1, 2), wp) * (y(ny) - y(1)) / real(ny - 1, wp)
                        call fig%backend_arrow(xp, yp, x2 - xp, y2 - yp, asize, astyle)
                    end if
                    j = j + max(1, traj_lengths(i) / 3)
                end do
            end do
        end if

        if (allocated(trajectories)) deallocate(trajectories)
        if (allocated(traj_lengths)) deallocate(traj_lengths)
    end subroutine streamplot

    subroutine add_contour(x, y, z, levels, label)
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in) :: z(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: label

        call ensure_fig_init()
        call fig%add_contour(x, y, z, levels=levels, label=label)
    end subroutine add_contour

    subroutine add_contour_filled(x, y, z, levels, colormap, show_colorbar, label)
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in) :: z(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: colormap, label
        logical, intent(in), optional :: show_colorbar

        real(wp), allocatable :: wp_x(:), wp_y(:), wp_levels(:)
        real(wp), allocatable :: wp_z(:,:)

        call ensure_fig_init()
        call convert_contour_arrays(x, y, z, levels, wp_x, wp_y, wp_z, wp_levels)
        call forward_contour_filled_params(fig, wp_x, wp_y, wp_z, wp_levels, &
                                             colormap, show_colorbar, label)
    end subroutine add_contour_filled

    subroutine add_pcolormesh(x, y, z, shading, colormap, show_colorbar, label, &
                                  edgecolors, linewidths, vmin, vmax)
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in) :: z(:,:)
        character(len=*), intent(in), optional :: shading, colormap, label
        logical, intent(in), optional :: show_colorbar
        real(wp), intent(in), optional :: edgecolors(3)
        real(wp), intent(in), optional :: linewidths
        real(wp), intent(in), optional :: vmin, vmax

        call pcolormesh(x, y, z, shading=shading, colormap=colormap, &
                        show_colorbar=show_colorbar, label=label, &
                        edgecolors=edgecolors, linewidths=linewidths, vmin=vmin, &
                        vmax=vmax)
    end subroutine add_pcolormesh

    subroutine add_surface(x, y, z, colormap, show_colorbar, alpha, edgecolor, &
                              linewidth, label)
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in) :: z(:,:)
        character(len=*), intent(in), optional :: colormap, label
        logical, intent(in), optional :: show_colorbar
        real(wp), intent(in), optional :: alpha, linewidth
        real(wp), intent(in), optional :: edgecolor(3)

        integer :: nx, ny

        call ensure_fig_init()

        nx = size(x)
        ny = size(y)
        if (.not. (size(z, 1) == ny - 1 .and. size(z, 2) == nx - 1) .and. &
            .not. (size(z, 1) == ny .and. size(z, 2) == nx) .and. &
            .not. (size(z, 1) == nx - 1 .and. size(z, 2) == ny - 1) .and. &
            .not. (size(z, 1) == nx .and. size(z, 2) == ny)) then
            call log_error( &
                "add_surface: z dimensions incompatible with x,y grid. " // &
                "Expected one of: z(ny-1,nx-1), z(ny,nx), z(nx-1,ny-1), or z(nx,ny)")
            return
        end if

        call fig%add_contour(x, y, z, levels=null(), label=label)
    end subroutine add_surface

    subroutine convert_contour_arrays(x, y, z, levels, wp_x, wp_y, wp_z, &
                                         wp_levels)
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in) :: z(:,:)
        real(wp), intent(in), optional :: levels(:)
        real(wp), allocatable, intent(out) :: wp_x(:), wp_y(:)
        real(wp), allocatable, intent(out) :: wp_z(:,:)
        real(wp), allocatable, intent(out) :: wp_levels(:)

        integer :: nx, ny

        nx = size(x)
        ny = size(y)

        allocate(wp_x(nx))
        allocate(wp_y(ny))
        allocate(wp_z(ny, nx))

        wp_x = x
        wp_y = y
        wp_z = z

        if (present(levels)) then
            allocate(wp_levels(size(levels)))
            wp_levels = levels
        else
            allocate(wp_levels(0))
        end if
    end subroutine convert_contour_arrays

    subroutine forward_contour_filled_params(fig_in, x, y, z, levels, colormap, &
                                                show_colorbar, label)
        class(figure_t), target, intent(inout) :: fig_in
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in) :: z(:,:)
        real(wp), intent(in) :: levels(:)
        character(len=*), intent(in), optional :: colormap, label
        logical, intent(in), optional :: show_colorbar

        call fig_in%add_contour_filled(x, y, z, levels=levels, colormap=colormap, &
                                       show_colorbar=show_colorbar, label=label)
    end subroutine forward_contour_filled_params

end module fortplot_matplotlib_field_wrappers

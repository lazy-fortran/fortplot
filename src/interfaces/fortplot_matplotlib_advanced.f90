module fortplot_matplotlib_advanced
    !! Consolidated matplotlib-compatible API implementations
    !! Combines plotting, contour/field, axes, and I/O into a single module

    use iso_fortran_env, only: wp => real64
    use fortplot_figure_core, only: figure_t
    use fortplot_figure_initialization, only: configure_figure_dimensions
    use fortplot_global, only: fig => global_figure
    use fortplot_logging, only: log_error, log_warning, log_info
    use fortplot_security, only: safe_launch_viewer, safe_remove_file

    ! Plotting helpers
    use fortplot_plotting_advanced, only: bar_impl, barh_impl
    use fortplot_scatter_plots, only: add_scatter_2d, add_scatter_3d
    use fortplot_errorbar_plots, only: errorbar_impl => errorbar
    use fortplot_3d_plots, only: add_3d_plot_impl => add_3d_plot

    implicit none
    private

    ! Public API surface (re-exported by fortplot_matplotlib)
    public :: plot, scatter, errorbar, boxplot
    public :: bar, barh, hist, histogram
    public :: add_plot, add_errorbar, add_scatter, add_3d_plot

    public :: contour, contour_filled, pcolormesh, streamplot
    public :: add_contour, add_contour_filled, add_pcolormesh, add_surface

    public :: xlabel, ylabel, title, legend, grid
    public :: xlim, ylim, set_xscale, set_yscale
    public :: set_line_width, set_ydata

    public :: figure, subplot, savefig, savefig_with_status
    public :: show, show_viewer
    public :: ensure_global_figure_initialized, get_global_figure

    interface show
        module procedure show_data, show_figure
    end interface show

    interface add_scatter
        module procedure add_scatter_2d_wrapper, add_scatter_3d_wrapper
    end interface add_scatter

contains

    ! ----- Shared helpers -------------------------------------------------

    subroutine ensure_fig_init()
        if (.not. allocated(fig)) then
            allocate(figure_t :: fig)
        end if
        if (.not. fig%backend_associated()) then
            call fig%initialize()
        end if
    end subroutine ensure_fig_init

    subroutine ensure_global_figure_initialized()
        call ensure_fig_init()
    end subroutine ensure_global_figure_initialized

    function get_global_figure() result(global_fig)
        class(figure_t), pointer :: global_fig
        call ensure_fig_init()
        global_fig => fig
    end function get_global_figure

    ! ----- Plotting API ---------------------------------------------------

    subroutine plot(x, y, label, linestyle)
        real(8), dimension(:), intent(in) :: x, y
        character(len=*), intent(in), optional :: label, linestyle
        call ensure_fig_init()
        call fig%add_plot(x, y, label=label, linestyle=linestyle)
    end subroutine plot

    subroutine errorbar(x, y, xerr, yerr, fmt, label, capsize, linestyle, marker, color)
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:), intent(in), optional :: xerr, yerr
        character(len=*), intent(in), optional :: fmt, label, linestyle, marker
        real(8), intent(in), optional :: capsize
        real(8), dimension(3), intent(in), optional :: color
        call ensure_fig_init()
        call fig%add_plot(x, y, label=label, linestyle=linestyle)
    end subroutine errorbar

    subroutine bar(x, height, width, bottom, label, color, edgecolor, align)
        real(8), dimension(:), intent(in) :: x, height
        real(8), intent(in), optional :: width
        real(8), dimension(:), intent(in), optional :: bottom
        character(len=*), intent(in), optional :: label, align
        real(8), dimension(3), intent(in), optional :: color, edgecolor
        real(8) :: bar_width
        real(8), allocatable :: bar_bottom(:)
        character(len=32) :: bar_align
        call ensure_fig_init()
        bar_width = 0.8d0
        if (present(width)) bar_width = width
        allocate(bar_bottom(size(x)))
        bar_bottom = 0.0d0
        if (present(bottom)) then
            if (size(bottom) == size(x)) then
                bar_bottom = bottom
            else if (size(bottom) == 1) then
                bar_bottom = bottom(1)
            else
                call log_error("bar: bottom array size must match x array or be scalar")
                return
            end if
        end if
        bar_align = 'center'
        if (present(align)) bar_align = align
        call bar_impl(fig, x, height, width, label, color)
    end subroutine bar

    subroutine barh(y, width, height, left, label, color, edgecolor, align)
        real(8), dimension(:), intent(in) :: y, width
        real(8), intent(in), optional :: height
        real(8), dimension(:), intent(in), optional :: left
        character(len=*), intent(in), optional :: label, align
        real(8), dimension(3), intent(in), optional :: color, edgecolor
        real(8) :: bar_height
        real(8), allocatable :: bar_left(:)
        character(len=32) :: bar_align
        call ensure_fig_init()
        bar_height = 0.8d0
        if (present(height)) bar_height = height
        allocate(bar_left(size(y)))
        bar_left = 0.0d0
        if (present(left)) then
            if (size(left) == size(y)) then
                bar_left = left
            else if (size(left) == 1) then
                bar_left = left(1)
            else
                call log_error("barh: left array size must match y array or be scalar")
                return
            end if
        end if
        bar_align = 'center'
        if (present(align)) bar_align = align
        call barh_impl(fig, y, width, height, label, color)
    end subroutine barh

    subroutine hist(data, bins, density, label, color)
        real(8), dimension(:), intent(in) :: data
        integer, intent(in), optional :: bins
        logical, intent(in), optional :: density
        character(len=*), intent(in), optional :: label
        real(8), dimension(3), intent(in), optional :: color
        call ensure_fig_init()
        call fig%hist(data, bins=bins, density=density, label=label, color=color)
    end subroutine hist

    subroutine histogram(data, bins, density, label, color)
        real(8), dimension(:), intent(in) :: data
        integer, intent(in), optional :: bins
        logical, intent(in), optional :: density
        character(len=*), intent(in), optional :: label
        real(8), dimension(3), intent(in), optional :: color
        call ensure_fig_init()
        call fig%hist(data, bins=bins, density=density, label=label, color=color)
    end subroutine histogram

    subroutine boxplot(data, position, width, label, show_outliers, horizontal, color)
        real(8), dimension(:), intent(in) :: data
        real(8), intent(in), optional :: position
        real(8), intent(in), optional :: width
        character(len=*), intent(in), optional :: label
        logical, intent(in), optional :: show_outliers, horizontal
        character(len=*), intent(in), optional :: color
        call ensure_fig_init()
        call fig%boxplot(data, position=position, width=width, label=label, &
                         show_outliers=show_outliers, horizontal=horizontal, color=color)
    end subroutine boxplot

    subroutine scatter(x, y, s, c, label, marker, markersize, color, &
                       linewidths, edgecolors, alpha)
        real(8), dimension(:), intent(in) :: x, y
        real(8), intent(in), optional :: s
        real(8), dimension(:), intent(in), optional :: c
        character(len=*), intent(in), optional :: label, marker
        real(8), intent(in), optional :: markersize
        real(8), dimension(3), intent(in), optional :: color
        real(8), intent(in), optional :: linewidths
        real(8), dimension(3), intent(in), optional :: edgecolors
        real(8), intent(in), optional :: alpha
        real(wp), allocatable :: wx(:), wy(:)
        call ensure_fig_init()
        allocate(wx(size(x))); allocate(wy(size(y)))
        wx = real(x, wp); wy = real(y, wp)
        call add_scatter_2d(fig, wx, wy, label=label, marker=marker)
    end subroutine scatter

    subroutine add_scatter_2d_wrapper(x, y, s, c, label, marker, markersize, color, &
                           linewidths, edgecolors, alpha)
        real(8), dimension(:), intent(in) :: x, y
        real(8), intent(in), optional :: s
        real(8), dimension(:), intent(in), optional :: c
        character(len=*), intent(in), optional :: label, marker
        real(8), intent(in), optional :: markersize
        real(8), dimension(3), intent(in), optional :: color
        real(8), intent(in), optional :: linewidths
        real(8), dimension(3), intent(in), optional :: edgecolors
        real(8), intent(in), optional :: alpha
        real(wp), allocatable :: wx(:), wy(:)
        call ensure_fig_init()
        allocate(wx(size(x))); allocate(wy(size(y)))
        wx = real(x, wp); wy = real(y, wp)
        call add_scatter_2d(fig, wx, wy, label=label, marker=marker)
    end subroutine add_scatter_2d_wrapper

    subroutine add_scatter_3d_wrapper(x, y, z, s, c, label, marker, markersize, color, &
                                      linewidths, edgecolors, alpha)
        real(8), dimension(:), intent(in) :: x, y, z
        real(8), intent(in), optional :: s
        real(8), dimension(:), intent(in), optional :: c
        character(len=*), intent(in), optional :: label, marker
        real(8), intent(in), optional :: markersize
        real(8), dimension(3), intent(in), optional :: color
        real(8), intent(in), optional :: linewidths
        real(8), dimension(3), intent(in), optional :: edgecolors
        real(8), intent(in), optional :: alpha
        real(wp), allocatable :: wx(:), wy(:), wz(:)
        call ensure_fig_init()
        allocate(wx(size(x))); allocate(wy(size(y))); allocate(wz(size(z)))
        wx = real(x, wp); wy = real(y, wp); wz = real(z, wp)
        call add_scatter_3d(fig, wx, wy, wz, label=label, marker=marker)
    end subroutine add_scatter_3d_wrapper

    subroutine add_plot(x, y, label, linestyle)
        real(8), dimension(:), intent(in) :: x, y
        character(len=*), intent(in), optional :: label, linestyle
        call ensure_fig_init()
        call fig%add_plot(x, y, label=label, linestyle=linestyle)
    end subroutine add_plot

    subroutine add_errorbar(x, y, xerr, yerr, fmt, label, capsize, linestyle, marker, color)
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:), intent(in), optional :: xerr, yerr
        character(len=*), intent(in), optional :: fmt, label, linestyle, marker
        real(8), intent(in), optional :: capsize
        real(8), dimension(3), intent(in), optional :: color
        call ensure_fig_init()
        call errorbar_impl(fig, x, y, xerr=xerr, yerr=yerr, label=label, &
                           capsize=capsize, marker=marker, color=color)
    end subroutine add_errorbar

    subroutine add_3d_plot(x, y, z, label, linestyle, color, linewidth, marker, markersize)
        real(8), dimension(:), intent(in) :: x, y, z
        character(len=*), intent(in), optional :: label, linestyle, marker
        real(8), dimension(3), intent(in), optional :: color
        real(8), intent(in), optional :: linewidth, markersize
        call ensure_fig_init()
        call add_3d_plot_impl(fig, x, y, z, label=label, linestyle=linestyle, &
                              markersize=markersize, linewidth=linewidth)
    end subroutine add_3d_plot

    ! ----- Contour/Field API ---------------------------------------------

    subroutine contour(x, y, z, levels, label)
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:,:), intent(in) :: z
        real(8), dimension(:), intent(in), optional :: levels
        character(len=*), intent(in), optional :: label
        call ensure_fig_init()
        call fig%add_contour(x, y, z, levels=levels, label=label)
    end subroutine contour

    subroutine contour_filled(x, y, z, levels, colormap, show_colorbar, label)
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:,:), intent(in) :: z
        real(8), dimension(:), intent(in), optional :: levels
        character(len=*), intent(in), optional :: colormap, label
        logical, intent(in), optional :: show_colorbar
        real(wp), allocatable :: wp_x(:), wp_y(:), wp_z(:,:), wp_levels(:)
        call ensure_fig_init()
        call convert_contour_arrays(x, y, z, levels, wp_x, wp_y, wp_z, wp_levels)
        call forward_contour_filled_params(fig, wp_x, wp_y, wp_z, wp_levels, &
                                          colormap, show_colorbar, label)
    end subroutine contour_filled

    subroutine pcolormesh(x, y, z, shading, colormap, show_colorbar, label, &
                          edgecolors, linewidths, vmin, vmax)
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:,:), intent(in) :: z
        character(len=*), intent(in), optional :: shading, colormap, label
        logical, intent(in), optional :: show_colorbar
        real(8), dimension(3), intent(in), optional :: edgecolors
        real(8), intent(in), optional :: linewidths, vmin, vmax
        character(len=32) :: shading_local, colormap_local, label_local
        logical :: show_colorbar_local
        real(8) :: vmin_local, vmax_local, linewidths_local
        integer :: nx, ny
        call ensure_fig_init()
        nx = size(x)
        ny = size(y)
        if (.not. (size(z, 1) == ny-1 .and. size(z, 2) == nx-1) .and. &
            .not. (size(z, 1) == ny .and. size(z, 2) == nx) .and. &
            .not. (size(z, 1) == nx-1 .and. size(z, 2) == ny-1) .and. &
            .not. (size(z, 1) == nx .and. size(z, 2) == ny)) then
            call log_error("pcolormesh: z dimensions incompatible with x,y grid. " // &
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
        linewidths_local = 1.0d0
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
        call fig%add_pcolormesh(x, y, z, colormap=colormap_local, &
                              vmin=vmin_local, vmax=vmax_local, &
                              linewidths=linewidths_local)
    end subroutine pcolormesh

    subroutine streamplot(x, y, u, v, density, linewidth_scale, arrow_scale, colormap, label)
        use fortplot_streamplot_matplotlib, only: streamplot_matplotlib
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:,:), intent(in) :: u, v
        real(8), intent(in), optional :: density, linewidth_scale, arrow_scale
        character(len=*), intent(in), optional :: colormap, label
        real(8) :: density_local
        real(wp) :: line_color(3)
        real, allocatable :: trajectories(:,:,:)
        integer, allocatable :: traj_lengths(:)
        integer :: n_traj, i, j
        real(wp), allocatable :: traj_x(:), traj_y(:)
        integer :: nx, ny
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
        density_local = 1.0d0
        if (present(density)) density_local = density
        ! Use a single default color for all streamlines (matplotlib-style)
        line_color = [0.0_wp, 0.447_wp, 0.698_wp]

        ! Generate trajectories using matplotlib-compatible algorithm
        call streamplot_matplotlib(x, y, u, v, density_local, trajectories, n_traj, traj_lengths)

        ! Add each trajectory as a line plot
        do i = 1, n_traj
            if (traj_lengths(i) <= 1) cycle
            allocate(traj_x(traj_lengths(i)), traj_y(traj_lengths(i)))
            do j = 1, traj_lengths(i)
                traj_x(j) = x(1) + real(trajectories(i, j, 1), wp) * (x(size(x)) - x(1)) / real(size(x) - 1, wp)
                traj_y(j) = y(1) + real(trajectories(i, j, 2), wp) * (y(size(y)) - y(1)) / real(size(y) - 1, wp)
            end do
            call fig%add_plot(traj_x, traj_y, color=line_color)
            deallocate(traj_x, traj_y)
        end do
    end subroutine streamplot

    subroutine add_contour(x, y, z, levels, label)
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:,:), intent(in) :: z
        real(8), dimension(:), intent(in), optional :: levels
        character(len=*), intent(in), optional :: label
        call ensure_fig_init()
        call fig%add_contour(x, y, z, levels=levels, label=label)
    end subroutine add_contour

    subroutine add_contour_filled(x, y, z, levels, colormap, show_colorbar, label)
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:,:), intent(in) :: z
        real(8), dimension(:), intent(in), optional :: levels
        character(len=*), intent(in), optional :: colormap, label
        logical, intent(in), optional :: show_colorbar
        real(wp), allocatable :: wp_x(:), wp_y(:), wp_z(:,:), wp_levels(:)
        call ensure_fig_init()
        call convert_contour_arrays(x, y, z, levels, wp_x, wp_y, wp_z, wp_levels)
        call forward_contour_filled_params(fig, wp_x, wp_y, wp_z, wp_levels, &
                                          colormap, show_colorbar, label)
    end subroutine add_contour_filled

    subroutine add_pcolormesh(x, y, z, shading, colormap, show_colorbar, label, &
                              edgecolors, linewidths, vmin, vmax)
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:,:), intent(in) :: z
        character(len=*), intent(in), optional :: shading, colormap, label
        logical, intent(in), optional :: show_colorbar
        real(8), dimension(3), intent(in), optional :: edgecolors
        real(8), intent(in), optional :: linewidths, vmin, vmax
        character(len=32) :: shading_local, colormap_local, label_local
        logical :: show_colorbar_local
        real(8) :: vmin_local, vmax_local, linewidths_local
        integer :: nx, ny
        call ensure_fig_init()
        nx = size(x)
        ny = size(y)
        if (.not. (size(z, 1) == ny-1 .and. size(z, 2) == nx-1) .and. &
            .not. (size(z, 1) == ny .and. size(z, 2) == nx) .and. &
            .not. (size(z, 1) == nx-1 .and. size(z, 2) == ny-1) .and. &
            .not. (size(z, 1) == nx .and. size(z, 2) == ny)) then
            call log_error("add_pcolormesh: z dimensions incompatible with x,y grid. " // &
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
        linewidths_local = 1.0d0
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
        call fig%add_pcolormesh(x, y, z, colormap=colormap_local, &
                              vmin=vmin_local, vmax=vmax_local, &
                              linewidths=linewidths_local)
    end subroutine add_pcolormesh

    subroutine add_surface(x, y, z, colormap, show_colorbar, alpha, edgecolor, linewidth, label)
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:,:), intent(in) :: z
        character(len=*), intent(in), optional :: colormap, label
        logical, intent(in), optional :: show_colorbar
        real(8), intent(in), optional :: alpha, linewidth
        real(8), dimension(3), intent(in), optional :: edgecolor
        integer :: nx, ny
        call ensure_fig_init()
        nx = size(x)
        ny = size(y)
        if (.not. (size(z, 1) == ny-1 .and. size(z, 2) == nx-1) .and. &
            .not. (size(z, 1) == ny .and. size(z, 2) == nx) .and. &
            .not. (size(z, 1) == nx-1 .and. size(z, 2) == ny-1) .and. &
            .not. (size(z, 1) == nx .and. size(z, 2) == ny)) then
            call log_error("add_surface: z dimensions incompatible with x,y grid. " // &
                          "Expected one of: z(ny-1,nx-1), z(ny,nx), z(nx-1,ny-1), or z(nx,ny)")
            return
        end if
        ! Placeholder: use contour as basic surface visualization
        call fig%add_contour(x, y, z, levels=null(), label=label)
    end subroutine add_surface

    subroutine convert_contour_arrays(x, y, z, levels, wp_x, wp_y, wp_z, wp_levels)
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:,:), intent(in) :: z
        real(8), dimension(:), intent(in), optional :: levels
        real(wp), allocatable, intent(out) :: wp_x(:), wp_y(:)
        real(wp), allocatable, intent(out) :: wp_z(:,:)
        real(wp), allocatable, intent(out) :: wp_levels(:)
        integer :: nx, ny
        nx = size(x); ny = size(y)
        allocate(wp_x(nx)); allocate(wp_y(ny)); allocate(wp_z(ny, nx))
        wp_x = real(x, wp); wp_y = real(y, wp); wp_z = real(z, wp)
        if (present(levels)) then
            allocate(wp_levels(size(levels)))
            wp_levels = real(levels, wp)
        else
            allocate(wp_levels(0))
        end if
    end subroutine convert_contour_arrays

    subroutine forward_contour_filled_params(fig_in, x, y, z, levels, colormap, show_colorbar, label)
        class(figure_t), target, intent(inout) :: fig_in
        real(wp), dimension(:), intent(in) :: x, y
        real(wp), dimension(:,:), intent(in) :: z
        real(wp), dimension(:), intent(in) :: levels
        character(len=*), intent(in), optional :: colormap, label
        logical, intent(in), optional :: show_colorbar
        call fig_in%add_contour_filled(x, y, z, levels=levels, colormap=colormap, &
                                       show_colorbar=show_colorbar, label=label)
    end subroutine forward_contour_filled_params

    ! Removed placeholder create_and_add_streamlines; real implementation is in figure_t

    ! ----- Axes/Labels/Scales API ----------------------------------------

    subroutine xlabel(label_text)
        character(len=*), intent(in) :: label_text
        call ensure_fig_init()
        call fig%set_xlabel(label_text)
    end subroutine xlabel

    subroutine ylabel(label_text)
        character(len=*), intent(in) :: label_text
        call ensure_fig_init()
        call fig%set_ylabel(label_text)
    end subroutine ylabel

    subroutine title(title_text)
        character(len=*), intent(in) :: title_text
        call ensure_fig_init()
        call fig%set_title(title_text)
    end subroutine title

    subroutine grid(enabled, which, axis, alpha, linestyle)
        logical, intent(in), optional :: enabled
        character(len=*), intent(in), optional :: which, axis, linestyle
        real(wp), intent(in), optional :: alpha
        call ensure_fig_init()
        call fig%grid(enabled=enabled, which=which, axis=axis, alpha=alpha, linestyle=linestyle)
    end subroutine grid

    subroutine legend(position, box, fontsize)
        character(len=*), intent(in), optional :: position
        logical, intent(in), optional :: box
        integer, intent(in), optional :: fontsize
        call ensure_fig_init()
        if (present(position)) then
            call fig%legend(location=position)
        else
            call fig%legend()
        end if
    end subroutine legend

    subroutine xlim(xmin, xmax)
        real(8), intent(in) :: xmin, xmax
        call ensure_fig_init()
        call fig%set_xlim(xmin, xmax)
    end subroutine xlim

    subroutine ylim(ymin, ymax)
        real(8), intent(in) :: ymin, ymax
        call ensure_fig_init()
        call fig%set_ylim(ymin, ymax)
    end subroutine ylim

    subroutine set_xscale(scale, threshold)
        character(len=*), intent(in) :: scale
        real(8), intent(in), optional :: threshold
        call ensure_fig_init()
        call fig%set_xscale(scale, threshold)
    end subroutine set_xscale

    subroutine set_yscale(scale, threshold)
        character(len=*), intent(in) :: scale
        real(8), intent(in), optional :: threshold
        call ensure_fig_init()
        call fig%set_yscale(scale, threshold)
    end subroutine set_yscale

    subroutine set_line_width(width)
        real(8), intent(in) :: width
        call ensure_fig_init()
        call fig%set_line_width(width)
    end subroutine set_line_width

    subroutine set_ydata(ydata)
        real(8), dimension(:), intent(in) :: ydata
        call ensure_fig_init()
        call fig%set_ydata(1, ydata)
    end subroutine set_ydata

    ! ----- Figure I/O and Display ----------------------------------------

    subroutine figure(num, figsize, dpi)
        integer, intent(in), optional :: num
        real(8), dimension(2), intent(in), optional :: figsize
        integer, intent(in), optional :: dpi
        integer :: fig_num, fig_dpi
        real(8), dimension(2) :: size, safe_size
        integer :: width_px, height_px
        character(len=256) :: msg
        fig_num = 1
        if (present(num)) fig_num = num
        size = [8.0d0, 6.0d0]
        if (present(figsize)) size = figsize
        fig_dpi = 100
        if (present(dpi)) fig_dpi = dpi
        if (size(1) <= 0.0d0 .or. size(2) <= 0.0d0) then
            call log_error("figure: Invalid figure size")
            return
        end if
        if (fig_dpi <= 0) then
            call log_error("figure: Invalid DPI value")
            return
        end if
        width_px = nint(size(1) * fig_dpi)
        height_px = nint(size(2) * fig_dpi)
        safe_size = size
        if (width_px > 10000 .or. height_px > 10000) then
            write(msg, '(A,F6.1,A,F6.1,A,I0,A,I0,A)') &
                "Large figure size ", size(1), "x", size(2), &
                " inches (", width_px, "x", height_px, " pixels) may cause memory issues"
            call log_warning(trim(msg))
            safe_size = size
            width_px = nint(safe_size(1) * fig_dpi)
            height_px = nint(safe_size(2) * fig_dpi)
        end if
        write(msg, '(A,I0,A,F6.2,A,F6.2,A,I0,A)') &
            "Creating figure ", fig_num, " with size ", safe_size(1), "x", safe_size(2), &
            " inches at ", fig_dpi, " DPI"
        call log_info(trim(msg))
        if (allocated(fig)) then
            deallocate(fig)
        end if
        allocate(figure_t :: fig)
        call fig%initialize()
        call configure_figure_dimensions(fig%state, width=width_px, height=height_px)
    end subroutine figure

    subroutine subplot(nrows, ncols, index)
        integer, intent(in) :: nrows, ncols, index
        character(len=256) :: msg
        logical, save :: subplot_warning_shown = .false.
        call ensure_global_figure_initialized()
        if (nrows <= 0 .or. ncols <= 0) then
            call log_error("subplot: Invalid grid dimensions")
            return
        end if
        if (index <= 0 .or. index > nrows*ncols) then
            call log_error("subplot: Invalid subplot index")
            return
        end if
        write(msg, '(A,I0,A,I0,A,I0,A)') &
            "Creating subplot ", index, " in ", nrows, "x", ncols, " grid"
        call log_info(trim(msg))
        if (index > 1 .and. .not. subplot_warning_shown) then
            call log_warning("subplot: Multiple subplots not yet implemented")
            subplot_warning_shown = .true.
        end if
    end subroutine subplot

    subroutine savefig(filename, dpi, transparent, bbox_inches)
        character(len=*), intent(in) :: filename
        integer, intent(in), optional :: dpi
        logical, intent(in), optional :: transparent
        character(len=*), intent(in), optional :: bbox_inches
        call ensure_global_figure_initialized()
        call fig%savefig(filename)
    end subroutine savefig

    subroutine savefig_with_status(filename, status, dpi, transparent, bbox_inches)
        character(len=*), intent(in) :: filename
        integer, intent(out) :: status
        integer, intent(in), optional :: dpi
        logical, intent(in), optional :: transparent
        character(len=*), intent(in), optional :: bbox_inches
        call ensure_global_figure_initialized()
        call fig%savefig_with_status(filename, status)
    end subroutine savefig_with_status

    subroutine show_data(x, y, label, title_text, xlabel_text, ylabel_text, blocking)
        real(8), dimension(:), intent(in) :: x, y
        character(len=*), intent(in), optional :: label, title_text
        character(len=*), intent(in), optional :: xlabel_text, ylabel_text
        logical, intent(in), optional :: blocking
        call ensure_global_figure_initialized()
        call fig%add_plot(x, y, label=label)
        if (present(title_text)) call fig%set_title(title_text)
        if (present(xlabel_text)) call fig%set_xlabel(xlabel_text)
        if (present(ylabel_text)) call fig%set_ylabel(ylabel_text)
        call fig%show(blocking=blocking)
    end subroutine show_data

    subroutine show_figure(blocking)
        logical, intent(in), optional :: blocking
        call ensure_global_figure_initialized()
        call fig%show(blocking=blocking)
    end subroutine show_figure

    subroutine show_viewer(blocking)
        logical, intent(in), optional :: blocking
        call ensure_global_figure_initialized()
        call show_viewer_implementation(blocking)
    end subroutine show_viewer

    function is_gui_available() result(gui_available)
        logical :: gui_available
        character(len=256) :: display_var
        integer :: status
        logical, save :: ssh_warning_shown = .false.
        gui_available = .false.
        call get_environment_variable("DISPLAY", display_var, status=status)
        if (status == 0 .and. len_trim(display_var) > 0) then
            gui_available = .true.
        end if
        call get_environment_variable("SSH_CLIENT", display_var, status=status)
        if (status == 0 .and. .not. gui_available .and. .not. ssh_warning_shown) then
            call log_warning("SSH session detected without X forwarding")
            ssh_warning_shown = .true.
        end if
    end function is_gui_available

    subroutine show_viewer_implementation(blocking)
        logical, intent(in), optional :: blocking
        character(len=512) :: temp_file
        logical :: is_blocking, success
        integer :: status
        real :: start_time, current_time
        logical, save :: no_gui_warning_shown = .false.
        is_blocking = .false.
        if (present(blocking)) is_blocking = blocking
        if (.not. is_gui_available()) then
            if (.not. no_gui_warning_shown) then
                call log_warning("No GUI available, saving to show_output.png instead")
                no_gui_warning_shown = .true.
            end if
            call fig%savefig("show_output.png")
            return
        end if
        call get_environment_variable("TMPDIR", temp_file, status=status)
        call get_environment_variable("TMPDIR", temp_file, status=status)
        if (status /= 0) temp_file = "/tmp"
        write(temp_file, '(A,A,I0,A)') trim(temp_file), "/fortplot_", &
                                       int(rand(0)*1000000), ".png"
        call fig%savefig_with_status(trim(temp_file), status)
        if (status /= 0) then
            call log_error("Failed to save figure for viewing")
            return
        end if
        call safe_launch_viewer(trim(temp_file), success)
        if (.not. success) then
            call log_error("Failed to launch image viewer")
            call safe_remove_file(trim(temp_file), success)
            return
        end if
        if (is_blocking) then
            call log_info("Viewer launched in blocking mode. Close viewer to continue.")
            call cpu_time(start_time)
            do
                call cpu_time(current_time)
                if (current_time - start_time > 30.0) exit
                call sleep_fortran(100)
            end do
        else
            call sleep_fortran(1000)
        end if
        call safe_remove_file(trim(temp_file), success)
    end subroutine show_viewer_implementation

    subroutine sleep_fortran(milliseconds)
        integer, intent(in) :: milliseconds
        real :: seconds
        integer :: start_count, end_count, count_rate, target_count
        seconds = real(milliseconds) / 1000.0
        call system_clock(start_count, count_rate)
        target_count = int(seconds * real(count_rate))
        do
            call system_clock(end_count)
            if (end_count - start_count >= target_count) exit
        end do
    end subroutine sleep_fortran

end module fortplot_matplotlib_advanced

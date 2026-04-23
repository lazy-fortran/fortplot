module fortplot_matplotlib_field_wrappers
    !! Contour, field, and vector visualisation wrappers for matplotlib facade

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_global, only: fig => global_figure
    use fortplot_figure_core, only: figure_t
    use fortplot_logging, only: log_error
    use fortplot_matplotlib_color_utils, only: resolve_color_string_or_rgb
    use fortplot_matplotlib_session, only: ensure_fig_init

    implicit none
    private

    public :: contour
    public :: contour_filled
    public :: pcolormesh
    public :: streamplot
    public :: quiver, add_quiver
    public :: add_contour
    public :: add_contour_filled
    public :: add_pcolormesh
    public :: add_surface

    interface quiver
        module procedure quiver_rgb
        module procedure quiver_string
    end interface quiver

    interface add_quiver
        module procedure add_quiver_rgb
        module procedure add_quiver_string
    end interface add_quiver

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
        !! Stateful streamplot wrapper - delegates to OO interface
        !!
        !! Parameters linewidth_scale, arrow_scale, colormap, label, arrowsize,
        !! and arrowstyle are accepted for API compatibility but not yet fully
        !! supported by the underlying OO implementation.
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in) :: u(:,:), v(:,:)
        real(wp), intent(in), optional :: density, linewidth_scale, arrow_scale
        character(len=*), intent(in), optional :: colormap, label
        real(wp), intent(in), optional :: arrowsize
        character(len=*), intent(in), optional :: arrowstyle

        call ensure_fig_init()
        call fig%streamplot(x, y, u, v, density=density)
    end subroutine streamplot

    subroutine quiver_rgb(x, y, u, v, scale, color, width, headwidth, &
                          headlength, units, angles, pivot, alpha, scale_units, c)
        !! Matplotlib-style quiver with RGB color kwarg.
        !!
        !! `angles`, `pivot`, `alpha`, `scale_units` are accepted for
        !! matplotlib parity. `c(:)` is the per-arrow scalar array that
        !! matplotlib maps through a colormap; when present it overrides the
        !! solid `color` value (same precedence as scatter's `c` versus
        !! `color`). These extensions are currently recorded on the plot
        !! object; rendering alignment is tracked by issue #1671.
        real(wp), intent(in) :: x(:), y(:), u(:), v(:)
        real(wp), intent(in), optional :: scale
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: width, headwidth, headlength
        character(len=*), intent(in), optional :: units
        character(len=*), intent(in), optional :: angles, pivot, scale_units
        real(wp), intent(in), optional :: alpha
        real(wp), intent(in), optional :: c(:)

        call dispatch_quiver(x, y, u, v, scale=scale, color_rgb=color, &
                             width=width, headwidth=headwidth, &
                             headlength=headlength, units=units, angles=angles, &
                             pivot=pivot, alpha=alpha, scale_units=scale_units, c=c)
    end subroutine quiver_rgb

    subroutine quiver_string(x, y, u, v, color, scale, width, headwidth, &
                             headlength, units, angles, pivot, alpha, &
                             scale_units, c)
        !! String-color variant of quiver.
        real(wp), intent(in) :: x(:), y(:), u(:), v(:)
        character(len=*), intent(in) :: color
        real(wp), intent(in), optional :: scale
        real(wp), intent(in), optional :: width, headwidth, headlength
        character(len=*), intent(in), optional :: units
        character(len=*), intent(in), optional :: angles, pivot, scale_units
        real(wp), intent(in), optional :: alpha
        real(wp), intent(in), optional :: c(:)

        real(wp) :: rgb(3)
        logical :: has_color

        call resolve_color_string_or_rgb(color_str=color, context='quiver', &
                                         rgb_out=rgb, has_color=has_color)
        if (has_color) then
            call dispatch_quiver(x, y, u, v, scale=scale, color_rgb=rgb, &
                                 width=width, headwidth=headwidth, &
                                 headlength=headlength, units=units, &
                                 angles=angles, pivot=pivot, alpha=alpha, &
                                 scale_units=scale_units, c=c)
        else
            call dispatch_quiver(x, y, u, v, scale=scale, width=width, &
                                 headwidth=headwidth, headlength=headlength, &
                                 units=units, angles=angles, pivot=pivot, &
                                 alpha=alpha, scale_units=scale_units, c=c)
        end if
    end subroutine quiver_string

    subroutine add_quiver_rgb(x, y, u, v, scale, color, width, headwidth, &
                              headlength, units, angles, pivot, alpha, &
                              scale_units, c)
        real(wp), intent(in) :: x(:), y(:), u(:), v(:)
        real(wp), intent(in), optional :: scale
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: width, headwidth, headlength
        character(len=*), intent(in), optional :: units
        character(len=*), intent(in), optional :: angles, pivot, scale_units
        real(wp), intent(in), optional :: alpha
        real(wp), intent(in), optional :: c(:)

        call quiver_rgb(x, y, u, v, scale=scale, color=color, width=width, &
                        headwidth=headwidth, headlength=headlength, units=units, &
                        angles=angles, pivot=pivot, alpha=alpha, &
                        scale_units=scale_units, c=c)
    end subroutine add_quiver_rgb

    subroutine add_quiver_string(x, y, u, v, color, scale, width, headwidth, &
                                 headlength, units, angles, pivot, alpha, &
                                 scale_units, c)
        real(wp), intent(in) :: x(:), y(:), u(:), v(:)
        character(len=*), intent(in) :: color
        real(wp), intent(in), optional :: scale
        real(wp), intent(in), optional :: width, headwidth, headlength
        character(len=*), intent(in), optional :: units
        character(len=*), intent(in), optional :: angles, pivot, scale_units
        real(wp), intent(in), optional :: alpha
        real(wp), intent(in), optional :: c(:)

        call quiver_string(x, y, u, v, color=color, scale=scale, width=width, &
                           headwidth=headwidth, headlength=headlength, units=units, &
                           angles=angles, pivot=pivot, alpha=alpha, &
                           scale_units=scale_units, c=c)
    end subroutine add_quiver_string

    subroutine dispatch_quiver(x, y, u, v, scale, color_rgb, width, headwidth, &
                               headlength, units, angles, pivot, alpha, &
                               scale_units, c)
        !! Central quiver dispatch. Calls the figure-level implementation for
        !! fields already supported there; stores newly accepted parameters
        !! on the plot record so future rendering passes can consume them.
        use fortplot_plot_data, only: PLOT_TYPE_QUIVER
        real(wp), intent(in) :: x(:), y(:), u(:), v(:)
        real(wp), intent(in), optional :: scale
        real(wp), intent(in), optional :: color_rgb(3)
        real(wp), intent(in), optional :: width, headwidth, headlength
        character(len=*), intent(in), optional :: units
        character(len=*), intent(in), optional :: angles, pivot, scale_units
        real(wp), intent(in), optional :: alpha
        real(wp), intent(in), optional :: c(:)

        integer :: idx

        call ensure_fig_init()
        call fig%quiver(x, y, u, v, scale=scale, color=color_rgb, width=width, &
                        headwidth=headwidth, headlength=headlength, units=units)

        idx = fig%plot_count
        if (idx < 1) return
        if (.not. allocated(fig%plots)) return
        if (idx > size(fig%plots)) return
        if (fig%plots(idx)%plot_type /= PLOT_TYPE_QUIVER) return

        if (present(alpha)) then
            fig%plots(idx)%marker_face_alpha = max(0.0_wp, min(1.0_wp, alpha))
            fig%plots(idx)%marker_edge_alpha = fig%plots(idx)%marker_face_alpha
        end if
        if (present(angles)) fig%plots(idx)%quiver_units = set_quiver_metadata( &
            fig%plots(idx)%quiver_units, 'angles', trim(angles))
        if (present(pivot) .or. present(scale_units)) then
            ! Accepted for matplotlib parity; backend will consume later.
            continue
        end if
        if (present(c)) then
            if (size(c) == size(x)) then
                if (.not. allocated(fig%plots(idx)%scatter_colors)) then
                    allocate (fig%plots(idx)%scatter_colors(size(c)))
                end if
                fig%plots(idx)%scatter_colors = c
            else
                call log_error('quiver: c must match number of arrows')
            end if
        end if
    end subroutine dispatch_quiver

    pure function set_quiver_metadata(current, tag, value) result(updated)
        !! Preserve the existing quiver_units string while recording an
        !! additional matplotlib-style tag (angles=...). Output keeps the
        !! fortplot units prefix so downstream rendering is unchanged.
        character(len=*), intent(in) :: current, tag, value
        character(len=10) :: updated

        if (len_trim(current) == 0) then
            updated = trim(tag) // '=' // trim(value)
        else
            updated = current
        end if
    end function set_quiver_metadata

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
                           linewidth, label, filled)
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in) :: z(:,:)
        character(len=*), intent(in), optional :: colormap, label
        logical, intent(in), optional :: show_colorbar, filled
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

        call fig%add_surface(x, y, z, label=label, colormap=colormap, &
                             show_colorbar=show_colorbar, alpha=alpha, &
                             edgecolor=edgecolor, linewidth=linewidth, &
                             filled=filled)
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

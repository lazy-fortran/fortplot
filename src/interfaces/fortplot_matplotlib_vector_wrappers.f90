module fortplot_matplotlib_vector_wrappers
    !! Streamplot and quiver visualisation wrappers for matplotlib facade

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_global, only: fig => global_figure
    use fortplot_figure_core, only: figure_t
    use fortplot_logging, only: log_error
    use fortplot_matplotlib_color_utils, only: resolve_color_string_or_rgb
    use fortplot_matplotlib_mesh_wrappers, only: resolve_cmap_alias
    use fortplot_matplotlib_session, only: ensure_fig_init

    implicit none
    private

    public :: streamplot
    public :: quiver, add_quiver

    interface quiver
        module procedure quiver_rgb
        module procedure quiver_string
    end interface quiver

    interface add_quiver
        module procedure add_quiver_rgb
        module procedure add_quiver_string
    end interface add_quiver

contains

   subroutine streamplot(x, y, u, v, density, linewidth, color, &
                              cmap, label, arrowsize, arrowstyle, colormap)
        !! Stateful streamplot wrapper - delegates to OO interface
        !!
        !! `cmap` matches matplotlib; `colormap` is a deprecated alias.
        !! `linewidth` controls streamline line width (matplotlib-canonical).
        !! `color(3)` sets a solid RGB color for all streamlines.
        !! `arrowsize` and `arrowstyle` control arrow glyphs on streamlines.
        real(wp), contiguous, intent(in) :: x(:), y(:)
        real(wp), contiguous, intent(in) :: u(:,:), v(:,:)
        real(wp), intent(in), optional :: density, linewidth, color(3)
        character(len=*), intent(in), optional :: cmap, label, colormap
        real(wp), intent(in), optional :: arrowsize
        character(len=*), intent(in), optional :: arrowstyle
        character(len=:), allocatable :: resolved_cmap

        call ensure_fig_init()
        call resolve_cmap_alias(cmap, colormap, resolved_cmap)
        call fig%streamplot(x, y, u, v, density=density, linewidth=linewidth, &
                            color=color, arrowsize=arrowsize, arrowstyle=arrowstyle)

        ! Store colormap and label on the last plot for colorbar/legend support
        if (fig%plot_count >= 1 .and. allocated(fig%plots) .and. &
            fig%plot_count <= size(fig%plots)) then
            if (allocated(resolved_cmap)) then
                fig%plots(fig%plot_count)%colormap = resolved_cmap
            end if
            if (present(label) .and. len_trim(label) > 0) then
                fig%plots(fig%plot_count)%label = label
            end if
        end if
    end subroutine streamplot

    subroutine quiver_rgb(x, y, u, v, scale, color, width, headwidth, &
                          headlength, units, angles, pivot, alpha, scale_units, c, colormap)
        !! Matplotlib-style quiver with RGB color kwarg.
        !!
        !! `angles`, `pivot`, `alpha`, `scale_units` are accepted for
        !! matplotlib parity. `c(:)` is the per-arrow scalar array that
        !! matplotlib maps through a colormap; when present it overrides the
        !! solid `color` value (same precedence as scatter's `c` versus
        !! `color`).
        real(wp), contiguous, intent(in) :: x(:), y(:), u(:), v(:)
        real(wp), intent(in), optional :: scale
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: width, headwidth, headlength
        character(len=*), intent(in), optional :: units
        character(len=*), intent(in), optional :: angles, pivot, scale_units
        real(wp), intent(in), optional :: alpha
        real(wp), intent(in), optional :: c(:)
        character(len=*), intent(in), optional :: colormap

        call dispatch_quiver(x, y, u, v, scale=scale, color_rgb=color, &
                             width=width, headwidth=headwidth, &
                             headlength=headlength, units=units, angles=angles, &
                             pivot=pivot, alpha=alpha, scale_units=scale_units, &
                             c=c, colormap=colormap)
    end subroutine quiver_rgb

    subroutine quiver_string(x, y, u, v, color, scale, width, headwidth, &
                             headlength, units, angles, pivot, alpha, &
                             scale_units, c, colormap)
        !! String-color variant of quiver.
        real(wp), contiguous, intent(in) :: x(:), y(:), u(:), v(:)
        character(len=*), intent(in) :: color
        real(wp), intent(in), optional :: scale
        real(wp), intent(in), optional :: width, headwidth, headlength
        character(len=*), intent(in), optional :: units
        character(len=*), intent(in), optional :: angles, pivot, scale_units
        real(wp), intent(in), optional :: alpha
        real(wp), intent(in), optional :: c(:)
        character(len=*), intent(in), optional :: colormap

        real(wp) :: rgb(3)
        logical :: has_color

        call resolve_color_string_or_rgb(color_str=color, context='quiver', &
                                         rgb_out=rgb, has_color=has_color)
        if (has_color) then
            call dispatch_quiver(x, y, u, v, scale=scale, color_rgb=rgb, &
                                 width=width, headwidth=headwidth, &
                                 headlength=headlength, units=units, &
                                 angles=angles, pivot=pivot, alpha=alpha, &
                                 scale_units=scale_units, c=c, &
                                 colormap=colormap)
        else
            call dispatch_quiver(x, y, u, v, scale=scale, width=width, &
                                 headwidth=headwidth, headlength=headlength, &
                                 units=units, angles=angles, pivot=pivot, &
                                 alpha=alpha, scale_units=scale_units, &
                                 c=c, colormap=colormap)
        end if
    end subroutine quiver_string

    subroutine add_quiver_rgb(x, y, u, v, scale, color, width, headwidth, &
                              headlength, units, angles, pivot, alpha, &
                              scale_units, c, colormap)
        real(wp), contiguous, intent(in) :: x(:), y(:), u(:), v(:)
        real(wp), intent(in), optional :: scale
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: width, headwidth, headlength
        character(len=*), intent(in), optional :: units
        character(len=*), intent(in), optional :: angles, pivot, scale_units
        real(wp), intent(in), optional :: alpha
        real(wp), intent(in), optional :: c(:)
        character(len=*), intent(in), optional :: colormap

        call quiver_rgb(x, y, u, v, scale=scale, color=color, width=width, &
                        headwidth=headwidth, headlength=headlength, units=units, &
                        angles=angles, pivot=pivot, alpha=alpha, &
                        scale_units=scale_units, c=c, colormap=colormap)
    end subroutine add_quiver_rgb

    subroutine add_quiver_string(x, y, u, v, color, scale, width, headwidth, &
                                 headlength, units, angles, pivot, alpha, &
                                 scale_units, c, colormap)
        real(wp), contiguous, intent(in) :: x(:), y(:), u(:), v(:)
        character(len=*), intent(in) :: color
        real(wp), intent(in), optional :: scale
        real(wp), intent(in), optional :: width, headwidth, headlength
        character(len=*), intent(in), optional :: units
        character(len=*), intent(in), optional :: angles, pivot, scale_units
        real(wp), intent(in), optional :: alpha
        real(wp), intent(in), optional :: c(:)
        character(len=*), intent(in), optional :: colormap

        call quiver_string(x, y, u, v, color=color, scale=scale, width=width, &
                           headwidth=headwidth, headlength=headlength, units=units, &
                           angles=angles, pivot=pivot, alpha=alpha, &
                           scale_units=scale_units, c=c, colormap=colormap)
    end subroutine add_quiver_string

  subroutine dispatch_quiver(x, y, u, v, scale, color_rgb, width, headwidth, &
                              headlength, units, angles, pivot, alpha, &
                              scale_units, c, colormap)
        !! Central quiver dispatch. Calls the figure-level implementation for
        !! fields already supported there; stores newly accepted parameters
        !! on the plot record so future rendering passes can consume them.
        use fortplot_plot_data, only: PLOT_TYPE_QUIVER
        real(wp), contiguous, intent(in) :: x(:), y(:), u(:), v(:)
        real(wp), intent(in), optional :: scale
        real(wp), intent(in), optional :: color_rgb(3)
        real(wp), intent(in), optional :: width, headwidth, headlength
        character(len=*), intent(in), optional :: units
        character(len=*), intent(in), optional :: angles, pivot, scale_units
        real(wp), intent(in), optional :: alpha
        real(wp), intent(in), optional :: c(:)
        character(len=*), intent(in), optional :: colormap

        integer :: idx

        call ensure_fig_init()
        call fig%quiver(x, y, u, v, scale=scale, color=color_rgb, width=width, &
                        headwidth=headwidth, headlength=headlength, units=units, &
                        pivot=pivot, scale_units=scale_units, angles=angles, &
                        colormap=colormap)

        idx = fig%plot_count
        if (idx < 1) return
        if (.not. allocated(fig%plots)) return
        if (idx > size(fig%plots)) return
        if (fig%plots(idx)%plot_type /= PLOT_TYPE_QUIVER) return

        if (present(alpha)) then
            fig%plots(idx)%marker_face_alpha = max(0.0_wp, min(1.0_wp, alpha))
            fig%plots(idx)%marker_edge_alpha = fig%plots(idx)%marker_face_alpha
        end if
        if (present(angles)) then
            fig%plots(idx)%quiver_angles = trim(adjustl(angles))
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
        if (present(colormap)) then
            fig%plots(idx)%quiver_colormap = trim(adjustl(colormap))
        end if
    end subroutine dispatch_quiver

end module fortplot_matplotlib_vector_wrappers

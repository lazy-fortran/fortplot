module fortplot_matplotlib_field_wrappers
    !! Contour visualisation wrappers for matplotlib facade

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_global, only: fig => global_figure
    use fortplot_figure_core, only: figure_t
    use fortplot_matplotlib_session, only: ensure_fig_init

    implicit none
    private

    public :: contour
    public :: contour_filled
    public :: contourf
    public :: add_contour
    public :: add_contour_filled
    public :: add_contourf

contains

    subroutine contour(x, y, z, levels, cmap, label, colormap)
        !! Draw contour lines (matplotlib-compatible)
        !!
        !! `cmap` selects the colormap name. `colormap` is a deprecated alias
        !! kept for backward compatibility.
        real(wp), contiguous, intent(in) :: x(:), y(:)
        real(wp), contiguous, intent(in) :: z(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: cmap, label, colormap
        character(len=:), allocatable :: resolved_cmap

        call ensure_fig_init()
        call fig%add_contour(x, y, z, levels=levels, label=label)
        call resolve_cmap_alias(cmap, colormap, resolved_cmap)
        if (allocated(resolved_cmap)) then
            if (allocated(fig%plots) .and. fig%plot_count > 0 .and. &
                fig%plot_count <= size(fig%plots)) then
                fig%plots(fig%plot_count)%colormap = resolved_cmap
            end if
        end if
    end subroutine contour

    subroutine contour_filled(x, y, z, levels, cmap, show_colorbar, label, &
                              colormap)
        !! Draw filled contour regions (matplotlib-compatible)
        !!
        !! `cmap` is the matplotlib canonical keyword; `colormap` is kept as
        !! a backward-compatible alias.
        real(wp), contiguous, intent(in) :: x(:), y(:)
        real(wp), contiguous, intent(in) :: z(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: cmap, label, colormap
        logical, intent(in), optional :: show_colorbar

        real(wp), allocatable :: wp_x(:), wp_y(:), wp_levels(:)
        real(wp), allocatable :: wp_z(:,:)
        character(len=:), allocatable :: resolved_cmap

        call ensure_fig_init()
        call convert_contour_arrays(x, y, z, levels, wp_x, wp_y, wp_z, wp_levels)
        call resolve_cmap_alias(cmap, colormap, resolved_cmap)
        call forward_contour_filled_params(fig, wp_x, wp_y, wp_z, wp_levels, &
                                             cmap=resolved_cmap, show_colorbar=show_colorbar, &
                                             label=label, colormap=resolved_cmap)
    end subroutine contour_filled

    subroutine contourf(x, y, z, levels, cmap, show_colorbar, label, colormap)
        !! matplotlib-canonical alias for contour_filled
        real(wp), contiguous, intent(in) :: x(:), y(:)
        real(wp), contiguous, intent(in) :: z(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: cmap, label, colormap
        logical, intent(in), optional :: show_colorbar

        call contour_filled(x, y, z, levels=levels, cmap=cmap, &
                            show_colorbar=show_colorbar, label=label, &
                            colormap=colormap)
    end subroutine contourf

    subroutine add_contour(x, y, z, levels, cmap, label, colormap)
        !! Object-oriented contour helper (matplotlib-compatible kwargs)
        real(wp), contiguous, intent(in) :: x(:), y(:)
        real(wp), contiguous, intent(in) :: z(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: cmap, label, colormap

        call contour(x, y, z, levels=levels, cmap=cmap, label=label, &
                     colormap=colormap)
    end subroutine add_contour

    subroutine add_contour_filled(x, y, z, levels, cmap, show_colorbar, label, &
                                  colormap)
        !! Object-oriented filled contour helper (matplotlib-compatible kwargs)
        !!
        !! `cmap` is the matplotlib-canonical keyword; `colormap` is a
        !! backward-compatible alias.
        real(wp), contiguous, intent(in) :: x(:), y(:)
        real(wp), contiguous, intent(in) :: z(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: cmap, label, colormap
        logical, intent(in), optional :: show_colorbar

        call contour_filled(x, y, z, levels=levels, cmap=cmap, &
                            show_colorbar=show_colorbar, label=label, &
                            colormap=colormap)
    end subroutine add_contour_filled

    subroutine add_contourf(x, y, z, levels, cmap, show_colorbar, label, colormap)
        !! matplotlib-canonical alias for add_contour_filled
        real(wp), contiguous, intent(in) :: x(:), y(:)
        real(wp), contiguous, intent(in) :: z(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: cmap, label, colormap
        logical, intent(in), optional :: show_colorbar

        call contour_filled(x, y, z, levels=levels, cmap=cmap, &
                            show_colorbar=show_colorbar, label=label, &
                            colormap=colormap)
    end subroutine add_contourf

    subroutine convert_contour_arrays(x, y, z, levels, wp_x, wp_y, wp_z, &
                                         wp_levels)
        real(wp), contiguous, intent(in) :: x(:), y(:)
        real(wp), contiguous, intent(in) :: z(:,:)
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

    subroutine forward_contour_filled_params(fig_in, x, y, z, levels, cmap, &
                                                 show_colorbar, label, colormap)
        class(figure_t), target, intent(inout) :: fig_in
        real(wp), contiguous, intent(in) :: x(:), y(:)
        real(wp), contiguous, intent(in) :: z(:,:)
        real(wp), contiguous, intent(in) :: levels(:)
        character(len=*), intent(in), optional :: cmap, label, colormap
        logical, intent(in), optional :: show_colorbar

        call fig_in%add_contour_filled(x, y, z, levels=levels, cmap=cmap, &
                                       show_colorbar=show_colorbar, label=label, &
                                       colormap=colormap)
    end subroutine forward_contour_filled_params

end module fortplot_matplotlib_field_wrappers

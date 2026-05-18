module fortplot_matplotlib_mesh_wrappers
    !! Pcolormesh and surface visualisation wrappers for matplotlib facade

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_global, only: fig => global_figure
    use fortplot_figure_core, only: figure_t
    use fortplot_logging, only: log_error, log_warning
    use fortplot_matplotlib_session, only: ensure_fig_init

contains

    subroutine pcolormesh(x, y, z, shading, cmap, show_colorbar, label, &
                              edgecolors, linewidths, vmin, vmax, colormap)
        !! Draw a pseudocolor mesh (matplotlib-compatible)
        !!
        !! `cmap` is the matplotlib canonical keyword; `colormap` is kept as
        !! a backward-compatible alias.
        real(wp), contiguous, intent(in) :: x(:), y(:)
        real(wp), contiguous, intent(in) :: z(:,:)
        character(len=*), intent(in), optional :: shading, cmap, label, colormap
        logical, intent(in), optional :: show_colorbar
        real(wp), intent(in), optional :: edgecolors(3)
        real(wp), intent(in), optional :: linewidths
        real(wp), intent(in), optional :: vmin, vmax

        character(len=32) :: shading_local, colormap_local, label_local
        character(len=:), allocatable :: resolved_cmap
        logical :: show_colorbar_local
        real(wp) :: vmin_local, vmax_local, linewidths_local
        integer :: nx, ny

        call ensure_fig_init()

        nx = size(x)
        ny = size(y)
        ! Matplotlib flat shading: z(ny-1, nx-1); nearest/gouraud: z(ny, nx).
        ! Transposed shapes are rejected to prevent silent masking of user errors.
        if (.not. (size(z, 1) == ny - 1 .and. size(z, 2) == nx - 1) .and. &
            .not. (size(z, 1) == ny .and. size(z, 2) == nx)) then
            call log_error( &
                "pcolormesh: z dimensions incompatible with x,y grid. " // &
                "Expected z(ny-1,nx-1) for flat shading or z(ny,nx) for nearest/gouraud.")
            return
        end if

        shading_local = 'flat'
        if (present(shading)) shading_local = shading

        call resolve_cmap_alias(cmap, colormap, resolved_cmap)
        colormap_local = 'viridis'
        if (allocated(resolved_cmap)) colormap_local = resolved_cmap

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

    subroutine add_pcolormesh(x, y, z, shading, cmap, show_colorbar, label, &
                                  edgecolors, linewidths, vmin, vmax, colormap)
        !! Object-oriented pcolormesh helper (matplotlib-compatible kwargs)
        !!
        !! `cmap` is the matplotlib-canonical keyword; `colormap` is a
        !! backward-compatible alias.
        real(wp), contiguous, intent(in) :: x(:), y(:)
        real(wp), contiguous, intent(in) :: z(:,:)
        character(len=*), intent(in), optional :: shading, cmap, label, colormap
        logical, intent(in), optional :: show_colorbar
        real(wp), intent(in), optional :: edgecolors(3)
        real(wp), intent(in), optional :: linewidths
        real(wp), intent(in), optional :: vmin, vmax

        call pcolormesh(x, y, z, shading=shading, cmap=cmap, &
                        show_colorbar=show_colorbar, label=label, &
                        edgecolors=edgecolors, linewidths=linewidths, vmin=vmin, &
                        vmax=vmax, colormap=colormap)
    end subroutine add_pcolormesh

    subroutine add_surface(x, y, z, cmap, show_colorbar, alpha, edgecolor, &
                           linewidth, label, filled, colormap)
        !! Object-oriented surface helper (matplotlib-compatible kwargs)
        !!
        !! `cmap` is the matplotlib-canonical keyword; `colormap` is a
        !! backward-compatible alias.
        real(wp), contiguous, intent(in) :: x(:), y(:)
        real(wp), contiguous, intent(in) :: z(:,:)
        character(len=*), intent(in), optional :: cmap, label, colormap
        logical, intent(in), optional :: show_colorbar, filled
        real(wp), intent(in), optional :: alpha, linewidth
        real(wp), intent(in), optional :: edgecolor(3)

        integer :: nx, ny
        character(len=:), allocatable :: resolved_cmap

        call ensure_fig_init()

        nx = size(x)
        ny = size(y)
        ! Matplotlib surface: z(ny, nx) with x(nx), y(ny).
        ! Transposed shapes are rejected to prevent silent masking of user errors.
        if (.not. (size(z, 1) == ny .and. size(z, 2) == nx)) then
            call log_error( &
                "add_surface: z dimensions incompatible with x,y grid. " // &
                "Expected z(ny,nx) where x has nx points and y has ny points.")
            return
        end if

        call resolve_cmap_alias(cmap, colormap, resolved_cmap)
        call fig%add_surface(x, y, z, label=label, cmap=resolved_cmap, &
                             show_colorbar=show_colorbar, alpha=alpha, &
                             edgecolor=edgecolor, linewidth=linewidth, &
                             filled=filled, colormap=resolved_cmap)
    end subroutine add_surface

    subroutine resolve_cmap_alias(cmap, colormap, resolved)
        !! Resolve matplotlib-canonical cmap against legacy colormap alias
        character(len=*), intent(in), optional :: cmap, colormap
        character(len=:), allocatable, intent(out) :: resolved

        if (present(cmap)) then
            resolved = cmap
        else if (present(colormap)) then
            call log_warning( &
                "field wrappers: 'colormap' is deprecated; use 'cmap' for parity")
            resolved = colormap
        end if
    end subroutine resolve_cmap_alias

end module fortplot_matplotlib_mesh_wrappers

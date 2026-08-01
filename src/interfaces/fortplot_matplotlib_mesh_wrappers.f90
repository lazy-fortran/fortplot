module fortplot_matplotlib_mesh_wrappers
    !! Pcolormesh and surface visualisation wrappers for matplotlib facade

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_global, only: fig => global_figure
    use fortplot_figure_core, only: figure_t
    use fortplot_logging, only: log_error, log_warning
    use fortplot_matplotlib_session, only: ensure_fig_init
    use fortplot_matplotlib_plot_wrappers, only: add_3d_plot

contains

    subroutine add_parametric_surface(x, y, z, color, linewidth, label, &
                                      row_stride, column_stride)
        !! Draw a tensor-product parametric surface as a 3D wireframe.
        !!
        !! Unlike ``add_surface``, which accepts a height field ``z(y,x)``,
        !! this helper accepts physical coordinates for every grid point.  It
        !! is therefore suitable for closed curved surfaces such as spheres,
        !! toroids, and trimmed spline patches.  Rows and columns are emitted
        !! as ordinary 3D curves, so the result participates in the normal
        !! 3D projection, autoscaling, and depth ordering paths.
        real(wp), contiguous, intent(in) :: x(:, :), y(:, :), z(:, :)
        character(len=*), intent(in), optional :: color, label
        real(wp), intent(in), optional :: linewidth
        integer, intent(in), optional :: row_stride, column_stride

        integer :: nrow, ncolumn, row_step, column_step, row, column
        logical :: first_curve

        nrow = size(x, 1)
        ncolumn = size(x, 2)
        if (size(y, 1) /= nrow .or. size(y, 2) /= ncolumn .or. &
            size(z, 1) /= nrow .or. size(z, 2) /= ncolumn) then
            call log_error("add_parametric_surface: x, y, and z must have " // &
                           "identical two-dimensional shapes")
            return
        end if
        if (nrow < 2 .or. ncolumn < 2) then
            call log_error("add_parametric_surface: surface grid must be at " // &
                           "least 2 by 2")
            return
        end if

        row_step = 1
        if (present(row_stride)) row_step = row_stride
        column_step = 1
        if (present(column_stride)) column_step = column_stride
        if (row_step < 1 .or. column_step < 1) then
            call log_error("add_parametric_surface: strides must be positive")
            return
        end if

        first_curve = .true.
        do row = 1, nrow, row_step
            call add_parametric_curve(x(row, :), y(row, :), z(row, :), &
                                      first_curve)
        end do
        do column = 1, ncolumn, column_step
            call add_parametric_curve(x(:, column), y(:, column), z(:, column), &
                                      first_curve)
        end do

    contains

        subroutine add_parametric_curve(x_curve, y_curve, z_curve, is_first)
            real(wp), contiguous, intent(in) :: x_curve(:), y_curve(:), z_curve(:)
            logical, intent(inout) :: is_first

            if (is_first .and. present(label)) then
                call add_curve_with_style(x_curve, y_curve, z_curve, &
                                          curve_label=label)
            else
                call add_curve_with_style(x_curve, y_curve, z_curve)
            end if
            is_first = .false.
        end subroutine add_parametric_curve

        subroutine add_curve_with_style(x_curve, y_curve, z_curve, curve_label)
            real(wp), contiguous, intent(in) :: x_curve(:), y_curve(:), z_curve(:)
            character(len=*), intent(in), optional :: curve_label

            if (present(curve_label)) then
                if (present(color)) then
                    if (present(linewidth)) then
                        call add_3d_plot(x_curve, y_curve, z_curve, &
                                         label=curve_label, color=color, &
                                         linewidth=linewidth)
                    else
                        call add_3d_plot(x_curve, y_curve, z_curve, &
                                         label=curve_label, color=color)
                    end if
                else if (present(linewidth)) then
                    call add_3d_plot(x_curve, y_curve, z_curve, &
                                     label=curve_label, linewidth=linewidth)
                else
                    call add_3d_plot(x_curve, y_curve, z_curve, &
                                     label=curve_label)
                end if
            else if (present(color)) then
                if (present(linewidth)) then
                    call add_3d_plot(x_curve, y_curve, z_curve, color=color, &
                                     linewidth=linewidth)
                else
                    call add_3d_plot(x_curve, y_curve, z_curve, color=color)
                end if
            else if (present(linewidth)) then
                call add_3d_plot(x_curve, y_curve, z_curve, linewidth=linewidth)
            else
                call add_3d_plot(x_curve, y_curve, z_curve)
            end if
        end subroutine add_curve_with_style

    end subroutine add_parametric_surface

    subroutine pcolormesh(x, y, z, shading, cmap, show_colorbar, label, &
                              edgecolors, linewidths, vmin, vmax, colormap)
        !! Draw a pseudocolor mesh (matplotlib-compatible)
        !!
        !! Parameters
        !! x : real(wp), contiguous, intent(in)
        !!     X grid coordinates.
        !! y : real(wp), contiguous, intent(in)
        !!     Y grid coordinates.
        !! z : real(wp), contiguous, intent(in)
        !!     Cell or node values on the grid.
        !! shading : character(len=*), optional
        !!     Matplotlib shading keyword.
        !! cmap : character(len=*), optional
        !!     Colormap name.
        !! show_colorbar : logical, optional
        !!     Accepted for matplotlib parity.
        !! label : character(len=*), optional
        !!     Legend label.
        !! edgecolors : real(wp)(3), optional
        !!     Accepted for matplotlib parity.
        !! linewidths : real(wp), optional
        !!     Mesh line width.
        !! vmin : real(wp), optional
        !!     Lower color limit.
        !! vmax : real(wp), optional
        !!     Upper color limit.
        !! colormap : character(len=*), optional
        !!     Deprecated alias for cmap.
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

        call fig%add_pcolormesh(x, y, z, cmap=colormap_local, vmin=vmin_local, &
                                vmax=vmax_local, linewidths=linewidths_local)
    end subroutine pcolormesh

    subroutine add_pcolormesh(x, y, z, shading, cmap, show_colorbar, label, &
                                  edgecolors, linewidths, vmin, vmax, colormap)
        !! Object-oriented pcolormesh helper.
        !!
        !! Parameters
        !! x : real(wp), contiguous, intent(in)
        !!     X grid coordinates.
        !! y : real(wp), contiguous, intent(in)
        !!     Y grid coordinates.
        !! z : real(wp), contiguous, intent(in)
        !!     Cell or node values on the grid.
        !! shading : character(len=*), optional
        !!     Matplotlib shading keyword.
        !! cmap : character(len=*), optional
        !!     Colormap name.
        !! show_colorbar : logical, optional
        !!     Accepted for matplotlib parity.
        !! label : character(len=*), optional
        !!     Legend label.
        !! edgecolors : real(wp)(3), optional
        !!     Accepted for matplotlib parity.
        !! linewidths : real(wp), optional
        !!     Mesh line width.
        !! vmin : real(wp), optional
        !!     Lower color limit.
        !! vmax : real(wp), optional
        !!     Upper color limit.
        !! colormap : character(len=*), optional
        !!     Deprecated alias for cmap.
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
        !! Object-oriented surface helper.
        !!
        !! Parameters
        !! x : real(wp), contiguous, intent(in)
        !!     X grid coordinates.
        !! y : real(wp), contiguous, intent(in)
        !!     Y grid coordinates.
        !! z : real(wp), contiguous, intent(in)
        !!     Surface values on the grid.
        !! cmap : character(len=*), optional
        !!     Colormap name.
        !! show_colorbar : logical, optional
        !!     Accepted for matplotlib parity.
        !! alpha : real(wp), optional
        !!     Surface transparency.
        !! edgecolor : real(wp)(3), optional
        !!     Surface edge color.
        !! linewidth : real(wp), optional
        !!     Surface line width.
        !! label : character(len=*), optional
        !!     Legend label.
        !! filled : logical, optional
        !!     Accepted for matplotlib parity.
        !! colormap : character(len=*), optional
        !!     Deprecated alias for cmap.
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

module fortplot_spec_field_rendering
    !! Field plot rendering (contour, contour_filled, pcolormesh, streamplot).
    !!
    !! Translates field_t data into core figure operations for 2D/3D visualizations.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_core_operations, only: core_add_contour, &
                                               core_add_contour_filled, &
                                               core_add_pcolormesh, &
                                               core_streamplot
    use fortplot_spec_types, only: mark_t, encoding_t, field_plot_t
    use fortplot_plot_data, only: plot_data_t
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_spec_rendering_utils, only: get_label_from_encoding

    implicit none
    private

    public :: render_field_plot_to_state

contains

    subroutine render_field_plot_to_state(mark, field, enc, state, plots, plot_count, &
                                          status)
        type(mark_t), intent(in) :: mark
        type(field_plot_t), intent(in) :: field
        type(encoding_t), intent(in) :: enc
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        integer, intent(inout) :: plot_count
        integer, intent(out) :: status

        real(wp), allocatable :: zmat(:, :), umat(:, :), vmat(:, :)

        status = 0
        if (.not. allocated(mark%type)) then
            status = 3
            return
        end if

        select case (trim(mark%type))
        case ('contour', 'contour_filled', 'pcolormesh')
            if (.not. allocated(field%z)) return
            call reshape_field_matrix(field%z, field%nrows, field%ncols, zmat)
            if (.not. allocated(zmat)) then
                status = 4
                return
            end if
            select case (trim(mark%type))
            case ('contour')
                call render_contour_to_state(field, enc, zmat, state, plots, plot_count)
            case ('contour_filled')
                call render_contour_filled_to_state(field, enc, zmat, state, plots, &
                                                    plot_count)
            case ('pcolormesh')
                call render_pcolormesh_to_state(field, zmat, state, plots, plot_count)
            end select
        case ('streamplot')
            if (.not. allocated(field%u) .or. .not. allocated(field%v)) return
            call reshape_field_matrix(field%u, field%nrows, field%ncols, umat)
            call reshape_field_matrix(field%v, field%nrows, field%ncols, vmat)
            if (.not. allocated(umat) .or. .not. allocated(vmat)) then
                status = 5
                return
            end if
            if (field%density >= 0.0_wp) then
                call core_streamplot(plots, state, plot_count, field%x, field%y, umat, &
                                     vmat, density=field%density)
            else
                call core_streamplot(plots, state, plot_count, field%x, field%y, umat, &
                                     vmat)
            end if
        case default
            status = 6
        end select
    end subroutine render_field_plot_to_state

    subroutine render_contour_to_state(field, enc, zmat, state, plots, plot_count)
        type(field_plot_t), intent(in) :: field
        type(encoding_t), intent(in) :: enc
        real(wp), contiguous, intent(in) :: zmat(:, :)
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        integer, intent(inout) :: plot_count

        character(len=:), allocatable :: label

        label = get_label_from_encoding(enc)

        if (allocated(field%levels) .and. allocated(label)) then
            call core_add_contour(plots, state, field%x, field%y, zmat, levels=field%levels, &
                                  label=label, plot_count=plot_count)
        else if (allocated(field%levels)) then
            call core_add_contour(plots, state, field%x, field%y, zmat, levels=field%levels, &
                                  plot_count=plot_count)
        else if (allocated(label)) then
            call core_add_contour(plots, state, field%x, field%y, zmat, label=label, &
                                  plot_count=plot_count)
        else
            call core_add_contour(plots, state, field%x, field%y, zmat, &
                                  plot_count=plot_count)
        end if
    end subroutine render_contour_to_state

    subroutine render_contour_filled_to_state(field, enc, zmat, state, plots, plot_count)
        type(field_plot_t), intent(in) :: field
        type(encoding_t), intent(in) :: enc
        real(wp), contiguous, intent(in) :: zmat(:, :)
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        integer, intent(inout) :: plot_count

        character(len=:), allocatable :: label

        label = get_label_from_encoding(enc)

        if (allocated(field%levels) .and. allocated(field%colormap) .and. &
            field%show_colorbar_set .and. allocated(label)) then
            call core_add_contour_filled(plots, state, field%x, field%y, zmat, &
                                         levels=field%levels, cmap=field%colormap, &
                                         show_colorbar=field%show_colorbar, label=label, &
                                         plot_count=plot_count)
        else if (allocated(field%levels) .and. allocated(field%colormap) .and. &
                 field%show_colorbar_set) then
            call core_add_contour_filled(plots, state, field%x, field%y, zmat, &
                                         levels=field%levels, cmap=field%colormap, &
                                         show_colorbar=field%show_colorbar, &
                                         plot_count=plot_count)
        else if (allocated(field%levels) .and. allocated(field%colormap) .and. &
                 allocated(label)) then
            call core_add_contour_filled(plots, state, field%x, field%y, zmat, &
                                         levels=field%levels, cmap=field%colormap, &
                                         label=label, plot_count=plot_count)
        else if (allocated(field%levels) .and. allocated(field%colormap)) then
            call core_add_contour_filled(plots, state, field%x, field%y, zmat, &
                                         levels=field%levels, cmap=field%colormap, &
                                         plot_count=plot_count)
        else if (allocated(field%levels) .and. field%show_colorbar_set .and. &
                 allocated(label)) then
            call core_add_contour_filled(plots, state, field%x, field%y, zmat, &
                                         levels=field%levels, &
                                         show_colorbar=field%show_colorbar, label=label, &
                                         plot_count=plot_count)
        else if (allocated(field%levels) .and. field%show_colorbar_set) then
            call core_add_contour_filled(plots, state, field%x, field%y, zmat, &
                                         levels=field%levels, &
                                         show_colorbar=field%show_colorbar, &
                                         plot_count=plot_count)
        else if (allocated(field%levels) .and. allocated(label)) then
            call core_add_contour_filled(plots, state, field%x, field%y, zmat, &
                                         levels=field%levels, label=label, &
                                         plot_count=plot_count)
        else if (allocated(field%levels)) then
            call core_add_contour_filled(plots, state, field%x, field%y, zmat, &
                                         levels=field%levels, plot_count=plot_count)
        else if (allocated(field%colormap) .and. field%show_colorbar_set .and. &
                 allocated(label)) then
            call core_add_contour_filled(plots, state, field%x, field%y, zmat, &
                                         cmap=field%colormap, &
                                         show_colorbar=field%show_colorbar, label=label, &
                                         plot_count=plot_count)
        else if (allocated(field%colormap) .and. field%show_colorbar_set) then
            call core_add_contour_filled(plots, state, field%x, field%y, zmat, &
                                         cmap=field%colormap, &
                                         show_colorbar=field%show_colorbar, &
                                         plot_count=plot_count)
        else if (allocated(field%colormap) .and. allocated(label)) then
            call core_add_contour_filled(plots, state, field%x, field%y, zmat, &
                                         cmap=field%colormap, label=label, &
                                         plot_count=plot_count)
        else if (allocated(field%colormap)) then
            call core_add_contour_filled(plots, state, field%x, field%y, zmat, &
                                         cmap=field%colormap, plot_count=plot_count)
        else if (field%show_colorbar_set .and. allocated(label)) then
            call core_add_contour_filled(plots, state, field%x, field%y, zmat, &
                                         show_colorbar=field%show_colorbar, label=label, &
                                         plot_count=plot_count)
        else if (field%show_colorbar_set) then
            call core_add_contour_filled(plots, state, field%x, field%y, zmat, &
                                         show_colorbar=field%show_colorbar, &
                                         plot_count=plot_count)
        else if (allocated(label)) then
            call core_add_contour_filled(plots, state, field%x, field%y, zmat, &
                                         label=label, plot_count=plot_count)
        else
            call core_add_contour_filled(plots, state, field%x, field%y, zmat, &
                                         plot_count=plot_count)
        end if
    end subroutine render_contour_filled_to_state

    subroutine render_pcolormesh_to_state(field, zmat, state, plots, plot_count)
        type(field_plot_t), intent(in) :: field
        real(wp), contiguous, intent(in) :: zmat(:, :)
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        integer, intent(inout) :: plot_count

        if (allocated(field%colormap) .and. field%vmin_set .and. field%vmax_set .and. &
            field%linewidths >= 0.0_wp) then
            call core_add_pcolormesh(plots, state, field%x, field%y, zmat, &
                                     cmap=field%colormap, vmin=field%vmin, &
                                     vmax=field%vmax, linewidths=field%linewidths, &
                                     plot_count=plot_count)
        else if (allocated(field%colormap) .and. field%vmin_set .and. field%vmax_set) then
            call core_add_pcolormesh(plots, state, field%x, field%y, zmat, &
                                     cmap=field%colormap, vmin=field%vmin, &
                                     vmax=field%vmax, plot_count=plot_count)
        else if (allocated(field%colormap) .and. field%linewidths >= 0.0_wp) then
            call core_add_pcolormesh(plots, state, field%x, field%y, zmat, &
                                     cmap=field%colormap, &
                                     linewidths=field%linewidths, &
                                     plot_count=plot_count)
        else if (allocated(field%colormap)) then
            call core_add_pcolormesh(plots, state, field%x, field%y, zmat, &
                                     cmap=field%colormap, plot_count=plot_count)
        else
            call core_add_pcolormesh(plots, state, field%x, field%y, zmat, &
                                     plot_count=plot_count)
        end if
    end subroutine render_pcolormesh_to_state

    subroutine reshape_field_matrix(values, nrows, ncols, matrix)
        real(wp), contiguous, intent(in) :: values(:)
        integer, intent(in) :: nrows
        integer, intent(in) :: ncols
        real(wp), allocatable, intent(out) :: matrix(:, :)
        if (nrows <= 0 .or. ncols <= 0) return
        if (size(values) /= nrows*ncols) return

        allocate (matrix(nrows, ncols))
        matrix = reshape(values, [nrows, ncols])
    end subroutine reshape_field_matrix

end module fortplot_spec_field_rendering

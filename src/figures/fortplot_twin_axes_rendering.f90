module fortplot_twin_axes_rendering
    !! Twin axes label rendering for raster, PDF, and ASCII backends
    !! Extracted from fortplot_figure_rendering_pipeline for size compliance (Issue #1747)

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context, only: plot_context
    use fortplot_raster, only: raster_context
    use fortplot_ascii, only: ascii_context
    use fortplot_ascii_secondary_axes, only: ascii_draw_secondary_y_axis, &
                                             ascii_draw_secondary_x_axis_top
    use fortplot_raster_axes, only: raster_draw_secondary_y_axis, &
                                    raster_draw_secondary_x_axis_top
    implicit none

    private
    public :: setup_twin_axes_state
    public :: render_twin_labels

    character(len=16), parameter :: DEFAULT_SCALE = 'linear          '

contains

    subroutine setup_twin_axes_state(has_twinx, has_twiny, twinx_y_min, twinx_y_max, &
                                      twiny_x_min, twiny_x_max, twinx_yscale, &
                                      twiny_xscale, twinx_ylabel, twiny_xlabel, &
                                      xscale, yscale, has_twinx_local, has_twiny_local, &
                                      twinx_y_min_local, twinx_y_max_local, &
                                      twiny_x_min_local, twiny_x_max_local, &
                                      twinx_scale_local, twiny_scale_local)
        !! Setup local state for twin axes configuration
        logical, intent(in), optional :: has_twinx, has_twiny
        real(wp), intent(in), optional :: twinx_y_min, twinx_y_max
        real(wp), intent(in), optional :: twiny_x_min, twiny_x_max
        character(len=*), intent(in), optional :: twinx_yscale, twiny_xscale
        character(len=:), allocatable, intent(in), optional :: twinx_ylabel, &
                                                               twiny_xlabel
        character(len=*), intent(in) :: xscale, yscale
        logical, intent(out) :: has_twinx_local, has_twiny_local
        real(wp), intent(out) :: twinx_y_min_local, twinx_y_max_local
        real(wp), intent(out) :: twiny_x_min_local, twiny_x_max_local
        character(len=16), intent(out) :: twinx_scale_local, twiny_scale_local

        has_twinx_local = .false.
        has_twiny_local = .false.
        twinx_scale_local = DEFAULT_SCALE
        twiny_scale_local = DEFAULT_SCALE

        if (present(has_twinx)) then
            has_twinx_local = has_twinx
            if (has_twinx_local) then
                if (present(twinx_y_min) .and. present(twinx_y_max)) then
                    twinx_y_min_local = twinx_y_min
                    twinx_y_max_local = twinx_y_max
                else
                    has_twinx_local = .false.
                end if
                if (has_twinx_local .and. present(twinx_yscale)) twinx_scale_local = &
                    trim(twinx_yscale) // repeat(' ', 16 - len_trim(twinx_yscale))
            end if
        end if

        if (present(has_twiny)) then
            has_twiny_local = has_twiny
            if (has_twiny_local) then
                if (present(twiny_x_min) .and. present(twiny_x_max)) then
                    twiny_x_min_local = twiny_x_min
                    twiny_x_max_local = twiny_x_max
                else
                    has_twiny_local = .false.
                end if
                if (has_twiny_local .and. present(twiny_xscale)) twiny_scale_local = &
                    trim(twiny_xscale) // repeat(' ', 16 - len_trim(twiny_xscale))
            end if
        end if
    end subroutine setup_twin_axes_state

    subroutine render_twin_labels(backend, xscale, yscale, symlog_threshold, &
                                  x_min, x_max, y_min, y_max, title, xlabel, &
                                  ylabel, custom_xticks, custom_xtick_labels, &
                                  custom_yticks, custom_ytick_labels, &
                                  has_twinx_local, has_twiny_local, &
                                  twinx_scale_local, twiny_scale_local, &
                                  twinx_y_min_local, twinx_y_max_local, &
                                  twiny_x_min_local, twiny_x_max_local, &
                                  x_date_format, y_date_format, &
                                  twinx_y_date_format, twiny_x_date_format, &
                                  twinx_ylabel, twiny_xlabel)
        !! Render labels for all backend types (raster, PDF, ASCII)
        use fortplot_pdf, only: pdf_context
        class(plot_context), intent(inout) :: backend
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=:), allocatable, intent(in) :: title, xlabel, ylabel
        real(wp), intent(in), optional :: custom_xticks(:), custom_yticks(:)
        character(len=*), intent(in), optional :: custom_xtick_labels(:)
        character(len=*), intent(in), optional :: custom_ytick_labels(:)
        character(len=*), intent(in), optional :: x_date_format, y_date_format
        character(len=*), intent(in), optional :: twinx_y_date_format, &
                                                  twiny_x_date_format
        logical, intent(in) :: has_twinx_local, has_twiny_local
        real(wp), intent(in) :: twinx_y_min_local, twinx_y_max_local
        real(wp), intent(in) :: twiny_x_min_local, twiny_x_max_local
        character(len=16), intent(in) :: twinx_scale_local, twiny_scale_local
        character(len=:), allocatable, intent(in), optional :: twinx_ylabel, &
                                                               twiny_xlabel

        select type (bk => backend)
        type is (raster_context)
            call bk%draw_axis_labels_only(xscale, yscale, symlog_threshold, &
                                          x_min, x_max, y_min, y_max, title, &
                                          xlabel, ylabel, custom_xticks, &
                                          custom_xtick_labels, custom_yticks, &
                                          custom_ytick_labels, &
                                          x_date_format=x_date_format, &
                                          y_date_format=y_date_format)

            if (has_twinx_local) then
                if (present(twinx_ylabel)) then
                    call raster_draw_secondary_y_axis( &
                        bk%raster, bk%width, bk%height, bk%plot_area, &
                        twinx_scale_local, symlog_threshold, twinx_y_min_local, &
                        twinx_y_max_local, ylabel=twinx_ylabel, &
                        date_format=twinx_y_date_format)
                else
                    call raster_draw_secondary_y_axis( &
                        bk%raster, bk%width, bk%height, bk%plot_area, &
                        twinx_scale_local, symlog_threshold, twinx_y_min_local, &
                        twinx_y_max_local, date_format=twinx_y_date_format)
                end if
            end if

            if (has_twiny_local) then
                if (present(twiny_xlabel)) then
                    call raster_draw_secondary_x_axis_top( &
                        bk%raster, bk%width, bk%height, bk%plot_area, &
                        twiny_scale_local, symlog_threshold, twiny_x_min_local, &
                        twiny_x_max_local, xlabel=twiny_xlabel, &
                        date_format=twiny_x_date_format)
                else
                    call raster_draw_secondary_x_axis_top( &
                        bk%raster, bk%width, bk%height, bk%plot_area, &
                        twiny_scale_local, symlog_threshold, twiny_x_min_local, &
                        twiny_x_max_local, date_format=twiny_x_date_format)
                end if
            end if

        type is (pdf_context)
            if (has_twinx_local) then
                call bk%draw_secondary_y_axis( &
                    twinx_scale_local, symlog_threshold, twinx_y_min_local, &
                    twinx_y_max_local, twinx_ylabel, &
                    date_format=twinx_y_date_format)
            end if

            if (has_twiny_local) then
                call bk%draw_secondary_x_axis_top( &
                    twiny_scale_local, symlog_threshold, twiny_x_min_local, &
                    twiny_x_max_local, twiny_xlabel, &
                    date_format=twiny_x_date_format)
            end if

        type is (ascii_context)
            if (has_twinx_local) then
                call ascii_draw_secondary_y_axis(bk, twinx_scale_local, &
                    symlog_threshold, twinx_y_min_local, twinx_y_max_local, &
                    twinx_ylabel, date_format=twinx_y_date_format)
            end if

            if (has_twiny_local) then
                call ascii_draw_secondary_x_axis_top(bk, twiny_scale_local, &
                    symlog_threshold, twiny_x_min_local, twiny_x_max_local, &
                    twiny_xlabel, date_format=twiny_x_date_format)
            end if
        end select
    end subroutine render_twin_labels

end module fortplot_twin_axes_rendering

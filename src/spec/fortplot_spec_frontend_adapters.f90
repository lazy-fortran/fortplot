module fortplot_spec_frontend_adapters
    !! Convert figure_t into a proper spec_t with marks, encodings,
    !! data columns, and config -- no scene_t wrapper.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_core, only: figure_t
    use fortplot_spec_types, only: spec_t, layer_t, mark_t, &
        encoding_t, channel_t, data_t, data_column_t, &
        field_plot_t
    use fortplot_spec_config_defaults, only: mpl_default_config
    use fortplot_plot_data, only: plot_data_t, &
        PLOT_TYPE_LINE, PLOT_TYPE_SCATTER, PLOT_TYPE_BAR, &
        PLOT_TYPE_FILL, PLOT_TYPE_CONTOUR, &
        PLOT_TYPE_PCOLORMESH, PLOT_TYPE_ERRORBAR, &
        PLOT_TYPE_HISTOGRAM
    implicit none

    private
    public :: figure_to_spec

contains

    subroutine figure_to_spec(fig, spec)
        type(figure_t), intent(in) :: fig
        type(spec_t), intent(out) :: spec

        integer :: i

        spec%width = fig%state%width
        spec%height = fig%state%height
        if (allocated(fig%state%title)) spec%title = fig%state%title

        spec%config = mpl_default_config(fig%state%dpi)

        call apply_state_metadata(fig, spec)

        if (fig%plot_count == 0) then
            spec%mark%type = 'line'
            spec%encoding%x = make_channel('x', 'quantitative')
            spec%encoding%y = make_channel('y', 'quantitative')
            return
        end if

        if (fig%plot_count == 1) then
            call convert_plot(fig%plots(1), spec)
            return
        end if

        spec%is_layered = .true.
        spec%layer_count = fig%plot_count
        allocate (spec%layers(fig%plot_count))
        spec%encoding%x = make_channel('x', 'quantitative')
        spec%encoding%y = make_channel('y', 'quantitative')

        do i = 1, fig%plot_count
            call convert_plot_to_layer(fig%plots(i), &
                spec%layers(i))
        end do
    end subroutine figure_to_spec

    subroutine apply_state_metadata(fig, spec)
        type(figure_t), intent(in) :: fig
        type(spec_t), intent(inout) :: spec

        if (allocated(fig%state%xlabel)) then
            spec%encoding%x%axis%title = fig%state%xlabel
            spec%encoding%x%axis%title_set = .true.
        end if
        if (allocated(fig%state%ylabel)) then
            spec%encoding%y%axis%title = fig%state%ylabel
            spec%encoding%y%axis%title_set = .true.
        end if

        if (fig%state%xlim_set) then
            spec%encoding%x%scale%domain_min = fig%state%x_min
            spec%encoding%x%scale%domain_max = fig%state%x_max
            spec%encoding%x%scale%domain_set = .true.
        end if
        if (fig%state%ylim_set) then
            spec%encoding%y%scale%domain_min = fig%state%y_min
            spec%encoding%y%scale%domain_max = fig%state%y_max
            spec%encoding%y%scale%domain_set = .true.
        end if

        if (fig%state%xscale /= 'linear') then
            spec%encoding%x%scale%type = trim(fig%state%xscale)
        end if
        if (fig%state%yscale /= 'linear') then
            spec%encoding%y%scale%type = trim(fig%state%yscale)
        end if

        if (fig%state%grid_enabled) then
            spec%encoding%x%axis%grid = .true.
            spec%encoding%y%axis%grid = .true.
        end if
    end subroutine apply_state_metadata

    subroutine convert_plot(pd, spec)
        type(plot_data_t), intent(in) :: pd
        type(spec_t), intent(inout) :: spec

        call build_mark_from_plot(pd, spec%mark)
        spec%encoding%x = make_channel('x', 'quantitative')
        spec%encoding%y = make_channel('y', 'quantitative')

        if (pd%plot_type == PLOT_TYPE_CONTOUR .or. &
            pd%plot_type == PLOT_TYPE_PCOLORMESH) then
            call build_field_from_plot(pd, spec%field)
        else
            call build_data_from_plot(pd, spec%data)
        end if

        if (allocated(pd%label)) then
            spec%encoding%color%value = pd%label
            spec%encoding%color%defined = .true.
        end if
    end subroutine convert_plot

    subroutine convert_plot_to_layer(pd, layer)
        type(plot_data_t), intent(in) :: pd
        type(layer_t), intent(out) :: layer

        call build_mark_from_plot(pd, layer%mark)

        if (pd%plot_type == PLOT_TYPE_CONTOUR .or. &
            pd%plot_type == PLOT_TYPE_PCOLORMESH) then
            call build_field_from_plot(pd, layer%field)
        else
            call build_data_from_plot(pd, layer%data)
            layer%has_data = .true.
        end if

        if (allocated(pd%label)) then
            layer%encoding%color%value = pd%label
            layer%encoding%color%defined = .true.
        end if
    end subroutine convert_plot_to_layer

    subroutine build_mark_from_plot(pd, m)
        type(plot_data_t), intent(in) :: pd
        type(mark_t), intent(out) :: m

        character(len=7) :: hex

        select case (pd%plot_type)
        case (PLOT_TYPE_LINE, PLOT_TYPE_ERRORBAR)
            m%type = 'line'
        case (PLOT_TYPE_SCATTER)
            m%type = 'point'
            m%size = pd%scatter_size_default
            m%filled = .true.
        case (PLOT_TYPE_BAR, PLOT_TYPE_HISTOGRAM)
            m%type = 'bar'
        case (PLOT_TYPE_FILL)
            m%type = 'area'
            m%opacity = pd%fill_alpha
        case (PLOT_TYPE_CONTOUR)
            if (pd%fill_contours) then
                m%type = 'contour_filled'
            else
                m%type = 'contour'
            end if
        case (PLOT_TYPE_PCOLORMESH)
            m%type = 'pcolormesh'
        case default
            m%type = 'line'
        end select

        call rgb_to_hex(pd%color, hex)
        m%stroke = hex

        if (pd%line_width >= 0.0_wp) m%stroke_width = pd%line_width

        if (allocated(pd%linestyle)) then
            call linestyle_to_dash(pd%linestyle, m)
        end if
    end subroutine build_mark_from_plot

    subroutine build_data_from_plot(pd, d)
        type(plot_data_t), intent(in) :: pd
        type(data_t), intent(out) :: d

        integer :: n

        select case (pd%plot_type)
        case (PLOT_TYPE_BAR, PLOT_TYPE_HISTOGRAM)
            if (allocated(pd%bar_x) .and. &
                allocated(pd%bar_heights)) then
                n = min(size(pd%bar_x), size(pd%bar_heights))
                allocate (d%columns(2))
                d%columns(1)%field = 'x'
                d%columns(1)%values = pd%bar_x(1:n)
                d%columns(2)%field = 'y'
                d%columns(2)%values = pd%bar_heights(1:n)
                d%nrows = n
            end if
        case default
            if (allocated(pd%x) .and. allocated(pd%y)) then
                n = min(size(pd%x), size(pd%y))
                allocate (d%columns(2))
                d%columns(1)%field = 'x'
                d%columns(1)%values = pd%x(1:n)
                d%columns(2)%field = 'y'
                d%columns(2)%values = pd%y(1:n)
                d%nrows = n
            end if
        end select
    end subroutine build_data_from_plot

    subroutine build_field_from_plot(pd, fp)
        type(plot_data_t), intent(in) :: pd
        type(field_plot_t), intent(out) :: fp

        fp%defined = .true.
        if (allocated(pd%x_grid)) fp%x = pd%x_grid
        if (allocated(pd%y_grid)) fp%y = pd%y_grid

        if (allocated(pd%z_grid)) then
            fp%nrows = size(pd%z_grid, 1)
            fp%ncols = size(pd%z_grid, 2)
            allocate (fp%z(fp%nrows * fp%ncols))
            fp%z = reshape(pd%z_grid, [fp%nrows * fp%ncols])
        end if

        if (allocated(pd%contour_levels)) fp%levels = pd%contour_levels
        fp%colormap = trim(pd%colormap)
        fp%show_colorbar = pd%show_colorbar
        fp%show_colorbar_set = .true.
    end subroutine build_field_from_plot

    pure function make_channel(field, ctype) result(ch)
        character(len=*), intent(in) :: field, ctype
        type(channel_t) :: ch

        ch%field = field
        ch%type = ctype
        ch%defined = .true.
    end function make_channel

    subroutine rgb_to_hex(rgb, hex)
        real(wp), intent(in) :: rgb(3)
        character(len=7), intent(out) :: hex

        integer :: r, g, b

        r = max(0, min(255, nint(rgb(1) * 255.0_wp)))
        g = max(0, min(255, nint(rgb(2) * 255.0_wp)))
        b = max(0, min(255, nint(rgb(3) * 255.0_wp)))
        write (hex, '(a,3z2.2)') '#', r, g, b
    end subroutine rgb_to_hex

    subroutine linestyle_to_dash(ls, m)
        character(len=*), intent(in) :: ls
        type(mark_t), intent(inout) :: m

        select case (trim(ls))
        case ('--', 'dashed')
            allocate (m%stroke_dash(2))
            m%stroke_dash = [6.0_wp, 3.0_wp]
        case (':', 'dotted')
            allocate (m%stroke_dash(2))
            m%stroke_dash = [2.0_wp, 3.0_wp]
        case ('-.', 'dashdot')
            allocate (m%stroke_dash(4))
            m%stroke_dash = [6.0_wp, 3.0_wp, 2.0_wp, 3.0_wp]
        end select
    end subroutine linestyle_to_dash

end module fortplot_spec_frontend_adapters

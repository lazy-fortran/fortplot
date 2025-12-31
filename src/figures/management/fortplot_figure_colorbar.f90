module fortplot_figure_colorbar
    !! Stateful colorbar rendering (matplotlib-style).
    !!
    !! Implements:
    !! - Plot-area splitting for right/left/top/bottom colorbar placement
    !! - Scalar-mappable detection (pcolormesh/scatter/filled contour)
    !! - Gradient rendering + ticks/labels using existing primitives

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context, only: plot_context
    use fortplot_plot_data, only: plot_data_t, PLOT_TYPE_PCOLORMESH, &
                                  PLOT_TYPE_SCATTER, &
                                  PLOT_TYPE_CONTOUR
    use fortplot_margins, only: plot_area_t
    use fortplot_png, only: png_context
    use fortplot_pdf, only: pdf_context
    use fortplot_colormap, only: get_colormap_color
    use fortplot_ticks, only: find_nice_tick_locations, format_tick_value_smart
    use fortplot_string_utils, only: to_lowercase
    implicit none

    private
    public :: prepare_colorbar_layout
    public :: resolve_colorbar_mappable
    public :: render_colorbar

contains

    subroutine prepare_colorbar_layout(backend, location, fraction, pad, shrink, &
                                       saved_area, main_area, colorbar_area, supported)
        class(plot_context), intent(inout) :: backend
        character(len=*), intent(in) :: location
        real(wp), intent(in) :: fraction, pad, shrink
        type(plot_area_t), intent(out) :: saved_area, main_area, colorbar_area
        logical, intent(out) :: supported

        supported = .false.

        call get_backend_plot_area(backend, saved_area, supported)
        if (.not. supported) then
            main_area = saved_area
            colorbar_area = saved_area
            return
        end if

        call compute_colorbar_plot_areas(saved_area, location, fraction, pad, shrink, &
                                         main_area, colorbar_area)
        call set_backend_plot_area(backend, main_area)
    end subroutine prepare_colorbar_layout

    subroutine resolve_colorbar_mappable(plots, plot_count, preferred_index, &
                                         plot_index, vmin, vmax, colormap, ok)
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        integer, intent(in) :: preferred_index
        integer, intent(out) :: plot_index
        real(wp), intent(out) :: vmin, vmax
        character(len=20), intent(out) :: colormap
        logical, intent(out) :: ok

        integer :: i, start_idx
        logical :: found

        ok = .false.
        plot_index = 0
        vmin = 0.0_wp
        vmax = 1.0_wp
        colormap = 'viridis'

        if (plot_count <= 0) return

        start_idx = preferred_index
        if (start_idx < 1 .or. start_idx > plot_count) start_idx = plot_count

        found = .false.
        do i = start_idx, 1, -1
            if (plots(i)%plot_type == PLOT_TYPE_PCOLORMESH) then
                if (allocated(plots(i)%pcolormesh_data%c_values)) then
                    vmin = plots(i)%pcolormesh_data%vmin
                    vmax = plots(i)%pcolormesh_data%vmax
                    if (.not. plots(i)%pcolormesh_data%vmin_set) vmin = &
                        minval(plots(i)%pcolormesh_data%c_values)
                    if (.not. plots(i)%pcolormesh_data%vmax_set) vmax = &
                        maxval(plots(i)%pcolormesh_data%c_values)
                    colormap = plots(i)%pcolormesh_data%colormap_name
                    plot_index = i
                    found = .true.
                    exit
                end if
            end if

            if (plots(i)%plot_type == PLOT_TYPE_SCATTER) then
                if (allocated(plots(i)%scatter_colors)) then
                    if (size(plots(i)%scatter_colors) > 0) then
                        if (plots(i)%scatter_vrange_set) then
                            vmin = plots(i)%scatter_vmin
                            vmax = plots(i)%scatter_vmax
                        else
                            vmin = minval(plots(i)%scatter_colors)
                            vmax = maxval(plots(i)%scatter_colors)
                        end if
                        colormap = plots(i)%scatter_colormap
                        plot_index = i
                        found = .true.
                        exit
                    end if
                end if
            end if

            if (plots(i)%plot_type == PLOT_TYPE_CONTOUR) then
                if (plots(i)%fill_contours .and. allocated(plots(i)%z_grid)) then
                    if (size(plots(i)%z_grid) > 0) then
                        vmin = minval(plots(i)%z_grid)
                        vmax = maxval(plots(i)%z_grid)
                        colormap = plots(i)%colormap
                        plot_index = i
                        found = .true.
                        exit
                    end if
                end if
            end if
        end do

        if (.not. found) return
        if (vmax <= vmin) vmax = vmin + 1.0_wp
        ok = .true.
    end subroutine resolve_colorbar_mappable

    subroutine render_colorbar(backend, plot_area, vmin, vmax, colormap, &
                               location, label, custom_ticks, custom_ticklabels)
        class(plot_context), intent(inout) :: backend
        type(plot_area_t), intent(in) :: plot_area
        real(wp), intent(in) :: vmin, vmax
        character(len=*), intent(in) :: colormap
        character(len=*), intent(in) :: location
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: custom_ticks(:)
        character(len=*), intent(in), optional :: custom_ticklabels(:)

        type(plot_area_t) :: saved_area
        logical :: supported, use_custom_ticks, use_custom_labels
        real(wp) :: x_min_saved, x_max_saved, y_min_saved, y_max_saved
        character(len=32) :: loc
        logical :: vertical
        integer :: n_slices, i
        real(wp) :: t, c(3)
        real(wp) :: x0, x1, y0, y1
        real(wp) :: quad_x(4), quad_y(4)
        real(wp) :: tick_locations(20), nice_min, nice_max, nice_step
        integer :: n_ticks, n_custom_ticks
        real(wp) :: tick, tick_len
        character(len=50) :: tick_label
        real(wp) :: mid_val
        real(wp) :: range_val

        supported = .false.
        call get_backend_plot_area(backend, saved_area, supported)
        if (.not. supported) return

        use_custom_ticks = present(custom_ticks)
        if (use_custom_ticks) use_custom_ticks = size(custom_ticks) > 0
        use_custom_labels = present(custom_ticklabels)
        if (use_custom_labels) use_custom_labels = size(custom_ticklabels) > 0

        call backend%save_coordinates(x_min_saved, x_max_saved, y_min_saved, &
                                      y_max_saved)
        call set_backend_plot_area(backend, plot_area)

        loc = to_lowercase(trim(location))
        vertical = .true.
        if (loc == 'top' .or. loc == 'bottom') vertical = .false.

        range_val = max(1.0e-12_wp, vmax - vmin)
        mid_val = 0.5_wp*(vmin + vmax)

        if (vertical) then
            call backend%set_coordinates(0.0_wp, 1.0_wp, vmin, vmax)
            n_slices = min(128, max(32, plot_area%height/4))
        else
            call backend%set_coordinates(vmin, vmax, 0.0_wp, 1.0_wp)
            n_slices = min(128, max(32, plot_area%width/4))
        end if

        do i = 1, n_slices
            if (n_slices == 1) then
                t = 0.5_wp
            else
                t = real(i - 1, wp)/real(n_slices - 1, wp)
            end if
            call get_colormap_color(t, colormap, c)
            call backend%color(c(1), c(2), c(3))

            if (vertical) then
                x0 = 0.0_wp
                x1 = 1.0_wp
                y0 = vmin + (real(i - 1, wp)/real(n_slices, wp))*range_val
                y1 = vmin + (real(i, wp)/real(n_slices, wp))*range_val
                quad_x = [x0, x1, x1, x0]
                quad_y = [y0, y0, y1, y1]
            else
                y0 = 0.0_wp
                y1 = 1.0_wp
                x0 = vmin + (real(i - 1, wp)/real(n_slices, wp))*range_val
                x1 = vmin + (real(i, wp)/real(n_slices, wp))*range_val
                quad_x = [x0, x1, x1, x0]
                quad_y = [y0, y0, y1, y1]
            end if

            call backend%fill_quad(quad_x, quad_y)
        end do

        call backend%color(0.0_wp, 0.0_wp, 0.0_wp)
        if (vertical) then
            call backend%line(0.0_wp, vmin, 1.0_wp, vmin)
            call backend%line(0.0_wp, vmax, 1.0_wp, vmax)
            call backend%line(0.0_wp, vmin, 0.0_wp, vmax)
            call backend%line(1.0_wp, vmin, 1.0_wp, vmax)
        else
            call backend%line(vmin, 0.0_wp, vmax, 0.0_wp)
            call backend%line(vmin, 1.0_wp, vmax, 1.0_wp)
            call backend%line(vmin, 0.0_wp, vmin, 1.0_wp)
            call backend%line(vmax, 0.0_wp, vmax, 1.0_wp)
        end if

        tick_len = 0.08_wp

        if (use_custom_ticks) then
            n_custom_ticks = size(custom_ticks)
            do i = 1, n_custom_ticks
                tick = custom_ticks(i)
                if (tick < vmin .or. tick > vmax) cycle
                if (use_custom_labels .and. i <= size(custom_ticklabels)) then
                    tick_label = trim(custom_ticklabels(i))
                else
                    tick_label = format_tick_value_smart(tick, 10)
                end if
                if (vertical) then
                    call backend%line(1.0_wp, tick, 1.0_wp + tick_len, tick)
                    call backend%text(1.0_wp + 0.12_wp, tick, trim(tick_label))
                else
                    call backend%line(tick, 0.0_wp, tick, -tick_len)
                    call backend%text(tick, -0.18_wp, trim(tick_label))
                end if
            end do
        else
            call find_nice_tick_locations(vmin, vmax, 5, nice_min, nice_max, &
                                          nice_step, tick_locations, n_ticks)
            do i = 1, n_ticks
                tick = tick_locations(i)
                tick_label = format_tick_value_smart(tick, 10)
                if (vertical) then
                    call backend%line(1.0_wp, tick, 1.0_wp + tick_len, tick)
                    call backend%text(1.0_wp + 0.12_wp, tick, trim(tick_label))
                else
                    call backend%line(tick, 0.0_wp, tick, -tick_len)
                    call backend%text(tick, -0.18_wp, trim(tick_label))
                end if
            end do
        end if

        if (present(label)) then
            if (len_trim(label) > 0) then
                if (vertical) then
                    call backend%text(1.35_wp, mid_val, trim(label))
                else
                    call backend%text(mid_val, -0.40_wp, trim(label))
                end if
            end if
        end if

        call backend%set_coordinates(x_min_saved, x_max_saved, y_min_saved, y_max_saved)
        call set_backend_plot_area(backend, saved_area)
    end subroutine render_colorbar

    subroutine compute_colorbar_plot_areas(orig, location, fraction, pad, &
                                           shrink, main, cb)
        type(plot_area_t), intent(in) :: orig
        character(len=*), intent(in) :: location
        real(wp), intent(in) :: fraction, pad, shrink
        type(plot_area_t), intent(out) :: main, cb

        character(len=32) :: loc
        logical :: vertical
        integer :: bar_px, pad_px
        integer :: long_px, shrink_px, delta_px

        main = orig
        cb = orig

        loc = to_lowercase(trim(location))
        vertical = .true.
        if (loc == 'top' .or. loc == 'bottom') vertical = .false.

        if (vertical) then
            bar_px = max(1, int(max(0.01_wp, fraction)*real(orig%width, wp)))
            pad_px = max(0, int(max(0.0_wp, pad)*real(orig%width, wp)))

            main%width = max(1, orig%width - bar_px - pad_px)
            cb%width = bar_px

            long_px = max(1, orig%height)
            shrink_px = max(1, int(max(0.05_wp, min(1.0_wp, shrink))*real(long_px, wp)))
            delta_px = (long_px - shrink_px)/2
            cb%height = shrink_px
            cb%bottom = orig%bottom + delta_px

            if (loc == 'left') then
                cb%left = orig%left
                main%left = orig%left + bar_px + pad_px
            else
                main%left = orig%left
                cb%left = orig%left + main%width + pad_px
            end if
        else
            bar_px = max(1, int(max(0.01_wp, fraction)*real(orig%height, wp)))
            pad_px = max(0, int(max(0.0_wp, pad)*real(orig%height, wp)))

            main%height = max(1, orig%height - bar_px - pad_px)
            cb%height = bar_px

            long_px = max(1, orig%width)
            shrink_px = max(1, int(max(0.05_wp, min(1.0_wp, shrink))*real(long_px, wp)))
            delta_px = (long_px - shrink_px)/2
            cb%width = shrink_px
            cb%left = orig%left + delta_px

            if (loc == 'bottom') then
                cb%bottom = orig%bottom
                main%bottom = orig%bottom + bar_px + pad_px
            else
                main%bottom = orig%bottom
                cb%bottom = orig%bottom + main%height + pad_px
            end if
        end if
    end subroutine compute_colorbar_plot_areas

    subroutine get_backend_plot_area(backend, plot_area, supported)
        class(plot_context), intent(in) :: backend
        type(plot_area_t), intent(out) :: plot_area
        logical, intent(out) :: supported

        supported = .false.
        plot_area%left = 0
        plot_area%bottom = 0
        plot_area%width = 0
        plot_area%height = 0

        select type (bk => backend)
        type is (png_context)
            plot_area = bk%plot_area
            supported = .true.
        type is (pdf_context)
            plot_area = bk%plot_area
            supported = .true.
        class default
            supported = .false.
        end select
    end subroutine get_backend_plot_area

    subroutine set_backend_plot_area(backend, plot_area)
        class(plot_context), intent(inout) :: backend
        type(plot_area_t), intent(in) :: plot_area

        select type (bk => backend)
        type is (png_context)
            bk%plot_area = plot_area
        type is (pdf_context)
            bk%plot_area = plot_area
        class default
            continue
        end select
    end subroutine set_backend_plot_area

end module fortplot_figure_colorbar

module fortplot_subplot_rendering
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_plot_data, only: subplot_data_t
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_figure_rendering_pipeline, only: calculate_figure_data_ranges, &
                                                  setup_coordinate_system, &
                                                  render_figure_axes, &
                                                  render_all_plots, &
                                                  render_figure_axes_labels_only
    use fortplot_margins, only: calculate_plot_area
    use fortplot_pdf_coordinate, only: calculate_pdf_plot_area
    use fortplot_subplot_layout, only: compute_tight_subplot_margins
    use fortplot_png, only: png_context
    use fortplot_pdf, only: pdf_context
    use fortplot_ascii, only: ascii_context
    implicit none

    private
    public :: render_subplots

contains

    subroutine render_subplots(state, subplots_array, subplot_rows, subplot_cols)
        type(figure_state_t), intent(inout) :: state
        type(subplot_data_t), intent(in) :: subplots_array(:, :)
        integer, intent(in) :: subplot_rows, subplot_cols

        integer :: nr, nc, i, j
        real(wp), allocatable :: left_f(:, :), right_f(:, :)
        real(wp), allocatable :: bottom_f(:, :), top_f(:, :)
        logical :: have_tight
        real(wp) :: base_left, base_right, base_bottom, base_top
        real(wp) :: wspace, hspace
        real(wp) :: total_w, total_h
        real(wp) :: ax_w, ax_h
        real(wp) :: gap_w, gap_h
        real(wp) :: subplot_left, subplot_right
        real(wp) :: subplot_bottom, subplot_top
        real(wp) :: lxmin, lxmax, lymin, lymax
        real(wp) :: lxmin_t, lxmax_t, lymin_t, lymax_t

        nr = subplot_rows
        nc = subplot_cols

        have_tight = .false.
        call compute_tight_subplot_margins(state%backend, subplots_array, nr, nc, &
                                           state%xscale, state%yscale, &
                                           state%symlog_threshold, &
                                           left_f, right_f, &
                                           bottom_f, top_f, have_tight)

        if (.not. have_tight) then
            base_left = 0.125_wp
            base_right = 0.90_wp
            base_bottom = 0.11_wp
            base_top = 0.88_wp
            wspace = 0.20_wp
            hspace = 0.20_wp

            total_w = base_right - base_left
            total_h = base_top - base_bottom

            ax_w = total_w/ &
                   (real(nc, wp) + wspace*real(max(0, nc - 1), wp))
            gap_w = wspace*ax_w

            ax_h = total_h/ &
                   (real(nr, wp) + hspace*real(max(0, nr - 1), wp))
            gap_h = hspace*ax_h
        end if

        do i = 1, nr
            do j = 1, nc
                if (have_tight) then
                    call set_subplot_margins(state%backend, left_f(i, j), &
                                             right_f(i, j), bottom_f(i, j), &
                                             top_f(i, j))
                else
                    subplot_left = base_left + real(j - 1, wp)*(ax_w + gap_w)
                    subplot_right = subplot_left + ax_w
                    subplot_top = base_top - real(i - 1, wp)*(ax_h + gap_h)
                    subplot_bottom = subplot_top - ax_h
                    call set_subplot_margins(state%backend, subplot_left, &
                                             subplot_right, subplot_bottom, &
                                             subplot_top)
                end if

                call calculate_figure_data_ranges(subplots_array(i, j)%plots, &
                                                  subplots_array(i, j)%plot_count, &
                                                  subplots_array(i, j)%xlim_set, &
                                                  subplots_array(i, j)%ylim_set, &
                                                  lxmin, lxmax, lymin, lymax, &
                                                  lxmin_t, lxmax_t, lymin_t, lymax_t, &
                                                  state%xscale, state%yscale, &
                                                  state%symlog_threshold)

                call setup_coordinate_system(state%backend, lxmin_t, lxmax_t, &
                                             lymin_t, lymax_t)

                call render_figure_axes(state%backend, state%xscale, state%yscale, &
                                        state%symlog_threshold, lxmin, lxmax, &
                                        lymin, lymax, subplots_array(i, j)%title, &
                                        subplots_array(i, j)%xlabel, &
                                        subplots_array(i, j)%ylabel, &
                                        subplots_array(i, j)%plots, &
                                        subplots_array(i, j)%plot_count, &
                                        has_twinx=.false., has_twiny=.false.)

                if (subplots_array(i, j)%plot_count > 0) then
                    call render_all_plots(state%backend, subplots_array(i, j)%plots, &
                                          subplots_array(i, j)%plot_count, &
                                          lxmin_t, lxmax_t, &
                                          lymin_t, lymax_t, state%xscale, &
                                          state%yscale, &
                                          state%symlog_threshold, state%width, &
                                          state%height, &
                                          state%margin_left, state%margin_right, &
                                          state%margin_bottom, &
                                          state%margin_top)
                end if

                call render_figure_axes_labels_only(state%backend, state%xscale, &
                                                    state%yscale, &
                                                    state%symlog_threshold, lxmin, &
                                                    lxmax, lymin, &
                                                    lymax, subplots_array(i, j)%title, &
                                                    subplots_array(i, j)%xlabel, &
                                                    subplots_array(i, j)%ylabel, &
                                                    subplots_array(i, j)%plots, &
                                                    subplots_array(i, j)%plot_count, &
                                                    has_twinx=.false., &
                                                    has_twiny=.false.)
            end do
        end do

        call render_suptitle(state)
    end subroutine render_subplots

    subroutine render_suptitle(state)
        !! Render the figure-level suptitle above all subplots
        use fortplot_raster_labels, only: render_title_centered_with_size
        use fortplot_pdf_text, only: estimate_pdf_text_width
        use fortplot_pdf_core, only: PDF_TITLE_SIZE
        type(figure_state_t), intent(inout) :: state

        real(wp) :: suptitle_y_frac, center_x
        integer :: suptitle_y_px
        real(wp) :: font_scale

        if (.not. allocated(state%suptitle)) return
        if (len_trim(state%suptitle) == 0) return

        font_scale = state%suptitle_fontsize / 12.0_wp

        select type (bk => state%backend)
        class is (png_context)
            suptitle_y_frac = 0.96_wp
            suptitle_y_px = int(real(bk%height, wp) * suptitle_y_frac)
            center_x = real(bk%width, wp) / 2.0_wp
            call render_title_centered_with_size(bk%raster, bk%width, bk%height, &
                                                 int(center_x), suptitle_y_px, &
                                                 trim(state%suptitle), font_scale)
        class is (pdf_context)
            block
                real(wp) :: title_width, x_center, y_pos
                real(wp) :: scaled_font_size
                real(wp) :: black_color(3)

                scaled_font_size = PDF_TITLE_SIZE * font_scale
                title_width = estimate_pdf_text_width(trim(state%suptitle), &
                                                      scaled_font_size)
                x_center = real(bk%width, wp) / 2.0_wp
                y_pos = real(bk%height, wp) * 0.96_wp
                black_color = [0.0_wp, 0.0_wp, 0.0_wp]
                call bk%draw_text_styled(x_center, y_pos, trim(state%suptitle), &
                                         scaled_font_size, 0.0_wp, 'center', 'bottom', &
                                         .false., black_color)
            end block
        class is (ascii_context)
            call bk%set_title(trim(state%suptitle))
        class default
        end select
    end subroutine render_suptitle

    subroutine set_subplot_margins(backend, left_f, right_f, bottom_f, top_f)
        class(*), intent(inout) :: backend
        real(wp), intent(in) :: left_f, right_f, bottom_f, top_f

        select type (bk => backend)
        class is (png_context)
            bk%margins%left = left_f
            bk%margins%right = right_f
            bk%margins%bottom = bottom_f
            bk%margins%top = top_f
            call calculate_plot_area(bk%width, bk%height, bk%margins, bk%plot_area)
        class is (pdf_context)
            bk%margins%left = left_f
            bk%margins%right = right_f
            bk%margins%bottom = bottom_f
            bk%margins%top = top_f
            call calculate_pdf_plot_area(bk%width, bk%height, bk%margins, bk%plot_area)
        class is (ascii_context)
        class default
        end select
    end subroutine set_subplot_margins

end module fortplot_subplot_rendering

submodule (fortplot_pdf) fortplot_pdf_axes_io

    !! PDF axes rendering, data extraction, and file I/O
    !!
    !! Single Responsibility: Handle PDF file writing, axes rendering,
    !! secondary axes, data extraction, and 3D data preparation.

    implicit none

contains

    module subroutine write_pdf_file_facade(this, filename)
        use fortplot_system_viewer, only: launch_system_viewer, &
                                          has_graphical_session, &
                                          get_temp_filename
        class(pdf_context), intent(inout) :: this
        character(len=*), intent(in) :: filename
        logical :: file_success
        character(len=1024) :: actual_filename
        logical :: viewer_success

        ! Handle terminal display
        if (trim(filename) == 'terminal') then
            if (has_graphical_session()) then
                call get_temp_filename('.pdf', actual_filename)
            else
                call log_info("No graphical session detected, cannot display PDF")
                call log_info("Use savefig('filename.pdf') to save to file or")
                call log_info("Use savefig('filename.txt') for ASCII rendering")
                return
            end if
        else
            actual_filename = filename
        end if

        ! Do not re-render axes here. The main rendering pipeline has already
        ! produced the complete `core_ctx%stream_data`, including axes, tick labels,
        ! titles/axis labels, legend text, and annotations. Re-rendering would
        ! clear or overwrite that state and can drop labels/legend.

        ! Merge vector drawing stream (lines, markers, etc.) with the core text
        ! stream. Keep existing `core_ctx%stream_data` intact to preserve labels
        ! and legend text that were rendered earlier in the pipeline.
        if (len_trim(this%stream_writer%content_stream) > 0) then
            if (len_trim(this%core_ctx%stream_data) > 0) then
                this%core_ctx%stream_data = trim(this%stream_writer%content_stream)// &
                                            new_line('a')// &
                                            trim(this%core_ctx%stream_data)
            else
                this%core_ctx%stream_data = this%stream_writer%content_stream
            end if
        end if

        ! Ensure a solid dash reset exists in the final content stream so that
        ! axes frame and tick marks are rendered with solid strokes regardless
        ! of prior plot linestyle state. This is harmless if plots later set a
        ! different dash pattern; the presence of this operator guarantees the
        ! PDF stream contains an explicit solid dash command.
        this%core_ctx%stream_data = &
            '[] 0 d'//new_line('a')//trim(this%core_ctx%stream_data)

        ! Balance top-level PDF graphics state saves. Both the PDF core stream and
        ! the vector stream writer introduce a top-level q save operator; close
        ! them here so consumers like Ghostscript do not need to repair the file.
        this%core_ctx%stream_data = trim(this%core_ctx%stream_data)// &
                                    new_line('a')//'Q'//new_line('a')//'Q'
        call write_pdf_file(this%core_ctx, actual_filename, file_success)
        if (.not. file_success) return

        ! Launch viewer if displaying to terminal
        if (trim(filename) == 'terminal' .and. has_graphical_session()) then
            call launch_system_viewer(actual_filename, viewer_success)
            if (.not. viewer_success) then
                call log_error("Failed to launch PDF viewer for: "// &
                               trim(actual_filename))
                call log_info("You can manually open: "//trim(actual_filename))
            end if
        end if
    end subroutine write_pdf_file_facade

    module subroutine extract_rgb_data_wrapper(this, width, height, rgb_data)
        class(pdf_context), intent(in) :: this
        integer, intent(in) :: width, height
        real(wp), intent(out) :: rgb_data(width, height, 3)
        type(pdf_context_handle) :: local_ctx

        local_ctx = this%make_coord_context()
        call pdf_extract_rgb_data(local_ctx, width, height, rgb_data)
    end subroutine extract_rgb_data_wrapper

    module subroutine get_png_data_wrapper(this, width, height, png_data, status)
        class(pdf_context), intent(in) :: this
        integer, intent(in) :: width, height
        integer(1), allocatable, intent(out) :: png_data(:)
        integer, intent(out) :: status
        type(pdf_context_handle) :: local_ctx

        local_ctx = this%make_coord_context()
        call pdf_get_png_data(local_ctx, width, height, png_data, status)
    end subroutine get_png_data_wrapper

    module subroutine prepare_3d_data_wrapper(this, plots)
        class(pdf_context), intent(inout) :: this
        type(plot_data_t), intent(in) :: plots(:)

        call this%update_coord_context()
        call pdf_prepare_3d_data(this%coord_ctx, plots)
    end subroutine prepare_3d_data_wrapper

    module subroutine render_ylabel_wrapper(this, ylabel)
        class(pdf_context), intent(inout) :: this
        character(len=*), intent(in) :: ylabel

        call this%update_coord_context()
        call pdf_render_ylabel(this%coord_ctx, ylabel)
    end subroutine render_ylabel_wrapper

    module subroutine draw_axes_and_labels_backend_wrapper(this, xscale, yscale, &
                                                    symlog_threshold, &
                                                    x_min, x_max, y_min, y_max, &
                                                    title, xlabel, ylabel, &
                                                    x_date_format, y_date_format, &
                                                    z_min, z_max, has_3d_plots)
        use fortplot_3d_axes, only: draw_3d_axes
        use fortplot_pdf_axes, only: draw_pdf_title_and_labels
        class(pdf_context), intent(inout) :: this
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=:), allocatable, intent(in), optional :: title, xlabel, ylabel
        character(len=*), intent(in), optional :: x_date_format, y_date_format
        real(wp), intent(in), optional :: z_min, z_max
        logical, intent(in) :: has_3d_plots

        character(len=256) :: title_str, xlabel_str, ylabel_str
        associate (dzmin => z_min, dzmax => z_max, dh3d => has_3d_plots); end associate

        title_str = ""; xlabel_str = ""; ylabel_str = ""
        if (present(title)) title_str = title
        if (present(xlabel)) xlabel_str = xlabel
        if (present(ylabel)) ylabel_str = ylabel

        if (has_3d_plots) then
            call draw_3d_axes(this, x_min, x_max, y_min, y_max, &
                              merge(z_min, 0.0_wp, present(z_min)), &
                              merge(z_max, 1.0_wp, present(z_max)))
            ! Draw only title/xlabel/ylabel using PDF helpers.
            ! Avoid 2D axes duplication.
            call draw_pdf_title_and_labels(this%core_ctx, title_str, xlabel_str, &
                                           ylabel_str, &
                                           real(this%plot_area%left, wp), &
                                           real(this%plot_area%bottom, wp), &
                                           real(this%plot_area%width, wp), &
                                           real(this%plot_area%height, wp))
        else
            call draw_pdf_axes_and_labels(this%core_ctx, xscale, yscale, &
                                          symlog_threshold, x_min, x_max, y_min, &
                                          y_max, title_str, xlabel_str, ylabel_str, &
                                          x_date_format=x_date_format, &
                                          y_date_format=y_date_format, &
                                          plot_area_left=real(this%plot_area%left, &
                                                              wp), &
                                          plot_area_bottom=real(this%plot_area%bottom, &
                                                                wp), &
                                          plot_area_width=real(this%plot_area%width, &
                                                               wp), &
                                          plot_area_height=real(this%plot_area%height, &
                                                                wp), &
                                          custom_xticks=this%custom_xtick_positions, &
                                          custom_xtick_labels=this%custom_xtick_labels, &
                                          custom_yticks=this%custom_ytick_positions, &
                                          custom_ytick_labels=this%custom_ytick_labels)
        end if
    end subroutine draw_axes_and_labels_backend_wrapper

    module subroutine render_pdf_axes_wrapper(this, title_text, xlabel_text, ylabel_text)
        !! Explicitly render axes with optional labels
        !! This allows low-level PDF users to add proper axes to their plots
        class(pdf_context), intent(inout) :: this
        character(len=*), intent(in), optional :: title_text, xlabel_text, ylabel_text

        character(len=256) :: title_str, xlabel_str, ylabel_str

        ! Only render axes once unless coordinates change
        if (this%axes_rendered) return

        ! Ensure coordinate system is set
        if (abs(this%x_max-this%x_min) <= epsilon(1.0_wp) .or. &
            abs(this%y_max-this%y_min) <= epsilon(1.0_wp)) then
            ! No valid coordinate system - skip axes
            return
        end if

        ! Set default empty strings for labels
        title_str = ""
        xlabel_str = ""
        ylabel_str = ""

        ! Use provided labels if present
        if (present(title_text)) title_str = title_text
        if (present(xlabel_text)) xlabel_str = xlabel_text
        if (present(ylabel_text)) ylabel_str = ylabel_text

        ! Clear any previous axes data in core context
        this%core_ctx%stream_data = ""

        ! Draw axes and labels with current coordinate system
        call draw_pdf_axes_and_labels(this%core_ctx, "linear", "linear", 1.0_wp, &
                                      this%x_min, this%x_max, this%y_min, this%y_max, &
                                      title_str, xlabel_str, ylabel_str, &
                                      plot_area_left=real(this%plot_area%left, wp), &
                                      plot_area_bottom=real(this%plot_area%bottom, &
                                                            wp), &
                                      plot_area_width=real(this%plot_area%width, wp), &
                                      plot_area_height=real(this%plot_area%height, wp))

        ! Add axes content to the stream
        call this%stream_writer%add_to_stream(this%core_ctx%stream_data)

        ! Mark axes as rendered
        this%axes_rendered = .true.
    end subroutine render_pdf_axes_wrapper

    module subroutine pdf_draw_secondary_y_axis_wrapper(this, yscale, symlog_threshold, &
                                                 y_min, y_max, ylabel, date_format)
        !! Draw secondary Y axis on the right side for twin axes support
        class(pdf_context), intent(inout) :: this
        character(len=*), intent(in) :: yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: y_min, y_max
        character(len=:), allocatable, intent(in), optional :: ylabel
        character(len=*), intent(in), optional :: date_format

        call draw_pdf_secondary_y_axis(this%core_ctx, yscale, symlog_threshold, &
                                       y_min, y_max, &
                                       real(this%plot_area%left, wp), &
                                       real(this%plot_area%bottom, wp), &
                                       real(this%plot_area%width, wp), &
                                       real(this%plot_area%height, wp), &
                                       ylabel, date_format=date_format)
    end subroutine pdf_draw_secondary_y_axis_wrapper

    module subroutine pdf_draw_secondary_x_axis_top_wrapper(this, xscale, symlog_threshold, &
                                                     x_min, x_max, xlabel, date_format)
        !! Draw secondary X axis at the top for twin axes support
        class(pdf_context), intent(inout) :: this
        character(len=*), intent(in) :: xscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max
        character(len=:), allocatable, intent(in), optional :: xlabel
        character(len=*), intent(in), optional :: date_format

        call draw_pdf_secondary_x_axis_top(this%core_ctx, xscale, symlog_threshold, &
                                           x_min, x_max, &
                                           real(this%plot_area%left, wp), &
                                           real(this%plot_area%bottom, wp), &
                                           real(this%plot_area%width, wp), &
                                           real(this%plot_area%height, wp), &
                                           xlabel, date_format=date_format)
    end subroutine pdf_draw_secondary_x_axis_top_wrapper

end submodule fortplot_pdf_axes_io

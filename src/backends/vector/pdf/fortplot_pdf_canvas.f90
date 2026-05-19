submodule (fortplot_pdf) fortplot_pdf_canvas

    !! PDF canvas creation and coordinate management
    !!
    !! Single Responsibility: Handle PDF canvas creation, coordinate
    !! context management, and scale calculations.

    implicit none

contains

    module function create_pdf_canvas(width, height) result(ctx)
        integer, intent(in) :: width, height
        type(pdf_context) :: ctx
        ! Align PDF canvas size with matplotlib inches and DPI semantics.
        ! Our figure dimensions are in pixels at a default DPI of 100.
        ! PDF units are points (1 pt = 1/72 inch). Convert pixels -> points
        ! so that an 800x600px figure maps to a 8x6 inch PDF page (576x432 pt).
        real(wp) :: width_pts, height_pts
        integer :: width_pts_i, height_pts_i

        width_pts = real(width, wp)*72.0_wp/REFERENCE_DPI
        height_pts = real(height, wp)*72.0_wp/REFERENCE_DPI
        ! Use integer canvas for downstream plot-area computations
        width_pts_i = max(1, nint(width_pts))
        height_pts_i = max(1, nint(height_pts))

        ! For the PDF backend, treat the logical canvas size as PDF points so
        ! downstream plot-area calculations remain consistent with the PDF page.
        call setup_canvas(ctx, width_pts_i, height_pts_i)

        ctx%core_ctx = create_pdf_canvas_core(real(width_pts_i, wp), &
                                              real(height_pts_i, wp))

        call ctx%stream_writer%initialize_stream()
        call ctx%stream_writer%add_to_stream("q")
        call ctx%stream_writer%add_to_stream("1 w")
        call ctx%stream_writer%add_to_stream("1 J")
        call ctx%stream_writer%add_to_stream("1 j")
        call ctx%stream_writer%add_to_stream("0 0 0 RG")
        call ctx%stream_writer%add_to_stream("0 0 0 rg")

        ctx%margins = plot_margins_t()
        call calculate_pdf_plot_area(width_pts_i, height_pts_i, ctx%margins, &
                                     ctx%plot_area)

        call ctx%update_coord_context()
    end function create_pdf_canvas

    module subroutine update_coord_context(this)
        class(pdf_context), intent(inout) :: this

        this%coord_ctx%x_min = this%x_min
        this%coord_ctx%x_max = this%x_max
        this%coord_ctx%y_min = this%y_min
        this%coord_ctx%y_max = this%y_max
        ! Coordinate context should operate in the same units as the PDF page
        ! dimensions (points). Keep plot area (already computed in points) and
        ! propagate the converted canvas size by recomputing from core context.
        this%coord_ctx%width = int(this%core_ctx%width)
        this%coord_ctx%height = int(this%core_ctx%height)
        this%coord_ctx%plot_area = this%plot_area
        this%coord_ctx%core_ctx = this%core_ctx
    end subroutine update_coord_context

    module function make_coord_context(this) result(ctx)
        class(pdf_context), intent(in) :: this
        type(pdf_context_handle) :: ctx

        ctx%x_min = this%x_min
        ctx%x_max = this%x_max
        ctx%y_min = this%y_min
        ctx%y_max = this%y_max
        ctx%width = int(this%core_ctx%width)
        ctx%height = int(this%core_ctx%height)
        ctx%plot_area = this%plot_area
        ctx%core_ctx = this%core_ctx
    end function make_coord_context

    module subroutine pdf_save_coordinates(this, x_min, x_max, y_min, y_max)
        class(pdf_context), intent(in) :: this
        real(wp), intent(out) :: x_min, x_max, y_min, y_max

        ! Return current coordinate bounds
        x_min = this%x_min
        x_max = this%x_max
        y_min = this%y_min
        y_max = this%y_max
    end subroutine pdf_save_coordinates

    module subroutine pdf_set_coordinates(this, x_min, x_max, y_min, y_max)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: x_min, x_max, y_min, y_max

        this%x_min = x_min
        this%x_max = x_max
        this%y_min = y_min
        this%y_max = y_max

        ! Reset axes flag when coordinates change
        this%axes_rendered = .false.
    end subroutine pdf_set_coordinates

    real(wp) module function get_width_scale_wrapper(this) result(scale)
        class(pdf_context), intent(in) :: this
        type(pdf_context_handle) :: local_ctx
        local_ctx = this%make_coord_context()
        scale = pdf_get_width_scale(local_ctx)
    end function get_width_scale_wrapper

    real(wp) module function get_height_scale_wrapper(this) result(scale)
        class(pdf_context), intent(in) :: this
        type(pdf_context_handle) :: local_ctx
        local_ctx = this%make_coord_context()
        scale = pdf_get_height_scale(local_ctx)
    end function get_height_scale_wrapper

end submodule fortplot_pdf_canvas

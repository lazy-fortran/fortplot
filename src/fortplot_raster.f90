module fortplot_raster
    use iso_c_binding
    use fortplot_context, only: plot_context, setup_canvas
    use fortplot_text, only: render_text_to_image, calculate_text_width, calculate_text_height
    use fortplot_latex_parser, only: process_latex_in_text
    ! use fortplot_unicode, only: unicode_to_ascii
    use fortplot_logging, only: log_error
    use fortplot_errors, only: fortplot_error_t, ERROR_INTERNAL
    use fortplot_margins, only: plot_margins_t, plot_area_t, calculate_plot_area, get_axis_tick_positions
    use fortplot_ticks, only: generate_scale_aware_tick_labels, format_tick_value_smart, find_nice_tick_locations
    use fortplot_scales, only: apply_scale_transform
    use fortplot_label_positioning, only: calculate_x_label_position, calculate_y_label_position, &
                                         calculate_x_tick_label_position, calculate_y_tick_label_position, &
                                         calculate_x_axis_label_position, calculate_y_axis_label_position
    use fortplot_markers, only: get_marker_size, MARKER_CIRCLE, MARKER_SQUARE, MARKER_DIAMOND, MARKER_CROSS
    use fortplot_colormap, only: colormap_value_to_color
    use fortplot_interpolation, only: interpolate_z_bilinear
    use fortplot_raster_drawing, only: draw_line_distance_aa, blend_pixel, distance_point_to_line_segment, &
                                       ipart, fpart, rfpart, color_to_byte, &
                                       draw_circle_antialiased, draw_circle_outline_antialiased, &
                                       draw_circle_with_edge_face, draw_square_with_edge_face, &
                                       draw_diamond_with_edge_face, draw_x_marker, draw_filled_quad_raster
    use fortplot_raster_line_styles, only: draw_styled_line, set_raster_line_style, reset_pattern_distance
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: raster_image_t, create_raster_image, destroy_raster_image
    public :: initialize_white_background, composite_image, composite_bitmap_to_raster
    public :: render_text_to_bitmap, rotate_bitmap_90_cw, rotate_bitmap_90_ccw, bitmap_to_png_buffer
    public :: raster_context, create_raster_canvas, raster_draw_axes_and_labels, raster_render_ylabel

    integer, parameter :: DEFAULT_RASTER_LINE_WIDTH_SCALING = 10

    type :: raster_image_t
        integer(1), allocatable :: image_data(:)
        integer :: width, height
        real(wp) :: current_r = 0.0_wp, current_g = 0.0_wp, current_b = 0.0_wp
        real(wp) :: current_line_width = 1.0_wp
        ! Line style pattern support
        character(len=10) :: line_style = '-'
        real(wp) :: line_pattern(20)
        integer :: pattern_size = 1
        real(wp) :: pattern_length = 1000.0_wp
        real(wp) :: pattern_distance = 0.0_wp
        ! Marker colors - separate edge and face colors with alpha
        real(wp) :: marker_edge_r = 0.0_wp, marker_edge_g = 0.0_wp, marker_edge_b = 0.0_wp, marker_edge_alpha = 1.0_wp
        real(wp) :: marker_face_r = 1.0_wp, marker_face_g = 0.0_wp, marker_face_b = 0.0_wp, marker_face_alpha = 1.0_wp
    contains
        procedure :: set_color => raster_set_color
        procedure :: get_color_bytes => raster_get_color_bytes
        procedure :: set_line_style => raster_set_line_style
    end type raster_image_t

    ! Raster plotting context - backend-agnostic bitmap operations
    type, extends(plot_context) :: raster_context
        type(raster_image_t) :: raster
        ! Plot area calculations (using common margin functionality)
        type(plot_margins_t) :: margins
        type(plot_area_t) :: plot_area
    contains
        procedure :: line => raster_draw_line
        procedure :: color => raster_set_color_context
        procedure :: text => raster_draw_text
        procedure :: set_line_width => raster_set_line_width
        procedure :: set_line_style => raster_set_line_style_context
        procedure :: save => raster_save_dummy
        procedure :: draw_marker => raster_draw_marker
        procedure :: set_marker_colors => raster_set_marker_colors
        procedure :: set_marker_colors_with_alpha => raster_set_marker_colors_with_alpha
        procedure :: fill_quad => raster_fill_quad
        procedure :: draw_arrow => raster_draw_arrow
        procedure :: get_ascii_output => raster_get_ascii_output
        
        !! New polymorphic methods to eliminate SELECT TYPE
        procedure :: get_width_scale => raster_get_width_scale
        procedure :: get_height_scale => raster_get_height_scale
        procedure :: fill_heatmap => raster_fill_heatmap
        procedure :: render_legend_specialized => raster_render_legend_specialized
        procedure :: calculate_legend_dimensions => raster_calculate_legend_dimensions
        procedure :: set_legend_border_width => raster_set_legend_border_width
        procedure :: calculate_legend_position_backend => raster_calculate_legend_position
        procedure :: extract_rgb_data => raster_extract_rgb_data
        procedure :: get_png_data_backend => raster_get_png_data
        procedure :: prepare_3d_data => raster_prepare_3d_data
        procedure :: render_ylabel => raster_render_ylabel
        procedure :: draw_axes_and_labels_backend => raster_draw_axes_and_labels
        procedure :: save_coordinates => raster_save_coordinates
        procedure :: set_coordinates => raster_set_coordinates
        procedure :: render_axes => raster_render_axes
    end type raster_context

contains

    function create_raster_image(width, height) result(image)
        integer, intent(in) :: width, height
        type(raster_image_t) :: image

        image%width = width
        image%height = height
        allocate(image%image_data(width * height * 3))
        call initialize_white_background(image%image_data, width, height)
        
        ! Initialize line style to solid
        call image%set_line_style('-')
    end function create_raster_image

    subroutine destroy_raster_image(image)
        type(raster_image_t), intent(inout) :: image
        if (allocated(image%image_data)) deallocate(image%image_data)
    end subroutine destroy_raster_image

    subroutine raster_set_color(this, r, g, b)
        class(raster_image_t), intent(inout) :: this
        real(wp), intent(in) :: r, g, b

        this%current_r = r
        this%current_g = g
        this%current_b = b
    end subroutine raster_set_color

    subroutine raster_get_color_bytes(this, r, g, b)
        class(raster_image_t), intent(in) :: this
        integer(1), intent(out) :: r, g, b

        r = color_to_byte(this%current_r)
        g = color_to_byte(this%current_g)
        b = color_to_byte(this%current_b)
    end subroutine raster_get_color_bytes

    subroutine initialize_white_background(image_data, w, h)
        integer(1), intent(out) :: image_data(:)
        integer, intent(in) :: w, h
        integer :: expected_size

        ! Validate inputs
        if (w <= 0 .or. h <= 0) return
        
        expected_size = w * h * 3
        
        ! Validate array size matches expected size
        if (size(image_data) < expected_size) then
            return
        end if
        
        ! Use intrinsic assignment to initialize entire array at once - safer
        image_data = -1_1  ! White = 255 = -1 in signed byte
        
    end subroutine initialize_white_background



    subroutine composite_image(main_image, main_width, main_height, &
                              overlay_image, overlay_width, overlay_height, dest_x, dest_y)
        integer(1), intent(inout) :: main_image(*)
        integer, intent(in) :: main_width, main_height
        integer(1), intent(in) :: overlay_image(*)
        integer, intent(in) :: overlay_width, overlay_height, dest_x, dest_y
        integer :: x, y, src_idx, dst_idx, img_x, img_y
        
        do y = 1, overlay_height
            do x = 1, overlay_width
                img_x = dest_x + x - 1
                img_y = dest_y + y - 1
                
                if (img_x >= 1 .and. img_x <= main_width .and. &
                    img_y >= 1 .and. img_y <= main_height) then
                    
                    src_idx = ((y - 1) * overlay_width + (x - 1)) * 3 + 1
                    dst_idx = ((img_y - 1) * main_width + (img_x - 1)) * 3 + 1
                    
                    if (overlay_image(src_idx) /= -1_1 .or. &
                        overlay_image(src_idx+1) /= -1_1 .or. &
                        overlay_image(src_idx+2) /= -1_1) then
                        main_image(dst_idx:dst_idx+2) = overlay_image(src_idx:src_idx+2)
                    end if
                end if
            end do
        end do
    end subroutine composite_image

    subroutine composite_bitmap_to_raster(raster_buffer, raster_width, raster_height, &
                                         bitmap, bitmap_width, bitmap_height, dest_x, dest_y)
        !! Composite 3D RGB bitmap directly onto raster image buffer
        integer(1), intent(inout) :: raster_buffer(*)
        integer, intent(in) :: raster_width, raster_height
        integer(1), intent(in) :: bitmap(:,:,:)
        integer, intent(in) :: bitmap_width, bitmap_height, dest_x, dest_y
        integer :: x, y, raster_x, raster_y, raster_idx
        
        do y = 1, bitmap_height
            do x = 1, bitmap_width
                raster_x = dest_x + x - 1
                raster_y = dest_y + y - 1
                
                ! Check bounds
                if (raster_x >= 1 .and. raster_x <= raster_width .and. &
                    raster_y >= 1 .and. raster_y <= raster_height) then
                    
                    ! Skip white pixels (don't composite background)
                    if (bitmap(x, y, 1) /= -1_1 .or. &
                        bitmap(x, y, 2) /= -1_1 .or. &
                        bitmap(x, y, 3) /= -1_1) then
                        
                        raster_idx = ((raster_y - 1) * raster_width + (raster_x - 1)) * 3 + 1
                        raster_buffer(raster_idx)     = bitmap(x, y, 1)  ! R
                        raster_buffer(raster_idx + 1) = bitmap(x, y, 2)  ! G
                        raster_buffer(raster_idx + 2) = bitmap(x, y, 3)  ! B
                    end if
                end if
            end do
        end do
    end subroutine composite_bitmap_to_raster


    subroutine render_text_to_bitmap(bitmap, width, height, x, y, text)
        !! Render text to RGB bitmap by using existing PNG rendering then converting
        use fortplot_text, only: render_text_to_image
        integer(1), intent(inout) :: bitmap(:,:,:)
        integer, intent(in) :: width, height, x, y
        character(len=*), intent(in) :: text
        
        ! Create temporary PNG buffer for text rendering
        integer(1), allocatable :: temp_buffer(:)
        integer :: i, j, buf_idx
        
        allocate(temp_buffer(width * height * 3))
        call initialize_white_background(temp_buffer, width, height)
        call render_text_to_image(temp_buffer, width, height, x, y, text, 0_1, 0_1, 0_1)
        
        ! Convert PNG buffer to bitmap
        do j = 1, height
            do i = 1, width
                buf_idx = ((j - 1) * width + (i - 1)) * 3 + 1
                bitmap(i, j, 1) = temp_buffer(buf_idx)     ! R
                bitmap(i, j, 2) = temp_buffer(buf_idx + 1) ! G  
                bitmap(i, j, 3) = temp_buffer(buf_idx + 2) ! B
            end do
        end do
        
        deallocate(temp_buffer)
    end subroutine render_text_to_bitmap

    subroutine rotate_bitmap_90_ccw(src_bitmap, dst_bitmap, src_width, src_height)
        !! Rotate bitmap 90 degrees clockwise: (x,y) -> (y, src_width-x+1)
        integer(1), intent(in) :: src_bitmap(:,:,:)
        integer(1), intent(out) :: dst_bitmap(:,:,:)
        integer, intent(in) :: src_width, src_height
        integer :: i, j
        
        do j = 1, src_height
            do i = 1, src_width
                dst_bitmap(j, src_width - i + 1, :) = src_bitmap(i, j, :)
            end do
        end do
    end subroutine rotate_bitmap_90_ccw

    subroutine rotate_bitmap_90_cw(src_bitmap, dst_bitmap, src_width, src_height)
        !! Rotate bitmap 90 degrees counter-clockwise: (x,y) -> (src_height-y+1, x)
        integer(1), intent(in) :: src_bitmap(:,:,:)
        integer(1), intent(out) :: dst_bitmap(:,:,:)
        integer, intent(in) :: src_width, src_height
        integer :: i, j
        
        do j = 1, src_height
            do i = 1, src_width
                dst_bitmap(src_height - j + 1, i, :) = src_bitmap(i, j, :)
            end do
        end do
    end subroutine rotate_bitmap_90_cw

    subroutine bitmap_to_png_buffer(bitmap, width, height, buffer)
        !! Convert 3D RGB bitmap to PNG buffer format with filter bytes
        integer(1), intent(in) :: bitmap(:,:,:)
        integer, intent(in) :: width, height
        integer(1), intent(out) :: buffer(:)
        integer :: i, j, buf_idx, row_start
        
        ! PNG format: each row starts with filter byte (0 = no filter) followed by RGB data
        do j = 1, height
            row_start = (j - 1) * (1 + width * 3) + 1
            buffer(row_start) = 0_1  ! PNG filter byte: 0 = no filter
            
            do i = 1, width
                buf_idx = row_start + 1 + (i - 1) * 3
                buffer(buf_idx)     = bitmap(i, j, 1) ! R
                buffer(buf_idx + 1) = bitmap(i, j, 2) ! G
                buffer(buf_idx + 2) = bitmap(i, j, 3) ! B
            end do
        end do
    end subroutine bitmap_to_png_buffer

    function create_raster_canvas(width, height) result(ctx)
        integer, intent(in) :: width, height
        type(raster_context) :: ctx

        call setup_canvas(ctx, width, height)

        ctx%raster = create_raster_image(width, height)

        ! Set up matplotlib-style margins using common module
        ctx%margins = plot_margins_t()  ! Use defaults
        call calculate_plot_area(width, height, ctx%margins, ctx%plot_area)
    end function create_raster_canvas

    subroutine raster_draw_line(this, x1, y1, x2, y2)
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: x1, y1, x2, y2
        real(wp) :: px1, py1, px2, py2


        ! Transform coordinates to plot area (like matplotlib)
        ! Note: Raster Y=0 at top, so we need to flip Y coordinates
        px1 = (x1 - this%x_min) / (this%x_max - this%x_min) * real(this%plot_area%width, wp) + real(this%plot_area%left, wp)
        py1 = real(this%plot_area%bottom + this%plot_area%height, wp) - &
              (y1 - this%y_min) / (this%y_max - this%y_min) * real(this%plot_area%height, wp)
        px2 = (x2 - this%x_min) / (this%x_max - this%x_min) * real(this%plot_area%width, wp) + real(this%plot_area%left, wp)
        py2 = real(this%plot_area%bottom + this%plot_area%height, wp) - &
              (y2 - this%y_min) / (this%y_max - this%y_min) * real(this%plot_area%height, wp)

        ! Draw line with pattern support
        call draw_styled_line(this%raster%image_data, this%width, this%height, &
                             px1, py1, px2, py2, &
                             this%raster%current_r, this%raster%current_g, this%raster%current_b, &
                             this%raster%current_line_width, &
                             this%raster%line_style, this%raster%line_pattern, &
                             this%raster%pattern_size, this%raster%pattern_length, &
                             this%raster%pattern_distance)
    end subroutine raster_draw_line

    subroutine raster_set_color_context(this, r, g, b)
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: r, g, b

        call this%raster%set_color(r, g, b)
    end subroutine raster_set_color_context

    subroutine raster_set_line_width(this, width)
        !! Set line width for raster drawing with proper pixel scaling
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: width

        ! Map line width to pixel thickness with reasonable scaling
        ! Use linear scaling: 1 point = 1 pixel for good visibility
        if (width <= 0.0_wp) then
            this%raster%current_line_width = 1.0_wp  ! Minimum visible width
        else if (width >= 10.0_wp) then
            this%raster%current_line_width = 10.0_wp  ! Maximum reasonable width
        else
            this%raster%current_line_width = width  ! Direct 1:1 mapping
        end if
    end subroutine raster_set_line_width

    subroutine raster_set_line_style(this, style)
        !! Set line style pattern for raster image
        class(raster_image_t), intent(inout) :: this
        character(len=*), intent(in) :: style
        
        call set_raster_line_style(style, this%line_style, this%line_pattern, &
                                  this%pattern_size, this%pattern_length, this%pattern_distance)
    end subroutine raster_set_line_style

    subroutine raster_set_line_style_context(this, style)
        !! Set line style for raster context
        class(raster_context), intent(inout) :: this
        character(len=*), intent(in) :: style
        
        call this%raster%set_line_style(style)
    end subroutine raster_set_line_style_context

    subroutine escape_unicode_for_raster(input_text, escaped_text)
        !! Pass through Unicode for raster rendering (STB TrueType supports Unicode)
        character(len=*), intent(in) :: input_text
        character(len=*), intent(out) :: escaped_text
        
        ! STB TrueType can handle Unicode directly, so just pass through
        escaped_text = input_text
    end subroutine escape_unicode_for_raster

    subroutine unicode_codepoint_to_ascii(codepoint, ascii_equiv)
        !! Convert Unicode codepoint to ASCII equivalent
        integer, intent(in) :: codepoint
        character(len=*), intent(out) :: ascii_equiv
        
        ! Convert Greek letters to ASCII names
        select case (codepoint)
        case (945) ! α
            ascii_equiv = "alpha"
        case (946) ! β
            ascii_equiv = "beta"
        case (947) ! γ
            ascii_equiv = "gamma"
        case (948) ! δ
            ascii_equiv = "delta"
        case (949) ! ε
            ascii_equiv = "epsilon"
        case (950) ! ζ
            ascii_equiv = "zeta"
        case (951) ! η
            ascii_equiv = "eta"
        case (952) ! θ
            ascii_equiv = "theta"
        case (953) ! ι
            ascii_equiv = "iota"
        case (954) ! κ
            ascii_equiv = "kappa"
        case (955) ! λ
            ascii_equiv = "lambda"
        case (956) ! μ
            ascii_equiv = "mu"
        case (957) ! ν
            ascii_equiv = "nu"
        case (958) ! ξ
            ascii_equiv = "xi"
        case (959) ! ο
            ascii_equiv = "omicron"
        case (960) ! π
            ascii_equiv = "pi"
        case (961) ! ρ
            ascii_equiv = "rho"
        case (963) ! σ
            ascii_equiv = "sigma"
        case (964) ! τ
            ascii_equiv = "tau"
        case (965) ! υ
            ascii_equiv = "upsilon"
        case (966) ! φ
            ascii_equiv = "phi"
        case (967) ! χ
            ascii_equiv = "chi"
        case (968) ! ψ
            ascii_equiv = "psi"
        case (969) ! ω
            ascii_equiv = "omega"
        case (913) ! Α
            ascii_equiv = "Alpha"
        case (914) ! Β
            ascii_equiv = "Beta"
        case (915) ! Γ
            ascii_equiv = "Gamma"
        case (916) ! Δ
            ascii_equiv = "Delta"
        case (917) ! Ε
            ascii_equiv = "Epsilon"
        case (918) ! Ζ
            ascii_equiv = "Zeta"
        case (919) ! Η
            ascii_equiv = "Eta"
        case (920) ! Θ
            ascii_equiv = "Theta"
        case (921) ! Ι
            ascii_equiv = "Iota"
        case (922) ! Κ
            ascii_equiv = "Kappa"
        case (923) ! Λ
            ascii_equiv = "Lambda"
        case (924) ! Μ
            ascii_equiv = "Mu"
        case (925) ! Ν
            ascii_equiv = "Nu"
        case (926) ! Ξ
            ascii_equiv = "Xi"
        case (927) ! Ο
            ascii_equiv = "Omicron"
        case (928) ! Π
            ascii_equiv = "Pi"
        case (929) ! Ρ
            ascii_equiv = "Rho"
        case (931) ! Σ
            ascii_equiv = "Sigma"
        case (932) ! Τ
            ascii_equiv = "Tau"
        case (933) ! Υ
            ascii_equiv = "Upsilon"
        case (934) ! Φ
            ascii_equiv = "Phi"
        case (935) ! Χ
            ascii_equiv = "Chi"
        case (936) ! Ψ
            ascii_equiv = "Psi"
        case (937) ! Ω
            ascii_equiv = "Omega"
        case default
            ! For other Unicode characters, use a placeholder
            write(ascii_equiv, '("U+", Z4.4)') codepoint
        end select
    end subroutine unicode_codepoint_to_ascii

    subroutine raster_draw_text(this, x, y, text)
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        real(wp) :: px, py
        integer(1) :: r, g, b
        character(len=500) :: processed_text, escaped_text
        integer :: processed_len

        ! Process LaTeX commands to Unicode
        call process_latex_in_text(text, processed_text, processed_len)

        ! Escape Unicode characters for raster rendering
        call escape_unicode_for_raster(processed_text(1:processed_len), escaped_text)

        ! Transform coordinates to plot area (like matplotlib)
        ! Note: Raster Y=0 at top, so we need to flip Y coordinates
        px = (x - this%x_min) / (this%x_max - this%x_min) * real(this%plot_area%width, wp) + real(this%plot_area%left, wp)
        py = real(this%plot_area%bottom + this%plot_area%height, wp) - &
             (y - this%y_min) / (this%y_max - this%y_min) * real(this%plot_area%height, wp)

        call this%raster%get_color_bytes(r, g, b)
        call render_text_to_image(this%raster%image_data, this%width, this%height, &
                                 int(px), int(py), trim(escaped_text), r, g, b)
    end subroutine raster_draw_text

    subroutine raster_save_dummy(this, filename)
        !! Dummy save method - raster context doesn't save files directly
        class(raster_context), intent(inout) :: this
        character(len=*), intent(in) :: filename
        
        ! This is a dummy implementation - concrete backends like PNG will override this
        call log_error("raster_context cannot save files directly. Use a concrete backend like PNG.")
        ! Return instead of stopping - this allows graceful error handling
        return
    end subroutine raster_save_dummy

    subroutine raster_draw_marker(this, x, y, style)
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: style
        real(wp) :: px, py
        integer(1) :: r, g, b

        ! Transform coordinates to plot area
        px = (x - this%x_min) / (this%x_max - this%x_min) * real(this%plot_area%width, wp) + real(this%plot_area%left, wp)
        py = real(this%plot_area%bottom + this%plot_area%height, wp) - &
             (y - this%y_min) / (this%y_max - this%y_min) * real(this%plot_area%height, wp)

        call this%raster%get_color_bytes(r, g, b)

        call draw_raster_marker_by_style(this, px, py, style)
    end subroutine raster_draw_marker

    subroutine draw_raster_marker_by_style(this, px, py, style)
        !! Draw marker using shared style dispatch logic (DRY compliance)
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: px, py
        character(len=*), intent(in) :: style
        real(wp) :: marker_size
        
        marker_size = get_marker_size(style)
        
        select case (trim(style))
        case (MARKER_CIRCLE)
            call draw_circle_with_edge_face(this%raster%image_data, this%width, this%height, px, py, marker_size, &
                                           this%raster%marker_edge_r, this%raster%marker_edge_g, this%raster%marker_edge_b, &
                                           this%raster%marker_edge_alpha, this%raster%marker_face_r, this%raster%marker_face_g, &
                                           this%raster%marker_face_b, this%raster%marker_face_alpha)
        case (MARKER_SQUARE)
            call draw_square_with_edge_face(this%raster%image_data, this%width, this%height, px, py, marker_size, &
                                           this%raster%marker_edge_r, this%raster%marker_edge_g, this%raster%marker_edge_b, &
                                           this%raster%marker_edge_alpha, this%raster%marker_face_r, this%raster%marker_face_g, &
                                           this%raster%marker_face_b, this%raster%marker_face_alpha)
        case (MARKER_DIAMOND)
            call draw_diamond_with_edge_face(this%raster%image_data, this%width, this%height, px, py, marker_size, &
                                            this%raster%marker_edge_r, this%raster%marker_edge_g, this%raster%marker_edge_b, &
                                            this%raster%marker_edge_alpha, this%raster%marker_face_r, this%raster%marker_face_g, &
                                            this%raster%marker_face_b, this%raster%marker_face_alpha)
        case (MARKER_CROSS)
            call draw_x_marker(this%raster%image_data, this%width, this%height, px, py, marker_size, &
                               this%raster%marker_edge_r, this%raster%marker_edge_g, this%raster%marker_edge_b)
        end select
    end subroutine draw_raster_marker_by_style

    subroutine raster_set_marker_colors(this, edge_r, edge_g, edge_b, face_r, face_g, face_b)
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: edge_r, edge_g, edge_b
        real(wp), intent(in) :: face_r, face_g, face_b

        this%raster%marker_edge_r = edge_r
        this%raster%marker_edge_g = edge_g
        this%raster%marker_edge_b = edge_b
        this%raster%marker_edge_alpha = 1.0_wp  ! Default to opaque
        this%raster%marker_face_r = face_r
        this%raster%marker_face_g = face_g
        this%raster%marker_face_b = face_b
        this%raster%marker_face_alpha = 1.0_wp  ! Default to opaque
    end subroutine raster_set_marker_colors

    subroutine raster_set_marker_colors_with_alpha(this, edge_r, edge_g, edge_b, edge_alpha, &
                                                   face_r, face_g, face_b, face_alpha)
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: edge_r, edge_g, edge_b, edge_alpha
        real(wp), intent(in) :: face_r, face_g, face_b, face_alpha

        this%raster%marker_edge_r = edge_r
        this%raster%marker_edge_g = edge_g
        this%raster%marker_edge_b = edge_b
        this%raster%marker_edge_alpha = edge_alpha
        this%raster%marker_face_r = face_r
        this%raster%marker_face_g = face_g
        this%raster%marker_face_b = face_b
        this%raster%marker_face_alpha = face_alpha
    end subroutine raster_set_marker_colors_with_alpha

    subroutine raster_fill_quad(this, x_quad, y_quad)
        !! Fill quadrilateral with current color
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: x_quad(4), y_quad(4)
        
        real(wp) :: px_quad(4), py_quad(4)
        integer :: i
        
        ! Transform data coordinates to pixel coordinates (same as raster_draw_line)
        ! This ensures the quad respects plot area margins
        do i = 1, 4
            px_quad(i) = (x_quad(i) - this%x_min) / (this%x_max - this%x_min) * real(this%plot_area%width, wp) + &
                        real(this%plot_area%left, wp)
            py_quad(i) = real(this%plot_area%bottom + this%plot_area%height, wp) - &
                        (y_quad(i) - this%y_min) / (this%y_max - this%y_min) * real(this%plot_area%height, wp)
        end do
        
        call draw_filled_quad_raster(this%raster%image_data, this%width, this%height, &
                                    px_quad, py_quad, &
                                    this%raster%current_r, this%raster%current_g, this%raster%current_b)
    end subroutine raster_fill_quad

    subroutine fill_triangle(image_data, img_w, img_h, x1, y1, x2, y2, x3, y3, r, g, b)
        !! Fill triangle using barycentric coordinates
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: img_w, img_h
        real(wp), intent(in) :: x1, y1, x2, y2, x3, y3
        integer(1), intent(in) :: r, g, b
        
        integer :: x, y, x_min, x_max, y_min, y_max
        real(wp) :: denom, a, b_coord, c
        integer :: pixel_index
        
        ! Find bounding box
        x_min = max(1, int(min(min(x1, x2), x3)))
        x_max = min(img_w, int(max(max(x1, x2), x3)) + 1)
        y_min = max(1, int(min(min(y1, y2), y3)))
        y_max = min(img_h, int(max(max(y1, y2), y3)) + 1)
        
        ! Precompute denominator for barycentric coordinates
        denom = (y2 - y3) * (x1 - x3) + (x3 - x2) * (y1 - y3)
        
        if (abs(denom) < 1e-10_wp) return  ! Degenerate triangle
        
        ! Check each pixel in bounding box
        do y = y_min, y_max
            do x = x_min, x_max
                ! Compute barycentric coordinates
                a = ((y2 - y3) * (real(x, wp) - x3) + (x3 - x2) * (real(y, wp) - y3)) / denom
                b_coord = ((y3 - y1) * (real(x, wp) - x3) + (x1 - x3) * (real(y, wp) - y3)) / denom
                c = 1.0_wp - a - b_coord
                
                ! Check if point is inside triangle
                if (a >= 0.0_wp .and. b_coord >= 0.0_wp .and. c >= 0.0_wp) then
                    pixel_index = 3 * ((y - 1) * img_w + (x - 1)) + 1
                    image_data(pixel_index) = r      ! Red
                    image_data(pixel_index + 1) = g  ! Green
                    image_data(pixel_index + 2) = b  ! Blue
                end if
            end do
        end do
    end subroutine fill_triangle

    subroutine fill_horizontal_line(image_data, img_w, img_h, x1, x2, y, r, g, b)
        !! Fill horizontal line segment
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: img_w, img_h, x1, x2, y
        integer(1), intent(in) :: r, g, b
        
        integer :: x, x_start, x_end, pixel_index
        
        x_start = max(1, min(x1, x2))
        x_end = min(img_w, max(x1, x2))
        
        if (y >= 1 .and. y <= img_h) then
            do x = x_start, x_end
                pixel_index = 3 * ((y - 1) * img_w + (x - 1)) + 1
                image_data(pixel_index) = r      ! Red
                image_data(pixel_index + 1) = g  ! Green  
                image_data(pixel_index + 2) = b  ! Blue
            end do
        end if
    end subroutine fill_horizontal_line

    subroutine raster_draw_arrow(this, x, y, dx, dy, size, style)
        !! Draw arrow head for streamplot arrows in raster backend
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: x, y, dx, dy, size
        character(len=*), intent(in) :: style
        
        ! Simple stub implementation to avoid compiler crash
        ! TODO: Implement proper arrow rendering
        
        ! Mark that arrows have been rendered
        this%has_rendered_arrows = .true.
        this%uses_vector_arrows = .false.
        this%has_triangular_arrows = .true.
    end subroutine raster_draw_arrow

    function raster_get_ascii_output(this) result(output)
        !! Get ASCII output (not applicable for raster backend)
        class(raster_context), intent(in) :: this
        character(len=:), allocatable :: output
        
        output = ""  ! Raster backend doesn't produce ASCII output
    end function raster_get_ascii_output

    function raster_get_width_scale(this) result(scale)
        !! Get width scaling factor for coordinate transformation
        class(raster_context), intent(in) :: this
        real(wp) :: scale
        
        ! Calculate scaling from logical to pixel coordinates
        if (this%width > 0 .and. this%x_max > this%x_min) then
            scale = real(this%width, wp) / (this%x_max - this%x_min)
        else
            scale = 1.0_wp
        end if
    end function raster_get_width_scale

    function raster_get_height_scale(this) result(scale)
        !! Get height scaling factor for coordinate transformation  
        class(raster_context), intent(in) :: this
        real(wp) :: scale
        
        ! Calculate scaling from logical to pixel coordinates
        if (this%height > 0 .and. this%y_max > this%y_min) then
            scale = real(this%height, wp) / (this%y_max - this%y_min)
        else
            scale = 1.0_wp
        end if
    end function raster_get_height_scale

    subroutine raster_fill_heatmap(this, x_grid, y_grid, z_grid, z_min, z_max)
        !! Fill contour plot using scanline method for pixel-by-pixel rendering
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in) :: z_min, z_max
        
        integer :: px, py, nx, ny
        real(wp) :: world_x, world_y, z_value, normalized_z
        real(wp) :: color_rgb(3)
        integer(1) :: r_byte, g_byte, b_byte
        integer :: offset
        real(wp) :: dx, dy
        real(wp) :: x_min, x_max, y_min, y_max
        
        nx = size(x_grid)
        ny = size(y_grid)
        
        ! Validate input dimensions
        if (size(z_grid, 1) /= nx .or. size(z_grid, 2) /= ny) return
        if (abs(z_max - z_min) < 1e-10_wp) return
        
        ! Get data bounds
        x_min = minval(x_grid)
        x_max = maxval(x_grid)
        y_min = minval(y_grid)
        y_max = maxval(y_grid)
        
        ! Grid spacing for interpolation
        if (nx > 1) then
            dx = (x_max - x_min) / real(nx - 1, wp)
        else
            dx = 1.0_wp
        end if
        
        if (ny > 1) then
            dy = (y_max - y_min) / real(ny - 1, wp) 
        else
            dy = 1.0_wp
        end if
        
        ! Scanline rendering: iterate over all pixels in plot area
        do py = this%plot_area%bottom, this%plot_area%bottom + this%plot_area%height - 1
            do px = this%plot_area%left, this%plot_area%left + this%plot_area%width - 1
                
                ! Map pixel to world coordinates
                world_x = this%x_min + (real(px - this%plot_area%left, wp) / &
                         real(this%plot_area%width - 1, wp)) * (this%x_max - this%x_min)
                         
                world_y = this%y_max - (real(py - this%plot_area%bottom, wp) / &
                         real(this%plot_area%height - 1, wp)) * (this%y_max - this%y_min)
                
                ! Interpolate Z value at this pixel
                call interpolate_z_bilinear(x_grid, y_grid, z_grid, world_x, world_y, z_value)
                
                ! Convert Z value to color using default colormap (viridis)
                call colormap_value_to_color(z_value, z_min, z_max, 'viridis', color_rgb)
                
                ! Convert to bytes
                r_byte = color_to_byte(color_rgb(1))
                g_byte = color_to_byte(color_rgb(2))
                b_byte = color_to_byte(color_rgb(3))
                
                ! Set pixel directly in image data (RGB format)
                if (px >= 1 .and. px <= this%width .and. py >= 1 .and. py <= this%height) then
                    offset = 3 * ((py - 1) * this%width + (px - 1)) + 1
                    if (offset >= 1 .and. offset + 2 <= size(this%raster%image_data)) then
                        this%raster%image_data(offset) = r_byte
                        this%raster%image_data(offset + 1) = g_byte
                        this%raster%image_data(offset + 2) = b_byte
                    end if
                end if
            end do
        end do
    end subroutine raster_fill_heatmap

    subroutine raster_render_legend_specialized(this, legend, legend_x, legend_y)
        !! Render legend using standard algorithm for PNG
        use fortplot_legend, only: legend_t
        class(raster_context), intent(inout) :: this
        type(legend_t), intent(in) :: legend
        real(wp), intent(in) :: legend_x, legend_y
        
        ! No-op: legend rendering handled by fortplot_legend module
        ! This method exists only for polymorphic compatibility
    end subroutine raster_render_legend_specialized

    subroutine raster_calculate_legend_dimensions(this, legend, legend_width, legend_height)
        !! Calculate standard legend dimensions for PNG
        use fortplot_legend, only: legend_t
        class(raster_context), intent(in) :: this
        type(legend_t), intent(in) :: legend
        real(wp), intent(out) :: legend_width, legend_height
        
        ! Use standard dimension calculation for PNG backend
        legend_width = 80.0_wp   ! Standard legend width
        legend_height = real(legend%num_entries * 20 + 10, wp)  ! 20 pixels per entry + margins
    end subroutine raster_calculate_legend_dimensions

    subroutine raster_set_legend_border_width(this)
        !! Set thin border width for PNG legend
        class(raster_context), intent(inout) :: this
        
        call this%set_line_width(0.1_wp)  ! Thin border for PNG like axes
    end subroutine raster_set_legend_border_width

    subroutine raster_calculate_legend_position(this, legend, x, y)
        !! Calculate standard legend position for PNG using plot coordinates
        use fortplot_legend, only: legend_t
        class(raster_context), intent(in) :: this
        type(legend_t), intent(in) :: legend
        real(wp), intent(out) :: x, y
        
        ! No-op: position calculation handled by fortplot_legend module
        ! This method exists only for polymorphic compatibility
        x = 0.0_wp
        y = 0.0_wp
    end subroutine raster_calculate_legend_position

    subroutine raster_extract_rgb_data(this, width, height, rgb_data)
        !! Extract RGB data from PNG backend
        use, intrinsic :: iso_fortran_env, only: real64
        class(raster_context), intent(in) :: this
        integer, intent(in) :: width, height
        real(real64), intent(out) :: rgb_data(width, height, 3)
        integer :: x, y, idx_base
        
        do y = 1, height
            do x = 1, width
                ! Calculate 1D index for packed RGB data (width * height * 3 array)
                ! Format: [R1, G1, B1, R2, G2, B2, ...]
                idx_base = ((y-1) * width + (x-1)) * 3
                
                ! Extract RGB values (normalized to 0-1)
                rgb_data(x, y, 1) = real(this%raster%image_data(idx_base + 1), real64) / 255.0_real64
                rgb_data(x, y, 2) = real(this%raster%image_data(idx_base + 2), real64) / 255.0_real64
                rgb_data(x, y, 3) = real(this%raster%image_data(idx_base + 3), real64) / 255.0_real64
            end do
        end do
    end subroutine raster_extract_rgb_data

    subroutine raster_get_png_data(this, width, height, png_data, status)
        !! Raster context doesn't generate PNG data - only PNG context does
        class(raster_context), intent(in) :: this
        integer, intent(in) :: width, height
        integer(1), allocatable, intent(out) :: png_data(:)
        integer, intent(out) :: status
        
        ! Raster context doesn't generate PNG data
        ! This should be overridden by PNG context
        allocate(png_data(0))
        status = -1
    end subroutine raster_get_png_data

    subroutine raster_prepare_3d_data(this, plots)
        !! Prepare 3D data for PNG backend (no-op - PNG doesn't use 3D data)
        use fortplot_plot_data, only: plot_data_t
        class(raster_context), intent(inout) :: this
        type(plot_data_t), intent(in) :: plots(:)
        
        ! PNG backend doesn't need 3D data preparation - no-op
    end subroutine raster_prepare_3d_data

    subroutine raster_render_ylabel(this, ylabel)
        !! Render rotated Y-axis label for PNG backend
        use fortplot_text, only: calculate_text_width, calculate_text_height
        class(raster_context), intent(inout) :: this
        character(len=*), intent(in) :: ylabel
        
        integer :: text_width, text_height
        integer :: rotated_width, rotated_height
        integer :: x_pos, y_pos
        integer(1), allocatable :: text_bitmap(:,:,:), rotated_bitmap(:,:,:)
        
        ! Calculate text dimensions
        text_width = calculate_text_width(ylabel)
        text_height = calculate_text_height(ylabel)
        
        ! Allocate bitmap for horizontal text
        allocate(text_bitmap(text_width, text_height, 3))
        text_bitmap = -1_1  ! Initialize to white
        
        ! Render text horizontally to bitmap (at origin)
        call render_text_to_bitmap(text_bitmap, text_width, text_height, 0, 0, ylabel)
        
        ! Allocate rotated bitmap (dimensions swapped for 90° rotation)
        rotated_width = text_height
        rotated_height = text_width
        allocate(rotated_bitmap(rotated_width, rotated_height, 3))
        
        ! Rotate the text 90 degrees counter-clockwise
        call rotate_bitmap_90_ccw(text_bitmap, rotated_bitmap, text_width, text_height)
        
        ! Calculate position for rotated text (left of plot area, centered vertically)
        x_pos = this%plot_area%left - 40 - rotated_width / 2
        y_pos = this%plot_area%bottom + this%plot_area%height / 2 - rotated_height / 2
        
        ! Composite the rotated text onto the main raster
        call composite_bitmap_to_raster(this%raster%image_data, this%raster%width, &
                                       this%raster%height, rotated_bitmap, &
                                       rotated_width, rotated_height, x_pos, y_pos)
        
        ! Clean up
        deallocate(text_bitmap)
        deallocate(rotated_bitmap)
    end subroutine raster_render_ylabel

    subroutine raster_draw_axes_and_labels(this, xscale, yscale, symlog_threshold, &
                                          x_min, x_max, y_min, y_max, &
                                          title, xlabel, ylabel, &
                                          z_min, z_max, has_3d_plots)
        !! Draw axes and labels for raster backends
        use fortplot_axes, only: compute_scale_ticks, format_tick_label, MAX_TICKS
        use fortplot_text, only: calculate_text_width, calculate_text_height
        class(raster_context), intent(inout) :: this
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=:), allocatable, intent(in), optional :: title, xlabel, ylabel
        real(wp), intent(in), optional :: z_min, z_max
        logical, intent(in) :: has_3d_plots
        
        real(wp) :: label_x, label_y
        real(wp) :: x_tick_positions(MAX_TICKS), y_tick_positions(MAX_TICKS)
        integer :: num_x_ticks, num_y_ticks, i
        character(len=50) :: tick_label
        real(wp) :: tick_x, tick_y
        integer :: tick_length  ! Tick length in pixels
        integer :: px, py, text_width, text_height
        real(wp) :: line_r, line_g, line_b
        integer(1) :: text_r, text_g, text_b
        real(wp) :: dummy_pattern(1)  ! Dummy pattern for solid lines
        real(wp) :: pattern_dist  ! Pattern distance (mutable)
        character(len=500) :: processed_text, escaped_text
        integer :: processed_len
        
        ! Set color to black for axes and text
        call this%color(0.0_wp, 0.0_wp, 0.0_wp)
        line_r = 0.0_wp; line_g = 0.0_wp; line_b = 0.0_wp  ! Black color for lines
        text_r = 0; text_g = 0; text_b = 0  ! Black color for text
        
        ! Draw axes
        call this%line(x_min, y_min, x_max, y_min)
        call this%line(x_min, y_min, x_min, y_max)
        
        ! Generate and draw tick marks and labels
        tick_length = 5  ! Tick length in pixels
        
        ! X-axis ticks
        call compute_scale_ticks(xscale, x_min, x_max, symlog_threshold, x_tick_positions, num_x_ticks)
        do i = 1, num_x_ticks
            tick_x = x_tick_positions(i)
            ! Transform tick position to pixel coordinates
            px = int((tick_x - x_min) / (x_max - x_min) * real(this%plot_area%width, wp) + real(this%plot_area%left, wp))
            py = this%plot_area%bottom + this%plot_area%height  ! Bottom of plot area
            
            ! Draw tick mark down from axis
            dummy_pattern = 0.0_wp
            pattern_dist = 0.0_wp
            call draw_styled_line(this%raster%image_data, this%width, this%height, &
                                 real(px, wp), real(py, wp), real(px, wp), real(py + tick_length, wp), &
                                 line_r, line_g, line_b, 1.0_wp, 'solid', dummy_pattern, 0, 0.0_wp, pattern_dist)
            
            ! Draw tick label below tick mark
            tick_label = format_tick_label(tick_x, xscale)
            call process_latex_in_text(trim(tick_label), processed_text, processed_len)
            call escape_unicode_for_raster(processed_text(1:processed_len), escaped_text)
            text_width = calculate_text_width(trim(escaped_text))
            ! Center the label horizontally under the tick
            call render_text_to_image(this%raster%image_data, this%width, this%height, &
                                    px - text_width/2, py + tick_length + 5, trim(escaped_text), text_r, text_g, text_b)
        end do
        
        ! Y-axis ticks
        call compute_scale_ticks(yscale, y_min, y_max, symlog_threshold, y_tick_positions, num_y_ticks)
        do i = 1, num_y_ticks
            tick_y = y_tick_positions(i)
            ! Transform tick position to pixel coordinates
            px = this%plot_area%left  ! Left edge of plot area
            py = int(real(this%plot_area%bottom + this%plot_area%height, wp) - &
                    (tick_y - y_min) / (y_max - y_min) * real(this%plot_area%height, wp))
            
            ! Draw tick mark to the left from axis
            dummy_pattern = 0.0_wp
            pattern_dist = 0.0_wp
            call draw_styled_line(this%raster%image_data, this%width, this%height, &
                                 real(px - tick_length, wp), real(py, wp), real(px, wp), real(py, wp), &
                                 line_r, line_g, line_b, 1.0_wp, 'solid', dummy_pattern, 0, 0.0_wp, pattern_dist)
            
            ! Draw tick label to the left of tick mark
            tick_label = format_tick_label(tick_y, yscale)
            call process_latex_in_text(trim(tick_label), processed_text, processed_len)
            call escape_unicode_for_raster(processed_text(1:processed_len), escaped_text)
            text_width = calculate_text_width(trim(escaped_text))
            text_height = calculate_text_height(trim(escaped_text))
            ! Right-align the label to the left of the tick
            call render_text_to_image(this%raster%image_data, this%width, this%height, &
                px - tick_length - text_width - 5, py - text_height/2, &
                trim(escaped_text), text_r, text_g, text_b)
        end do
        
        ! Draw title at top if present
        if (present(title)) then
            if (allocated(title)) then
                ! Position title centered horizontally over plot area (like matplotlib)
                ! Use plot area center instead of data coordinate center for proper positioning
                call render_title_centered(this, title)
            end if
        end if
        
        ! Draw xlabel centered below x-axis (below tick labels)
        if (present(xlabel)) then
            if (allocated(xlabel)) then
                call process_latex_in_text(xlabel, processed_text, processed_len)
                call escape_unicode_for_raster(processed_text(1:processed_len), escaped_text)
                text_width = calculate_text_width(trim(escaped_text))
                ! Center horizontally in plot area, position below tick labels
                px = this%plot_area%left + this%plot_area%width / 2 - text_width / 2
                py = this%plot_area%bottom + this%plot_area%height + 30  ! 30 pixels below plot area
                call render_text_to_image(this%raster%image_data, this%width, this%height, &
                                        px, py, trim(escaped_text), text_r, text_g, text_b)
            end if
        end if
        
        ! Draw ylabel to the left of y-axis (rotated would be better, but for now horizontal)
        if (present(ylabel)) then
            if (allocated(ylabel)) then
                call process_latex_in_text(ylabel, processed_text, processed_len)
                call escape_unicode_for_raster(processed_text(1:processed_len), escaped_text)
                text_width = calculate_text_width(trim(escaped_text))
                text_height = calculate_text_height(trim(escaped_text))
                ! Position to the left of plot area, centered vertically
                px = 10  ! 10 pixels from left edge
                py = this%plot_area%bottom + this%plot_area%height / 2 - text_height / 2
                call render_text_to_image(this%raster%image_data, this%width, this%height, &
                                        px, py, trim(escaped_text), text_r, text_g, text_b)
            end if
        end if
    end subroutine raster_draw_axes_and_labels

    subroutine raster_save_coordinates(this, x_min, x_max, y_min, y_max)
        !! Save current coordinate system
        class(raster_context), intent(in) :: this
        real(wp), intent(out) :: x_min, x_max, y_min, y_max
        
        x_min = this%x_min
        x_max = this%x_max
        y_min = this%y_min
        y_max = this%y_max
    end subroutine raster_save_coordinates

    subroutine raster_set_coordinates(this, x_min, x_max, y_min, y_max)
        !! Set coordinate system
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        
        this%x_min = x_min
        this%x_max = x_max
        this%y_min = y_min
        this%y_max = y_max
    end subroutine raster_set_coordinates

    subroutine raster_render_axes(this, title_text, xlabel_text, ylabel_text)
        !! Render axes for raster context (stub implementation)
        class(raster_context), intent(inout) :: this
        character(len=*), intent(in), optional :: title_text, xlabel_text, ylabel_text
        
        ! Raster axes are rendered as part of draw_axes_and_labels_backend
        ! This is a stub to satisfy the interface
    end subroutine raster_render_axes

    subroutine render_title_centered(this, title_text)
        !! Render title centered horizontally over plot area (matplotlib-style positioning)
        class(raster_context), intent(inout) :: this
        character(len=*), intent(in) :: title_text
        
        real(wp) :: title_px, title_py
        integer(1) :: r, g, b
        character(len=500) :: processed_text, escaped_text
        integer :: processed_len
        
        ! Process LaTeX commands and Unicode
        call process_latex_in_text(title_text, processed_text, processed_len)
        call escape_unicode_for_raster(processed_text(1:processed_len), escaped_text)
        
        ! Calculate title position centered over plot area
        ! X position: center of plot area horizontally
        title_px = real(this%plot_area%left + this%plot_area%width / 2, wp)
        
        ! Y position: above plot area (like matplotlib)  
        ! Place title approximately 30 pixels above the plot area
        title_py = real(this%plot_area%bottom - 30, wp)
        
        ! Get current color and render title directly in pixel coordinates
        call this%raster%get_color_bytes(r, g, b)
        call render_text_to_image(this%raster%image_data, this%width, this%height, &
                                 int(title_px), int(title_py), trim(escaped_text), r, g, b)
    end subroutine render_title_centered

end module fortplot_raster
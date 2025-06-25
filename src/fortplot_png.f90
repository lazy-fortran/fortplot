module fortplot_png
    use iso_c_binding
    use fortplot_context
    use fortplot_text
    use fortplot_raster, only: raster_image_t, create_raster_image, initialize_white_background, &
                              color_to_byte, draw_line_distance_aa, blend_pixel, composite_image
    use fortplot_margins, only: plot_margins_t, plot_area_t, calculate_plot_area, get_axis_tick_positions
    use fortplot_ticks, only: generate_scale_aware_tick_labels
    use fortplot_label_positioning, only: calculate_x_label_position, calculate_y_label_position, &
                                         calculate_x_tick_label_position, calculate_y_tick_label_position, &
                                         calculate_x_axis_label_position, calculate_y_axis_label_position
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: png_context, create_png_canvas, draw_axes_and_labels, draw_rotated_ylabel_png

    ! PNG plotting context
    type, extends(plot_context) :: png_context
        type(raster_image_t) :: raster
        ! Plot area calculations (using common margin functionality)
        type(plot_margins_t) :: margins
        type(plot_area_t) :: plot_area
    contains
        procedure :: line => png_draw_line
        procedure :: color => png_set_color
        procedure :: text => png_draw_text
        procedure :: save => png_finalize
        procedure :: set_line_width => png_set_line_width
    end type png_context

contains

    function create_png_canvas(width, height) result(ctx)
        integer, intent(in) :: width, height
        type(png_context) :: ctx

        call setup_canvas(ctx, width, height)

        ctx%raster = create_raster_image(width, height)

        ! Set up matplotlib-style margins using common module
        ctx%margins = plot_margins_t()  ! Use defaults
        call calculate_plot_area(width, height, ctx%margins, ctx%plot_area)
    end function create_png_canvas

    subroutine png_draw_line(this, x1, y1, x2, y2)
        class(png_context), intent(inout) :: this
        real(wp), intent(in) :: x1, y1, x2, y2
        real(wp) :: px1, py1, px2, py2
        integer(1) :: r, g, b

        ! Transform coordinates to plot area (like matplotlib)
        ! Note: PNG Y=0 at top, so we need to flip Y coordinates
        px1 = (x1 - this%x_min) / (this%x_max - this%x_min) * real(this%plot_area%width, wp) + real(this%plot_area%left, wp)
        py1 = real(this%plot_area%bottom + this%plot_area%height, wp) - &
              (y1 - this%y_min) / (this%y_max - this%y_min) * real(this%plot_area%height, wp)
        px2 = (x2 - this%x_min) / (this%x_max - this%x_min) * real(this%plot_area%width, wp) + real(this%plot_area%left, wp)
        py2 = real(this%plot_area%bottom + this%plot_area%height, wp) - &
              (y2 - this%y_min) / (this%y_max - this%y_min) * real(this%plot_area%height, wp)

        call this%raster%get_color_bytes(r, g, b)

        call draw_line_distance_aa(this%raster%image_data, this%width, this%height, px1, py1, px2, py2, r, g, b, this%raster%current_line_width)
    end subroutine png_draw_line

    subroutine png_set_color(this, r, g, b)
        class(png_context), intent(inout) :: this
        real(wp), intent(in) :: r, g, b

        call this%raster%set_color(r, g, b)
    end subroutine png_set_color

    subroutine png_set_line_width(this, width)
        !! Set line width for PNG drawing with automatic scaling for pixel rendering
        class(png_context), intent(inout) :: this
        real(wp), intent(in) :: width

        ! PNG needs specific line widths due to pixel-based rendering
        ! Map common width values to appropriate PNG pixel thickness
        if (abs(width - 2.0_wp) < 1e-6_wp) then
            ! Plot data lines: use 0.5 for main plot visibility
            this%raster%current_line_width = 0.5_wp
        else
            ! Axes and other elements: use 0.1 for fine lines
            this%raster%current_line_width = 0.1_wp
        end if
    end subroutine png_set_line_width

    subroutine png_draw_text(this, x, y, text)
        class(png_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        real(wp) :: px, py
        integer(1) :: r, g, b

        ! Transform coordinates to plot area (like matplotlib)
        ! Note: PNG Y=0 at top, so we need to flip Y coordinates
        px = (x - this%x_min) / (this%x_max - this%x_min) * real(this%plot_area%width, wp) + real(this%plot_area%left, wp)
        py = real(this%plot_area%bottom + this%plot_area%height, wp) - &
             (y - this%y_min) / (this%y_max - this%y_min) * real(this%plot_area%height, wp)

        call this%raster%get_color_bytes(r, g, b)
        call render_text_to_image(this%raster%image_data, this%width, this%height, &
                                 int(px), int(py), text, r, g, b)
    end subroutine png_draw_text

    subroutine png_finalize(this, filename)
        class(png_context), intent(inout) :: this
        character(len=*), intent(in) :: filename

        call write_png_file(filename, this%width, this%height, this%raster%image_data)
    end subroutine png_finalize







    ! PNG file writing functionality
    subroutine write_png_file(filename, width, height, image_data)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: width, height
        integer(1), intent(in) :: image_data(:)

        integer(1) :: png_signature(8) = &
            [int(-119,1), int(80,1), int(78,1), int(71,1), int(13,1), int(10,1), int(26,1), int(10,1)]
        integer, parameter :: bit_depth = 8, color_type = 2
        integer :: png_unit = 10
        integer(1), allocatable, target :: compressed_data(:)
        integer(c_long), target :: compressed_size
        integer :: status, data_size

        open(unit=png_unit, file=filename, access='stream', form='unformatted', status='replace')

        write(png_unit) png_signature
        call write_ihdr_chunk(png_unit, width, height, bit_depth, color_type)

        data_size = size(image_data)
        compressed_size = int(data_size * 1.1 + 12, c_long)
        allocate(compressed_data(compressed_size))

        status = compress_data(image_data, data_size, compressed_data, compressed_size)
        if (status /= 0) then
            print *, "Compression failed with status:", status
            stop
        end if

        call write_idat_chunk(png_unit, compressed_data, int(compressed_size))
        call write_iend_chunk(png_unit)

        close(png_unit)
        deallocate(compressed_data)

        print *, "PNG file '", trim(filename), "' created successfully!"
    end subroutine write_png_file

    ! PNG chunk writing functions (simplified versions)
    subroutine write_ihdr_chunk(unit, w, h, bd, ct)
        integer, intent(in) :: unit, w, h, bd, ct
        integer(1) :: ihdr_data(13)
        integer :: w_be, h_be

        w_be = to_big_endian(w)
        h_be = to_big_endian(h)

        ihdr_data(1:4) = transfer(w_be, ihdr_data(1:4))
        ihdr_data(5:8) = transfer(h_be, ihdr_data(5:8))
        ihdr_data(9) = int(bd, 1)
        ihdr_data(10) = int(ct, 1)
        ihdr_data(11) = 0_1
        ihdr_data(12) = 0_1
        ihdr_data(13) = 0_1

        call write_chunk(unit, "IHDR", ihdr_data, 13)
    end subroutine write_ihdr_chunk

    subroutine write_idat_chunk(unit, data, size)
        integer, intent(in) :: unit, size
        integer(1), intent(in) :: data(size)
        call write_chunk(unit, "IDAT", data, size)
    end subroutine write_idat_chunk

    subroutine write_iend_chunk(unit)
        integer, intent(in) :: unit
        integer(1) :: dummy(1)
        call write_chunk(unit, "IEND", dummy, 0)
    end subroutine write_iend_chunk

    subroutine write_chunk(unit, chunk_type, chunk_data, data_size)
        integer, intent(in) :: unit
        character(len=4), intent(in) :: chunk_type
        integer(1), intent(in) :: chunk_data(*)
        integer, intent(in) :: data_size

        integer :: length_be
        integer(1) :: type_bytes(4)
        integer(1), allocatable, target :: full_data(:)
        integer(c_int32_t) :: crc_value
        integer :: crc_be
        integer :: i

        length_be = to_big_endian(data_size)

        do i = 1, 4
            type_bytes(i) = int(iachar(chunk_type(i:i)), 1)
        end do

        allocate(full_data(4 + data_size))
        full_data(1:4) = type_bytes
        if (data_size > 0) then
            full_data(5:4+data_size) = chunk_data(1:data_size)
        end if

        crc_value = calculate_crc32(full_data, 4 + data_size)
        crc_be = to_big_endian(int(crc_value))

        write(unit) length_be
        write(unit) type_bytes
        if (data_size > 0) then
            write(unit) chunk_data(1:data_size)
        end if
        write(unit) crc_be

        deallocate(full_data)
    end subroutine write_chunk

    function to_big_endian(value) result(be_value)
        integer, intent(in) :: value
        integer :: be_value
        integer(1) :: bytes(4)

        bytes(1) = int(ibits(value, 24, 8), 1)
        bytes(2) = int(ibits(value, 16, 8), 1)
        bytes(3) = int(ibits(value, 8, 8), 1)
        bytes(4) = int(ibits(value, 0, 8), 1)

        be_value = transfer(bytes, be_value)
    end function to_big_endian

    function compress_data(source, source_len, dest, dest_len) result(status)
        integer(1), target, intent(in) :: source(*)
        integer, intent(in) :: source_len
        integer(1), target, intent(out) :: dest(*)
        integer(c_long), target, intent(inout) :: dest_len
        integer :: status

        interface
            function compress(dest, destLen, source, sourceLen) bind(C, name="compress")
                import :: c_ptr, c_long, c_int
                type(c_ptr), value :: dest
                type(c_ptr), value :: destLen
                type(c_ptr), value :: source
                integer(c_long), value :: sourceLen
                integer(c_int) :: compress
            end function compress
        end interface

        status = compress(c_loc(dest), c_loc(dest_len), c_loc(source), int(source_len, c_long))
    end function compress_data

    function calculate_crc32(data, len) result(crc)
        integer(1), target, intent(in) :: data(*)
        integer, intent(in) :: len
        integer(c_int32_t) :: crc

        interface
            function crc32(crc, buf, len) bind(C, name="crc32")
                import :: c_int32_t, c_ptr, c_int
                integer(c_int32_t), value :: crc
                type(c_ptr), value :: buf
                integer(c_int), value :: len
                integer(c_int32_t) :: crc32
            end function crc32
        end interface

        crc = crc32(0_c_int32_t, c_loc(data), int(len, c_int))
    end function calculate_crc32

    subroutine draw_axes_and_labels(ctx, xscale, yscale, symlog_threshold, &
                                   x_min_orig, x_max_orig, y_min_orig, y_max_orig, &
                                   title, xlabel, ylabel)
        !! Draw plot axes and frame with scale-aware tick generation
        type(png_context), intent(inout) :: ctx
        character(len=*), intent(in), optional :: xscale, yscale
        real(wp), intent(in), optional :: symlog_threshold
        real(wp), intent(in), optional :: x_min_orig, x_max_orig, y_min_orig, y_max_orig
        character(len=*), intent(in), optional :: title, xlabel, ylabel
        integer :: i
        real(wp) :: x_positions(10), y_positions(10)
        integer :: num_x, num_y
        character(len=20) :: x_labels(10), y_labels(10)

        ! Set color to black for axes
        call ctx%raster%set_color(0.0_wp, 0.0_wp, 0.0_wp)

        ! Draw plot frame using common functionality
        call draw_png_frame(ctx)

        ! Draw tick marks and labels with scale-aware generation
        call get_axis_tick_positions(ctx%plot_area, 5, 5, x_positions, y_positions, num_x, num_y)

        ! Use original coordinates for tick generation if provided, otherwise use backend coordinates
        if (present(x_min_orig) .and. present(x_max_orig)) then
            call generate_scale_aware_tick_labels(x_min_orig, x_max_orig, num_x, x_labels, xscale, symlog_threshold)
        else
            call generate_scale_aware_tick_labels(ctx%x_min, ctx%x_max, num_x, x_labels, xscale, symlog_threshold)
        end if

        if (present(y_min_orig) .and. present(y_max_orig)) then
            call generate_scale_aware_tick_labels(y_min_orig, y_max_orig, num_y, y_labels, yscale, symlog_threshold)
        else
            call generate_scale_aware_tick_labels(ctx%y_min, ctx%y_max, num_y, y_labels, yscale, symlog_threshold)
        end if
        call draw_png_tick_marks(ctx, x_positions, y_positions, num_x, num_y)
        call draw_png_tick_labels(ctx, x_positions, y_positions, x_labels, y_labels, num_x, num_y)

        ! Draw title and axis labels
        call draw_png_title_and_labels(ctx, title, xlabel, ylabel)
    end subroutine draw_axes_and_labels

    subroutine draw_png_frame(ctx)
        !! Draw the plot frame for PNG backend
        type(png_context), intent(inout) :: ctx

        ! Draw plot frame (PNG coordinates with Y=0 at top)
        call draw_line_distance_aa(ctx%raster%image_data, ctx%width, ctx%height, &
                                  real(ctx%plot_area%left, wp), &
                                  real(ctx%plot_area%bottom + ctx%plot_area%height, wp), &
                                  real(ctx%plot_area%left + ctx%plot_area%width, wp), &
                                  real(ctx%plot_area%bottom + ctx%plot_area%height, wp), &
                                  0_1, 0_1, 0_1, 0.1_wp)  ! Bottom edge (top in PNG)

        call draw_line_distance_aa(ctx%raster%image_data, ctx%width, ctx%height, &
                                  real(ctx%plot_area%left, wp), &
                                  real(ctx%plot_area%bottom, wp), &
                                  real(ctx%plot_area%left, wp), &
                                  real(ctx%plot_area%bottom + ctx%plot_area%height, wp), &
                                  0_1, 0_1, 0_1, 0.1_wp)  ! Left edge

        call draw_line_distance_aa(ctx%raster%image_data, ctx%width, ctx%height, &
                                  real(ctx%plot_area%left + ctx%plot_area%width, wp), &
                                  real(ctx%plot_area%bottom, wp), &
                                  real(ctx%plot_area%left + ctx%plot_area%width, wp), &
                                  real(ctx%plot_area%bottom + ctx%plot_area%height, wp), &
                                  0_1, 0_1, 0_1, 0.1_wp)  ! Right edge

        call draw_line_distance_aa(ctx%raster%image_data, ctx%width, ctx%height, &
                                  real(ctx%plot_area%left, wp), &
                                  real(ctx%plot_area%bottom, wp), &
                                  real(ctx%plot_area%left + ctx%plot_area%width, wp), &
                                  real(ctx%plot_area%bottom, wp), &
                                  0_1, 0_1, 0_1, 0.1_wp)  ! Top edge (bottom in PNG)
    end subroutine draw_png_frame

    subroutine draw_png_tick_marks(ctx, x_positions, y_positions, num_x, num_y)
        !! Draw tick marks for PNG backend
        type(png_context), intent(inout) :: ctx
        real(wp), intent(in) :: x_positions(:), y_positions(:)
        integer, intent(in) :: num_x, num_y
        integer :: i

        ! Draw X-axis tick marks (at bottom of plot - top in PNG coords)
        do i = 1, num_x
            call draw_line_distance_aa(ctx%raster%image_data, ctx%width, ctx%height, &
                                      x_positions(i), &
                                      real(ctx%plot_area%bottom + ctx%plot_area%height, wp), &
                                      x_positions(i), &
                                      real(ctx%plot_area%bottom + ctx%plot_area%height + 5, wp), &
                                      0_1, 0_1, 0_1, 0.1_wp)
        end do

        ! Draw Y-axis tick marks (at left of plot)
        do i = 1, num_y
            call draw_line_distance_aa(ctx%raster%image_data, ctx%width, ctx%height, &
                                      real(ctx%plot_area%left, wp), y_positions(i), &
                                      real(ctx%plot_area%left - 5, wp), y_positions(i), &
                                      0_1, 0_1, 0_1, 0.1_wp)
        end do
    end subroutine draw_png_tick_marks

    subroutine draw_png_tick_labels(ctx, x_positions, y_positions, x_labels, y_labels, num_x, num_y)
        !! Draw tick labels for PNG backend like matplotlib
        type(png_context), intent(inout) :: ctx
        real(wp), intent(in) :: x_positions(:), y_positions(:)
        character(len=*), intent(in) :: x_labels(:), y_labels(:)
        integer, intent(in) :: num_x, num_y
        integer :: i
        real(wp) :: label_x, label_y

        ! Draw X-axis tick labels with proper spacing and center alignment
        do i = 1, num_x
            call calculate_x_tick_label_position(x_positions(i), &
                                               real(ctx%plot_area%bottom + ctx%plot_area%height, wp), &
                                               trim(x_labels(i)), label_x, label_y)
            call render_text_to_image(ctx%raster%image_data, ctx%width, ctx%height, &
                                     int(label_x), int(label_y), trim(x_labels(i)), &
                                     0_1, 0_1, 0_1)  ! Black text
        end do

        ! Draw Y-axis tick labels with right alignment and proper spacing
        do i = 1, num_y
            call calculate_y_tick_label_position(y_positions(i), real(ctx%plot_area%left, wp), &
                                               trim(y_labels(i)), label_x, label_y)
            call render_text_to_image(ctx%raster%image_data, ctx%width, ctx%height, &
                                     int(label_x), int(label_y), trim(y_labels(i)), &
                                     0_1, 0_1, 0_1)  ! Black text
        end do
    end subroutine draw_png_tick_labels

    subroutine draw_png_title_and_labels(ctx, title, xlabel, ylabel)
        !! Draw figure title and axis labels
        type(png_context), intent(inout) :: ctx
        character(len=*), intent(in), optional :: title, xlabel, ylabel
        real(wp) :: label_x, label_y, text_width

        ! Draw title at top center with proper margin (matplotlib-style)
        if (present(title)) then
            ! Center horizontally across the entire figure width (like matplotlib)
            text_width = real(calculate_text_width(trim(title)), wp)
            if (text_width <= 0.0_wp) then
                text_width = real(len_trim(title) * 8, wp)  ! 8 pixels per char for 12pt font
            end if
            label_x = real(ctx%width, wp) / 2.0_wp - text_width / 2.0_wp
            ! Position title in the top margin area (matplotlib uses ~25px from top)
            label_y = 25.0_wp
            call render_text_to_image(ctx%raster%image_data, ctx%width, ctx%height, &
                                     int(label_x), int(label_y), trim(title), &
                                     0_1, 0_1, 0_1)  ! Black text, normal weight (non-bold)
        end if

        ! Draw X-axis label using proper axis label positioning
        if (present(xlabel)) then
            call calculate_x_axis_label_position(real(ctx%plot_area%left + ctx%plot_area%width / 2, wp), &
                                               real(ctx%plot_area%bottom + ctx%plot_area%height, wp), &
                                               trim(xlabel), label_x, label_y)
            call render_text_to_image(ctx%raster%image_data, ctx%width, ctx%height, &
                                     int(label_x), int(label_y), trim(xlabel), &
                                     0_1, 0_1, 0_1)  ! Black text
        end if

        ! Draw Y-axis label rotated 90 degrees using render-then-rotate approach
        if (present(ylabel)) then
            call draw_rotated_ylabel_png(ctx, ylabel)
        end if
    end subroutine draw_png_title_and_labels

    subroutine draw_rotated_ylabel_png(ctx, text)
        !! Draw Y-axis label by rendering text then rotating the image 90 degrees
        type(png_context), intent(inout) :: ctx
        character(len=*), intent(in) :: text
        real(wp) :: label_x, label_y
        integer :: text_width, text_height, padding
        integer :: buf_width, buf_height, i, j, src_idx, dst_idx
        integer(1), allocatable :: text_buffer(:), rotated_buffer(:)
        
        ! Calculate text dimensions and position
        text_width = calculate_text_width(trim(text))
        text_height = calculate_text_height(trim(text))
        
        padding = 2
        buf_width = text_width + 2 * padding  
        buf_height = text_height + 2 * padding
        
        ! Create text buffer and render text normally
        allocate(text_buffer(buf_height * (1 + buf_width * 3)))
        call initialize_white_background(text_buffer, buf_width, buf_height)
        call render_text_to_image(text_buffer, buf_width, buf_height, &
                                 padding, padding, trim(text), &
                                 0_1, 0_1, 0_1)  ! Black text
        
        ! Create rotated buffer (dimensions swapped for 90-degree rotation)
        allocate(rotated_buffer(buf_width * (1 + buf_height * 3)))
        call initialize_white_background(rotated_buffer, buf_height, buf_width)
        
        ! Rotate image 90 degrees clockwise: (x,y) -> (y, width-1-x)
        do j = 1, buf_height
            do i = 1, buf_width
                src_idx = (j - 1) * (1 + buf_width * 3) + 1 + (i - 1) * 3 + 1
                dst_idx = (i - 1) * (1 + buf_height * 3) + 1 + (buf_height - j) * 3 + 1
                
                ! Copy RGB values
                rotated_buffer(dst_idx:dst_idx+2) = text_buffer(src_idx:src_idx+2)
            end do
        end do
        
        ! Calculate position and composite rotated text onto main image
        call calculate_y_axis_label_position(real(ctx%plot_area%bottom + ctx%plot_area%height / 2, wp), &
                                           real(ctx%plot_area%left, wp), text, label_x, label_y)
        
        call composite_image(ctx%raster%image_data, ctx%width, ctx%height, &
                            rotated_buffer, buf_height, buf_width, &
                            int(label_x - buf_height/2), int(label_y - buf_width/2))
        
        deallocate(text_buffer, rotated_buffer)
    end subroutine draw_rotated_ylabel_png






end module fortplot_png

module fortplot_pdf
    use fortplot_context
    use fortplot_margins, only: plot_margins_t, plot_area_t, calculate_plot_area, get_axis_tick_positions
    use fortplot_ticks, only: generate_scale_aware_tick_labels
    use fortplot_label_positioning, only: calculate_x_label_position, calculate_y_label_position
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    private
    public :: pdf_context, create_pdf_canvas, draw_pdf_axes_and_labels
    
    type, extends(plot_context) :: pdf_context
        character(len=:), allocatable :: content_stream
        real(wp) :: stroke_r, stroke_g, stroke_b
        ! Plot area calculations (using common margin functionality)  
        type(plot_margins_t) :: margins
        type(plot_area_t) :: plot_area
    contains
        procedure :: line => draw_pdf_line
        procedure :: color => set_pdf_color
        procedure :: text => draw_pdf_text
        procedure :: save => write_pdf_file
    end type pdf_context
    
contains

    function create_pdf_canvas(width, height) result(ctx)
        integer, intent(in) :: width, height
        type(pdf_context) :: ctx
        
        call setup_canvas(ctx, width, height)
        call initialize_pdf_stream(ctx)
        
        ! Set up matplotlib-style margins using common module
        ctx%margins = plot_margins_t()  ! Use defaults
        call calculate_plot_area(width, height, ctx%margins, ctx%plot_area)
    end function create_pdf_canvas

    subroutine initialize_pdf_stream(ctx)
        type(pdf_context), intent(inout) :: ctx
        
        ctx%content_stream = ""
        ctx%stroke_r = 0.0_wp
        ctx%stroke_g = 0.0_wp
        ctx%stroke_b = 1.0_wp
        
        call add_to_stream(ctx, "q")
        call add_to_stream(ctx, "2 w")
        call add_to_stream(ctx, "1 J")
        call add_to_stream(ctx, "1 j")
        call add_to_stream(ctx, "0 0 1 RG")
    end subroutine initialize_pdf_stream
    
    subroutine draw_pdf_line(this, x1, y1, x2, y2)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: x1, y1, x2, y2
        real(wp) :: pdf_x1, pdf_y1, pdf_x2, pdf_y2
        
        call normalize_to_pdf_coords(this, x1, y1, pdf_x1, pdf_y1)
        call normalize_to_pdf_coords(this, x2, y2, pdf_x2, pdf_y2)
        call draw_vector_line(this, pdf_x1, pdf_y1, pdf_x2, pdf_y2)
    end subroutine draw_pdf_line
    
    subroutine set_pdf_color(this, r, g, b)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: r, g, b
        character(len=50) :: color_cmd
        
        this%stroke_r = r
        this%stroke_g = g  
        this%stroke_b = b
        
        write(color_cmd, '(F4.2, 1X, F4.2, 1X, F4.2, 1X, "RG")') r, g, b
        call add_to_stream(this, color_cmd)
    end subroutine set_pdf_color
    
    subroutine draw_pdf_text(this, x, y, text)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        real(wp) :: pdf_x, pdf_y
        character(len=200) :: text_cmd
        
        call normalize_to_pdf_coords(this, x, y, pdf_x, pdf_y)
        
        call add_to_stream(this, "BT")
        write(text_cmd, '("/F1 12 Tf")') 
        call add_to_stream(this, text_cmd)
        write(text_cmd, '(F8.2, 1X, F8.2, 1X, "Td")') pdf_x, pdf_y
        call add_to_stream(this, text_cmd)
        write(text_cmd, '("(", A, ") Tj")') trim(text)
        call add_to_stream(this, text_cmd)
        call add_to_stream(this, "ET")
    end subroutine draw_pdf_text
    
    subroutine write_pdf_file(this, filename)
        class(pdf_context), intent(inout) :: this
        character(len=*), intent(in) :: filename
        integer :: unit
        
        call finalize_pdf_stream(this)
        call create_pdf_document(unit, filename, this)
        print *, "PDF file '", trim(filename), "' created successfully!"
    end subroutine write_pdf_file

    subroutine normalize_to_pdf_coords(ctx, x, y, pdf_x, pdf_y)
        class(pdf_context), intent(in) :: ctx
        real(wp), intent(in) :: x, y
        real(wp), intent(out) :: pdf_x, pdf_y
        
        ! Transform coordinates to plot area (like matplotlib)
        ! Note: PDF coordinates have Y=0 at bottom (same as plot coordinates)
        pdf_x = (x - ctx%x_min) / (ctx%x_max - ctx%x_min) * real(ctx%plot_area%width, wp) + real(ctx%plot_area%left, wp)
        pdf_y = (y - ctx%y_min) / (ctx%y_max - ctx%y_min) * real(ctx%plot_area%height, wp) + &
                real(ctx%height - ctx%plot_area%bottom - ctx%plot_area%height, wp)
    end subroutine normalize_to_pdf_coords

    subroutine draw_vector_line(ctx, x1, y1, x2, y2)
        class(pdf_context), intent(inout) :: ctx
        real(wp), intent(in) :: x1, y1, x2, y2
        character(len=50) :: move_cmd, line_cmd
        
        write(move_cmd, '(F8.2, 1X, F8.2, 1X, "m")') x1, y1
        write(line_cmd, '(F8.2, 1X, F8.2, 1X, "l")') x2, y2
        
        call add_to_stream(ctx, move_cmd)
        call add_to_stream(ctx, line_cmd)
        call add_to_stream(ctx, "S")
    end subroutine draw_vector_line

    subroutine finalize_pdf_stream(ctx)
        type(pdf_context), intent(inout) :: ctx
        call add_to_stream(ctx, "Q")
    end subroutine finalize_pdf_stream

    subroutine add_to_stream(ctx, command)
        type(pdf_context), intent(inout) :: ctx
        character(len=*), intent(in) :: command
        
        if (allocated(ctx%content_stream)) then
            ctx%content_stream = ctx%content_stream // command // char(10)
        else
            ctx%content_stream = command // char(10)
        end if
    end subroutine add_to_stream
    
    subroutine create_pdf_document(unit, filename, ctx)
        integer, intent(out) :: unit
        character(len=*), intent(in) :: filename
        type(pdf_context), intent(in) :: ctx
        
        open(newunit=unit, file=filename, access='stream', form='unformatted', status='replace')
        call write_pdf_structure(unit, ctx)
        close(unit)
    end subroutine create_pdf_document

    subroutine write_pdf_structure(unit, ctx)
        integer, intent(in) :: unit
        type(pdf_context), intent(in) :: ctx
        integer :: obj_positions(5), xref_pos
        
        call write_string_to_unit(unit, "%PDF-1.4")
        call write_all_objects(unit, ctx, obj_positions)
        call write_xref_and_trailer(unit, obj_positions, xref_pos)
    end subroutine write_pdf_structure
    
    subroutine write_all_objects(unit, ctx, positions)
        integer, intent(in) :: unit
        type(pdf_context), intent(in) :: ctx
        integer, intent(out) :: positions(5)
        
        call write_catalog_object(unit, positions(1))
        call write_pages_object(unit, ctx, positions(2))
        call write_page_object(unit, ctx, positions(3))
        call write_content_object(unit, ctx, positions(4))
        call write_font_object(unit, positions(5))
    end subroutine write_all_objects

    subroutine write_xref_and_trailer(unit, positions, xref_pos)
        integer, intent(in) :: unit, positions(5)
        integer, intent(out) :: xref_pos
        character(len=200) :: xref_entry
        character(len=100) :: trailer_str
        
        xref_pos = get_position(unit)
        call write_string_to_unit(unit, "xref")
        call write_string_to_unit(unit, "0 6")
        call write_string_to_unit(unit, "0000000000 65535 f")
        
        write(xref_entry, '(I10.10, " 00000 n")') positions(1)
        call write_string_to_unit(unit, xref_entry)
        write(xref_entry, '(I10.10, " 00000 n")') positions(2)
        call write_string_to_unit(unit, xref_entry)
        write(xref_entry, '(I10.10, " 00000 n")') positions(3)
        call write_string_to_unit(unit, xref_entry)
        write(xref_entry, '(I10.10, " 00000 n")') positions(4)
        call write_string_to_unit(unit, xref_entry)
        write(xref_entry, '(I10.10, " 00000 n")') positions(5)
        call write_string_to_unit(unit, xref_entry)
        
        call write_string_to_unit(unit, "trailer")
        call write_string_to_unit(unit, "<</Size 6/Root 1 0 R>>")
        call write_string_to_unit(unit, "startxref")
        write(trailer_str, '(I0)') xref_pos
        call write_string_to_unit(unit, trailer_str)
        call write_string_to_unit(unit, "%%EOF")
    end subroutine write_xref_and_trailer

    subroutine write_catalog_object(unit, pos)
        integer, intent(in) :: unit
        integer, intent(out) :: pos
        pos = get_position(unit)
        call write_string_to_unit(unit, "1 0 obj")
        call write_string_to_unit(unit, "<</Type /Catalog/Pages 2 0 R>>")
        call write_string_to_unit(unit, "endobj")
    end subroutine write_catalog_object

    subroutine write_pages_object(unit, ctx, pos)
        integer, intent(in) :: unit
        type(pdf_context), intent(in) :: ctx
        integer, intent(out) :: pos
        
        pos = get_position(unit)
        call write_string_to_unit(unit, "2 0 obj")
        call write_string_to_unit(unit, "<</Type /Pages/Kids [3 0 R]/Count 1>>")
        call write_string_to_unit(unit, "endobj")
    end subroutine write_pages_object

    subroutine write_page_object(unit, ctx, pos)
        integer, intent(in) :: unit
        type(pdf_context), intent(in) :: ctx
        integer, intent(out) :: pos
        character(len=200) :: page_str, size_str
        
        pos = get_position(unit)
        write(size_str, '(I0, 1X, I0)') ctx%width, ctx%height
        call write_string_to_unit(unit, "3 0 obj")
        page_str = "<</Type /Page/Parent 2 0 R/MediaBox [0 0 " // trim(size_str) // "]" // &
                   "/Contents 4 0 R/Resources <</Font <</F1 5 0 R>>>>>>>>"
        call write_string_to_unit(unit, page_str)
        call write_string_to_unit(unit, "endobj")
    end subroutine write_page_object

    subroutine write_content_object(unit, ctx, pos)
        integer, intent(in) :: unit
        type(pdf_context), intent(in) :: ctx
        integer, intent(out) :: pos
        character(len=50) :: len_str
        
        pos = get_position(unit)
        write(len_str, '("/Length ", I0)') len(ctx%content_stream)
        call write_string_to_unit(unit, "4 0 obj")
        call write_string_to_unit(unit, "<<" // trim(len_str) // ">>")
        call write_string_to_unit(unit, "stream")
        call write_string_to_unit(unit, ctx%content_stream)
        call write_string_to_unit(unit, "endstream")
        call write_string_to_unit(unit, "endobj")
    end subroutine write_content_object

    subroutine write_font_object(unit, pos)
        integer, intent(in) :: unit
        integer, intent(out) :: pos
        
        pos = get_position(unit)
        call write_string_to_unit(unit, "5 0 obj")
        call write_string_to_unit(unit, "<</Type /Font/Subtype /Type1/BaseFont /Helvetica>>")
        call write_string_to_unit(unit, "endobj")
    end subroutine write_font_object

    integer function get_position(unit)
        integer, intent(in) :: unit
        inquire(unit=unit, pos=get_position)
        get_position = get_position - 1
    end function get_position

    subroutine write_string_to_unit(unit, str)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: str
        integer :: i
        integer(1), allocatable :: bytes(:)
        
        allocate(bytes(len(str) + 1))
        do i = 1, len(str)
            bytes(i) = int(iachar(str(i:i)), 1)
        end do
        bytes(len(str) + 1) = 10_1
        write(unit) bytes
        deallocate(bytes)
    end subroutine write_string_to_unit

    subroutine draw_pdf_axes_and_labels(ctx, xscale, yscale, symlog_threshold, &
                                      x_min_orig, x_max_orig, y_min_orig, y_max_orig, &
                                      title, xlabel, ylabel)
        !! Draw plot axes and frame for PDF backend with scale-aware tick generation
        type(pdf_context), intent(inout) :: ctx
        character(len=*), intent(in), optional :: xscale, yscale
        real(wp), intent(in), optional :: symlog_threshold
        real(wp), intent(in), optional :: x_min_orig, x_max_orig, y_min_orig, y_max_orig
        character(len=*), intent(in), optional :: title, xlabel, ylabel
        real(wp) :: x_positions(10), y_positions(10)
        integer :: num_x, num_y
        character(len=20) :: x_labels(10), y_labels(10)
        
        ! Set color to black for axes
        call ctx%color(0.0_wp, 0.0_wp, 0.0_wp)
        
        ! Draw plot frame using common functionality via procedure pointer
        call draw_pdf_frame(ctx)
        
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
        call draw_pdf_tick_marks(ctx, x_positions, y_positions, num_x, num_y)
        call draw_pdf_tick_labels(ctx, x_positions, y_positions, x_labels, y_labels, num_x, num_y)
        
        ! Draw title and axis labels
        call draw_pdf_title_and_labels(ctx, title, xlabel, ylabel)
    end subroutine draw_pdf_axes_and_labels
    
    subroutine draw_pdf_frame(ctx)
        !! Draw the plot frame for PDF backend
        type(pdf_context), intent(inout) :: ctx
        real(wp) :: left, right, top, bottom
        
        ! Calculate frame coordinates (PDF coordinates Y=0 at bottom)
        left = real(ctx%plot_area%left, wp)
        right = real(ctx%plot_area%left + ctx%plot_area%width, wp)
        bottom = real(ctx%height - ctx%plot_area%bottom - ctx%plot_area%height, wp)
        top = real(ctx%height - ctx%plot_area%bottom, wp)
        
        ! Draw frame (rectangle)
        call draw_vector_line(ctx, left, bottom, right, bottom)  ! Bottom edge
        call draw_vector_line(ctx, left, top, right, top)        ! Top edge
        call draw_vector_line(ctx, left, bottom, left, top)      ! Left edge  
        call draw_vector_line(ctx, right, bottom, right, top)    ! Right edge
    end subroutine draw_pdf_frame
    
    subroutine draw_pdf_tick_marks(ctx, x_positions, y_positions, num_x, num_y)
        !! Draw tick marks for PDF backend
        type(pdf_context), intent(inout) :: ctx
        real(wp), intent(in) :: x_positions(:), y_positions(:)
        integer, intent(in) :: num_x, num_y
        integer :: i
        real(wp) :: bottom, top, left, right, pdf_y_pos
        
        ! Calculate axis positions
        bottom = real(ctx%height - ctx%plot_area%bottom - ctx%plot_area%height, wp)
        top = real(ctx%height - ctx%plot_area%bottom, wp)
        left = real(ctx%plot_area%left, wp)
        right = real(ctx%plot_area%left + ctx%plot_area%width, wp)
        
        ! Draw X-axis tick marks (at bottom of plot)
        do i = 1, num_x
            call draw_vector_line(ctx, x_positions(i), bottom, x_positions(i), bottom - 5.0_wp)
        end do
        
        ! Draw Y-axis tick marks (at left of plot)  
        do i = 1, num_y
            ! Convert Y position to PDF coordinates
            pdf_y_pos = real(ctx%height - ctx%plot_area%bottom, wp) - &
                       (y_positions(i) - real(ctx%plot_area%bottom, wp))
            call draw_vector_line(ctx, left, pdf_y_pos, left - 5.0_wp, pdf_y_pos)
        end do
    end subroutine draw_pdf_tick_marks
    
    subroutine draw_pdf_tick_labels(ctx, x_positions, y_positions, x_labels, y_labels, num_x, num_y)
        !! Draw tick labels for PDF backend like matplotlib
        type(pdf_context), intent(inout) :: ctx
        real(wp), intent(in) :: x_positions(:), y_positions(:)
        character(len=*), intent(in) :: x_labels(:), y_labels(:)
        integer, intent(in) :: num_x, num_y
        integer :: i
        real(wp) :: label_x, label_y, bottom, left
        character(len=200) :: text_cmd
        
        bottom = real(ctx%height - ctx%plot_area%bottom - ctx%plot_area%height, wp)
        left = real(ctx%plot_area%left, wp)
        
        ! Draw X-axis labels with proper spacing and center alignment
        do i = 1, num_x
            ! Use common positioning (with PNG coordinates) then convert to PDF
            call calculate_x_label_position(x_positions(i), real(ctx%plot_area%bottom, wp), &
                                          real(ctx%plot_area%height, wp), trim(x_labels(i)), &
                                          label_x, label_y)
            ! Convert to PDF coordinates (Y is flipped)
            label_y = bottom - (label_y - real(ctx%plot_area%bottom + ctx%plot_area%height, wp))
            
            call add_to_stream(ctx, "BT")
            write(text_cmd, '("/F1 12 Tf")')
            call add_to_stream(ctx, text_cmd)
            write(text_cmd, '(F8.2, 1X, F8.2, 1X, "Td")') label_x, label_y
            call add_to_stream(ctx, text_cmd)
            write(text_cmd, '("(", A, ") Tj")') trim(x_labels(i))
            call add_to_stream(ctx, text_cmd)
            call add_to_stream(ctx, "ET")
        end do
        
        ! Draw Y-axis labels with right alignment and proper spacing
        do i = 1, num_y
            ! Use common positioning
            call calculate_y_label_position(y_positions(i), real(ctx%plot_area%left, wp), &
                                          trim(y_labels(i)), label_x, label_y)
            ! Convert Y position to PDF coordinates
            label_y = real(ctx%height - ctx%plot_area%bottom, wp) - &
                     (y_positions(i) - real(ctx%plot_area%bottom, wp))
            
            call add_to_stream(ctx, "BT")
            write(text_cmd, '("/F1 12 Tf")')
            call add_to_stream(ctx, text_cmd)
            write(text_cmd, '(F8.2, 1X, F8.2, 1X, "Td")') label_x, label_y
            call add_to_stream(ctx, text_cmd)
            write(text_cmd, '("(", A, ") Tj")') trim(y_labels(i))
            call add_to_stream(ctx, text_cmd)
            call add_to_stream(ctx, "ET")
        end do
    end subroutine draw_pdf_tick_labels
    
    subroutine draw_pdf_title_and_labels(ctx, title, xlabel, ylabel)
        !! Draw figure title and axis labels for PDF
        type(pdf_context), intent(inout) :: ctx
        character(len=*), intent(in), optional :: title, xlabel, ylabel
        real(wp) :: label_x, label_y
        
        ! Draw title at top center of plot
        if (present(title)) then
            label_x = real(ctx%width, wp) / 2.0_wp
            label_y = real(ctx%height - ctx%plot_area%bottom + 30, wp)  ! Above plot area
            call draw_pdf_text(ctx, label_x, label_y, trim(title))
        end if
        
        ! Draw X-axis label centered below plot
        if (present(xlabel)) then
            label_x = real(ctx%plot_area%left + ctx%plot_area%width / 2, wp)
            label_y = real(ctx%height - ctx%plot_area%bottom - 60, wp)  ! Below tick labels
            call draw_pdf_text(ctx, label_x, label_y, trim(xlabel))
        end if
        
        ! Draw Y-axis label on left side
        if (present(ylabel)) then
            label_x = real(30, wp)  ! Left margin
            label_y = real(ctx%height - ctx%plot_area%bottom - ctx%plot_area%height / 2, wp)
            call draw_pdf_text(ctx, label_x, label_y, trim(ylabel))
        end if
    end subroutine draw_pdf_title_and_labels

end module fortplot_pdf

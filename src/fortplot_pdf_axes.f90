module fortplot_pdf_axes
    !! PDF axes, grid, and tick drawing operations
    !! Handles plot frame, axes, tick marks, and grid lines
    
    use iso_fortran_env, only: wp => real64
    use fortplot_pdf_core, only: pdf_context_core, PDF_MARGIN, &
                                PDF_TICK_SIZE, PDF_LABEL_SIZE, &
                                PDF_TICK_LABEL_SIZE, PDF_TITLE_SIZE
    use fortplot_pdf_drawing, only: pdf_stream_writer
    use fortplot_pdf_text, only: draw_pdf_text, draw_pdf_text_bold, &
                                draw_mixed_font_text, draw_rotated_mixed_font_text
    implicit none
    private
    
    ! Public procedures
    public :: draw_pdf_axes_and_labels
    public :: draw_pdf_3d_axes_frame
    public :: draw_pdf_frame_with_area
    public :: draw_pdf_tick_marks_with_area
    public :: draw_pdf_tick_labels_with_area
    public :: draw_pdf_title_and_labels
    public :: setup_axes_data_ranges
    public :: generate_tick_data

contains

    subroutine setup_axes_data_ranges(ctx, x_min_orig, x_max_orig, y_min_orig, y_max_orig, &
                                     x_min_adj, x_max_adj, y_min_adj, y_max_adj, xscale, yscale)
        !! Set up data ranges for axes with optional log scaling
        type(pdf_context_core), intent(inout) :: ctx
        real(wp), intent(in) :: x_min_orig, x_max_orig, y_min_orig, y_max_orig
        real(wp), intent(out) :: x_min_adj, x_max_adj, y_min_adj, y_max_adj
        character(len=*), intent(in), optional :: xscale, yscale
        
        real(wp) :: x_range, y_range
        
        ! Initialize adjusted values
        x_min_adj = x_min_orig
        x_max_adj = x_max_orig
        y_min_adj = y_min_orig
        y_max_adj = y_max_orig
        
        ! Apply log scaling if requested
        if (present(xscale)) then
            if (xscale == 'log' .and. x_min_adj > 0.0_wp) then
                x_min_adj = log10(x_min_adj)
                x_max_adj = log10(x_max_adj)
            end if
        end if
        
        if (present(yscale)) then
            if (yscale == 'log' .and. y_min_adj > 0.0_wp) then
                y_min_adj = log10(y_min_adj)
                y_max_adj = log10(y_max_adj)
            end if
        end if
        
        ! Ensure valid ranges
        x_range = x_max_adj - x_min_adj
        y_range = y_max_adj - y_min_adj
        
        if (abs(x_range) < 1.0e-10_wp) then
            x_min_adj = x_min_adj - 0.5_wp
            x_max_adj = x_max_adj + 0.5_wp
        end if
        
        if (abs(y_range) < 1.0e-10_wp) then
            y_min_adj = y_min_adj - 0.5_wp
            y_max_adj = y_max_adj + 0.5_wp
        end if
    end subroutine setup_axes_data_ranges

    subroutine generate_tick_data(ctx, data_x_min, data_x_max, data_y_min, data_y_max, &
                                 x_positions, y_positions, x_labels, y_labels, &
                                 num_x_ticks, num_y_ticks, xscale, yscale, &
                                 plot_area_left, plot_area_bottom, plot_area_width, plot_area_height)
        !! Generate tick positions and labels for axes
        type(pdf_context_core), intent(in) :: ctx
        real(wp), intent(in) :: data_x_min, data_x_max, data_y_min, data_y_max
        real(wp), allocatable, intent(out) :: x_positions(:), y_positions(:)
        character(len=32), allocatable, intent(out) :: x_labels(:), y_labels(:)
        integer, intent(out) :: num_x_ticks, num_y_ticks
        character(len=*), intent(in), optional :: xscale, yscale
        real(wp), intent(in) :: plot_area_left, plot_area_bottom, plot_area_width, plot_area_height
        
        real(wp) :: x_range, y_range, x_step, y_step
        real(wp) :: x_tick, y_tick
        integer :: i
        integer, parameter :: TARGET_TICKS = 8
        real(wp), parameter :: EPSILON = 1.0e-10_wp
        
        ! Calculate ranges with epsilon protection
        x_range = data_x_max - data_x_min
        y_range = data_y_max - data_y_min
        
        ! Determine number of ticks using actual plot area dimensions
        num_x_ticks = min(TARGET_TICKS, max(2, int(plot_area_width / 50.0_wp)))
        num_y_ticks = min(TARGET_TICKS, max(2, int(plot_area_height / 40.0_wp)))
        
        ! Allocate arrays
        allocate(x_positions(num_x_ticks))
        allocate(y_positions(num_y_ticks))
        allocate(x_labels(num_x_ticks))
        allocate(y_labels(num_y_ticks))
        
        ! Generate X ticks with zero-range protection
        if (abs(x_range) < EPSILON) then
            ! Zero or near-zero X range: distribute evenly across plot area
            x_step = 0.0_wp
            do i = 1, num_x_ticks
                x_tick = data_x_min  ! All ticks at same data value
                x_positions(i) = plot_area_left + plot_area_width * 0.5_wp  ! Center position
            end do
        else
            ! Normal X range calculation
            x_step = x_range / real(num_x_ticks - 1, wp)
            do i = 1, num_x_ticks
                x_tick = data_x_min + real(i - 1, wp) * x_step
                
                ! Convert to plot coordinates
                x_positions(i) = plot_area_left + &
                    (x_tick - data_x_min) / x_range * plot_area_width
            end do
        end if
        
        ! Generate X tick labels
        do i = 1, num_x_ticks
            if (abs(x_range) < EPSILON) then
                x_tick = data_x_min
            else
                x_tick = data_x_min + real(i - 1, wp) * x_step
            end if
            
            ! Generate label
            if (present(xscale)) then
                if (xscale == 'log') then
                    write(x_labels(i), '(ES10.2)') 10.0_wp ** x_tick
                else
                    write(x_labels(i), '(F10.2)') x_tick
                end if
            else
                write(x_labels(i), '(F10.2)') x_tick
            end if
            x_labels(i) = adjustl(x_labels(i))
        end do
        
        ! Generate Y ticks with zero-range protection
        if (abs(y_range) < EPSILON) then
            ! Zero or near-zero Y range: distribute evenly across plot area
            y_step = 0.0_wp
            do i = 1, num_y_ticks
                y_tick = data_y_min  ! All ticks at same data value
                y_positions(i) = plot_area_bottom + plot_area_height * 0.5_wp  ! Center position
            end do
        else
            ! Normal Y range calculation
            y_step = y_range / real(num_y_ticks - 1, wp)
            do i = 1, num_y_ticks
                y_tick = data_y_min + real(i - 1, wp) * y_step
                
                ! Convert to plot coordinates
                y_positions(i) = plot_area_bottom + &
                    (y_tick - data_y_min) / y_range * plot_area_height
            end do
        end if
        
        ! Generate Y tick labels
        do i = 1, num_y_ticks
            if (abs(y_range) < EPSILON) then
                y_tick = data_y_min
            else
                y_tick = data_y_min + real(i - 1, wp) * y_step
            end if
            
            ! Generate label
            if (present(yscale)) then
                if (yscale == 'log') then
                    write(y_labels(i), '(ES10.2)') 10.0_wp ** y_tick
                else
                    write(y_labels(i), '(F10.2)') y_tick
                end if
            else
                write(y_labels(i), '(F10.2)') y_tick
            end if
            y_labels(i) = adjustl(y_labels(i))
        end do
    end subroutine generate_tick_data

    subroutine draw_pdf_axes_and_labels(ctx, xscale, yscale, symlog_threshold, &
                                       data_x_min, data_x_max, data_y_min, data_y_max, &
                                       title, xlabel, ylabel, plot_area_left, plot_area_bottom, &
                                       plot_area_width, plot_area_height, canvas_height)
        !! Draw complete axes system with labels using actual plot area coordinates
        type(pdf_context_core), intent(inout) :: ctx
        character(len=*), intent(in), optional :: xscale, yscale
        real(wp), intent(in), optional :: symlog_threshold
        real(wp), intent(in) :: data_x_min, data_x_max, data_y_min, data_y_max
        character(len=*), intent(in), optional :: title, xlabel, ylabel
        real(wp), intent(in) :: plot_area_left, plot_area_bottom, plot_area_width, plot_area_height, canvas_height
        
        real(wp), allocatable :: x_positions(:), y_positions(:)
        character(len=32), allocatable :: x_labels(:), y_labels(:)
        integer :: num_x_ticks, num_y_ticks
        real(wp) :: x_min_adj, x_max_adj, y_min_adj, y_max_adj
        
        ! Setup data ranges
        call setup_axes_data_ranges(ctx, data_x_min, data_x_max, data_y_min, data_y_max, &
                                   x_min_adj, x_max_adj, y_min_adj, y_max_adj, xscale, yscale)
        
        ! Generate tick data
        call generate_tick_data(ctx, x_min_adj, x_max_adj, y_min_adj, y_max_adj, &
                               x_positions, y_positions, x_labels, y_labels, &
                               num_x_ticks, num_y_ticks, xscale, yscale, &
                               plot_area_left, plot_area_bottom, plot_area_width, plot_area_height)
        
        ! Grid functionality removed - PDF plots now display without grid lines
        
        ! Plot area parameters are now mandatory (non-optional)
        
        call draw_pdf_frame_with_area(ctx, plot_area_left, plot_area_bottom, &
                                     plot_area_width, plot_area_height, canvas_height)
        
        call draw_pdf_tick_marks_with_area(ctx, x_positions, y_positions, num_x_ticks, num_y_ticks, &
                                          plot_area_left, plot_area_bottom, canvas_height)
        
        call draw_pdf_tick_labels_with_area(ctx, x_positions, y_positions, x_labels, y_labels, &
                                           num_x_ticks, num_y_ticks, plot_area_left, plot_area_bottom, canvas_height)
        
        ! Draw title and axis labels
        if (present(title) .or. present(xlabel) .or. present(ylabel)) then
            call draw_pdf_title_and_labels(ctx, title, xlabel, ylabel, &
                                      plot_area_left, plot_area_bottom, &
                                      plot_area_width, plot_area_height)
        end if
    end subroutine draw_pdf_axes_and_labels

    subroutine draw_pdf_3d_axes_frame(ctx, x_min, x_max, y_min, y_max, z_min, z_max)
        !! Draw 3D axes frame (placeholder for future implementation)
        type(pdf_context_core), intent(inout) :: ctx
        real(wp), intent(in) :: x_min, x_max, y_min, y_max, z_min, z_max
        
        ! PDF backend does not support 3D axes projection
        ! 3D plots in PDF backend fall back to 2D projections handled by the
        ! standard 2D axes drawing functions. This is consistent with many
        ! PDF plotting libraries that focus on vector graphics in 2D space.
        ! 3D visualization is better suited to raster backends (PNG) that can
        ! render projected 3D axes with proper visual depth cues.
    end subroutine draw_pdf_3d_axes_frame

    subroutine draw_pdf_frame_with_area(ctx, plot_left, plot_bottom, plot_width, plot_height, canvas_height)
        !! Draw the plot frame using actual plot area coordinates (FIXED version)
        type(pdf_context_core), intent(inout) :: ctx
        real(wp), intent(in) :: plot_left, plot_bottom, plot_width, plot_height, canvas_height
        character(len=256) :: frame_cmd
        real(wp) :: x1, y1
        
        ! Convert to PDF coordinates (Y=0 at bottom)
        x1 = plot_left
        y1 = canvas_height - plot_bottom - plot_height  ! PDF Y coordinate conversion
        
        ! Draw rectangle frame
        write(frame_cmd, '(F0.3, 1X, F0.3, " ", F0.3, 1X, F0.3, " re S")') &
            x1, y1, plot_width, plot_height
        ctx%stream_data = ctx%stream_data // trim(adjustl(frame_cmd)) // new_line('a')
    end subroutine draw_pdf_frame_with_area


    subroutine draw_pdf_tick_marks_with_area(ctx, x_positions, y_positions, num_x, num_y, &
                                           plot_left, plot_bottom, canvas_height)
        !! Draw tick marks using actual plot area coordinates (FIXED version)
        type(pdf_context_core), intent(inout) :: ctx
        real(wp), intent(in) :: x_positions(:), y_positions(:)
        integer, intent(in) :: num_x, num_y
        real(wp), intent(in) :: plot_left, plot_bottom, canvas_height
        
        integer :: i
        character(len=256) :: tick_cmd
        real(wp) :: tick_length, bottom_y
        
        tick_length = PDF_TICK_SIZE
        bottom_y = canvas_height - plot_bottom  ! Convert to PDF coordinates
        
        ! Draw X-axis ticks (bottom of plot area)
        do i = 1, num_x
            write(tick_cmd, '(F0.3, 1X, F0.3, " m ", F0.3, 1X, F0.3, " l S")') &
                x_positions(i), bottom_y, &
                x_positions(i), bottom_y - tick_length
            ctx%stream_data = ctx%stream_data // trim(adjustl(tick_cmd)) // new_line('a')
        end do
        
        ! Draw Y-axis ticks (left side of plot area)
        do i = 1, num_y
            write(tick_cmd, '(F0.3, 1X, F0.3, " m ", F0.3, 1X, F0.3, " l S")') &
                plot_left, canvas_height - y_positions(i), &
                plot_left - tick_length, canvas_height - y_positions(i)
            ctx%stream_data = ctx%stream_data // trim(adjustl(tick_cmd)) // new_line('a')
        end do
    end subroutine draw_pdf_tick_marks_with_area


    subroutine draw_pdf_tick_labels_with_area(ctx, x_positions, y_positions, x_labels, y_labels, &
                                            num_x, num_y, plot_left, plot_bottom, canvas_height)
        !! Draw tick labels using actual plot area coordinates (FIXED version)
        type(pdf_context_core), intent(inout) :: ctx
        real(wp), intent(in) :: x_positions(:), y_positions(:)
        character(len=*), intent(in) :: x_labels(:), y_labels(:)
        integer, intent(in) :: num_x, num_y
        real(wp), intent(in) :: plot_left, plot_bottom, canvas_height
        
        integer :: i
        real(wp) :: label_x, label_y, bottom_y
        real(wp) :: x_offset, y_offset
        
        x_offset = 5.0_wp   ! Offset for X labels below ticks
        y_offset = 10.0_wp  ! Offset for Y labels left of ticks
        bottom_y = canvas_height - plot_bottom  ! Convert to PDF coordinates
        
        ! Draw X-axis labels
        do i = 1, num_x
            label_x = x_positions(i) - 15.0_wp  ! Center horizontally
            label_y = bottom_y - PDF_TICK_SIZE - x_offset - 10.0_wp
            call draw_pdf_text(ctx, label_x, label_y, trim(x_labels(i)))
        end do
        
        ! Draw Y-axis labels with overlap detection
        call draw_pdf_y_labels_with_overlap_detection(ctx, y_positions, y_labels, num_y, &
                                                     plot_left - PDF_TICK_SIZE - y_offset, canvas_height)
    end subroutine draw_pdf_tick_labels_with_area

    subroutine draw_pdf_title_and_labels(ctx, title, xlabel, ylabel, &
                                         plot_area_left, plot_area_bottom, &
                                         plot_area_width, plot_area_height)
        !! Draw plot title and axis labels
        type(pdf_context_core), intent(inout) :: ctx
        character(len=*), intent(in), optional :: title, xlabel, ylabel
        real(wp), intent(in) :: plot_area_left, plot_area_bottom, plot_area_width, plot_area_height
        
        real(wp) :: title_x, title_y
        real(wp) :: xlabel_x, xlabel_y
        real(wp) :: ylabel_x, ylabel_y
        
        ! Draw title (centered at top)
        if (present(title)) then
            if (len_trim(title) > 0) then
                title_x = plot_area_left + plot_area_width * 0.5_wp - &
                         real(len_trim(title), wp) * 3.5_wp
                title_y = plot_area_bottom + plot_area_height + 20.0_wp
                call draw_pdf_text_bold(ctx, title_x, title_y, trim(title))
            end if
        end if
        
        ! Draw X-axis label (centered at bottom)
        if (present(xlabel)) then
            if (len_trim(xlabel) > 0) then
                xlabel_x = plot_area_left + plot_area_width * 0.5_wp - &
                          real(len_trim(xlabel), wp) * 3.0_wp
                xlabel_y = plot_area_bottom - 35.0_wp
                call draw_mixed_font_text(ctx, xlabel_x, xlabel_y, trim(xlabel))
            end if
        end if
        
        ! Draw Y-axis label (rotated on left)
        if (present(ylabel)) then
            if (len_trim(ylabel) > 0) then
                ylabel_x = plot_area_left - 45.0_wp
                ylabel_y = plot_area_bottom + plot_area_height * 0.5_wp - &
                          real(len_trim(ylabel), wp) * 3.0_wp
                call draw_rotated_mixed_font_text(ctx, ylabel_x, ylabel_y, trim(ylabel))
            end if
        end if
    end subroutine draw_pdf_title_and_labels

    subroutine draw_pdf_y_labels_with_overlap_detection(ctx, y_positions, y_labels, num_y, plot_left, canvas_height)
        !! Draw Y-axis labels with overlap detection to prevent clustering
        type(pdf_context_core), intent(inout) :: ctx
        real(wp), intent(in) :: y_positions(:)
        character(len=*), intent(in) :: y_labels(:)
        integer, intent(in) :: num_y
        real(wp), intent(in) :: plot_left, canvas_height
        
        real(wp) :: last_y_drawn
        real(wp) :: min_spacing
        integer :: i
        real(wp) :: label_x, label_y
        
        min_spacing = 15.0_wp  ! Minimum vertical spacing between labels
        last_y_drawn = -1000.0_wp  ! Initialize to ensure first label is drawn
        
        do i = 1, num_y
            label_y = canvas_height - y_positions(i) - 3.0_wp  ! Convert to PDF coordinates and vertically center
            
            ! Only draw if sufficient spacing from last label
            if (abs(label_y - last_y_drawn) >= min_spacing) then
                label_x = plot_left - real(len_trim(y_labels(i)), wp) * 5.0_wp
                call draw_pdf_text(ctx, label_x, label_y, trim(y_labels(i)))
                last_y_drawn = label_y
            end if
        end do
    end subroutine draw_pdf_y_labels_with_overlap_detection

end module fortplot_pdf_axes
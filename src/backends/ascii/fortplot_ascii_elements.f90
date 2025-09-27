module fortplot_ascii_elements  
    !! ASCII terminal plotting backend - Drawing Elements
    !!
    !! This module contains specialized drawing functions for ASCII plotting
    !! including markers, arrows, heatmaps, legends, and axes.
    !!
    !! Author: fortplot contributors
    
    use fortplot_constants, only: EPSILON_COMPARE
    use fortplot_legend, only: legend_t, render_ascii_legend
    use fortplot_legend, only: LEGEND_UPPER_LEFT, LEGEND_UPPER_RIGHT, LEGEND_LOWER_LEFT, LEGEND_LOWER_RIGHT
    use fortplot_axes, only: compute_scale_ticks, format_tick_label, MAX_TICKS
    use fortplot_tick_calculation, only: determine_decimals_from_ticks, &
        format_tick_value_consistent
    use fortplot_plot_data, only: plot_data_t
    use fortplot_latex_parser, only: process_latex_in_text
    use fortplot_ascii_utils, only: get_char_density, ASCII_CHARS, text_element_t
    use fortplot_ascii_utils, only: is_legend_entry_text, is_registered_legend_label, is_autopct_text
    use fortplot_ascii_primitives, only: ascii_draw_text_primitive
    use, intrinsic :: iso_fortran_env, only: wp => real64, real64
    implicit none
    
    private
    public :: draw_ascii_marker, fill_ascii_heatmap, draw_ascii_arrow
    public :: render_ascii_legend_specialized, calculate_ascii_legend_dimensions
    public :: set_ascii_legend_border_width, calculate_ascii_legend_position
    public :: draw_ascii_axes_and_labels
    public :: reset_ascii_legend_lines_helper, append_ascii_legend_line_helper
    public :: register_legend_entry_helper, assign_pending_autopct_helper
    public :: ascii_draw_text_helper
    
    
contains

    subroutine draw_ascii_marker(canvas, x, y, style, x_min, x_max, y_min, y_max, plot_width, plot_height)
        character(len=1), intent(inout) :: canvas(:,:)
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: style
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        integer, intent(in) :: plot_width, plot_height
        integer :: px, py
        character(len=1) :: marker_char

        ! Map to usable plot area (excluding 1-char border on each side)
        px = int((x - x_min) / (x_max - x_min) * real(plot_width - 3, wp)) + 2
        py = (plot_height - 1) - int((y - y_min) / (y_max - y_min) * real(plot_height - 3, wp))

        ! Map marker styles to distinct ASCII characters for visual differentiation
        select case (trim(style))
        case ('o')
            marker_char = 'o'  ! Circle
        case ('s')
            marker_char = '#'  ! Square
        case ('D', 'd')
            marker_char = '%'  ! Diamond (ASCII representation)
        case ('x')
            marker_char = 'x'  ! Cross
        case ('+')
            marker_char = '+'  ! Plus
        case ('*')
            marker_char = '*'  ! Star
        case ('^')
            marker_char = '^'  ! Triangle up
        case ('v')
            marker_char = 'v'  ! Triangle down
        case ('<')
            marker_char = '<'  ! Triangle left
        case ('>')
            marker_char = '>'  ! Triangle right
        case ('p')
            marker_char = 'P'  ! Pentagon
        case ('h', 'H')
            marker_char = 'H'  ! Hexagon
        case default
            marker_char = '*'  ! Default fallback
        end select

        if (px >= 2 .and. px <= plot_width - 1 .and. py >= 2 .and. py <= plot_height - 1) then
            canvas(py, px) = marker_char
        end if
    end subroutine draw_ascii_marker
    
    subroutine fill_ascii_heatmap(canvas, x_grid, y_grid, z_grid, z_min, z_max, &
                                  x_min, x_max, y_min, y_max, plot_width, plot_height)
        !! Fill ASCII canvas with heatmap representation of 2D data
        character(len=1), intent(inout) :: canvas(:,:)
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in) :: z_min, z_max
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        integer, intent(in) :: plot_width, plot_height
        
        integer :: nx, ny, i, j, px, py
        real(wp) :: z_normalized
        integer :: char_idx
        
        nx = size(x_grid)
        ny = size(y_grid)
        
        ! z_grid should have dimensions (ny, nx) - rows by columns
        if (size(z_grid, 1) /= ny .or. size(z_grid, 2) /= nx) return
        
        ! Fill the canvas with density characters based on z values
        do i = 1, nx
            do j = 1, ny
                ! Map grid coordinates to canvas coordinates
                px = int((x_grid(i) - x_min) / (x_max - x_min) * &
                        real(plot_width - 3, wp)) + 2
                py = (plot_height - 1) - int((y_grid(j) - y_min) / &
                        (y_max - y_min) * real(plot_height - 3, wp))
                
                ! Check bounds
                if (px >= 2 .and. px <= plot_width - 1 .and. py >= 2 .and. py <= plot_height - 1) then
                    ! Normalize z value to character index
                    ! z_grid is (ny, nx) so access as z_grid(j, i)
                    if (abs(z_max - z_min) > EPSILON_COMPARE) then
                        z_normalized = (z_grid(j, i) - z_min) / (z_max - z_min)
                    else
                        z_normalized = 0.5_wp
                    end if
                    
                    ! Map to character index (1 to len(ASCII_CHARS))
                    char_idx = min(len(ASCII_CHARS), max(1, int(z_normalized * real(len(ASCII_CHARS) - 1, wp)) + 1))
                    
                    ! Only overwrite if current position is empty or has lower density
                    if (canvas(py, px) == ' ' .or. char_idx > index(ASCII_CHARS, canvas(py, px))) then
                        canvas(py, px) = ASCII_CHARS(char_idx:char_idx)
                    end if
                end if
            end do
        end do
    end subroutine fill_ascii_heatmap

    subroutine draw_ascii_arrow(canvas, x, y, dx, dy, size, style, &
                                x_min, x_max, y_min, y_max, width, height, &
                                has_rendered_arrows, uses_vector_arrows, has_triangular_arrows)
        !! Draw arrow using Unicode directional characters for ASCII backend
        character(len=1), intent(inout) :: canvas(:,:)
        real(wp), intent(in) :: x, y, dx, dy, size
        character(len=*), intent(in) :: style
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        integer, intent(in) :: width, height
        logical, intent(out) :: has_rendered_arrows, uses_vector_arrows, has_triangular_arrows
        
        integer :: px, py
        character(len=1) :: arrow_char
        real(wp) :: angle
        
        ! Reference otherwise-unused parameters without unreachable branches
        associate(unused_s => size, unused_ls => len_trim(style)); end associate
        
        ! Convert world coordinates to pixel coordinates
        px = int((x - x_min) / (x_max - x_min) * real(width, wp))
        py = int((y - y_min) / (y_max - y_min) * real(height, wp))
        
        ! Ensure coordinates are within bounds
        if (px < 1 .or. px > width .or. py < 1 .or. py > height) return
        
        ! Calculate angle for direction
        angle = atan2(dy, dx)
        
        ! Choose ASCII-compatible arrow character based on direction
        if (abs(angle) < 0.393_wp) then          ! 0 ± 22.5 degrees (right)
            arrow_char = '>'
        else if (angle >= 0.393_wp .and. angle < 1.178_wp) then  ! 22.5-67.5 degrees (up-right)
            arrow_char = '/'
        else if (angle >= 1.178_wp .and. angle < 1.963_wp) then  ! 67.5-112.5 degrees (up)
            arrow_char = '^'
        else if (angle >= 1.963_wp .and. angle < 2.749_wp) then  ! 112.5-157.5 degrees (up-left)
            arrow_char = '\'
        else if (abs(angle) >= 2.749_wp) then    ! 157.5-180 degrees (left)
            arrow_char = '<'
        else if (angle <= -0.393_wp .and. angle > -1.178_wp) then  ! -22.5 to -67.5 degrees (down-right)
            arrow_char = '\'
        else if (angle <= -1.178_wp .and. angle > -1.963_wp) then  ! -67.5 to -112.5 degrees (down)
            arrow_char = 'v'
        else  ! -112.5 to -157.5 degrees (down-left)
            arrow_char = '/'
        end if
        
        ! Place arrow character on canvas
        canvas(py, px) = arrow_char
        
        ! Mark that arrows have been rendered
        has_rendered_arrows = .true.
        uses_vector_arrows = .false.
        has_triangular_arrows = .false.
    end subroutine draw_ascii_arrow

    subroutine render_ascii_legend_specialized(legend, canvas_context, legend_x, legend_y)
        !! Render legend using ASCII-specific compact layout
        use fortplot_context, only: plot_context
        type(legend_t), intent(in) :: legend
        class(plot_context), intent(inout) :: canvas_context  
        real(wp), intent(in) :: legend_x, legend_y
        
        ! Use ASCII-specific legend rendering
        call render_ascii_legend(legend, canvas_context, legend_x, legend_y)
    end subroutine render_ascii_legend_specialized

    subroutine calculate_ascii_legend_dimensions(legend, width, legend_width, legend_height)
        !! Calculate ASCII-specific legend dimensions
        type(legend_t), intent(in) :: legend
        integer, intent(in) :: width
        real(wp), intent(out) :: legend_width, legend_height
        integer :: i
        character(len=512) :: processed_label
        integer :: processed_len
        
        ! Calculate actual legend width based on longest entry
        legend_width = 15.0_wp  ! Default minimum width
        do i = 1, legend%num_entries
            ! Process LaTeX commands for accurate width calculation
            call process_latex_in_text(legend%entries(i)%label, processed_label, processed_len)
            legend_width = max(legend_width, real(processed_len + 5, wp))  ! +5 for "-- " prefix and margin
        end do
        
        ! For ASCII backend, limit legend width to prevent overflow  
        if (legend_width > real(width, wp) * 0.3) then
            legend_width = real(width, wp) * 0.3
        end if
        
        legend_height = real(legend%num_entries + 2, wp)  ! Each entry + border
    end subroutine calculate_ascii_legend_dimensions

    subroutine set_ascii_legend_border_width()
        !! ASCII doesn't use line widths - no-op
        ! ASCII backend doesn't have line widths - no operation needed
    end subroutine set_ascii_legend_border_width

    subroutine calculate_ascii_legend_position(legend, width, height, x, y)
        !! Calculate ASCII-specific legend position using character coordinates
        type(legend_t), intent(in) :: legend
        integer, intent(in) :: width, height
        real(wp), intent(out) :: x, y
        real(wp) :: legend_width, legend_height, margin_x, margin_y
        
        ! Get ASCII-specific dimensions
        call calculate_ascii_legend_dimensions(legend, width, legend_width, legend_height)
        
        margin_x = 2.0_wp      ! 2 character margin
        margin_y = 1.0_wp      ! 1 line margin
        
        select case (legend%position)
        case (LEGEND_UPPER_LEFT)
            x = margin_x
            y = margin_y
        case (LEGEND_UPPER_RIGHT)
            ! Position legend so its text fits within the canvas
            ! For ASCII, be more conservative to avoid clipping
            x = real(width, wp) - legend_width - margin_x - 5.0_wp
            x = max(margin_x, x)  ! But not too far left
            y = margin_y + 2.0_wp  ! Start lower to leave room for multiple entries
        case (LEGEND_LOWER_LEFT)
            x = margin_x
            y = real(height, wp) - legend_height - margin_y
        case (LEGEND_LOWER_RIGHT)
            x = real(width, wp) - legend_width - margin_x
            y = real(height, wp) - legend_height - margin_y
        case default
            ! Default to upper right corner  
            x = real(width, wp) - legend_width - margin_x
            y = margin_y
        end select
    end subroutine calculate_ascii_legend_position

    subroutine draw_ascii_axes_and_labels(canvas, xscale, yscale, symlog_threshold, &
                                         x_min, x_max, y_min, y_max, &
                                         title, xlabel, ylabel, &
                                         z_min, z_max, has_3d_plots, &
                                         current_r, current_g, current_b, &
                                         plot_width, plot_height, &
                                         title_text, xlabel_text, ylabel_text, &
                                         text_elements, num_text_elements)
        !! Draw axes and labels for ASCII backend
        use fortplot_ascii_utils, only: get_blend_char, text_element_t
        use fortplot_latex_parser, only: process_latex_in_text
        character(len=1), intent(inout) :: canvas(:,:)
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=:), allocatable, intent(in), optional :: title, xlabel, ylabel
        real(wp), intent(in), optional :: z_min, z_max
        logical, intent(in) :: has_3d_plots
        real(wp), intent(in) :: current_r, current_g, current_b
        integer, intent(in) :: plot_width, plot_height
        character(len=:), allocatable, intent(inout) :: title_text, xlabel_text, ylabel_text
        type(text_element_t), intent(inout) :: text_elements(:)
        integer, intent(inout) :: num_text_elements
        
        real(wp) :: x_tick_positions(MAX_TICKS), y_tick_positions(MAX_TICKS)
        integer :: num_x_ticks, num_y_ticks, i
        character(len=50) :: tick_label
        real(wp) :: tick_x, tick_y
        integer :: decimals
        real(wp) :: luminance
        character(len=1) :: line_char
        character(len=500) :: processed_title
        integer :: processed_len
        ! For y-axis ASCII label de-duplication by row
        integer :: row
        integer, allocatable :: row_best_len(:)
        character(len=64), allocatable :: row_best_label(:)
        
        ! Reference optional parameters without unreachable branches
        if (present(z_min)) then; associate(unused_zmin => z_min); end associate; end if
        if (present(z_max)) then; associate(unused_zmax => z_max); end associate; end if
        associate(unused_h3d => has_3d_plots); end associate
        
        ! ASCII backend: explicitly set title and draw simple axes
        if (present(title)) then
            if (allocated(title)) then
                call process_latex_in_text(title, processed_title, processed_len)
                title_text = processed_title(1:processed_len)
            end if
        end if
        
        ! Calculate luminance for better character selection
        luminance = 0.299_wp * current_r + 0.587_wp * current_g + 0.114_wp * current_b
        
        ! Select character based on color dominance and luminance
        if (luminance > 0.9_wp) then
            line_char = ':'
        else if (current_g > 0.7_wp) then
            line_char = '@'
        else if (current_g > 0.3_wp) then
            line_char = '#'
        else if (current_b > 0.7_wp) then
            line_char = '*'
        else if (current_b > 0.3_wp) then
            line_char = 'o'
        else if (current_r > 0.7_wp) then
            line_char = '%'
        else if (current_r > 0.3_wp) then
            line_char = '+'
        else
            line_char = '.'
        end if
        
        ! Draw horizontal axis
        call draw_line_on_canvas(canvas, x_min, y_min, x_max, y_min, &
                                x_min, x_max, y_min, y_max, plot_width, plot_height, line_char)
        ! Draw vertical axis  
        call draw_line_on_canvas(canvas, x_min, y_min, x_min, y_max, &
                                x_min, x_max, y_min, y_max, plot_width, plot_height, line_char)
        
        ! Generate tick marks and labels for ASCII
        ! X-axis ticks (drawn as characters along bottom axis)
        call compute_scale_ticks(xscale, x_min, x_max, symlog_threshold, x_tick_positions, num_x_ticks)
        ! Determine decimals for linear scale based on tick spacing
        decimals = 0
        if (trim(xscale) == 'linear' .and. num_x_ticks >= 2) then
            decimals = determine_decimals_from_ticks(x_tick_positions, num_x_ticks)
        end if
        do i = 1, num_x_ticks
            tick_x = x_tick_positions(i)
            ! For ASCII, draw tick marks as characters in the text output
            if (trim(xscale) == 'linear') then
                tick_label = format_tick_value_consistent(tick_x, decimals)
            else
                tick_label = format_tick_label(tick_x, xscale)
            end if
            call add_text_element(text_elements, num_text_elements, &
                                 tick_x, y_min - 0.05_wp * (y_max - y_min), trim(tick_label), &
                                 current_r, current_g, current_b, &
                                 x_min, x_max, y_min, y_max, plot_width, plot_height)
        end do
        
        ! Y-axis ticks (drawn as characters along left axis)
        ! Avoid overlapping multiple labels on the same ASCII row by keeping only the longest label per row.

        call compute_scale_ticks(yscale, y_min, y_max, symlog_threshold, y_tick_positions, num_y_ticks)
        decimals = 0
        if (trim(yscale) == 'linear' .and. num_y_ticks >= 2) then
            decimals = determine_decimals_from_ticks(y_tick_positions, num_y_ticks)
        end if

        allocate(row_best_len(plot_height))
        allocate(row_best_label(plot_height))
        row_best_len = 0
        row_best_label = ''

        do i = 1, num_y_ticks
            tick_y = y_tick_positions(i)
            if (trim(yscale) == 'linear') then
                tick_label = format_tick_value_consistent(tick_y, decimals)
            else
                tick_label = format_tick_label(tick_y, yscale)
            end if
            ! Project tick_y to canvas row (same mapping as add_text_element)
            row = nint((y_max - tick_y) / (y_max - y_min) * real(plot_height, wp))
            row = max(1, min(row, plot_height))
            if (len_trim(tick_label) > row_best_len(row)) then
                row_best_len(row) = len_trim(tick_label)
                row_best_label(row) = adjustl(tick_label)
            end if
        end do

        ! Emit at most one y-label per row at the left edge (screen coordinates)
        do row = 1, plot_height
            ! Skip bottom row which is used for X tick labels to avoid overlap
            if (row_best_len(row) > 0 .and. row < plot_height) then
                call add_text_element(text_elements, num_text_elements, &
                                     2.0_wp, real(row, wp), trim(row_best_label(row)), &
                                     current_r, current_g, current_b, &
                                     x_min, x_max, y_min, y_max, plot_width, plot_height)
            end if
        end do
        
        ! Store processed xlabel and ylabel for rendering outside the plot frame
        ! Note: title has already been processed and stored at line 296-300
        if (present(xlabel)) then
            if (allocated(xlabel)) then
                call process_latex_in_text(xlabel, processed_title, processed_len)
                xlabel_text = processed_title(1:processed_len)
            end if
        end if
        
        if (present(ylabel)) then
            if (allocated(ylabel)) then
                call process_latex_in_text(ylabel, processed_title, processed_len)
                ylabel_text = processed_title(1:processed_len)
            end if
        end if
    end subroutine draw_ascii_axes_and_labels
    
    subroutine draw_line_on_canvas(canvas, x1, y1, x2, y2, x_min, x_max, y_min, y_max, plot_width, plot_height, line_char)
        use fortplot_ascii_utils, only: get_blend_char
        character(len=1), intent(inout) :: canvas(:,:)
        real(wp), intent(in) :: x1, y1, x2, y2
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        integer, intent(in) :: plot_width, plot_height
        character(len=1), intent(in) :: line_char
        
        real(wp) :: dx, dy, length, step_x, step_y, x, y
        integer :: steps, i, px, py
        
        dx = x2 - x1
        dy = y2 - y1
        length = sqrt(dx*dx + dy*dy)
        
        if (length < 1e-6_wp) return
        
        steps = max(int(length * 4), max(abs(int(dx)), abs(int(dy)))) + 1
        step_x = dx / real(steps, wp)
        step_y = dy / real(steps, wp)
        
        x = x1
        y = y1
        
        do i = 0, steps
            ! Map to usable plot area (excluding 1-char border on each side)
            px = int((x - x_min) / (x_max - x_min) * real(plot_width - 3, wp)) + 2
            py = (plot_height - 1) - int((y - y_min) / (y_max - y_min) * real(plot_height - 3, wp))
            
            if (px >= 2 .and. px <= plot_width - 1 .and. py >= 2 .and. py <= plot_height - 1) then
                if (canvas(py, px) == ' ') then
                    canvas(py, px) = line_char
                else if (canvas(py, px) /= line_char) then
                    canvas(py, px) = get_blend_char(canvas(py, px), line_char)
                end if
            end if
            
            x = x + step_x
            y = y + step_y
        end do
    end subroutine draw_line_on_canvas
    
    subroutine add_text_element(text_elements, num_text_elements, x, y, text, &
                               current_r, current_g, current_b, &
                               x_min, x_max, y_min, y_max, plot_width, plot_height)
        use fortplot_ascii_utils, only: text_element_t
        use fortplot_latex_parser, only: process_latex_in_text
        type(text_element_t), intent(inout) :: text_elements(:)
        integer, intent(inout) :: num_text_elements
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        real(wp), intent(in) :: current_r, current_g, current_b
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        integer, intent(in) :: plot_width, plot_height
        
        integer :: text_x, text_y
        character(len=500) :: processed_text
        integer :: processed_len
        
        ! Process LaTeX commands to Unicode
        call process_latex_in_text(text, processed_text, processed_len)
        ! Simplify mathtext braces for ASCII readability: 10^{3} -> 10^3
        call simplify_mathtext_for_ascii(processed_text(1:processed_len), processed_text, processed_len)
        
        ! Store text element for later rendering
        if (num_text_elements < size(text_elements)) then
            num_text_elements = num_text_elements + 1
            
            ! Convert coordinates - check if already in screen coordinates
            if (x >= 1.0_wp .and. x <= real(plot_width, wp) .and. &
                y >= 1.0_wp .and. y <= real(plot_height, wp)) then
                ! Already in screen coordinates (e.g., from legend)
                text_x = nint(x)
                text_y = nint(y)
            else
                ! Convert from data coordinates to canvas coordinates
                text_x = nint((x - x_min) / (x_max - x_min) * real(plot_width, wp))
                text_y = nint((y_max - y) / (y_max - y_min) * real(plot_height, wp))
            end if
            
            ! Clamp to canvas bounds
            ! For legend text (already in screen coordinates), don't truncate based on length
            if (x >= 1.0_wp .and. x <= real(plot_width, wp) .and. &
                y >= 1.0_wp .and. y <= real(plot_height, wp)) then
                ! For legend text, only clamp starting position, let text extend as needed
                text_x = max(1, min(text_x, plot_width))
            else
                ! For other text, prevent overflow
                text_x = max(1, min(text_x, plot_width - processed_len))
            end if
            text_y = max(1, min(text_y, plot_height))
            
            text_elements(num_text_elements)%text = processed_text(1:processed_len)
            text_elements(num_text_elements)%x = text_x
            text_elements(num_text_elements)%y = text_y
            text_elements(num_text_elements)%color_r = current_r
            text_elements(num_text_elements)%color_g = current_g
            text_elements(num_text_elements)%color_b = current_b
        end if
    end subroutine add_text_element

    subroutine simplify_mathtext_for_ascii(input, output, out_len)
        !! Convert simple mathtext like 10^{3} to 10^3 for ASCII output
        character(len=*), intent(in) :: input
        character(len=*), intent(inout) :: output
        integer, intent(inout) :: out_len
        integer :: i, j, n
        character(len=len(output)) :: tmp
        logical :: in_braces
        
        n = len_trim(input)
        i = 1
        j = 0
        in_braces = .false.
        tmp = ''
        do while (i <= n)
            if (input(i:i) == '^' .or. input(i:i) == '_') then
                if (i < n .and. input(i+1:i+1) == '{') then
                    j = j + 1; tmp(j:j) = input(i:i)
                    i = i + 2  ! skip ^ and opening {
                    do while (i <= n .and. input(i:i) /= '}')
                        j = j + 1
                        tmp(j:j) = input(i:i)
                        i = i + 1
                    end do
                    if (i <= n .and. input(i:i) == '}') then
                        i = i + 1  ! skip closing }
                    end if
                else
                    j = j + 1
                    tmp(j:j) = input(i:i)
                    i = i + 1
                end if
            else
                j = j + 1
                tmp(j:j) = input(i:i)
                i = i + 1
            end if
        end do
        output = tmp
        out_len = j
        if (j < len(output)) output(j+1:) = ' '
    end subroutine simplify_mathtext_for_ascii

    subroutine reset_ascii_legend_lines_helper(legend_lines, num_legend_lines)
        !! Reset ASCII legend lines array
        character(len=96), allocatable, intent(inout) :: legend_lines(:)
        integer, intent(inout) :: num_legend_lines

        if (.not. allocated(legend_lines)) then
            allocate(character(len=96) :: legend_lines(0))
        end if

        num_legend_lines = 0
    end subroutine reset_ascii_legend_lines_helper

    subroutine append_ascii_legend_line_helper(legend_lines, num_legend_lines, raw_line)
        !! Append a legend line to ASCII backend array
        character(len=96), allocatable, intent(inout) :: legend_lines(:)
        integer, intent(inout) :: num_legend_lines
        character(len=*), intent(in) :: raw_line

        integer :: current_size, new_size
        character(len=96), allocatable :: tmp(:)
        character(len=96) :: line_buffer

        if (.not. allocated(legend_lines)) then
            allocate(character(len=96) :: legend_lines(0))
        end if

        current_size = size(legend_lines)
        if (num_legend_lines == current_size) then
            new_size = max(4, max(1, current_size) * 2)
            allocate(tmp(new_size))
            tmp = ' '
            if (num_legend_lines > 0) tmp(1:num_legend_lines) = legend_lines
            call move_alloc(tmp, legend_lines)
        end if

        num_legend_lines = num_legend_lines + 1
        line_buffer = adjustl(raw_line)
        legend_lines(num_legend_lines) = line_buffer
    end subroutine append_ascii_legend_line_helper

    subroutine register_legend_entry_helper(legend_entry_indices, legend_entry_has_autopct, &
                                          legend_entry_labels, legend_entry_count, &
                                          legend_autopct_cursor, &
                                          line_idx, label, has_autopct)
        !! Register a legend entry for processing
        integer, allocatable, intent(inout) :: legend_entry_indices(:)
        logical, allocatable, intent(inout) :: legend_entry_has_autopct(:)
        character(len=64), allocatable, intent(inout) :: legend_entry_labels(:)
        integer, intent(inout) :: legend_entry_count, legend_autopct_cursor
        integer, intent(in) :: line_idx
        character(len=*), intent(in) :: label
        logical, intent(in) :: has_autopct

        integer :: current_size, new_size
        integer, allocatable :: idx_tmp(:)
        logical, allocatable :: flag_tmp(:)
        character(len=64), allocatable :: label_tmp(:)

        if (.not. allocated(legend_entry_indices)) then
            allocate(legend_entry_indices(0))
            allocate(legend_entry_has_autopct(0))
            allocate(legend_entry_labels(0))
        end if

        current_size = size(legend_entry_indices)
        if (legend_entry_count == current_size) then
            new_size = max(4, max(1, current_size) * 2)
            allocate(idx_tmp(new_size))
            idx_tmp = 0
            if (legend_entry_count > 0) then
                idx_tmp(1:legend_entry_count) = legend_entry_indices
            end if
            call move_alloc(idx_tmp, legend_entry_indices)

            allocate(flag_tmp(new_size))
            flag_tmp = .false.
            if (legend_entry_count > 0) then
                flag_tmp(1:legend_entry_count) = legend_entry_has_autopct
            end if
            call move_alloc(flag_tmp, legend_entry_has_autopct)

            allocate(label_tmp(new_size))
            label_tmp = ''
            if (legend_entry_count > 0) then
                label_tmp(1:legend_entry_count) = legend_entry_labels
            end if
            call move_alloc(label_tmp, legend_entry_labels)
        end if

        legend_entry_count = legend_entry_count + 1
        legend_entry_indices(legend_entry_count) = line_idx
        legend_entry_has_autopct(legend_entry_count) = has_autopct
        legend_entry_labels(legend_entry_count) = adjustl(trim(label))

        if (has_autopct) then
            if (legend_autopct_cursor == legend_entry_count) then
                legend_autopct_cursor = legend_autopct_cursor + 1
            end if
        end if
    end subroutine register_legend_entry_helper

    subroutine assign_pending_autopct_helper(legend_lines, num_legend_lines, &
                                           pie_autopct_queue, pie_autopct_count, &
                                           legend_entry_indices, legend_entry_has_autopct, &
                                           legend_autopct_cursor, legend_entry_count)
        !! Assign pending autopct values to legend lines
        character(len=96), allocatable, intent(inout) :: legend_lines(:)
        integer, intent(in) :: num_legend_lines
        character(len=32), allocatable, intent(inout) :: pie_autopct_queue(:)
        integer, intent(inout) :: pie_autopct_count
        integer, allocatable, intent(inout) :: legend_entry_indices(:)
        logical, allocatable, intent(inout) :: legend_entry_has_autopct(:)
        integer, intent(inout) :: legend_autopct_cursor
        integer, intent(in) :: legend_entry_count

        character(len=32) :: value
        integer :: target_idx

        do while (pie_autopct_count > 0 .and. legend_autopct_cursor <= legend_entry_count)
            if (legend_entry_has_autopct(legend_autopct_cursor)) then
                legend_autopct_cursor = legend_autopct_cursor + 1
                cycle
            end if

            ! Dequeue autopct value
            if (pie_autopct_count > 0) then
                value = pie_autopct_queue(1)
                ! Shift queue
                if (pie_autopct_count > 1) then
                    pie_autopct_queue(1:pie_autopct_count-1) = pie_autopct_queue(2:pie_autopct_count)
                end if
                pie_autopct_count = pie_autopct_count - 1
            else
                value = ''
            end if

            target_idx = legend_entry_indices(legend_autopct_cursor)
            call append_autopct_to_line_helper(legend_lines, num_legend_lines, target_idx, value)
            legend_entry_has_autopct(legend_autopct_cursor) = .true.
            legend_autopct_cursor = legend_autopct_cursor + 1
        end do
    end subroutine assign_pending_autopct_helper

    subroutine append_autopct_to_line_helper(legend_lines, num_legend_lines, line_idx, value)
        !! Append autopct value to a legend line
        character(len=96), allocatable, intent(inout) :: legend_lines(:)
        integer, intent(in) :: num_legend_lines
        integer, intent(in) :: line_idx
        character(len=*), intent(in) :: value

        character(len=96) :: updated_line

        if (len_trim(value) == 0) return
        if (line_idx < 1 .or. line_idx > num_legend_lines) return

        updated_line = trim(legend_lines(line_idx))
        if (index(updated_line, '(') > 0) then
            legend_lines(line_idx) = updated_line
        else
            updated_line = trim(updated_line) // ' (' // trim(value) // ')'
            legend_lines(line_idx) = adjustl(updated_line)
        end if
    end subroutine append_autopct_to_line_helper

    subroutine ascii_draw_text_helper(text_elements, num_text_elements, &
                                     legend_lines, num_legend_lines, capturing_legend, &
                                     pie_legend_labels, pie_legend_values, pie_legend_count, &
                                     pie_autopct_queue, pie_autopct_count, &
                                     legend_entry_indices, legend_entry_has_autopct, &
                                     legend_entry_labels, legend_entry_count, legend_autopct_cursor, &
                                     x, y, text, x_min, x_max, y_min, y_max, &
                                     plot_width, plot_height, current_r, current_g, current_b)
        !! ASCII text drawing with legend processing (moved from main module)
        type(text_element_t), intent(inout) :: text_elements(:)
        integer, intent(inout) :: num_text_elements
        character(len=96), allocatable, intent(inout) :: legend_lines(:)
        integer, intent(inout) :: num_legend_lines
        logical, intent(inout) :: capturing_legend
        character(len=64), allocatable, intent(inout) :: pie_legend_labels(:)
        character(len=32), allocatable, intent(inout) :: pie_legend_values(:)
        integer, intent(inout) :: pie_legend_count
        character(len=32), allocatable, intent(inout) :: pie_autopct_queue(:)
        integer, intent(inout) :: pie_autopct_count
        integer, allocatable, intent(inout) :: legend_entry_indices(:)
        logical, allocatable, intent(inout) :: legend_entry_has_autopct(:)
        character(len=64), allocatable, intent(inout) :: legend_entry_labels(:)
        integer, intent(inout) :: legend_entry_count, legend_autopct_cursor
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        integer, intent(in) :: plot_width, plot_height
        real(wp), intent(in) :: current_r, current_g, current_b
        
        integer :: text_x, text_y
        character(len=:), allocatable :: processed_text, trimmed_text
        character(len=96) :: formatted_line
        character(len=64) :: entry_label
        character(len=32) :: autopct_value

        trimmed_text = trim(adjustl(text))

        if (trimmed_text == 'ASCII Legend') then
            call reset_ascii_legend_lines_helper(legend_lines, num_legend_lines)
            call append_ascii_legend_line_helper(legend_lines, num_legend_lines, 'Legend:')
            capturing_legend = .true.
            legend_entry_count = 0
            legend_autopct_cursor = 1
            return
        end if

        if (is_autopct_text(trimmed_text)) then
            call enqueue_pie_autopct_helper(pie_autopct_queue, pie_autopct_count, trimmed_text)
            call assign_pending_autopct_helper(legend_lines, num_legend_lines, &
                                             pie_autopct_queue, pie_autopct_count, &
                                             legend_entry_indices, legend_entry_has_autopct, &
                                             legend_autopct_cursor, legend_entry_count)
            return
        end if

        if (capturing_legend) then
            if (len_trim(trimmed_text) == 0) then
                capturing_legend = .false.
                call clear_pie_legend_entries_helper(pie_legend_labels, pie_legend_values, &
                                                   pie_legend_count, pie_autopct_queue, pie_autopct_count)
                return
            end if

            if (.not. is_legend_entry_text(trimmed_text)) then
                capturing_legend = .false.
                return
            end if

            call decode_ascii_legend_line_helper(trimmed_text, formatted_line, entry_label)
            if (len_trim(entry_label) > 0 .and. len_trim(formatted_line) > 0) then
                autopct_value = ''
                if (pie_autopct_count > 0) then
                    autopct_value = dequeue_pie_autopct_helper(pie_autopct_queue, pie_autopct_count)
                else
                    autopct_value = get_pie_autopct_helper(pie_legend_labels, pie_legend_values, &
                                                         pie_legend_count, entry_label)
                end if
                if (len_trim(autopct_value) > 0) then
                    formatted_line = trim(formatted_line) // ' (' // trim(autopct_value) // ')'
                end if
                call append_ascii_legend_line_helper(legend_lines, num_legend_lines, trim(formatted_line))
                call register_legend_entry_helper(legend_entry_indices, legend_entry_has_autopct, &
                                               legend_entry_labels, legend_entry_count, &
                                               legend_autopct_cursor, &
                                               num_legend_lines, entry_label, len_trim(autopct_value) > 0)
                call assign_pending_autopct_helper(legend_lines, num_legend_lines, &
                                                 pie_autopct_queue, pie_autopct_count, &
                                                 legend_entry_indices, legend_entry_has_autopct, &
                                                 legend_autopct_cursor, legend_entry_count)
                return
            end if

            ! Non-legend content encountered; end capture and fall through to regular rendering
            capturing_legend = .false.
            call clear_pie_legend_entries_helper(pie_legend_labels, pie_legend_values, &
                                               pie_legend_count, pie_autopct_queue, pie_autopct_count)
        end if

        if (legend_entry_count > 0) then
            if (is_registered_legend_label(legend_entry_labels, legend_entry_count, trimmed_text)) return
        end if

        ! Store text element for later rendering
        if (num_text_elements < size(text_elements)) then
            num_text_elements = num_text_elements + 1
            
            call ascii_draw_text_primitive(text_x, text_y, processed_text, &
                                          x, y, text, &
                                          x_min, x_max, y_min, y_max, &
                                          plot_width, plot_height, &
                                          current_r, current_g, current_b)
            
            text_elements(num_text_elements)%text = processed_text
            text_elements(num_text_elements)%x = text_x
            text_elements(num_text_elements)%y = text_y
            text_elements(num_text_elements)%color_r = current_r
            text_elements(num_text_elements)%color_g = current_g
            text_elements(num_text_elements)%color_b = current_b
        end if
    end subroutine ascii_draw_text_helper

    ! Helper procedures for legend management
    
    subroutine enqueue_pie_autopct_helper(pie_autopct_queue, pie_autopct_count, value)
        character(len=32), allocatable, intent(inout) :: pie_autopct_queue(:)
        integer, intent(inout) :: pie_autopct_count
        character(len=*), intent(in) :: value

        integer :: current_size, new_size
        character(len=32), allocatable :: tmp(:)

        if (.not. allocated(pie_autopct_queue)) then
            allocate(character(len=32) :: pie_autopct_queue(0))
        end if

        current_size = size(pie_autopct_queue)
        if (pie_autopct_count == current_size) then
            new_size = max(4, max(1, current_size) * 2)
            allocate(tmp(new_size))
            tmp = ''
            if (pie_autopct_count > 0) tmp(1:pie_autopct_count) = pie_autopct_queue
            call move_alloc(tmp, pie_autopct_queue)
        end if

        pie_autopct_count = pie_autopct_count + 1
        pie_autopct_queue(pie_autopct_count) = adjustl(trim(value))
    end subroutine enqueue_pie_autopct_helper

    function dequeue_pie_autopct_helper(pie_autopct_queue, pie_autopct_count) result(value)
        character(len=32), allocatable, intent(inout) :: pie_autopct_queue(:)
        integer, intent(inout) :: pie_autopct_count
        character(len=32) :: value

        integer :: idx

        if (.not. allocated(pie_autopct_queue)) then
            allocate(character(len=32) :: pie_autopct_queue(0))
        end if

        if (pie_autopct_count <= 0) then
            value = ''
            return
        end if

        value = pie_autopct_queue(1)
        if (pie_autopct_count > 1) then
            do idx = 1, pie_autopct_count - 1
                pie_autopct_queue(idx) = pie_autopct_queue(idx + 1)
            end do
        end if
        pie_autopct_queue(pie_autopct_count) = ''
        pie_autopct_count = pie_autopct_count - 1
    end function dequeue_pie_autopct_helper

    subroutine clear_pie_legend_entries_helper(pie_legend_labels, pie_legend_values, &
                                             pie_legend_count, pie_autopct_queue, pie_autopct_count)
        character(len=64), allocatable, intent(inout) :: pie_legend_labels(:)
        character(len=32), allocatable, intent(inout) :: pie_legend_values(:)
        integer, intent(inout) :: pie_legend_count
        character(len=32), allocatable, intent(inout) :: pie_autopct_queue(:)
        integer, intent(inout) :: pie_autopct_count

        if (.not. allocated(pie_legend_labels)) then
            allocate(character(len=64) :: pie_legend_labels(0))
        else
            if (size(pie_legend_labels) > 0) pie_legend_labels = ''
        end if

        if (.not. allocated(pie_legend_values)) then
            allocate(character(len=32) :: pie_legend_values(0))
        else
            if (size(pie_legend_values) > 0) pie_legend_values = ''
        end if

        pie_legend_count = 0
        pie_autopct_count = 0
        if (allocated(pie_autopct_queue)) then
            if (size(pie_autopct_queue) > 0) pie_autopct_queue = ''
        end if
    end subroutine clear_pie_legend_entries_helper

    function get_pie_autopct_helper(pie_legend_labels, pie_legend_values, pie_legend_count, label) result(value)
        character(len=64), allocatable, intent(inout) :: pie_legend_labels(:)
        character(len=32), allocatable, intent(inout) :: pie_legend_values(:)
        integer, intent(in) :: pie_legend_count
        character(len=*), intent(in) :: label
        character(len=32) :: value

        integer :: i
        character(len=:), allocatable :: normalized

        value = ''
        if (.not. allocated(pie_legend_labels)) return
        if (pie_legend_count <= 0) return

        normalized = adjustl(trim(label))

        do i = 1, pie_legend_count
            if (adjustl(trim(pie_legend_labels(i))) == normalized) then
                value = trim(pie_legend_values(i))
                pie_legend_values(i) = ''
                return
            end if
        end do
    end function get_pie_autopct_helper

    subroutine decode_ascii_legend_line_helper(raw_text, formatted_line, entry_label)
        character(len=*), intent(in) :: raw_text
        character(len=96), intent(out) :: formatted_line
        character(len=64), intent(out) :: entry_label

        character(len=:), allocatable :: trimmed_text
        integer :: first_space

        formatted_line = ''
        entry_label = ''

        trimmed_text = trim(adjustl(raw_text))
        if (len_trim(trimmed_text) == 0) return

        if (len(trimmed_text) >= 3 .and. trimmed_text(1:3) == '-- ') then
            if (len(trimmed_text) > 3) then
                entry_label = trim(adjustl(trimmed_text(4:)))
            else
                entry_label = ''
            end if
            formatted_line = '  - ' // trim(entry_label)
        else
            formatted_line = '  ' // trim(trimmed_text)
            first_space = index(trimmed_text, ' ')
            if (first_space > 0 .and. first_space < len(trimmed_text)) then
                entry_label = trim(adjustl(trimmed_text(first_space + 1:)))
            else
                entry_label = trim(trimmed_text)
            end if
        end if
    end subroutine decode_ascii_legend_line_helper

end module fortplot_ascii_elements

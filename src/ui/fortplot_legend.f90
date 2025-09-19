module fortplot_legend
    !! Legend module following SOLID principles
    !!
    !! Single Responsibility: Legend rendering and positioning
    !! Open/Closed: Extensible legend types via interfaces
    !! Liskov Substitution: Legend renderers work across backends
    !! Interface Segregation: Focused legend interface
    !! Dependency Inversion: Depends on abstractions, not concrete backends
    
    use fortplot_context, only: plot_context
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    private
    public :: legend_t, legend_entry_t, create_legend, legend_render, render_ascii_legend, render_standard_legend
    public :: LEGEND_UPPER_LEFT, LEGEND_UPPER_RIGHT, LEGEND_LOWER_LEFT, LEGEND_LOWER_RIGHT
    
    ! Legend position constants
    integer, parameter :: LEGEND_UPPER_LEFT = 1
    integer, parameter :: LEGEND_UPPER_RIGHT = 2  
    integer, parameter :: LEGEND_LOWER_LEFT = 3
    integer, parameter :: LEGEND_LOWER_RIGHT = 4
    
    type :: legend_entry_t
        !! Single Responsibility: Represents one legend entry
        character(len=:), allocatable :: label
        real(wp), dimension(3) :: color
        character(len=:), allocatable :: linestyle
        character(len=:), allocatable :: marker
    end type legend_entry_t
    
    type :: legend_t
        !! Single Responsibility: Legend layout and rendering coordination
        type(legend_entry_t), allocatable :: entries(:)
        integer :: position = LEGEND_UPPER_RIGHT
        integer :: num_entries = 0
        real(wp) :: x_offset = 10.0_wp
        real(wp) :: y_offset = 10.0_wp
        real(wp) :: entry_height = 20.0_wp
        real(wp) :: line_length = 30.0_wp
        real(wp) :: text_padding = 10.0_wp
    contains
        procedure :: add_entry => legend_add_entry
        procedure :: render => legend_render
        procedure :: set_position => legend_set_position
        procedure :: clear => legend_clear
    end type legend_t
    
contains

    function create_legend() result(legend)
        !! Factory function following Dependency Inversion
        type(legend_t) :: legend
        
        allocate(legend%entries(0))
        legend%num_entries = 0
    end function create_legend
    
    
    subroutine legend_add_entry(this, label, color, linestyle, marker)
        !! Add entry following Open/Closed principle
        class(legend_t), intent(inout) :: this
        character(len=*), intent(in) :: label
        real(wp), dimension(3), intent(in) :: color
        character(len=*), intent(in), optional :: linestyle, marker
        type(legend_entry_t), allocatable :: temp_entries(:)
        integer :: new_size
        
        ! Expand entries array (DRY: could be extracted to utility)
        new_size = this%num_entries + 1
        allocate(temp_entries(new_size))
        
        if (this%num_entries > 0) then
            temp_entries(1:this%num_entries) = this%entries
        end if
        
        ! Add new entry
        temp_entries(new_size)%label = label
        temp_entries(new_size)%color = color
        if (present(linestyle)) then
            temp_entries(new_size)%linestyle = linestyle
        else
            temp_entries(new_size)%linestyle = "-"
        end if
        if (present(marker)) then
            temp_entries(new_size)%marker = marker
        else
            temp_entries(new_size)%marker = "None"
        end if
        
        ! Replace entries array
        call move_alloc(temp_entries, this%entries)
        this%num_entries = new_size
    end subroutine legend_add_entry
    
    subroutine legend_clear(this)
        !! Clear all legend entries
        class(legend_t), intent(inout) :: this
        
        if (allocated(this%entries)) then
            deallocate(this%entries)
        end if
        allocate(this%entries(0))
        this%num_entries = 0
    end subroutine legend_clear
    
    subroutine legend_set_position(this, location)
        !! Set legend position using string interface
        class(legend_t), intent(inout) :: this
        character(len=*), intent(in) :: location
        
        select case (trim(location))
        case ("upper left")
            this%position = LEGEND_UPPER_LEFT
        case ("upper right")
            this%position = LEGEND_UPPER_RIGHT
        case ("lower left") 
            this%position = LEGEND_LOWER_LEFT
        case ("lower right")
            this%position = LEGEND_LOWER_RIGHT
        case default
            this%position = LEGEND_UPPER_RIGHT  ! Default
        end select
    end subroutine legend_set_position
    
    subroutine legend_render(this, backend)
        !! Render legend following Liskov Substitution
        !! Works with any backend that implements plot_context interface
        class(legend_t), intent(in) :: this
        class(plot_context), intent(inout) :: backend
        real(wp) :: legend_x, legend_y
        
        if (this%num_entries == 0) return
        
        ! Calculate legend position based on backend dimensions
        call calculate_legend_position(this, backend, legend_x, legend_y)
        
        ! Render legend based on backend type  
        ! ASCII backends use compact layout, others use standard
        if (backend%width <= 80 .and. backend%height <= 24) then
            ! ASCII-like dimensions, use compact layout
            call render_ascii_legend(this, backend, legend_x, legend_y)
        else
            ! Standard legend rendering for other backends
            call render_standard_legend(this, backend, legend_x, legend_y)
        end if
    end subroutine legend_render
    
    subroutine render_ascii_legend(legend, backend, legend_x, legend_y)
        !! Render compact ASCII legend with proper formatting
        type(legend_t), intent(in) :: legend
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: legend_x, legend_y
        integer :: i
        real(wp) :: text_x, text_y
        character(len=20) :: legend_line

        ! Optional header for better readability in ASCII output
        text_x = legend_x
        text_y = max(1.0_wp, min(legend_y - 1.0_wp, real(28, wp)))
        call backend%color(0.0_wp, 0.0_wp, 0.0_wp)
        call backend%text(text_x, text_y, 'Legend')

        do i = 1, legend%num_entries
            ! For ASCII, arrange entries vertically going downward
            text_x = legend_x
            text_y = legend_y + real(i-1, wp)  ! Go downward: y+0, y+1, y+2, etc.
            
            ! Ensure text fits within canvas bounds  
            text_y = max(1.0_wp, min(text_y, real(28, wp)))  ! Leave space for bottom border
            
            ! Set color for this entry
            call backend%color(legend%entries(i)%color(1), &
                              legend%entries(i)%color(2), &
                              legend%entries(i)%color(3))
            
            ! Create legend entry with actual marker character or line symbol
            if (allocated(legend%entries(i)%marker) .and. &
                trim(legend%entries(i)%marker) /= '' .and. &
                trim(legend%entries(i)%marker) /= 'None') then
                ! Show marker character
                legend_line = get_ascii_marker_char(legend%entries(i)%marker) // " " // trim(legend%entries(i)%label)
            else
                ! Show line symbol for line-only plots
                legend_line = "-- " // trim(legend%entries(i)%label)
            end if
            call backend%text(text_x, text_y, legend_line)
        end do
    end subroutine render_ascii_legend
    
    subroutine render_standard_legend(legend, backend, legend_x, legend_y)
        !! Render standard legend for PNG/PDF backends with improved sizing
        !! Refactored to be under 100 lines (QADS compliance)
        use fortplot_legend_layout, only: legend_box_t, calculate_legend_box
        use fortplot_text, only: calculate_text_height, get_font_ascent_ratio
        type(legend_t), intent(in) :: legend
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: legend_x, legend_y
        
        type(legend_box_t) :: box
        character(len=:), allocatable :: labels(:)
        real(wp) :: data_width, data_height
        
        ! Initialize legend rendering
        call initialize_legend_rendering(legend, backend, box, labels, data_width, data_height)
        
        ! Draw legend box and border
        call draw_legend_frame(backend, legend_x, legend_y, box)
        
        ! Render all legend entries
        call render_legend_entries(legend, backend, legend_x, legend_y, box)
        
        if (allocated(labels)) deallocate(labels)
    end subroutine render_standard_legend
    
    subroutine initialize_legend_rendering(legend, backend, box, labels, data_width, data_height)
        !! Initialize legend rendering components
        use fortplot_legend_layout, only: legend_box_t, calculate_legend_box
        use fortplot_layout, only: plot_margins_t, plot_area_t, calculate_plot_area
        type(legend_t), intent(in) :: legend
        class(plot_context), intent(in) :: backend
        type(legend_box_t), intent(out) :: box
        character(len=:), allocatable, intent(out) :: labels(:)
        real(wp), intent(out) :: data_width, data_height

        integer :: i
        type(plot_margins_t) :: margins
        type(plot_area_t) :: plot_area
        integer :: px_w, px_h
        
        ! Extract labels for box calculation (no truncation). Allocate to max length.
        allocate(character(len=256) :: labels(legend%num_entries))
        do i = 1, legend%num_entries
            labels(i) = legend%entries(i)%label
        end do
        
        ! Calculate data coordinate dimensions
        data_width = backend%x_max - backend%x_min
        data_height = backend%y_max - backend%y_min

        ! Determine actual plot-area pixel dimensions to get exact text/data scaling
        call calculate_plot_area(backend%width, backend%height, margins, plot_area)
        px_w = max(1, plot_area%width)
        px_h = max(1, plot_area%height)

        ! Use shared layout calculation with real pixel scale for precise sizing
        box = calculate_legend_box(labels, data_width, data_height, &
                                  legend%num_entries, legend%position, px_w, px_h)
    end subroutine initialize_legend_rendering
    
    subroutine draw_legend_frame(backend, legend_x, legend_y, box)
        !! Draw legend background box and border
        use fortplot_legend_layout, only: legend_box_t
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: legend_x, legend_y
        type(legend_box_t), intent(in) :: box
        
        real(wp) :: box_x1, box_y1, box_x2, box_y2
        
        ! Calculate box boundaries
        box_x1 = legend_x
        box_y1 = legend_y
        box_x2 = box_x1 + box%width
        box_y2 = box_y1 - box%height
        
        ! Draw white background box
        call backend%color(1.0_wp, 1.0_wp, 1.0_wp)  ! White background
        call draw_legend_box(backend, box_x1, box_y1, box_x2, box_y2)
        
        ! Draw black border
        call backend%color(0.0_wp, 0.0_wp, 0.0_wp)  ! Black border
        call draw_legend_border(backend, box_x1, box_y1, box_x2, box_y2)
    end subroutine draw_legend_frame
    
    subroutine render_legend_entries(legend, backend, legend_x, legend_y, box)
        !! Render all legend entries (lines, markers, text)
        use fortplot_legend_layout, only: legend_box_t
        use fortplot_text, only: get_font_ascent_ratio
        type(legend_t), intent(in) :: legend
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: legend_x, legend_y
        type(legend_box_t), intent(in) :: box
        
        real(wp) :: ascent_ratio, line_x1, line_x2, line_center_y, text_x, text_baseline
        integer :: i

        ! Get font ascent ratio for proper text centering
        ascent_ratio = get_font_ascent_ratio()

        do i = 1, legend%num_entries
            ! Calculate positions for this entry
            call calculate_entry_positions(legend_x, legend_y, box, ascent_ratio, i, &
                                         line_x1, line_x2, line_center_y, text_x, &
                                         text_baseline)

            ! Render entry components
            call render_legend_line(legend%entries(i), backend, line_x1, line_x2, &
                                    line_center_y)
            call render_legend_marker(legend%entries(i), backend, line_x1, line_x2, &
                                      line_center_y)
            call render_legend_text(legend%entries(i), backend, text_x, text_baseline)
        end do
    end subroutine render_legend_entries

    subroutine calculate_entry_positions(legend_x, legend_y, box, ascent_ratio, entry_idx, &
                                         line_x1, line_x2, line_center_y, text_x, &
                                         text_baseline)
        !! Calculate positions for legend entry components
        use fortplot_legend_layout, only: legend_box_t
        real(wp), intent(in) :: legend_x, legend_y, ascent_ratio
        type(legend_box_t), intent(in) :: box
        integer, intent(in) :: entry_idx
        real(wp), intent(out) :: line_x1, line_x2, line_center_y, text_x, text_baseline
        real(wp) :: entry_stride, entry_top_y, entry_baseline, entry_offset

        ! Calculate line position
        line_x1 = legend_x + box%padding_x
        line_x2 = line_x1 + box%line_length

        entry_stride = box%entry_height + box%entry_spacing
        entry_top_y = legend_y - box%padding - real(entry_idx - 1, wp) * entry_stride

        entry_baseline = entry_top_y - ascent_ratio * box%entry_height
        entry_offset = (ascent_ratio - 0.5_wp) * box%entry_height

        line_center_y = entry_baseline + entry_offset

        ! Text positioning (baseline coordinates)
        text_x = line_x2 + box%text_spacing
        text_baseline = entry_baseline
    end subroutine calculate_entry_positions
    
    subroutine render_legend_line(entry, backend, line_x1, line_x2, line_center_y)
        !! Render legend line for entry
        type(legend_entry_t), intent(in) :: entry
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: line_x1, line_x2, line_center_y
        
        ! Set color
        call backend%color(entry%color(1), entry%color(2), entry%color(3))
        
        ! Draw line if style permits (treat 'none'/'None' as no line)
        if (allocated(entry%linestyle)) then
            if (trim(entry%linestyle) /= 'None' .and. trim(entry%linestyle) /= 'none') then
                ! Set the line style before drawing the line
                call backend%set_line_style(entry%linestyle)
                call backend%line(line_x1, line_center_y, line_x2, line_center_y)
            end if
        else
            ! Default to solid line style if not specified
            call backend%set_line_style('-')
            call backend%line(line_x1, line_center_y, line_x2, line_center_y)
        end if
    end subroutine render_legend_line
    
    subroutine render_legend_marker(entry, backend, line_x1, line_x2, line_center_y)
        !! Render legend marker for entry
        type(legend_entry_t), intent(in) :: entry
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: line_x1, line_x2, line_center_y
        
        ! Draw marker in the middle of the line, using entry color
        if (allocated(entry%marker)) then
            if (trim(entry%marker) /= 'None' .and. trim(entry%marker) /= 'none' .and. &
                len_trim(entry%marker) > 0) then
                call backend%set_marker_colors(entry%color(1), entry%color(2), entry%color(3), &
                                               entry%color(1), entry%color(2), entry%color(3))
                call backend%draw_marker((line_x1 + line_x2) / 2.0_wp, &
                                        line_center_y, entry%marker)
            end if
        end if
    end subroutine render_legend_marker
    
    subroutine render_legend_text(entry, backend, text_x, text_y)
        !! Render legend text for entry
        type(legend_entry_t), intent(in) :: entry
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: text_x, text_y
        
        ! Draw legend text in black
        call backend%color(0.0_wp, 0.0_wp, 0.0_wp)  ! Black text
        call backend%text(text_x, text_y, entry%label)
    end subroutine render_legend_text
    
    subroutine calculate_legend_position(legend, backend, x, y)
        !! Calculate legend position based on backend and position setting
        !! Interface Segregation: Only depends on backend dimensions
        use fortplot_legend_layout, only: legend_box_t, calculate_legend_box
        use fortplot_layout, only: plot_margins_t, plot_area_t, calculate_plot_area
        type(legend_t), intent(in) :: legend
        class(plot_context), intent(in) :: backend
        real(wp), intent(out) :: x, y
        real(wp) :: data_width, data_height
        type(legend_box_t) :: box
        character(len=:), allocatable :: labels(:)
        integer :: i
        type(plot_margins_t) :: margins
        type(plot_area_t) :: plot_area
        integer :: px_w, px_h
        
        ! Get data coordinate ranges
        data_width = backend%x_max - backend%x_min
        data_height = backend%y_max - backend%y_min
        
        ! Calculate position based on backend dimensions
        ! ASCII backends have different positioning logic
        if (backend%width <= 80 .and. backend%height <= 24) then
            ! ASCII-like dimensions, position at top-right corner in data coords
            ! Use proportional positioning in data space
            x = backend%x_min + 0.8_wp * data_width
            y = backend%y_min + 0.95_wp * data_height
            ! Note: labels not used in ASCII path, but must be declared due to Fortran scoping
        else
            ! Standard backends with margin support
            allocate(character(len=256) :: labels(legend%num_entries))
            do i = 1, legend%num_entries
                labels(i) = legend%entries(i)%label
            end do
            
            ! Pass data coordinate dimensions, not pixel dimensions
            call calculate_plot_area(backend%width, backend%height, margins, plot_area)
            px_w = max(1, plot_area%width)
            px_h = max(1, plot_area%height)

            box = calculate_legend_box(labels, data_width, data_height, &
                                     legend%num_entries, legend%position, px_w, px_h)
            
            ! Box positions are already in data coordinates relative to origin
            ! Need to add the backend's minimum values to get absolute positions
            x = backend%x_min + box%x
            y = backend%y_min + box%y
            
            if (allocated(labels)) deallocate(labels)
        end if
    end subroutine calculate_legend_position

    subroutine draw_legend_box(backend, x1, y1, x2, y2)
        !! Draw filled white rectangle for legend background
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: x1, y1, x2, y2
        
        ! Draw filled rectangle by drawing horizontal lines
        real(wp) :: y, dy
        integer :: num_lines, i
        
        ! Ensure legend background is rendered with solid style and white fill
        call backend%set_line_style('-')
        call backend%color(1.0_wp, 1.0_wp, 1.0_wp)

        num_lines = 50  ! Number of horizontal lines to fill the box
        dy = abs(y1 - y2) / real(num_lines, wp)
        
        do i = 0, num_lines
            y = y1 - real(i, wp) * dy
            call backend%line(x1, y, x2, y)
        end do
    end subroutine draw_legend_box

    subroutine draw_legend_border(backend, x1, y1, x2, y2)
        !! Draw thin border around legend box matching axes frame style
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: x1, y1, x2, y2
        
        ! Set border width based on backend type (thinner for high-res backends)
        if (backend%width > 80 .or. backend%height > 24) then
            call backend%set_line_width(0.5_wp)  ! Thin border for PNG/PDF
        end if
        ! Border must be solid regardless of prior plot line style
        call backend%set_line_style('-')
        ! Ensure border color is black
        call backend%color(0.0_wp, 0.0_wp, 0.0_wp)
        
        ! Draw rectangle border
        call backend%line(x1, y1, x2, y1)  ! Top
        call backend%line(x2, y1, x2, y2)  ! Right  
        call backend%line(x2, y2, x1, y2)  ! Bottom
        call backend%line(x1, y2, x1, y1)  ! Left
    end subroutine draw_legend_border

    pure function get_ascii_marker_char(marker_style) result(marker_char)
        !! Convert marker style to ASCII character (same logic as ascii backend)
        character(len=*), intent(in) :: marker_style
        character(len=1) :: marker_char
        
        ! Map marker styles to distinct ASCII characters for visual differentiation
        select case (trim(marker_style))
        case ('o')
            marker_char = 'o'  ! Circle
        case ('s')
            marker_char = '#'  ! Square
        case ('D', 'd')
            marker_char = '%'  ! Diamond
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
    end function get_ascii_marker_char

end module fortplot_legend

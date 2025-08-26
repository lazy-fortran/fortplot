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
        real(wp) :: legend_x, legend_y, line_x1, line_x2, line_y
        real(wp) :: text_x, text_y
        integer :: i
        
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
        !! Uses shared legend_layout module for DRY compliance
        use fortplot_legend_layout, only: legend_box_t, calculate_legend_box
        use fortplot_text, only: calculate_text_height, get_font_ascent_ratio
        type(legend_t), intent(in) :: legend
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: legend_x, legend_y
        real(wp) :: line_x1, line_x2, line_y, text_x, text_y, text_offset
        real(wp) :: data_width, data_height
        real(wp) :: box_x1, box_y1, box_x2, box_y2
        real(wp) :: ascent_ratio, line_center_y
        real(wp) :: actual_text_height, data_to_pixel_y
        type(legend_box_t) :: box
        character(len=:), allocatable :: labels(:)
        integer :: i
        
        ! Extract labels for box calculation
        allocate(character(len=50) :: labels(legend%num_entries))
        do i = 1, legend%num_entries
            labels(i) = legend%entries(i)%label
        end do
        
        ! Calculate data coordinate dimensions
        data_width = backend%x_max - backend%x_min
        data_height = backend%y_max - backend%y_min
        
        ! Use shared layout calculation for improved sizing
        box = calculate_legend_box(labels, data_width, data_height, &
                                  legend%num_entries, legend%position)
        
        ! legend_x, legend_y is the top-left corner of the legend box
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
        
        ! Get font ascent ratio for proper text centering
        ascent_ratio = get_font_ascent_ratio()
        
        ! Calculate data to pixel ratio for accurate text positioning
        data_to_pixel_y = 369.6_wp / data_height
        ! Get actual text height in pixels (typically ~14-16 pixels)
        actual_text_height = real(calculate_text_height("Ay"), wp) / data_to_pixel_y
        
        do i = 1, legend%num_entries
            ! Calculate line position using entry height + spacing
            ! Content starts at padding distance from box edges
            line_x1 = legend_x + box%padding
            line_x2 = line_x1 + box%line_length
            ! First entry starts at top - padding - enough space for text ascent
            ! Account for text extending upward from baseline by ascent_ratio * entry_height
            line_y = legend_y - box%padding - ascent_ratio * box%entry_height - &
                     real(i-1, wp) * (box%entry_height + box%entry_spacing)
            
            ! Text positioning with proper vertical alignment
            text_x = line_x2 + box%text_spacing
            ! Text should be vertically centered on the line
            ! In PNG backend, text_y is the BASELINE of the text
            ! Text extends mostly upward from baseline (ascent ~70% of height)
            ! To visually center: baseline should be at line_y + descent_portion
            text_y = line_y + (1.0_wp - ascent_ratio) * box%entry_height * 0.3_wp
            
            line_center_y = line_y
            
            ! Set color and draw legend line (only if linestyle is not None)
            call backend%color(legend%entries(i)%color(1), &
                              legend%entries(i)%color(2), &
                              legend%entries(i)%color(3))
            if (allocated(legend%entries(i)%linestyle)) then
                if (legend%entries(i)%linestyle /= 'None') then
                    call backend%line(line_x1, line_center_y, line_x2, line_center_y)
                end if
            else
                ! Default to drawing line if linestyle not specified
                call backend%line(line_x1, line_center_y, line_x2, line_center_y)
            end if

            ! Draw marker in the middle of the line (on the line)
            if (allocated(legend%entries(i)%marker)) then
                if (legend%entries(i)%marker /= 'None') then
                    call backend%draw_marker((line_x1 + line_x2) / 2.0_wp, &
                                            line_center_y, &
                                            legend%entries(i)%marker)
                end if
            end if
            
            ! Draw legend text in black
            call backend%color(0.0_wp, 0.0_wp, 0.0_wp)  ! Black text
            call backend%text(text_x, text_y, legend%entries(i)%label)
        end do
        
        deallocate(labels)
    end subroutine render_standard_legend
    
    subroutine calculate_legend_position(legend, backend, x, y)
        !! Calculate legend position based on backend and position setting
        !! Interface Segregation: Only depends on backend dimensions
        use fortplot_legend_layout, only: legend_box_t, calculate_legend_box
        type(legend_t), intent(in) :: legend
        class(plot_context), intent(in) :: backend
        real(wp), intent(out) :: x, y
        real(wp) :: data_width, data_height
        type(legend_box_t) :: box
        character(len=:), allocatable :: labels(:)
        integer :: i
        
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
        else
            ! Standard backends with margin support
            allocate(character(len=20) :: labels(legend%num_entries))
            do i = 1, legend%num_entries
                labels(i) = legend%entries(i)%label
            end do
            
            ! Pass data coordinate dimensions, not pixel dimensions
            box = calculate_legend_box(labels, data_width, data_height, &
                                     legend%num_entries, legend%position)
            
            ! Box positions are already in data coordinates relative to origin
            ! Need to add the backend's minimum values to get absolute positions
            x = backend%x_min + box%x
            y = backend%y_min + box%y
            
            deallocate(labels)
        end if
    end subroutine calculate_legend_position

    subroutine draw_legend_box(backend, x1, y1, x2, y2)
        !! Draw filled white rectangle for legend background
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: x1, y1, x2, y2
        
        ! Draw filled rectangle by drawing horizontal lines
        real(wp) :: y, dy
        integer :: num_lines, i
        
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
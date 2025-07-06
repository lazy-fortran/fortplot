module fortplot_legend_layout
    !! Shared legend layout calculations following DRY principle
    !! 
    !! Single Responsibility: Legend box sizing and positioning calculations
    !! DRY: Centralized legend layout logic for consistent behavior
    !! KISS: Simple, clear calculation functions
    !! Uses text system measurements for accurate sizing
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_text, only: calculate_text_width, calculate_text_height, init_text_system
    implicit none
    
    private
    public :: legend_box_t, calculate_legend_box, calculate_optimal_legend_dimensions
    public :: get_actual_text_dimensions, get_legend_margins
    
    type :: legend_box_t
        !! Single Responsibility: Legend box dimensions and position
        real(wp) :: x, y              ! Top-left corner position
        real(wp) :: width, height     ! Box dimensions  
        real(wp) :: padding           ! Internal padding
        real(wp) :: entry_height      ! Height per legend entry
        real(wp) :: line_length       ! Length of legend line samples
        real(wp) :: text_spacing      ! Space between line and text
    end type legend_box_t
    
contains

    function calculate_legend_box(labels, data_width, data_height, num_entries, position) result(box)
        !! Calculate optimal legend box dimensions and position
        !! DRY: Centralized legend box calculation logic
        character(len=*), intent(in) :: labels(:)
        real(wp), intent(in) :: data_width, data_height
        integer, intent(in) :: num_entries, position
        type(legend_box_t) :: box
        real(wp) :: max_text_width, total_text_width, margins(2)
        
        if (num_entries == 0) then
            box%width = 0.0_wp
            box%height = 0.1_wp
            return
        end if
        
        ! Calculate optimal dimensions based on content
        call calculate_optimal_legend_dimensions(labels, data_width, data_height, &
                                                max_text_width, total_text_width, box)
        
        ! Get appropriate margins for this backend
        margins = get_legend_margins(data_width, data_height)
        
        ! Calculate position based on legend location
        call calculate_legend_position(box, data_width, data_height, position, margins)
        
    end function calculate_legend_box
    
    subroutine calculate_optimal_legend_dimensions(labels, data_width, data_height, &
                                                  max_text_width, total_text_width, box)
        !! Calculate optimal legend dimensions using actual text system measurements
        !! KISS: Based on measured text content, not estimates
        character(len=*), intent(in) :: labels(:)
        real(wp), intent(in) :: data_width, data_height
        real(wp), intent(out) :: max_text_width, total_text_width
        type(legend_box_t), intent(inout) :: box
        integer :: i, text_width_pixels, text_height_pixels, max_text_height_pixels
        real(wp) :: data_to_pixel_ratio_x, data_to_pixel_ratio_y
        logical :: text_system_available
        real(wp) :: entry_text_width
        character(len=:), allocatable :: trimmed_label
        integer, parameter :: fudge_pixels = 2

        ! Initialize text system for measurements
        text_system_available = init_text_system()

        ! Calculate data-to-pixel conversion ratio (approximate)
        data_to_pixel_ratio_x = 640.0_wp / data_width  
        data_to_pixel_ratio_y = 480.0_wp / data_height

        max_text_width = 0.0_wp
        total_text_width = 0.0_wp
        max_text_height_pixels = 16  ! Default font height

        do i = 1, size(labels)
            trimmed_label = trim(labels(i))
            if (text_system_available) then
                ! Use actual text system measurements, add fudge factor for overhang
                text_width_pixels = calculate_text_width(trimmed_label) + fudge_pixels
                text_height_pixels = calculate_text_height(trimmed_label)
                max_text_height_pixels = max(max_text_height_pixels, text_height_pixels)
                entry_text_width = real(text_width_pixels, wp) / data_to_pixel_ratio_x
            else
                ! Fallback estimation if text system not available
                entry_text_width = real(len_trim(trimmed_label), wp) * data_width * 0.012_wp
            end if
            total_text_width = total_text_width + entry_text_width
            max_text_width = max(max_text_width, entry_text_width)
        end do

        ! Set legend box components based on actual measurements (matplotlib-style spacing)
        ! Matplotlib uses borderpad as internal padding in font-size units
        ! Default matplotlib borderpad is 0.4 fontsize units (~6.4 pixels for 16px font)
        box%padding = 8.0_wp / data_to_pixel_ratio_x      ! 8 pixels padding (matplotlib-style)
        box%line_length = 20.0_wp / data_to_pixel_ratio_x  ! 20 pixels for legend line 
        box%text_spacing = 6.0_wp / data_to_pixel_ratio_x  ! 6 pixels between line and text

        ! Calculate entry height based on actual text height with proper spacing
        ! Matplotlib uses labelspacing (default 0.5) as fraction of fontsize between entries
        box%entry_height = real(max_text_height_pixels, wp) * 1.3_wp / data_to_pixel_ratio_y  ! 1.3x text height for spacing

        ! Calculate total box dimensions with proper padding
        ! Width: padding + line + spacing + text + padding (both sides)
        box%width = 2.0_wp * box%padding + box%line_length + box%text_spacing + max_text_width
        ! Height: padding + ((n-1) entries * entry_height) + last entry text height + padding
        ! The last entry doesn't need full entry_height spacing below it
        box%height = 2.0_wp * box%padding + real(size(labels) - 1, wp) * box%entry_height + &
                     real(max_text_height_pixels, wp) / data_to_pixel_ratio_y

    end subroutine calculate_optimal_legend_dimensions
    
    function get_actual_text_dimensions(label, data_to_pixel_x, data_to_pixel_y) result(dimensions)
        !! Get actual text dimensions using text system measurements  
        !! Returns [width, height] in data coordinates
        character(len=*), intent(in) :: label
        real(wp), intent(in) :: data_to_pixel_x, data_to_pixel_y
        real(wp) :: dimensions(2)  ! [width, height]
        integer :: width_pixels, height_pixels
        logical :: text_system_available
        
        text_system_available = init_text_system()
        
        if (text_system_available) then
            width_pixels = calculate_text_width(label)
            height_pixels = calculate_text_height(label)
            dimensions(1) = real(width_pixels, wp) / data_to_pixel_x
            dimensions(2) = real(height_pixels, wp) / data_to_pixel_y
        else
            ! Fallback estimation
            dimensions(1) = real(len_trim(label), wp) * 8.0_wp / data_to_pixel_x  ! 8 pixels per char
            dimensions(2) = 16.0_wp / data_to_pixel_y  ! 16 pixels height
        end if
        
    end function get_actual_text_dimensions
    
    function get_legend_margins(data_width, data_height) result(margins)
        !! Get appropriate margins for legend placement
        !! DRY: Centralized margin calculation
        real(wp), intent(in) :: data_width, data_height
        real(wp) :: margins(2)  ! [x_margin, y_margin]
        
        ! Professional margins similar to matplotlib
        margins(1) = data_width * 0.08_wp    ! 8% horizontal margin  
        margins(2) = data_height * 0.08_wp   ! 8% vertical margin
        
    end function get_legend_margins
    
    subroutine calculate_legend_position(box, data_width, data_height, position, margins)
        !! Calculate legend position based on placement preference
        !! Single Responsibility: Position calculation only
        type(legend_box_t), intent(inout) :: box
        real(wp), intent(in) :: data_width, data_height, margins(2)
        integer, intent(in) :: position
        
        ! Local constants to avoid circular dependency
        integer, parameter :: LEGEND_UPPER_LEFT = 1
        integer, parameter :: LEGEND_UPPER_RIGHT = 2  
        integer, parameter :: LEGEND_LOWER_LEFT = 3
        integer, parameter :: LEGEND_LOWER_RIGHT = 4
        
        select case (position)
        case (LEGEND_UPPER_LEFT)
            box%x = margins(1)
            box%y = data_height - margins(2)
        case (LEGEND_UPPER_RIGHT)
            box%x = data_width - box%width - margins(1)
            box%y = data_height - margins(2)
        case (LEGEND_LOWER_LEFT)
            box%x = margins(1)
            box%y = box%height + margins(2)
        case (LEGEND_LOWER_RIGHT)
            box%x = data_width - box%width - margins(1)
            box%y = box%height + margins(2)
        case default  ! Default to upper right
            box%x = data_width - box%width - margins(1)
            box%y = data_height - margins(2)
        end select
        
    end subroutine calculate_legend_position

end module fortplot_legend_layout
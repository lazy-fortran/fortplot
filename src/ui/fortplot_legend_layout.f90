module fortplot_legend_layout
    !! Shared legend layout calculations following DRY principle
    !! 
    !! Single Responsibility: Legend box sizing and positioning calculations
    !! DRY: Centralized legend layout logic for consistent behavior
    !! KISS: Simple, clear calculation functions
    !! Uses text system measurements for accurate sizing
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_text, only: calculate_text_width, calculate_text_height, init_text_system
    use fortplot_constants, only: STANDARD_WIDTH_PIXELS, STANDARD_HEIGHT_PIXELS, TEXT_WIDTH_RATIO
    use fortplot_latex_parser, only: process_latex_in_text
    implicit none
    
    private
    public :: legend_box_t, calculate_legend_box, calculate_optimal_legend_dimensions
    public :: get_actual_text_dimensions, get_legend_margins
    
    type :: legend_box_t
        !! Single Responsibility: Legend box dimensions and position
        real(wp) :: x, y              ! Top-left corner position
        real(wp) :: width, height     ! Box dimensions  
        real(wp) :: padding           ! Internal padding
        real(wp) :: entry_height      ! Height of each legend entry (text height)
        real(wp) :: entry_spacing     ! Vertical spacing between entries
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
        
        real(wp) :: data_to_pixel_ratio_x, data_to_pixel_ratio_y
        integer :: max_text_height_pixels
        logical :: text_system_available
        
        ! Initialize text system for measurements
        text_system_available = init_text_system()
        
        ! Calculate data-to-pixel conversion ratio
        data_to_pixel_ratio_x = STANDARD_WIDTH_PIXELS / data_width
        data_to_pixel_ratio_y = STANDARD_HEIGHT_PIXELS / data_height
        
        ! Measure text dimensions
        call measure_label_dimensions(labels, text_system_available, data_to_pixel_ratio_x, &
                                     data_width, max_text_width, total_text_width, &
                                     max_text_height_pixels)
        
        ! Calculate box dimensions based on measurements
        call set_legend_box_dimensions(box, max_text_width, max_text_height_pixels, &
                                      data_to_pixel_ratio_x, data_to_pixel_ratio_y, &
                                      size(labels))
    end subroutine calculate_optimal_legend_dimensions
    
    subroutine measure_label_dimensions(labels, text_system_available, data_to_pixel_ratio_x, &
                                       data_width, max_text_width, total_text_width, &
                                       max_text_height_pixels)
        !! Measure text dimensions for all labels
        character(len=*), intent(in) :: labels(:)
        logical, intent(in) :: text_system_available
        real(wp), intent(in) :: data_to_pixel_ratio_x, data_width
        real(wp), intent(out) :: max_text_width, total_text_width
        integer, intent(out) :: max_text_height_pixels
        
        integer :: i, text_width_pixels, text_height_pixels
        real(wp) :: entry_text_width
        character(len=:), allocatable :: trimmed_label, processed_label
        character(len=512) :: temp_processed_label
        integer :: processed_len
        integer, parameter :: fudge_pixels = 2
        
        max_text_width = 0.0_wp
        total_text_width = 0.0_wp
        max_text_height_pixels = 16  ! Default font height
        
        do i = 1, size(labels)
            trimmed_label = trim(labels(i))
            
            ! Process LaTeX commands to get actual Unicode characters for width calculation
            call process_latex_in_text(trimmed_label, temp_processed_label, processed_len)
            processed_label = temp_processed_label(1:processed_len)
            
            if (text_system_available) then
                text_width_pixels = calculate_text_width(processed_label) + fudge_pixels
                text_height_pixels = calculate_text_height(processed_label)
                max_text_height_pixels = max(max_text_height_pixels, text_height_pixels)
                entry_text_width = real(text_width_pixels, wp) / data_to_pixel_ratio_x
                ! Add extra width for complex mathematical expressions that may render wider than calculated
                entry_text_width = entry_text_width * 1.8_wp  ! 80% extra width for math text
            else
                entry_text_width = real(len_trim(processed_label), wp) * data_width * TEXT_WIDTH_RATIO
                entry_text_width = entry_text_width * 1.8_wp  ! 80% extra width for math text
            end if
            total_text_width = total_text_width + entry_text_width
            max_text_width = max(max_text_width, entry_text_width)
        end do
    end subroutine measure_label_dimensions
    
    subroutine set_legend_box_dimensions(box, max_text_width, max_text_height_pixels, &
                                        data_to_pixel_ratio_x, data_to_pixel_ratio_y, num_labels)
        !! Set legend box dimensions based on measurements
        type(legend_box_t), intent(inout) :: box
        real(wp), intent(in) :: max_text_width
        integer, intent(in) :: max_text_height_pixels
        real(wp), intent(in) :: data_to_pixel_ratio_x, data_to_pixel_ratio_y
        integer, intent(in) :: num_labels
        
        real(wp) :: padding_x, label_spacing
        
        ! Set legend box components (matplotlib-style spacing)
        box%line_length = 20.0_wp / data_to_pixel_ratio_x  ! 20 pixels for legend line
        box%text_spacing = 6.0_wp / data_to_pixel_ratio_x  ! 6 pixels between line and text
        box%entry_height = real(max_text_height_pixels, wp) / data_to_pixel_ratio_y
        box%entry_spacing = 5.0_wp / data_to_pixel_ratio_y  ! 0.5 * 10pt font = 5 pixels
        box%padding = 4.0_wp / data_to_pixel_ratio_y  ! 0.4 * 10pt font = 4 pixels
        
        label_spacing = box%entry_spacing
        padding_x = 4.0_wp / data_to_pixel_ratio_x
        
        ! Calculate total box dimensions
        box%width = 2.0_wp * padding_x + box%line_length + box%text_spacing + max_text_width
        box%height = 2.0_wp * box%padding + &
                     real(num_labels, wp) * box%entry_height + &
                     real(num_labels - 1, wp) * label_spacing
    end subroutine set_legend_box_dimensions
    
    function get_actual_text_dimensions(label, data_to_pixel_x, data_to_pixel_y) result(dimensions)
        !! Get actual text dimensions using text system measurements  
        !! Returns [width, height] in data coordinates
        character(len=*), intent(in) :: label
        real(wp), intent(in) :: data_to_pixel_x, data_to_pixel_y
        real(wp) :: dimensions(2)  ! [width, height]
        integer :: width_pixels, height_pixels, processed_len
        logical :: text_system_available
        character(len=512) :: processed_label
        
        text_system_available = init_text_system()
        
        ! Process LaTeX commands to get actual Unicode characters for width calculation
        call process_latex_in_text(label, processed_label, processed_len)
        
        if (text_system_available) then
            width_pixels = calculate_text_width(processed_label(1:processed_len))
            height_pixels = calculate_text_height(processed_label(1:processed_len))
            dimensions(1) = real(width_pixels, wp) / data_to_pixel_x
            dimensions(2) = real(height_pixels, wp) / data_to_pixel_y
        else
            ! Fallback estimation using processed Unicode characters
            dimensions(1) = real(processed_len, wp) * 8.0_wp / data_to_pixel_x  ! 8 pixels per char
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
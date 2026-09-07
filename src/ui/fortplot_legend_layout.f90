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
    use fortplot_legend_overlap, only: legend_overlap_cost
    use fortplot_text_fonts, only: get_global_font, get_font_scale
    use fortplot_truetype, only: truetype_font_t
    use fortplot_text_layout, only: has_mathtext
    use fortplot_latex_parser, only: process_latex_in_text
    implicit none
    
    private
    public :: legend_box_t, calculate_legend_box, calculate_optimal_legend_dimensions
    public :: get_actual_text_dimensions, get_legend_margins
    public :: choose_best_legend_position
    
    ! Local position constants mirror fortplot_legend_state to avoid a cyclic
    ! module dependency (state has no dependency on this module, and adding one
    ! would close a cycle through fortplot_legend_drawing).
    integer, parameter :: LEGEND_UPPER_LEFT = 1
    integer, parameter :: LEGEND_UPPER_RIGHT = 2
    integer, parameter :: LEGEND_LOWER_LEFT = 3
    integer, parameter :: LEGEND_LOWER_RIGHT = 4
    integer, parameter :: LEGEND_EAST = 5
    integer, parameter :: LEGEND_RIGHT = 6
    integer, parameter :: LEGEND_CENTER_LEFT = 7
    integer, parameter :: LEGEND_CENTER_RIGHT = 8
    integer, parameter :: LEGEND_LOWER_CENTER = 9
    integer, parameter :: LEGEND_UPPER_CENTER = 10
    integer, parameter :: LEGEND_CENTER = 11

    type :: legend_box_t
        !! Single Responsibility: Legend box dimensions and position
        real(wp) :: x, y              ! Top-left corner position
        real(wp) :: width, height     ! Box dimensions  
        real(wp) :: padding           ! Vertical padding (Y)
        real(wp) :: padding_x         ! Horizontal padding (X)
        real(wp) :: entry_height      ! Height of each legend entry (text height)
        real(wp) :: entry_spacing     ! Vertical spacing between entries
        real(wp) :: line_length       ! Length of legend line samples
        real(wp) :: text_spacing      ! Space between line and text
        real(wp) :: handle_height     ! Matplotlib 0.7-font-size handle box
    end type legend_box_t
    
contains

    function calculate_legend_box(labels, data_width, data_height, num_entries, position, &
                                  pixel_plot_width, pixel_plot_height) result(box)
        !! Calculate optimal legend box dimensions and position
        !! DRY: Centralized legend box calculation logic
        character(len=*), intent(in) :: labels(:)
        real(wp), intent(in) :: data_width, data_height
        integer, intent(in) :: num_entries, position
        integer, intent(in), optional :: pixel_plot_width, pixel_plot_height
        type(legend_box_t) :: box
        real(wp) :: max_text_width, total_text_width, margins(2)
        integer :: px_w, px_h
        
        if (num_entries == 0) then
            box%width = 0.0_wp
            box%height = 0.1_wp
            return
        end if
        
        ! Resolve pixel plot area dimensions if provided (prefer exact values)
        if (present(pixel_plot_width) .and. present(pixel_plot_height)) then
            px_w = max(1, pixel_plot_width)
            px_h = max(1, pixel_plot_height)
        else
            px_w = int(STANDARD_WIDTH_PIXELS)
            px_h = int(STANDARD_HEIGHT_PIXELS)
        end if

        ! Calculate optimal dimensions based on content with real pixel scale
        call calculate_optimal_legend_dimensions(labels, data_width, data_height, &
                                                max_text_width, total_text_width, box, px_w, px_h)
        
        ! Get appropriate margins for this backend
        margins = get_legend_margins(data_width, data_height, px_w, px_h)

        ! Calculate position based on legend location
        call calculate_legend_position(box, data_width, data_height, position, margins)

    end function calculate_legend_box

    function choose_best_legend_position(labels, data_width, data_height, &
                                         num_entries, artist_x, artist_y, &
                                         pixel_plot_width, pixel_plot_height, &
                                         artist_paths, rectangles) result(position)
        !! Resolve matplotlib 'best' placement across all ten anchored positions.
        !! Score covered vertices, intersecting paths, offsets and patch bounds;
        !! ties follow Matplotlib location-code order (upper right first).
        character(len=*), intent(in) :: labels(:)
        real(wp), intent(in) :: data_width, data_height
        integer, intent(in) :: num_entries
        real(wp), intent(in) :: artist_x(:), artist_y(:)
        integer, intent(in), optional :: pixel_plot_width, pixel_plot_height
        integer :: position
        integer, intent(in), optional :: artist_paths(:)
        real(wp), intent(in), optional :: rectangles(:, :)

        ! Order encodes the tie-break preference (matplotlib: upper right first).
        integer, parameter :: candidates(10) = &
            [LEGEND_UPPER_RIGHT, LEGEND_UPPER_LEFT, &
             LEGEND_LOWER_LEFT, LEGEND_LOWER_RIGHT, LEGEND_RIGHT, &
             LEGEND_CENTER_LEFT, LEGEND_CENTER_RIGHT, LEGEND_LOWER_CENTER, &
             LEGEND_UPPER_CENTER, LEGEND_CENTER]
        type(legend_box_t) :: box
        integer :: i, overlap, best_overlap
        real(wp) :: margins(2)

        position = LEGEND_UPPER_RIGHT
        best_overlap = huge(0)

        box = calculate_legend_box(labels, data_width, data_height, &
                                   num_entries, candidates(1), &
                                   pixel_plot_width, pixel_plot_height)
        margins = [data_width - box%width - box%x, data_height - box%y]
        do i = 1, size(candidates)
            call calculate_legend_position(box, data_width, data_height, &
                                           candidates(i), margins)
            overlap = legend_overlap_cost( &
                [box%x, box%y - box%height, box%x + box%width, box%y], &
                artist_x, artist_y, artist_paths, rectangles)
            if (overlap < best_overlap) then
                best_overlap = overlap
                position = candidates(i)
            end if
            if (overlap == 0) exit
        end do
    end function choose_best_legend_position

    subroutine calculate_optimal_legend_dimensions(labels, data_width, data_height, &
                                                  max_text_width, total_text_width, box, &
                                                  pixel_plot_width, pixel_plot_height)
        !! Calculate optimal legend dimensions using actual text system measurements
        !! KISS: Based on measured text content, not estimates
        character(len=*), intent(in) :: labels(:)
        real(wp), intent(in) :: data_width, data_height
        real(wp), intent(out) :: max_text_width, total_text_width
        type(legend_box_t), intent(inout) :: box
        integer, intent(in) :: pixel_plot_width, pixel_plot_height
        
        real(wp) :: data_to_pixel_ratio_x, data_to_pixel_ratio_y
        integer :: max_text_height_pixels
        logical :: text_system_available
        
        ! Initialize text system for measurements
        text_system_available = init_text_system()
        
        ! Calculate data-to-pixel conversion ratio using actual plot-area pixels when available
        data_to_pixel_ratio_x = real(pixel_plot_width, wp) / data_width
        data_to_pixel_ratio_y = real(pixel_plot_height, wp) / data_height
        
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
        
        integer :: i, text_height_pixels, plain_text_height
        real(wp) :: entry_text_width, text_width_pixels
        character(len=:), allocatable :: trimmed_label, processed_label
        character(len=512) :: temp_processed_label
        integer :: processed_len
        
        max_text_width = 0.0_wp
        total_text_width = 0.0_wp
        plain_text_height = legend_line_height()
        max_text_height_pixels = plain_text_height
        
        do i = 1, size(labels)
            trimmed_label = trim(labels(i))
            
            ! Process LaTeX to Unicode first for accurate width calculation
            call process_latex_in_text(trimmed_label, temp_processed_label, processed_len)
            processed_label = temp_processed_label(1:processed_len)
            
            if (text_system_available) then
                ! Calculate width of the processed text (after LaTeX conversion)
                text_width_pixels = real(calculate_text_width(trimmed_label), wp)
                text_height_pixels = plain_text_height
                if (has_mathtext(trimmed_label)) then
                    text_height_pixels = calculate_text_height(trimmed_label)
                end if
                max_text_height_pixels = max(max_text_height_pixels, text_height_pixels)
                entry_text_width = text_width_pixels / data_to_pixel_ratio_x
            else
                ! For fallback, use processed text length
                entry_text_width = real(len_trim(processed_label), wp) * data_width * TEXT_WIDTH_RATIO
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
        
        real(wp), parameter :: font_pixels = 10.0_wp*100.0_wp/72.0_wp

        ! Matplotlib rcParams express these lengths as multiples of font size.
        box%line_length = 2.0_wp*font_pixels/data_to_pixel_ratio_x
        box%text_spacing = 0.8_wp*font_pixels/data_to_pixel_ratio_x
        box%entry_height = real(max_text_height_pixels, wp)/data_to_pixel_ratio_y
        box%entry_spacing = 0.5_wp*font_pixels/data_to_pixel_ratio_y
        box%padding = 0.4_wp*font_pixels/data_to_pixel_ratio_y
        box%padding_x = 0.4_wp*font_pixels/data_to_pixel_ratio_x
        box%handle_height = 0.7_wp*font_pixels/data_to_pixel_ratio_y
        box%width = 2.0_wp*box%padding_x + box%line_length + &
                    box%text_spacing + max_text_width
        box%height = 2.0_wp*box%padding + &
                     real(num_labels, wp)*box%entry_height + &
                     real(num_labels - 1, wp)*box%entry_spacing
    end subroutine set_legend_box_dimensions
    
    function legend_line_height() result(height)
        !! Use ascender/descender ink bounds, as Matplotlib TextArea does for lp.
        integer :: height, x0, y0, x1, y1, top, bottom, i
        type(truetype_font_t) :: font
        real(wp) :: scale
        character(len=2), parameter :: sample = 'lp'

        height = 14
        if (.not. init_text_system()) return
        font = get_global_font()
        scale = get_font_scale()
        top = 0
        bottom = 0
        do i = 1, len(sample)
            call font%get_bitmap_box(iachar(sample(i:i)), scale, scale, &
                                     x0, y0, x1, y1)
            top = min(top, y0)
            bottom = max(bottom, y1)
        end do
        height = max(1, bottom - top)
    end function legend_line_height

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
        
        if (text_system_available) then
            ! Pass original text directly - calculate_text_width handles mathtext internally
            width_pixels = calculate_text_width(label)
            height_pixels = calculate_text_height(label)
            dimensions(1) = real(width_pixels, wp) / data_to_pixel_x
            dimensions(2) = real(height_pixels, wp) / data_to_pixel_y
        else
            ! Fallback estimation - process LaTeX for accurate character count
            call process_latex_in_text(label, processed_label, processed_len)
            dimensions(1) = real(processed_len, wp) * 8.0_wp / data_to_pixel_x  ! 8 pixels per char
            dimensions(2) = 16.0_wp / data_to_pixel_y  ! 16 pixels height
        end if
        
    end function get_actual_text_dimensions
    
    function get_legend_margins(data_width, data_height, &
                                pixel_plot_width, pixel_plot_height) result(margins)
        !! Inset between the legend box and the axes frame.
        !! Matches matplotlib's borderaxespad default (0.5 * 10pt font = 5pt),
        !! which at the standard ~100 dpi is ~7 px, independent of the data
        !! range. A fixed pixel inset keeps the legend flush in the corner the
        !! way matplotlib draws it, instead of a large data-proportional gap.
        real(wp), intent(in) :: data_width, data_height
        integer, intent(in), optional :: pixel_plot_width, pixel_plot_height
        real(wp) :: margins(2)  ! [x_margin, y_margin]
        real(wp), parameter :: BORDER_AXES_PAD_PX = 5.0_wp*100.0_wp/72.0_wp
        integer :: px_w, px_h

        if (present(pixel_plot_width) .and. present(pixel_plot_height)) then
            px_w = max(1, pixel_plot_width)
            px_h = max(1, pixel_plot_height)
            margins(1) = BORDER_AXES_PAD_PX * data_width / real(px_w, wp)
            margins(2) = BORDER_AXES_PAD_PX * data_height / real(px_h, wp)
        else
            margins(1) = data_width * 0.02_wp
            margins(2) = data_height * 0.02_wp
        end if

    end function get_legend_margins
    
    subroutine calculate_legend_position(box, data_width, data_height, position, margins)
        !! Calculate legend position based on placement preference
        !! Single Responsibility: Position calculation only
        type(legend_box_t), intent(inout) :: box
        real(wp), intent(in) :: data_width, data_height, margins(2)
        integer, intent(in) :: position

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
        case (LEGEND_RIGHT, LEGEND_CENTER_RIGHT)
            box%x = data_width - box%width - margins(1)
            box%y = (data_height + box%height)*0.5_wp
        case (LEGEND_CENTER_LEFT)
            box%x = margins(1)
            box%y = (data_height + box%height)*0.5_wp
        case (LEGEND_LOWER_CENTER)
            box%x = (data_width - box%width)*0.5_wp
            box%y = box%height + margins(2)
        case (LEGEND_UPPER_CENTER)
            box%x = (data_width - box%width)*0.5_wp
            box%y = data_height - margins(2)
        case (LEGEND_CENTER)
            box%x = (data_width - box%width)*0.5_wp
            box%y = (data_height + box%height)*0.5_wp
        case (LEGEND_EAST)
            box%x = data_width + margins(1)
            box%y = (data_height + box%height)*0.5_wp
        case default  ! Default to upper right
            box%x = data_width - box%width - margins(1)
            box%y = data_height - margins(2)
        end select
        
    end subroutine calculate_legend_position

end module fortplot_legend_layout

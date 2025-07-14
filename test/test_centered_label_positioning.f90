program test_centered_label_positioning
    use fortplot_label_positioning, only: calculate_x_label_position, calculate_y_label_position
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    call test_x_labels_centered_horizontally()
    call test_y_labels_centered_vertically()
    call test_x_labels_closer_to_axis()
    print *, "All centered label positioning tests passed!"
    
contains

    subroutine test_x_labels_centered_horizontally()
        real(wp) :: tick_x, plot_bottom, plot_height, label_x, label_y
        character(len=10) :: label_text
        
        ! Arrange: X tick at position 100, varying text lengths
        tick_x = 100.0_wp
        plot_bottom = 50.0_wp
        plot_height = 300.0_wp
        
        ! Act & Assert: Short text should be centered on tick
        label_text = "10"
        call calculate_x_label_position(tick_x, plot_bottom, plot_height, label_text, label_x, label_y)
        
        ! X position should center the text on the tick (fallback width: 8px/char, so half-width is 4px/char)
        if (abs(label_x - (tick_x - len_trim(label_text) * 4.0_wp)) > 2.0_wp) then
            print *, "FAIL: X label not horizontally centered for short text"
            print *, "Expected around:", tick_x - len_trim(label_text) * 4.0_wp, "Got:", label_x
            stop 1
        end if
        
        ! Act & Assert: Long text should also be centered
        label_text = "1000"
        call calculate_x_label_position(tick_x, plot_bottom, plot_height, label_text, label_x, label_y)
        
        if (abs(label_x - (tick_x - len_trim(label_text) * 4.0_wp)) > 2.0_wp) then
            print *, "FAIL: X label not horizontally centered for long text"
            stop 1
        end if
    end subroutine test_x_labels_centered_horizontally

    subroutine test_y_labels_centered_vertically()
        real(wp) :: tick_y, plot_left, label_x, label_y
        character(len=10) :: label_text
        
        ! Arrange: Y tick at position 150
        tick_y = 150.0_wp
        plot_left = 80.0_wp
        label_text = "100"
        
        ! Act
        call calculate_y_label_position(tick_y, plot_left, label_text, label_x, label_y)
        
        ! Assert: Y position should be centered on tick (accounting for text height)
        ! Text height is approximately 12 pixels, so center should be tick_y + 6
        if (abs(label_y - (tick_y + 6.0_wp)) > 2.0_wp) then
            print *, "FAIL: Y label not vertically centered on tick"
            print *, "Expected around:", tick_y + 6.0_wp, "Got:", label_y
            stop 1
        end if
    end subroutine test_y_labels_centered_vertically

    subroutine test_x_labels_closer_to_axis()
        real(wp) :: tick_x, plot_bottom, plot_height, label_x, label_y
        character(len=10) :: label_text
        
        ! Arrange
        tick_x = 100.0_wp
        plot_bottom = 50.0_wp
        plot_height = 300.0_wp
        label_text = "10"
        
        ! Act
        call calculate_x_label_position(tick_x, plot_bottom, plot_height, label_text, label_x, label_y)
        
        ! Assert: Y position should be reasonably close to axis (allow for font variations)
        if (label_y > plot_bottom + plot_height + 25.0_wp) then
            print *, "FAIL: X labels too far from axis"
            print *, "Expected max:", plot_bottom + plot_height + 25.0_wp, "Got:", label_y
            stop 1
        end if
    end subroutine test_x_labels_closer_to_axis

end program test_centered_label_positioning
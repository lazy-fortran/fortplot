program test_fine_tuned_positioning
    use fortplot_label_positioning, only: calculate_x_label_position, calculate_y_label_position
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    call test_y_labels_moved_up()
    call test_x_labels_avoid_axis_overlap()
    print *, "All fine-tuned positioning tests passed!"
    
contains

    subroutine test_y_labels_moved_up()
        real(wp) :: tick_y, plot_left, label_x, label_y
        character(len=10) :: label_text
        
        ! Arrange: Y tick at position 150
        tick_y = 150.0_wp
        plot_left = 80.0_wp
        label_text = "100"
        
        ! Act
        call calculate_y_label_position(tick_y, plot_left, label_text, label_x, label_y)
        
        ! Assert: Y position should be slightly above tick center
        ! Should be above tick_y (moved up from center)
        if (label_y <= tick_y) then
            print *, "FAIL: Y label not positioned above tick center"
            print *, "Tick Y:", tick_y, "Label Y:", label_y
            stop 1
        end if
        
        ! Should not be too far above (max 8 pixels up)
        if (label_y > tick_y + 8.0_wp) then
            print *, "FAIL: Y label positioned too far above tick"
            print *, "Expected max:", tick_y + 8.0_wp, "Got:", label_y
            stop 1
        end if
    end subroutine test_y_labels_moved_up

    subroutine test_x_labels_avoid_axis_overlap()
        real(wp) :: tick_x, plot_bottom, plot_height, label_x, label_y
        character(len=10) :: label_text
        
        ! Arrange
        tick_x = 100.0_wp
        plot_bottom = 50.0_wp
        plot_height = 300.0_wp
        label_text = "10"
        
        ! Act
        call calculate_x_label_position(tick_x, plot_bottom, plot_height, label_text, label_x, label_y)
        
        ! Assert: Y position should be well below axis to avoid overlap
        ! Should be at least 12 pixels below axis (more than previous 8px)
        if (label_y < plot_bottom + plot_height + 12.0_wp) then
            print *, "FAIL: X labels too close to axis, may overlap"
            print *, "Expected min:", plot_bottom + plot_height + 12.0_wp, "Got:", label_y
            stop 1
        end if
        
        ! Should not be excessively far (max 20 pixels below)
        if (label_y > plot_bottom + plot_height + 20.0_wp) then
            print *, "FAIL: X labels positioned too far from axis"
            print *, "Expected max:", plot_bottom + plot_height + 20.0_wp, "Got:", label_y
            stop 1
        end if
    end subroutine test_x_labels_avoid_axis_overlap

end program test_fine_tuned_positioning
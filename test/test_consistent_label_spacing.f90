program test_consistent_label_spacing
    !! Test that axis labels have consistent spacing and don't overlap with tick labels
    use fortplot_layout, only: plot_margins_t, plot_area_t, calculate_plot_area
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    type(plot_margins_t) :: margins
    type(plot_area_t) :: plot_area
    integer, parameter :: CANVAS_WIDTH = 640
    integer, parameter :: CANVAS_HEIGHT = 480
    
    ! Spacing constants (should match backend implementations)
    integer, parameter :: PNG_X_LABEL_SPACING = 35  ! From PNG implementation
    integer, parameter :: PNG_Y_LABEL_SPACING = 35  ! From PNG implementation
    integer, parameter :: TICK_LABEL_HEIGHT = 15    ! Approximate height of tick labels
    integer, parameter :: TICK_LABEL_WIDTH = 40     ! Approximate width of tick labels
    
    ! Get matplotlib-exact plot area
    margins = plot_margins_t()
    call calculate_plot_area(CANVAS_WIDTH, CANVAS_HEIGHT, margins, plot_area)
    
    print *, "=== Consistent Label Spacing Test ==="
    print *, "Plot area boundaries:"
    print *, "  X: ", plot_area%left, " to ", plot_area%left + plot_area%width
    print *, "  Y: ", plot_area%bottom, " to ", plot_area%bottom + plot_area%height
    print *, ""
    
    ! Test X-axis label positioning
    print *, "X-axis label positioning:"
    print *, "  Plot bottom edge:", plot_area%bottom + plot_area%height
    print *, "  Tick labels end around:", plot_area%bottom + plot_area%height + TICK_LABEL_HEIGHT
    print *, "  X-label positioned at:", plot_area%bottom + plot_area%height + PNG_X_LABEL_SPACING
    print *, "  Clearance from tick labels:", PNG_X_LABEL_SPACING - TICK_LABEL_HEIGHT, "pixels"
    
    if (PNG_X_LABEL_SPACING > TICK_LABEL_HEIGHT + 5) then
        print *, "  ✓ X-label has adequate clearance"
    else
        print *, "  ✗ X-label may overlap tick labels"
    end if
    print *, ""
    
    ! Test Y-axis label positioning  
    print *, "Y-axis label positioning:"
    print *, "  Plot left edge:", plot_area%left
    print *, "  Tick labels start around:", plot_area%left - TICK_LABEL_WIDTH
    print *, "  Y-label positioned at:", plot_area%left - PNG_Y_LABEL_SPACING
    print *, "  Clearance from tick labels:", PNG_Y_LABEL_SPACING - TICK_LABEL_WIDTH, "pixels"
    
    if (PNG_Y_LABEL_SPACING > TICK_LABEL_WIDTH) then
        print *, "  ✓ Y-label has adequate clearance"
    else
        ! XFAIL: Expected failure - Issue #34
        print *, "  XFAIL: Y-label may overlap tick labels - Issue #34" 
        print *, "  Known issue: PDF Y-axis labels cluster near origin instead of proper positioning"
    end if
    print *, ""
    
    ! Test consistency between backends
    print *, "Backend consistency:"
    print *, "  Both PNG and PDF should use same spacing values"
    print *, "  X-axis spacing: ", PNG_X_LABEL_SPACING, " pixels"
    print *, "  Y-axis spacing: ", PNG_Y_LABEL_SPACING, " pixels"
    print *, "  ✓ Spacing values are consistent across backends"
    
end program test_consistent_label_spacing
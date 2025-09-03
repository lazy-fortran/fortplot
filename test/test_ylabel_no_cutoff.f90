program test_ylabel_no_cutoff
    !! Test that ylabel is not cut off at canvas edge with various configurations
    use fortplot
    use fortplot_raster_axes, only: compute_ylabel_x_pos_old
    use fortplot_layout, only: plot_area_t, plot_margins_t, calculate_plot_area
    implicit none
    
    integer :: canvas_width, canvas_height
    type(plot_margins_t) :: margins
    type(plot_area_t) :: plot_area
    integer :: ylabel_width, ytick_max_width, x_pos
    logical :: ok
    
    ok = .true.
    
    ! Test case 1: Standard 800x600 canvas with long ylabel and wide tick labels
    canvas_width = 800
    canvas_height = 600
    call calculate_plot_area(canvas_width, canvas_height, margins, plot_area)
    
    ! Simulate a 40-pixel wide ylabel (rotated) and 60-pixel wide tick labels
    ylabel_width = 40
    ytick_max_width = 60
    
    x_pos = compute_ylabel_x_pos_old(plot_area, ylabel_width, ytick_max_width)
    
    ! Verify ylabel doesn't go negative (would be cut off)
    if (x_pos < 1) then
        print *, 'FAIL: Y-label would be cut off (x_pos=', x_pos, ')'
        ok = .false.
    end if
    
    ! Verify adequate spacing from tick labels
    ! The ylabel right edge should have at least some gap from tick labels
    if (x_pos + ylabel_width > plot_area%left - 15) then
        print *, 'FAIL: Y-label too close to tick labels'
        ok = .false.
    end if
    
    ! Test case 2: Smaller canvas with same label sizes
    canvas_width = 400
    canvas_height = 300
    call calculate_plot_area(canvas_width, canvas_height, margins, plot_area)
    
    x_pos = compute_ylabel_x_pos_old(plot_area, ylabel_width, ytick_max_width)
    
    if (x_pos < 1) then
        print *, 'FAIL: Y-label cut off on small canvas (x_pos=', x_pos, ')'
        ok = .false.
    end if
    
    ! Test case 3: Very long ylabel
    canvas_width = 800
    canvas_height = 600
    call calculate_plot_area(canvas_width, canvas_height, margins, plot_area)
    ylabel_width = 80  ! Very long label
    
    x_pos = compute_ylabel_x_pos_old(plot_area, ylabel_width, ytick_max_width)
    
    if (x_pos < 1) then
        print *, 'FAIL: Long y-label cut off (x_pos=', x_pos, ')'
        ok = .false.
    end if
    
    if (ok) then
        print *, 'PASS: Y-label positioning prevents cutoff in all test cases'
        stop 0
    else
        stop 1
    end if
    
end program test_ylabel_no_cutoff

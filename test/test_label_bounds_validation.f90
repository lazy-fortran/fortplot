program test_label_bounds_validation
    ! Validates that labels are positioned correctly within canvas bounds
    use fortplot
    use fortplot_constants, only: XLABEL_VERTICAL_OFFSET
    use fortplot_layout, only: plot_margins_t, plot_area_t, calculate_plot_area
    use fortplot_text, only: calculate_text_height
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none

    integer, parameter :: canvas_width = 640
    integer, parameter :: canvas_height = 480
    type(plot_margins_t) :: margins
    type(plot_area_t) :: plot_area
    real(wp), dimension(100) :: x, y
    integer :: i
    integer :: xlabel_y_position, available_space, text_height
    logical :: ok, dir_ok

    ! Calculate plot area
    margins = plot_margins_t()
    call calculate_plot_area(canvas_width, canvas_height, margins, plot_area)

    ! Calculate xlabel position
    xlabel_y_position = plot_area%bottom + plot_area%height + XLABEL_VERTICAL_OFFSET
    available_space = canvas_height - xlabel_y_position
    text_height = 16  ! Typical text height for default font

    ok = .true.

    ! Test 1: Ensure xlabel has enough space below baseline
    if (available_space < text_height) then
        print *, 'FAIL: xlabel out of bounds - only', available_space, 'px available,', text_height, 'px needed'
        ok = .false.
    else
        print *, 'PASS: xlabel has', available_space, 'px space (needs', text_height, 'px)'
    end if

    ! Test 2: Ensure xlabel doesn't overlap with tick labels
    ! Tick labels are at plot_area%bottom + plot_area%height + TICK_LENGTH + X_TICK_LABEL_PAD
    ! = plot_area%bottom + plot_area%height + 5 + 14 = plot_area%bottom + plot_area%height + 19
    ! Plus ~16px for tick label height = plot_area%bottom + plot_area%height + 35
    ! xlabel is at plot_area%bottom + plot_area%height + XLABEL_VERTICAL_OFFSET
    if (XLABEL_VERTICAL_OFFSET < 35) then
        print *, 'FAIL: xlabel too close to tick labels (offset=', XLABEL_VERTICAL_OFFSET, ', min needed=35)'
        ok = .false.
    else
        print *, 'PASS: xlabel properly spaced from tick labels'
    end if

    ! Create test figure to verify visually
    call create_directory_runtime('test/output', dir_ok)
    
    x = [(real(i, wp) * 0.1_wp, i=1, 100)]
    do i = 1, 100
        y(i) = sin(x(i))
    end do
    
    call figure()
    call plot(x, y)
    call xlabel('X Label Test')
    call ylabel('Y Label Test')
    call title('Label Bounds Validation')
    call savefig('test/output/test_label_bounds_validation.png')

    if (ok) then
        print *, 'All label positioning tests PASSED'
        stop 0
    else
        print *, 'Some label positioning tests FAILED'
        stop 1
    end if

end program test_label_bounds_validation
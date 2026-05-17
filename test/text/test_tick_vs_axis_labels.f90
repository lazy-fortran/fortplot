program test_tick_vs_axis_labels
    !! Test that calculate_plot_area produces sane plot area geometry for
    !! a standard 640x480 canvas with default matplotlib margins.
    use fortplot_layout, only: plot_margins_t, plot_area_t, calculate_plot_area
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    type(plot_margins_t) :: margins
    type(plot_area_t) :: plot_area
    integer, parameter :: CANVAS_WIDTH = 640
    integer, parameter :: CANVAS_HEIGHT = 480
    logical :: passed

    passed = .true.
    margins = plot_margins_t()
    call calculate_plot_area(CANVAS_WIDTH, CANVAS_HEIGHT, margins, plot_area)

    print *, "=== Tick vs Axis Label Positioning Test ==="
    print *, "Plot area: left=", plot_area%left, " bottom=", plot_area%bottom, &
             " width=", plot_area%width, " height=", plot_area%height

    ! Plot left edge must be > 0 (room for y-tick and y-axis labels)
    if (plot_area%left <= 0) then
        print *, "FAIL: plot_area%left must be > 0, got", plot_area%left
        passed = .false.
    else
        print *, "PASS: plot_area%left =", plot_area%left, " > 0"
    end if

    ! Plot bottom edge must be > 0 (room for x-tick and x-axis labels)
    if (plot_area%bottom <= 0) then
        print *, "FAIL: plot_area%bottom must be > 0, got", plot_area%bottom
        passed = .false.
    else
        print *, "PASS: plot_area%bottom =", plot_area%bottom, " > 0"
    end if

    ! Width must be positive and fit within canvas
    if (plot_area%width <= 0) then
        print *, "FAIL: plot_area%width must be > 0, got", plot_area%width
        passed = .false.
    else if (plot_area%left + plot_area%width > CANVAS_WIDTH) then
        print *, "FAIL: plot right edge exceeds canvas width"
        passed = .false.
    else
        print *, "PASS: plot_area%width =", plot_area%width
    end if

    ! Height must be positive and fit within canvas
    if (plot_area%height <= 0) then
        print *, "FAIL: plot_area%height must be > 0, got", plot_area%height
        passed = .false.
    else if (plot_area%bottom + plot_area%height > CANVAS_HEIGHT) then
        print *, "FAIL: plot bottom edge + height exceeds canvas height"
        passed = .false.
    else
        print *, "PASS: plot_area%height =", plot_area%height
    end if

    ! Left margin must leave at least 40px for tick+axis labels (realistic minimum)
    if (plot_area%left < 40) then
        print *, "FAIL: left margin too narrow for tick+axis labels, got", plot_area%left
        passed = .false.
    else
        print *, "PASS: left margin", plot_area%left, ">= 40px (room for tick+axis labels)"
    end if

    ! Bottom margin must leave at least 30px for tick+axis labels
    if (plot_area%bottom < 30) then
        print *, "FAIL: bottom margin too narrow for tick+axis labels, got", plot_area%bottom
        passed = .false.
    else
        print *, "PASS: bottom margin", plot_area%bottom, ">= 30px (room for tick+axis labels)"
    end if

    if (.not. passed) then
        error stop "test_tick_vs_axis_labels: one or more assertions failed"
    end if

    print *, "All tick vs axis label positioning assertions passed."

end program test_tick_vs_axis_labels

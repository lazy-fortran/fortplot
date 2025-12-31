program test_minor_ticks
    !! Test minor ticks functionality (Issue #1465)
    use fortplot
    use test_output_helpers, only: ensure_test_output_dir
    implicit none

    real(wp), dimension(50) :: x, y
    character(len=:), allocatable :: output_dir
    type(figure_t) :: fig
    integer :: i

    call ensure_test_output_dir('minor_ticks', output_dir)

    x = [(real(i - 1, wp)*0.2_wp, i=1, 50)]
    y = sin(x)

    ! Test 1: Stateful API - minorticks_on()
    call figure()
    call plot(x, y)
    call title("Minor Ticks - Stateful API")
    call xlabel("x")
    call ylabel("sin(x)")
    call minorticks_on()
    call savefig(trim(output_dir)//'minor_ticks_stateful.png')
    print *, "Test 1: Stateful API - PASS"

    ! Test 2: OO API - set_minor_ticks
    call fig%initialize()
    call fig%plot(x, y)
    call fig%set_title("Minor Ticks - OO API set_minor_ticks")
    call fig%set_xlabel("x")
    call fig%set_ylabel("sin(x)")
    call fig%set_minor_ticks(x=.true., y=.true.)
    call fig%savefig(trim(output_dir)//'minor_ticks_oo_set.png')
    print *, "Test 2: OO API set_minor_ticks - PASS"

    ! Test 3: OO API - minorticks_on()
    call fig%initialize()
    call fig%plot(x, y)
    call fig%set_title("Minor Ticks - OO API minorticks_on")
    call fig%set_xlabel("x")
    call fig%set_ylabel("sin(x)")
    call fig%minorticks_on()
    call fig%savefig(trim(output_dir)//'minor_ticks_oo_on.png')
    print *, "Test 3: OO API minorticks_on - PASS"

    ! Test 4: Custom minor tick count
    call fig%initialize()
    call fig%plot(x, y)
    call fig%set_title("Minor Ticks - Custom Count (9)")
    call fig%set_xlabel("x")
    call fig%set_ylabel("sin(x)")
    call fig%set_minor_tick_count(9)
    call fig%set_minor_ticks(x=.true., y=.true.)
    call fig%savefig(trim(output_dir)//'minor_ticks_count_9.png')
    print *, "Test 4: Custom minor tick count - PASS"

    ! Test 5: X-axis only
    call fig%initialize()
    call fig%plot(x, y)
    call fig%set_title("Minor Ticks - X-axis Only")
    call fig%set_xlabel("x")
    call fig%set_ylabel("sin(x)")
    call fig%set_minor_ticks(x=.true.)
    call fig%savefig(trim(output_dir)//'minor_ticks_x_only.png')
    print *, "Test 5: X-axis only - PASS"

    ! Test 6: Y-axis only
    call fig%initialize()
    call fig%plot(x, y)
    call fig%set_title("Minor Ticks - Y-axis Only")
    call fig%set_xlabel("x")
    call fig%set_ylabel("sin(x)")
    call fig%set_minor_ticks(y=.true.)
    call fig%savefig(trim(output_dir)//'minor_ticks_y_only.png')
    print *, "Test 6: Y-axis only - PASS"

    print *, "All minor ticks tests completed successfully"

end program test_minor_ticks

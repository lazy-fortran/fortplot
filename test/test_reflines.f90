program test_reflines
    !! Comprehensive test for horizontal and vertical reference lines
    !! Tests axhline, axvline, hlines, and vlines functionality
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_core, only: figure_t
    use test_output_helpers, only: ensure_test_output_dir
    implicit none

    type(figure_t) :: fig
    real(wp) :: x(10), y(10)
    real(wp) :: y_positions(3), x_positions(3)
    integer :: i, status
    logical :: all_passed
    character(len=:), allocatable :: output_dir

    all_passed = .true.
    call ensure_test_output_dir('reflines', output_dir)

    ! Generate sample data for context
    do i = 1, 10
        x(i) = real(i, wp)
        y(i) = sin(real(i, wp) * 0.5_wp)
    end do

    ! Test 1: axhline - horizontal line at y=0
    print '(A)', 'Test 1: axhline at y=0'
    call fig%initialize(width=640, height=480)
    call fig%add_plot(x, y, label='sin(x)')
    call fig%axhline(0.0_wp, color='red', linestyle='--', label='y=0')
    call fig%savefig_with_status(trim(output_dir)//'test_axhline.png', status)
    if (status /= 0) then
        print '(A)', '  FAIL: could not save axhline test'
        all_passed = .false.
    else
        print '(A)', '  PASS: axhline saved successfully'
    end if
    call fig%clear()

    ! Test 2: axvline - vertical line at x=5
    print '(A)', 'Test 2: axvline at x=5'
    call fig%initialize(width=640, height=480)
    call fig%add_plot(x, y, label='sin(x)')
    call fig%axvline(5.0_wp, color='blue', linestyle=':', label='x=5')
    call fig%savefig_with_status(trim(output_dir)//'test_axvline.png', status)
    if (status /= 0) then
        print '(A)', '  FAIL: could not save axvline test'
        all_passed = .false.
    else
        print '(A)', '  PASS: axvline saved successfully'
    end if
    call fig%clear()

    ! Test 3: hlines - multiple horizontal lines
    print '(A)', 'Test 3: hlines at y=-0.5, 0, 0.5'
    y_positions = [-0.5_wp, 0.0_wp, 0.5_wp]
    call fig%initialize(width=640, height=480)
    call fig%add_plot(x, y, label='sin(x)')
    call fig%hlines(y_positions, 1.0_wp, 10.0_wp, colors='green', &
                   linestyles='--', label='thresholds')
    call fig%savefig_with_status(trim(output_dir)//'test_hlines.png', status)
    if (status /= 0) then
        print '(A)', '  FAIL: could not save hlines test'
        all_passed = .false.
    else
        print '(A)', '  PASS: hlines saved successfully'
    end if
    call fig%clear()

    ! Test 4: vlines - multiple vertical lines
    print '(A)', 'Test 4: vlines at x=2, 5, 8'
    x_positions = [2.0_wp, 5.0_wp, 8.0_wp]
    call fig%initialize(width=640, height=480)
    call fig%add_plot(x, y, label='sin(x)')
    call fig%vlines(x_positions, -1.0_wp, 1.0_wp, colors='purple', &
                   linestyles='-.', label='markers')
    call fig%savefig_with_status(trim(output_dir)//'test_vlines.png', status)
    if (status /= 0) then
        print '(A)', '  FAIL: could not save vlines test'
        all_passed = .false.
    else
        print '(A)', '  PASS: vlines saved successfully'
    end if
    call fig%clear()

    ! Test 5: Combined - axhline and axvline with data
    print '(A)', 'Test 5: Combined axhline and axvline'
    call fig%initialize(width=640, height=480)
    call fig%add_plot(x, y, label='sin(x)')
    call fig%axhline(0.0_wp, color='red', linestyle='--')
    call fig%axvline(5.0_wp, color='blue', linestyle=':')
    call fig%set_title('Combined Reference Lines')
    call fig%set_xlabel('x')
    call fig%set_ylabel('y')
    call fig%savefig_with_status(trim(output_dir)//'test_reflines_combined.png', status)
    if (status /= 0) then
        print '(A)', '  FAIL: could not save combined test'
        all_passed = .false.
    else
        print '(A)', '  PASS: combined test saved successfully'
    end if

    ! Summary
    print '(A)', ''
    if (all_passed) then
        print '(A)', 'All reference line tests PASSED'
    else
        print '(A)', 'Some reference line tests FAILED'
        stop 1
    end if

end program test_reflines

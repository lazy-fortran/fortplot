program test_suptitle
    !! Test suite for suptitle functionality (fixes #1480)
    !! Tests figure-level title above subplots

    use iso_fortran_env, only: wp => real64
    use fortplot, only: figure_t, subplots, suptitle, plot, savefig, &
                        subplot, xlabel, ylabel, title
    use fortplot_test_output_helpers, only: ensure_test_output_dir
    implicit none

    logical :: all_passed

    print *, 'Running suptitle tests...'

    all_passed = .true.

    call test_suptitle_oo_interface(all_passed)
    call test_suptitle_stateful_interface(all_passed)
    call test_suptitle_with_fontsize(all_passed)
    call test_suptitle_rendering(all_passed)
    call test_subplot_suptitle_layout(all_passed)

    if (all_passed) then
        print *, 'All suptitle tests PASSED!'
    else
        print *, 'Some suptitle tests FAILED!'
        stop 1
    end if

contains

    subroutine test_suptitle_oo_interface(passed)
        !! Test suptitle with OO interface
        logical, intent(inout) :: passed
        type(figure_t) :: fig
        real(wp), parameter :: x(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        real(wp), parameter :: y(5) = [1.0_wp, 4.0_wp, 9.0_wp, 16.0_wp, 25.0_wp]

        print *, 'Testing: suptitle OO interface'

        call fig%initialize(800, 600)
        call fig%subplots(2, 2)
        call fig%suptitle('Overall Figure Title')

        if (allocated(fig%state%suptitle)) then
            if (trim(fig%state%suptitle) == 'Overall Figure Title') then
                print *, '  PASS: suptitle set correctly'
            else
                print *, '  FAIL: suptitle text mismatch'
                passed = .false.
            end if
        else
            print *, '  FAIL: suptitle not allocated'
            passed = .false.
        end if

        call fig%subplot_plot(1, 1, x, y)
        call fig%subplot_plot(1, 2, x, y*2.0_wp)
        call fig%subplot_plot(2, 1, x, y*0.5_wp)
        call fig%subplot_plot(2, 2, x, y*1.5_wp)

        print *, 'Test completed'
    end subroutine test_suptitle_oo_interface

    subroutine test_suptitle_stateful_interface(passed)
        !! Test suptitle with stateful interface
        logical, intent(inout) :: passed
        real(wp), parameter :: x(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        real(wp), parameter :: y(5) = [1.0_wp, 4.0_wp, 9.0_wp, 16.0_wp, 25.0_wp]

        print *, 'Testing: suptitle stateful interface'

        call subplots(2, 2)
        call suptitle('Stateful Figure Title')

        call subplot(2, 2, 1)
        call plot(x, y)
        call title('Subplot 1')

        call subplot(2, 2, 2)
        call plot(x, y*2.0_wp)
        call title('Subplot 2')

        call subplot(2, 2, 3)
        call plot(x, y*0.5_wp)
        call title('Subplot 3')

        call subplot(2, 2, 4)
        call plot(x, y*1.5_wp)
        call title('Subplot 4')

        print *, '  PASS: stateful suptitle call succeeded'
        print *, 'Test completed'
    end subroutine test_suptitle_stateful_interface

    subroutine test_suptitle_with_fontsize(passed)
        !! Test suptitle with custom font size
        logical, intent(inout) :: passed
        type(figure_t) :: fig

        print *, 'Testing: suptitle with fontsize'

        call fig%initialize(800, 600)
        call fig%subplots(2, 1)
        call fig%suptitle('Large Title', fontsize=18.0_wp)

        if (abs(fig%state%suptitle_fontsize - 18.0_wp) < 0.001_wp) then
            print *, '  PASS: fontsize set correctly'
        else
            print *, '  FAIL: fontsize not set correctly'
            passed = .false.
        end if

        print *, 'Test completed'
    end subroutine test_suptitle_with_fontsize

    subroutine test_suptitle_rendering(passed)
        !! Test that suptitle renders correctly to PNG output
        logical, intent(inout) :: passed
        type(figure_t) :: fig
        real(wp), parameter :: x(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        real(wp), parameter :: y(5) = [1.0_wp, 4.0_wp, 9.0_wp, 16.0_wp, 25.0_wp]
        logical :: file_exists
        integer :: status
        character(len=:), allocatable :: output_dir, output_file

        print *, 'Testing: suptitle rendering to PNG'

        call ensure_test_output_dir('suptitle', output_dir)
        output_file = output_dir // 'test_suptitle.png'

        call fig%initialize(800, 600, backend='png')
        call fig%subplots(2, 2)
        call fig%suptitle('Test Suptitle Rendering', fontsize=16.0_wp)

        call fig%subplot_plot(1, 1, x, y, label='Plot 1')
        call fig%subplot_set_title(1, 1, 'Subplot 1')

        call fig%subplot_plot(1, 2, x, y*2.0_wp, label='Plot 2')
        call fig%subplot_set_title(1, 2, 'Subplot 2')

        call fig%subplot_plot(2, 1, x, y*0.5_wp, label='Plot 3')
        call fig%subplot_set_title(2, 1, 'Subplot 3')

        call fig%subplot_plot(2, 2, x, y*1.5_wp, label='Plot 4')
        call fig%subplot_set_title(2, 2, 'Subplot 4')

        call fig%savefig_with_status(output_file, status)

        if (status == 0) then
            inquire(file=output_file, exist=file_exists)
            if (file_exists) then
                print *, '  PASS: PNG file created with suptitle'
            else
                print *, '  FAIL: PNG file not found'
                passed = .false.
            end if
        else
            print *, '  FAIL: savefig returned error status'
            passed = .false.
        end if

        print *, 'Test completed'
    end subroutine test_suptitle_rendering

    subroutine test_subplot_suptitle_layout(passed)
        !! Regression coverage for subplot suptitle placement/spacing (#1966).
        logical, intent(inout) :: passed
        type(figure_t) :: fig
        real(wp), allocatable :: rgb(:, :, :)
        real(wp) :: x(5), y(5)
        integer :: top_dark, sparse_rows

        x = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        y = [1.0_wp, 4.0_wp, 9.0_wp, 16.0_wp, 25.0_wp]

        print *, 'Testing: subplot suptitle layout regression'

        call fig%initialize(900, 300, backend='png')
        call fig%subplots(1, 3)
        call fig%suptitle('Polynomial Growth Comparison')
        call populate_subplot(fig, 1, 1, x, y, 'Linear', 'x', 'x')
        call populate_subplot(fig, 1, 2, x, y, 'Quadratic', 'x', 'x^2')
        call populate_subplot(fig, 1, 3, x, y, 'Cubic', 'x', 'x^3')

        allocate (rgb(fig%get_width(), fig%get_height(), 3))
        call fig%extract_rgb_data_for_animation(rgb)
        top_dark = count_dark_pixels(rgb, 1, int(0.12_wp * real(size(rgb, 2), wp)))
        deallocate (rgb)
        if (top_dark >= 1000) then
            print *, '  PASS: 1x3 suptitle renders in the top band'
        else
            print *, '  FAIL: 1x3 suptitle is not clearly above the subplot grid'
            passed = .false.
        end if

        call fig%initialize(800, 600, backend='png')
        call fig%subplots(2, 2)
        call fig%suptitle('Trigonometric and Polynomial Functions')
        call populate_subplot(fig, 1, 1, x, y, 'Sine Wave', 'x', 'sin(x)')
        call populate_subplot(fig, 1, 2, x, y, 'Cosine Wave', 'x', 'cos(x)')
        call populate_subplot(fig, 2, 1, x, y, 'Damped Oscillation', 'x', 'y')
        call populate_subplot(fig, 2, 2, x, y, 'Quadratic Function', 'x', 'x^2/50')

        allocate (rgb(fig%get_width(), fig%get_height(), 3))
        call fig%extract_rgb_data_for_animation(rgb)
        sparse_rows = count_blank_rows(rgb, int(0.40_wp * real(size(rgb, 2), wp)), &
                                       int(0.60_wp * real(size(rgb, 2), wp)))
        deallocate (rgb)
        if (sparse_rows >= 20) then
            print *, '  PASS: 2x2 subplot rows keep visible clearance'
        else
            print *, '  FAIL: 2x2 subplot rows crowd the title/xlabel separator'
            passed = .false.
        end if
    end subroutine test_subplot_suptitle_layout

    subroutine populate_subplot(fig, row, col, x, y, subplot_title, x_label, y_label)
        type(figure_t), intent(inout) :: fig
        integer, intent(in) :: row, col
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in) :: subplot_title, x_label, y_label

        call fig%subplot_plot(row, col, x, y)
        call fig%subplot_set_title(row, col, subplot_title)
        call fig%subplot_set_xlabel(row, col, x_label)
        call fig%subplot_set_ylabel(row, col, y_label)
    end subroutine populate_subplot

    function count_dark_pixels(rgb, y_start, y_end) result(count)
        real(wp), intent(in) :: rgb(:, :, :)
        integer, intent(in) :: y_start, y_end
        integer :: count
        integer :: x, y
        real(wp) :: luminance

        count = 0
        do y = max(1, y_start), min(size(rgb, 2), y_end)
            do x = 1, size(rgb, 1)
                luminance = sum(rgb(x, y, :)) / 3.0_wp
                if (luminance < 0.7_wp) count = count + 1
            end do
        end do
    end function count_dark_pixels

    function count_blank_rows(rgb, y_start, y_end) result(count)
        real(wp), intent(in) :: rgb(:, :, :)
        integer, intent(in) :: y_start, y_end
        integer :: count
        integer :: x, y, dark_in_row
        real(wp) :: luminance

        count = 0
        do y = max(1, y_start), min(size(rgb, 2), y_end)
            dark_in_row = 0
            do x = 1, size(rgb, 1)
                luminance = sum(rgb(x, y, :)) / 3.0_wp
                if (luminance < 0.7_wp) dark_in_row = dark_in_row + 1
            end do
            if (dark_in_row <= 40) count = count + 1
        end do
    end function count_blank_rows

end program test_suptitle

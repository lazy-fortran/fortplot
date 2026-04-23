program test_ascii_animation_output
    !! Regression test for PR #1639: ASCII backend configurable canvas size
    !! and animation frame clearing.
    !!
    !! Verifies:
    !!   1. ASCII savefig writes a non-empty file with plot content.
    !!   2. Re-rendering after set_ydata produces different output (frame
    !!      clearing prevents ghosting from the previous render).
    use fortplot
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none

    type(figure_t) :: fig_a, fig_b
    real(wp) :: x(5), y1(5), y2(5)
    integer :: i
    character(len=*), parameter :: frame1 = 'build/test/output/anim_frame1.txt'
    character(len=*), parameter :: frame2 = 'build/test/output/anim_frame2.txt'
    logical :: dir_ok

    call create_directory_runtime('build/test/output', dir_ok)

    x  = [(real(i, wp), i = 1, 5)]
    y1 = [1.0_wp, 2.0_wp, 3.0_wp, 2.0_wp, 1.0_wp]
    y2 = [3.0_wp, 2.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]

    ! --- Test 1: savefig writes a non-empty ASCII file ---
    call fig_a%initialize(40, 12)
    call fig_a%add_plot(x, y1, label='frame1')
    call fig_a%savefig(frame1)
    call assert_file_nonempty(frame1, 'ASCII output frame1')

    ! --- Test 2: re-rendering after set_ydata produces different content ---
    ! The first render saves y1 data; after set_ydata(y2), a re-render should
    ! clear the canvas so frame2 differs from frame1.
    call fig_b%initialize(40, 12)
    call fig_b%add_plot(x, y1, label='curve')
    call fig_b%savefig(frame1)

    call fig_b%set_ydata(1, y2)
    call fig_b%set_rendered(.false.)
    call fig_b%savefig(frame2)

    call assert_file_nonempty(frame2, 'ASCII output frame2')
    call assert_files_differ(frame1, frame2)

    print *, 'All ASCII animation regression tests PASSED'

contains

    subroutine assert_file_nonempty(path, label)
        character(len=*), intent(in) :: path, label
        logical :: exists
        integer :: sz

        inquire(file=path, exist=exists, size=sz)
        if (.not. exists) then
            print *, 'FAIL: file not created: ', path
            error stop 'ASCII savefig missing'
        end if
        if (sz <= 0) then
            print *, 'FAIL: ASCII file is empty: ', path
            error stop 'empty ASCII output'
        end if
        print *, 'PASS: ', label, ' created (', sz, ' bytes)'
    end subroutine assert_file_nonempty

    subroutine assert_files_differ(path_a, path_b)
        character(len=*), intent(in) :: path_a, path_b
        integer :: ua, ub, ios_a, ios_b
        character(len=512) :: la, lb
        integer :: diff_count, lines_checked

        diff_count = 0
        lines_checked = 0

        open(newunit=ua, file=path_a, status='old', action='read', iostat=ios_a)
        open(newunit=ub, file=path_b, status='old', action='read', iostat=ios_b)
        if (ios_a /= 0 .or. ios_b /= 0) then
            print *, 'FAIL: cannot open frames for comparison'
            error stop 'cannot compare frames'
        end if

        do
            read(ua, '(A)', iostat=ios_a) la
            read(ub, '(A)', iostat=ios_b) lb
            if (ios_a /= 0 .and. ios_b /= 0) exit
            lines_checked = lines_checked + 1
            if (la /= lb) diff_count = diff_count + 1
        end do
        close(ua)
        close(ub)

        if (diff_count == 0) then
            print *, 'FAIL: frames are identical - ASCII canvas not cleared between renders'
            error stop 'frame clearing regression'
        end if
        print *, 'PASS: frames differ (', diff_count, '/', lines_checked, ' lines changed)'
    end subroutine assert_files_differ

end program test_ascii_animation_output

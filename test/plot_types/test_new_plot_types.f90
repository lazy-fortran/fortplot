program test_new_plot_types
    !! Behavioral smoke tests for imshow, step, stem, fill, fill_between,
    !! twinx, twiny. Each sub-test verifies that:
    !!  - the function accepts valid arguments without crashing
    !!  - savefig produces a non-empty output file
    use fortplot
    use fortplot_matplotlib_session, only: get_global_figure
    use fortplot_figure_core, only: figure_t
    use fortplot_system_runtime, only: create_directory_runtime, is_windows
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    character(len=1024) :: arg0, arg1
    character(len=4096) :: command, output_log
    logical :: all_passed, dir_ok, found_warning
    integer :: exitstat, cmdstat

    call get_command_argument(1, arg1)
    if (trim(arg1) == '--probe') then
        call run_imshow_cmap_probe()
        stop 0
    end if

    all_passed = .true.
    call create_directory_runtime('build/test/output', dir_ok)

    call test_imshow(all_passed)
    call test_step(all_passed)
    call test_stem(all_passed)
    call test_fill(all_passed)
    call test_fill_between(all_passed)
    call test_fill_between_step(all_passed)
    call test_fill_between_rgb(all_passed)
    call test_fill_between_y2_default(all_passed)
    call test_twinx(all_passed)
    call test_twiny(all_passed)

    if (.not. all_passed) then
        error stop "test_new_plot_types: one or more smoke tests failed"
    end if
    print *, "All new plot type smoke tests PASSED"

contains

    subroutine assert_nonempty(path, label, passed)
        character(len=*), intent(in) :: path, label
        logical, intent(inout) :: passed
        logical :: exists
        integer :: sz
        inquire(file=path, exist=exists, size=sz)
        if (.not. exists) then
            print *, "FAIL (", label, "): file not created: ", path
            passed = .false.
        else if (sz <= 0) then
            print *, "FAIL (", label, "): file is empty"
            passed = .false.
        else
            print *, "PASS (", label, "): ", sz, " bytes"
        end if
    end subroutine assert_nonempty

    subroutine test_imshow(passed)
        logical, intent(inout) :: passed
        real(wp) :: z(10, 10)
        integer :: i, j
        do i = 1, 10
            do j = 1, 10
                z(i, j) = sin(real(i, wp)) * cos(real(j, wp))
            end do
        end do
        call figure()
        call imshow(z)
        call title('imshow smoke test')
        call savefig('build/test/output/smoke_imshow.png')
        call assert_nonempty('build/test/output/smoke_imshow.png', 'imshow', passed)
        call assert_no_cmap_warning()
    end subroutine test_imshow

    subroutine test_step(passed)
        logical, intent(inout) :: passed
        real(wp) :: x(8), y(8)
        integer :: i
        do i = 1, 8
            x(i) = real(i, wp)
            y(i) = mod(i, 3) * 1.0_wp
        end do
        call figure()
        call step(x, y)
        call title('step smoke test')
        call savefig('build/test/output/smoke_step.png')
        call assert_nonempty('build/test/output/smoke_step.png', 'step', passed)
    end subroutine test_step

    subroutine test_stem(passed)
        logical, intent(inout) :: passed
        real(wp) :: x(6), y(6)
        integer :: i
        do i = 1, 6
            x(i) = real(i, wp)
            y(i) = sin(real(i, wp))
        end do
        call figure()
        call stem(x, y)
        call title('stem smoke test')
        call savefig('build/test/output/smoke_stem.png')
        call assert_nonempty('build/test/output/smoke_stem.png', 'stem', passed)
    end subroutine test_stem

    subroutine test_fill(passed)
        logical, intent(inout) :: passed
        real(wp) :: x(5), y(5)
        integer :: i
        do i = 1, 5
            x(i) = real(i, wp)
            y(i) = real(i, wp) ** 2
        end do
        call figure()
        call fill(x, y)
        call title('fill smoke test')
        call savefig('build/test/output/smoke_fill.png')
        call assert_nonempty('build/test/output/smoke_fill.png', 'fill', passed)
    end subroutine test_fill

    subroutine test_fill_between(passed)
        logical, intent(inout) :: passed
        real(wp) :: x(10), y1(10), y2(10)
        integer :: i
        do i = 1, 10
            x(i) = real(i, wp)
            y1(i) = real(i, wp)
            y2(i) = real(i, wp) * 0.5_wp
        end do
        call figure()
        call fill_between(x, y1=y1, y2=y2)
        call title('fill_between smoke test')
        call savefig('build/test/output/smoke_fill_between.png')
        call assert_nonempty('build/test/output/smoke_fill_between.png', 'fill_between', passed)
    end subroutine test_fill_between

    subroutine test_fill_between_step(passed)
        !! Verify fill_between accepts step='pre', 'post', and 'mid'
        !! without crashing and produces valid output (ref #1673).
        logical, intent(inout) :: passed
        real(wp) :: x(6), y1(6), y2(6)
        integer :: i

        do i = 1, 6
            x(i) = real(i, wp)
            y1(i) = real(i, wp)
            y2(i) = real(i, wp) * 0.5_wp
        end do

        call figure()
        call fill_between(x, y1=y1, y2=y2, step='pre')
        call title('fill_between step=pre smoke test')
        call savefig('build/test/output/smoke_fill_between_step_pre.png')
        call assert_nonempty('build/test/output/smoke_fill_between_step_pre.png', &
                             'fill_between step=pre', passed)

        call figure()
        call fill_between(x, y1=y1, y2=y2, step='post')
        call title('fill_between step=post smoke test')
        call savefig('build/test/output/smoke_fill_between_step_post.png')
        call assert_nonempty('build/test/output/smoke_fill_between_step_post.png', &
                             'fill_between step=post', passed)

        call figure()
        call fill_between(x, y1=y1, y2=y2, step='mid')
        call title('fill_between step=mid smoke test')
        call savefig('build/test/output/smoke_fill_between_step_mid.png')
        call assert_nonempty('build/test/output/smoke_fill_between_step_mid.png', &
                             'fill_between step=mid', passed)
    end subroutine test_fill_between_step

    subroutine test_fill_between_rgb(passed)
        !! Verify fill_between accepts an RGB triple color (ref #1673).
        logical, intent(inout) :: passed
        real(wp) :: x(8), y1(8), y2(8)
        real(wp) :: rgb_color(3)
        integer :: i

        do i = 1, 8
            x(i) = real(i, wp)
            y1(i) = sin(real(i, wp))
            y2(i) = sin(real(i, wp)) * 0.5_wp
        end do
        rgb_color = [0.2_wp, 0.5_wp, 0.8_wp]

        call figure()
        call fill_between(x, y1=y1, y2=y2, color=rgb_color)
        call title('fill_between RGB color smoke test')
        call savefig('build/test/output/smoke_fill_between_rgb.png')
        call assert_nonempty('build/test/output/smoke_fill_between_rgb.png', &
                             'fill_between RGB color', passed)
    end subroutine test_fill_between_rgb

    subroutine test_fill_between_y2_default(passed)
        !! Verify fill_between works with y1 only; y2 defaults to zero
        !! (ref #1673).
        logical, intent(inout) :: passed
        real(wp) :: x(8), y1(8)
        integer :: i

        do i = 1, 8
            x(i) = real(i, wp)
            y1(i) = sin(real(i, wp))
        end do

        call figure()
        call fill_between(x, y1=y1)
        call title('fill_between y2=default smoke test')
        call savefig('build/test/output/smoke_fill_between_y2_default.png')
        call assert_nonempty('build/test/output/smoke_fill_between_y2_default.png', &
                             'fill_between y2=default', passed)
    end subroutine test_fill_between_y2_default

    subroutine test_twinx(passed)
        logical, intent(inout) :: passed
        class(figure_t), pointer :: fig
        real(wp) :: x(5), y1(5), y2(5)
        integer :: i

        do i = 1, 5
            x(i) = real(i, wp)
            y1(i) = real(i, wp)
            y2(i) = 100.0_wp - real(i, wp) * 10.0_wp
        end do
        call figure()
        call plot(x, y1, label='primary')
        call twinx()
        call plot(x, y2, label='secondary')
        call savefig('build/test/output/smoke_twinx.png')
        call assert_nonempty('build/test/output/smoke_twinx.png', 'twinx', passed)

        fig => get_global_figure()
        if (.not. associated(fig)) then
            print *, "FAIL (twinx): global figure not associated"
            passed = .false.
        else if (.not. fig%state%has_twinx) then
            print *, "FAIL (twinx): state%%has_twinx not set"
            passed = .false.
        else
            print *, "PASS (twinx): state%%has_twinx set"
        end if
    end subroutine test_twinx

    subroutine test_twiny(passed)
        logical, intent(inout) :: passed
        class(figure_t), pointer :: fig
        real(wp) :: x(5), y(5)
        integer :: i

        do i = 1, 5
            x(i) = real(i, wp)
            y(i) = real(i, wp)
        end do
        call figure()
        call plot(x, y, label='primary')
        call twiny()
        call plot(x * 2.0_wp, y, label='top axis')
        call savefig('build/test/output/smoke_twiny.png')
        call assert_nonempty('build/test/output/smoke_twiny.png', 'twiny', passed)

        fig => get_global_figure()
        if (.not. associated(fig)) then
            print *, "FAIL (twiny): global figure not associated"
            passed = .false.
        else if (.not. fig%state%has_twiny) then
            print *, "FAIL (twiny): state%%has_twiny not set"
            passed = .false.
        else
            print *, "PASS (twiny): state%%has_twiny set"
        end if
    end subroutine test_twiny

    subroutine run_imshow_cmap_probe()
        real(wp) :: z(10, 10)
        integer :: i, j
        class(*), pointer :: any_fig
        class(figure_t), pointer :: fig

        do i = 1, 10
            do j = 1, 10
                z(i, j) = sin(real(i, wp)) * cos(real(j, wp))
            end do
        end do

        call figure()
        call imshow(z, cmap='viridis')
        call savefig('build/test/output/smoke_imshow_cmap.png')

        any_fig => get_global_figure()
        select type (any_fig)
        type is (figure_t)
            fig => any_fig
        class default
            print *, 'FAIL: get_global_figure did not return figure_t'
            stop 1
        end select

        if (fig%plot_count < 1) then
            print *, 'FAIL: imshow cmap probe did not record a plot'
            stop 1
        end if

        if (trim(fig%plots(1)%pcolormesh_data%colormap_name) /= 'viridis') then
            print *, 'FAIL: imshow cmap was not preserved'
            stop 1
        end if
    end subroutine run_imshow_cmap_probe

    subroutine assert_no_cmap_warning()
        character(len=512) :: warning_line
        integer :: probe_unit, probe_ios

        call get_command_argument(0, arg0)
        output_log = 'build/test/output/test_new_plot_types_warning.log'
        if (is_windows()) then
            command = 'set FORTPLOT_FORCE_WARNINGS=1 && "' // trim(arg0) // &
                      '" --probe > "' // trim(output_log) // '" 2>&1'
        else
            command = 'FORTPLOT_FORCE_WARNINGS=1 "' // trim(arg0) // &
                      '" --probe > "' // trim(output_log) // '" 2>&1'
        end if

        call execute_command_line(command, wait=.true., exitstat=exitstat, &
                                  cmdstat=cmdstat)
        if (cmdstat /= 0 .or. exitstat /= 0) then
            print *, 'FAIL: imshow cmap warning probe execution failed'
            stop 1
        end if

        inquire(file=trim(output_log), exist=found_warning)
        if (.not. found_warning) then
            print *, 'FAIL: imshow cmap warning probe log missing'
            stop 1
        end if

        open(newunit=probe_unit, file=trim(output_log), status='old', &
             action='read', iostat=probe_ios)
        if (probe_ios /= 0) then
            print *, 'FAIL: could not read imshow cmap warning probe log'
            stop 1
        end if

        found_warning = .false.
        do
            read(probe_unit, '(A)', iostat=probe_ios) warning_line
            if (probe_ios /= 0) exit
            if (index(warning_line, "register_pcolormesh_plot_data: 'colormap' is deprecated; use 'cmap'") > 0) then
                found_warning = .true.
                exit
            end if
        end do

        close(probe_unit)

        if (found_warning) then
            print *, 'FAIL: unexpected imshow cmap deprecation warning'
            stop 1
        end if
    end subroutine assert_no_cmap_warning

end program test_new_plot_types

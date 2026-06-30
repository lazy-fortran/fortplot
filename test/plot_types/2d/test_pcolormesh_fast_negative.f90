program test_pcolormesh_fast_negative
    !! Fast, minimal pcolormesh test covering negative coords/values
    !! Verifies ASCII output contains negative tick labels (no external tools)
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure_t, figure, pcolormesh, savefig, title, &
                        add_plot, ensure_global_figure_initialized, get_global_figure
    use fortplot_system_runtime, only: is_windows
    use fortplot_plot_data, only: plot_data_t, PLOT_TYPE_PCOLORMESH, &
                                  PLOT_TYPE_LINE
    implicit none

    character(len=1024) :: arg0, arg1
    character(len=4096) :: command, output_log
    integer :: exitstat, cmdstat
    logical :: found_warning
    integer, parameter :: nx = 16, ny = 12
    real(wp) :: x(nx), y(ny)
    real(wp) :: c(ny-1, nx-1)
    real(wp) :: xline(2), yline(2)
    real(wp), parameter :: black(3) = [0.0_wp, 0.0_wp, 0.0_wp]
    integer :: i, j
    real(wp) :: xc, yc
    character(len=*), parameter :: out_txt = 'build/test/output/test_pcolormesh_fast_negative.txt'
    integer :: unit, ios
    character(len=8192) :: buf
    logical :: ok
    class(*), pointer :: any_fig
    class(figure_t), pointer :: fig
    type(plot_data_t), pointer :: plots(:)
    integer :: n

    call get_command_argument(1, arg1)
    if (trim(arg1) == '--probe') then
        call run_pcolormesh_probe()
        stop 0
    end if

    ! Negative to positive coordinates (small grid for speed)
    do i = 1, nx
        x(i) = -1.0_wp + 2.0_wp * real(i-1, wp) / real(nx-1, wp)
    end do
    do j = 1, ny
        y(j) = -0.8_wp + 1.6_wp * real(j-1, wp) / real(ny-1, wp)
    end do

    ! Cell-centered values spanning negative/positive
    do j = 1, nx-1
        do i = 1, ny-1
            xc = 0.5_wp * (x(j) + x(j+1))
            yc = 0.5_wp * (y(i) + y(i+1))
            c(i, j) = xc - yc  ! simple saddle: negative and positive
        end do
    end do

    call figure()
    call title('fast pcolormesh negative test')
    call pcolormesh(x, y, c, cmap='coolwarm')

    ! Overlay a simple diagonal line in explicit black
    xline = [x(1), x(nx)]
    yline = [y(1), y(ny)]
    call add_plot(xline, yline, color=black)

    ! Introspect global figure state to verify overlay ordering and color
    call ensure_global_figure_initialized()
    any_fig => get_global_figure()
    select type(any_fig)
    type is (figure_t)
        fig => any_fig
    class default
        print *, 'FAIL: get_global_figure did not return figure_t'
        stop 1
    end select

    plots => fig%get_plots()
    n = fig%get_plot_count()

    if (n < 2) then
        print *, 'FAIL: expected at least 2 plots, found', n
        stop 1
    end if

    if (plots(1)%plot_type /= PLOT_TYPE_PCOLORMESH) then
        print *, 'FAIL: first plot is not PLOT_TYPE_PCOLORMESH'
        stop 1
    end if

    if (plots(2)%plot_type /= PLOT_TYPE_LINE) then
        print *, 'FAIL: second plot is not PLOT_TYPE_LINE'
        stop 1
    end if

    if (any(abs(plots(2)%color - black) > 1.0e-12_wp)) then
        print *, 'FAIL: add_plot color was not propagated correctly'
        stop 1
    end if

    call savefig(out_txt)

    ! Read ASCII file and verify negative ticks appear
    ok = .false.
    open(newunit=unit, file=out_txt, status='old', action='read', iostat=ios)
    if (ios == 0) then
        do
            read(unit, '(A)', iostat=ios) buf
            if (ios /= 0) exit
            if (index(buf, '-') > 0) then
                ok = .true.
                exit
            end if
        end do
        close(unit)
    end if

    if (.not. ok) then
        print *, 'FAIL: negative tick label not found in ASCII output'
        stop 1
    end if

    call assert_no_cmap_warning()

    print *, 'PASS: fast negative pcolormesh ASCII output contains negative tick labels'

contains

    subroutine run_pcolormesh_probe()
        real(wp) :: x_probe(11), y_probe(11)
        real(wp) :: c_probe(10, 10)
        integer :: i, j

        do i = 1, 11
            x_probe(i) = real(i - 1, wp)
            y_probe(i) = real(i - 1, wp)
        end do

        do i = 1, 10
            do j = 1, 10
                c_probe(i, j) = real(i + j, wp)
            end do
        end do

        call figure()
        call pcolormesh(x_probe, y_probe, c_probe, cmap='plasma')
    end subroutine run_pcolormesh_probe

    subroutine assert_no_cmap_warning()
        character(len=512) :: warning_line
        integer :: probe_unit, probe_ios

        call get_command_argument(0, arg0)
        output_log = 'build/test/output/test_pcolormesh_fast_negative_warning.log'
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
            print *, 'FAIL: warning probe execution failed'
            stop 1
        end if

        inquire(file=trim(output_log), exist=found_warning)
        if (.not. found_warning) then
            print *, 'FAIL: warning probe log missing'
            stop 1
        end if

        open(newunit=probe_unit, file=trim(output_log), status='old', &
             action='read', iostat=probe_ios)
        if (probe_ios /= 0) then
            print *, 'FAIL: could not read warning probe log'
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
            print *, 'FAIL: unexpected pcolormesh colormap deprecation warning'
            stop 1
        end if
    end subroutine assert_no_cmap_warning
end program test_pcolormesh_fast_negative

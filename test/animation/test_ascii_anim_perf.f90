program test_ascii_anim_perf
    !! Micro-benchmark: time save_animation('*.txt') for a 100-frame run.
    !!
    !! Treats anything slower than 5 seconds as a regression on a modern
    !! laptop. Mainly here to keep the optimized path honest after PR #3.
    use iso_fortran_env, only: wp => real64, int64
    use fortplot
    use fortplot_animation
    use fortplot_system_runtime, only: create_directory_runtime, delete_file_runtime
    implicit none

    integer, parameter :: NFRAMES = 1000
    character(len=*), parameter :: PATH = "build/test/output/test_ascii_anim_perf.txt"

    type(figure_t), pointer :: pfig
    type(animation_t) :: anim
    real(wp) :: x(200), y(200), z(200)
    integer :: i, status
    integer(int64) :: tic, toc, rate
    real(wp) :: elapsed
    logical :: ok

    call create_directory_runtime("build/test/output", ok)
    if (.not. ok) error stop "could not create build/test/output"

    do i = 1, 200
        x(i) = real(i - 1, wp) * 0.05_wp
        y(i) = sin(x(i))
        z(i) = cos(x(i))
    end do

    call figure(figsize=[6.0_wp, 4.0_wp])
    pfig => get_global_figure()
    call add_3d_plot(x, y, z, label='wave', linestyle='-')

    anim = FuncAnimation(update, frames=NFRAMES, interval=10, fig=pfig)

    block
        integer :: render_only_loops
        real(wp) :: render_only_time
        integer(int64) :: r_tic, r_toc
        integer :: f
        render_only_loops = NFRAMES
        call system_clock(r_tic, rate)
        do f = 1, render_only_loops
            call update(f)
            call pfig%savefig_with_status(PATH, status)
            if (status /= 0) error stop "savefig per-frame failed"
        end do
        call system_clock(r_toc)
        render_only_time = real(r_toc - r_tic, wp) / real(rate, wp)
        print '(A,F8.3,A,I0,A)', 'per-frame savefig only: ', render_only_time, &
            ' s for ', render_only_loops, ' frames'
    end block

    call system_clock(tic, rate)
    call save_animation(anim, PATH, status=status)
    call system_clock(toc)
    if (status /= 0) error stop "save_animation .txt failed"

    elapsed = real(toc - tic, wp) / real(rate, wp)
    print '(A,F8.3,A,I0,A)', 'save_animation took ', elapsed, ' s for ', &
        NFRAMES, ' frames'

    if (elapsed > 5.0_wp) then
        print *, 'PERF: regression - took longer than 5 s'
        error stop
    end if

    call delete_file_runtime(PATH, ok)
    print *, "PASS test_ascii_anim_perf"

contains
    subroutine update(frame)
        integer, intent(in) :: frame
        real(wp) :: phase
        integer :: k
        phase = real(frame, wp) * 0.05_wp
        do k = 1, 200
            x(k) = real(k - 1, wp) * 0.05_wp
            y(k) = sin(x(k) + phase)
            z(k) = cos(x(k) + phase)
        end do
        call pfig%clear()
        call add_3d_plot(x, y, z, label='wave', linestyle='-')
        call pfig%set_rendered(.false.)
    end subroutine update
end program test_ascii_anim_perf

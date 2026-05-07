program three_d_animation_demo
    !! Animate a rotating 3D Lissajous curve and save it to MP4 and ASCII.
    !!
    !! Demonstrates that animation works the same way for vector/raster
    !! backends (.mp4) and the ASCII backend (.txt).
    use iso_fortran_env, only: wp => real64
    use fortplot
    use fortplot_animation
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none

    integer, parameter :: N_POINTS = 200
    integer, parameter :: N_FRAMES = 60
    character(len=*), parameter :: OUTDIR = "output/example/fortran/3d_animation_demo"

    type(figure_t), pointer :: pfig
    type(animation_t) :: anim
    real(wp), dimension(N_POINTS) :: xs, ys, zs
    real(wp) :: t
    integer :: i, status
    logical :: ok

    call create_directory_runtime(OUTDIR, ok)
    if (.not. ok) print *, "WARNING: could not create ", OUTDIR

    do i = 1, N_POINTS
        t = real(i - 1, wp) * 2.0_wp * 3.141592653589793_wp / real(N_POINTS - 1, wp)
        xs(i) = sin(3.0_wp * t)
        ys(i) = cos(2.0_wp * t)
        zs(i) = sin(t) * cos(t)
    end do

    call figure(figsize=[8.0_wp, 6.0_wp])
    pfig => get_global_figure()
    call add_3d_plot(xs, ys, zs, label="Lissajous 3D", linestyle='-')
    call title("Rotating 3D Lissajous")

    anim = FuncAnimation(rotate_curve, frames=N_FRAMES, interval=33, fig=pfig)

    print *, "Saving 3D animation as MP4..."
    call save_animation(anim, OUTDIR // "/animation.mp4", fps=30, status=status)
    call report_status("MP4", status)

    print *, "Saving 3D animation as ASCII..."
    call save_animation(anim, OUTDIR // "/animation.txt", status=status)
    call report_status("ASCII", status)

    if (status == 0) then
        print *, "Replay with: fpm run --target fortplot_play_ascii -- ", &
            OUTDIR // "/animation.txt --fps 24 --loop"
    end if

contains

    subroutine rotate_curve(frame)
        integer, intent(in) :: frame
        real(wp) :: phase, c, s
        integer :: k

        phase = real(frame - 1, wp) * 2.0_wp * 3.141592653589793_wp / real(N_FRAMES, wp)
        c = cos(phase)
        s = sin(phase)
        do k = 1, N_POINTS
            t = real(k - 1, wp) * 2.0_wp * 3.141592653589793_wp / real(N_POINTS - 1, wp)
            xs(k) =  c * sin(3.0_wp * t) - s * cos(2.0_wp * t)
            ys(k) =  s * sin(3.0_wp * t) + c * cos(2.0_wp * t)
            zs(k) =  sin(t) * cos(t)
        end do

        call pfig%clear()
        call add_3d_plot(xs, ys, zs, label="Lissajous 3D", linestyle='-')
        call title("Rotating 3D Lissajous")
        call pfig%set_rendered(.false.)
    end subroutine rotate_curve

    subroutine report_status(label, st)
        character(len=*), intent(in) :: label
        integer, intent(in) :: st

        select case (st)
        case (0)
            print *, label // ": OK"
        case (-1)
            print *, label // ": ffmpeg not found (.mp4 needs ffmpeg)"
        case (-3)
            print *, label // ": unsupported file format"
        case default
            print *, label // ": save failed, status=", st
        end select
    end subroutine report_status

end program three_d_animation_demo

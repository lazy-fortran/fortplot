program test_3d_backend_parity
    !! Verify 3D rendering and 3D animation work across PNG, PDF, and ASCII.
    !!
    !! Covers:
    !!   * 3D line plot (add_3d_plot) -> .png, .pdf, .txt
    !!   * 3D surface plot (add_surface) -> .png, .pdf, .txt
    !!   * 3D animation -> .txt (frame headers must be present)
    use iso_fortran_env, only: wp => real64, iostat_end
    use fortplot
    use fortplot_animation
    use fortplot_ascii_player, only: count_animation_frames
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none

    integer, parameter :: NLINE = 64
    integer, parameter :: NGRID = 9
    integer, parameter :: NFRAMES = 4
    character(len=*), parameter :: OUTDIR = "build/test/output/3d_parity"

    type(figure_t), pointer :: pfig
    type(animation_t) :: anim
    real(wp) :: x(NLINE), y(NLINE), z(NLINE)
    real(wp) :: xg(NGRID), yg(NGRID), zg(NGRID, NGRID)
    integer :: i, j, status, n_frames_in_file
    logical :: ok

    call create_directory_runtime(OUTDIR, ok)
    if (.not. ok) error stop "could not create build/test/output/3d_parity"

    call build_helix(x, y, z)
    call build_gauss(xg, yg, zg)

    call run_3d_line_render('.png')
    call run_3d_line_render('.pdf')
    call run_3d_line_render('.txt')

    call run_3d_surface_render('.png')
    call run_3d_surface_render('.pdf')
    call run_3d_surface_render('.txt')

    call run_3d_animation_ascii()

    print *, "PASS test_3d_backend_parity"

contains

    subroutine build_helix(xa, ya, za)
        real(wp), intent(out) :: xa(:), ya(:), za(:)
        integer :: k
        do k = 1, size(xa)
            xa(k) = cos(real(k - 1, wp) * 0.2_wp)
            ya(k) = sin(real(k - 1, wp) * 0.2_wp)
            za(k) = real(k - 1, wp) * 0.05_wp
        end do
    end subroutine build_helix

    subroutine build_gauss(xa, ya, za)
        real(wp), intent(out) :: xa(:), ya(:), za(:, :)
        integer :: ii, jj
        do ii = 1, size(xa)
            xa(ii) = -2.0_wp + (ii - 1) * 4.0_wp / real(size(xa) - 1, wp)
            ya(ii) = -2.0_wp + (ii - 1) * 4.0_wp / real(size(ya) - 1, wp)
        end do
        do ii = 1, size(xa)
            do jj = 1, size(ya)
                za(ii, jj) = exp(-(xa(ii)**2 + ya(jj)**2))
            end do
        end do
    end subroutine build_gauss

    subroutine run_3d_line_render(ext)
        character(len=*), intent(in) :: ext
        character(len=:), allocatable :: path
        path = OUTDIR // "/line" // ext
        call figure(figsize=[6.0_wp, 4.0_wp])
        call add_3d_plot(x, y, z, label="helix")
        call title("3D line " // ext)
        call savefig(path)
        call assert_nonempty(path, "3D line " // ext)
    end subroutine run_3d_line_render

    subroutine run_3d_surface_render(ext)
        character(len=*), intent(in) :: ext
        character(len=:), allocatable :: path
        path = OUTDIR // "/surface" // ext
        call figure(figsize=[6.0_wp, 4.0_wp])
        call add_surface(xg, yg, zg, cmap='viridis', filled=.true.)
        call title("3D surface " // ext)
        call savefig(path)
        call assert_nonempty(path, "3D surface " // ext)
    end subroutine run_3d_surface_render

    subroutine run_3d_animation_ascii()
        character(len=:), allocatable :: path
        path = OUTDIR // "/anim.txt"

        call figure(figsize=[6.0_wp, 4.0_wp])
        pfig => get_global_figure()
        call add_3d_plot(x, y, z, label="helix")
        call title("3D animation labels")
        call xlabel("X axis")
        call ylabel("Y axis")

        anim = FuncAnimation(rotate_helix, frames=NFRAMES, interval=10, fig=pfig)
        call save_animation(anim, path, status=status)
        if (status /= 0) then
            print *, "save_animation .txt status=", status
            error stop "3D animation save to .txt failed"
        end if

        call count_animation_frames(path, n_frames_in_file)
        if (n_frames_in_file /= NFRAMES) then
            print *, "expected ", NFRAMES, " frames, got ", n_frames_in_file
            error stop "3D animation .txt frame count mismatch"
        end if
        call assert_line_count(path, "3D animation labels", NFRAMES)
        call assert_line_count(path, "X axis", NFRAMES)
        call assert_line_count(path, "Y axis", NFRAMES)
    end subroutine run_3d_animation_ascii

    subroutine rotate_helix(frame)
        integer, intent(in) :: frame
        real(wp) :: phase
        integer :: k
        phase = real(frame - 1, wp) * 0.5_wp
        do k = 1, NLINE
            x(k) = cos(real(k - 1, wp) * 0.2_wp + phase)
            y(k) = sin(real(k - 1, wp) * 0.2_wp + phase)
            z(k) = real(k - 1, wp) * 0.05_wp
        end do
        call pfig%clear()
        call add_3d_plot(x, y, z, label="helix")
        call title("3D animation labels")
        call xlabel("X axis")
        call ylabel("Y axis")
        call pfig%set_rendered(.false.)
    end subroutine rotate_helix

    subroutine assert_line_count(path, needle, expected)
        character(len=*), intent(in) :: path, needle
        integer, intent(in) :: expected
        integer :: unit, ios, count
        character(len=512) :: line

        open(newunit=unit, file=path, status='old', action='read', iostat=ios)
        if (ios /= 0) error stop "could not open animation text output"
        count = 0
        do
            read(unit, '(A)', iostat=ios) line
            if (ios == iostat_end) exit
            if (ios /= 0) error stop "could not read animation text output"
            if (trim(adjustl(line)) == needle) count = count + 1
        end do
        close(unit)
        if (count /= expected) then
            print *, "expected ", expected, " lines for ", needle, " got ", count
            error stop "3D animation label line count mismatch"
        end if
    end subroutine assert_line_count

    subroutine assert_nonempty(path, what)
        character(len=*), intent(in) :: path, what
        integer :: unit, ios, byte_count
        character(len=1) :: ch

        open(newunit=unit, file=path, status='old', action='read', &
             access='stream', form='unformatted', iostat=ios)
        if (ios /= 0) then
            print *, what // ": cannot open ", trim(path)
            error stop
        end if
        byte_count = 0
        do
            read(unit, iostat=ios) ch
            if (ios /= 0) exit
            byte_count = byte_count + 1
            if (byte_count > 256) exit
        end do
        close(unit)
        if (byte_count < 64) then
            print *, what // ": output too small (", byte_count, " bytes) at ", trim(path)
            error stop
        end if
    end subroutine assert_nonempty

end program test_3d_backend_parity

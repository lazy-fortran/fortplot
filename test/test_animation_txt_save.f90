program test_animation_txt_save
    !! Test that save_animation accepts .txt format and produces
    !! a text file with === Frame N === delimiters.
    use fortplot
    use fortplot_animation
    use fortplot_system_runtime, only: create_directory_runtime, delete_file_runtime
    implicit none

    integer, parameter :: nframes = 3
    type(figure_t), pointer :: pfig
    type(animation_t) :: anim
    real(wp) :: x(32), y(32)
    integer :: i, status
    logical :: ok, exists
    character(len=*), parameter :: outdir = "build/test/output/"
    character(len=*), parameter :: outfile = "build/test/output/test_animation_txt_save.txt"
    character(len=256) :: line
    integer :: unit, ios, frame_count

    do i = 1, size(x)
        x(i) = real(i - 1, wp)
        y(i) = sin(x(i) * 0.5_wp)
    end do

    call create_directory_runtime(outdir, ok)
    if (.not. ok) error stop "failed to create output directory"

    call figure(figsize=[6.0_wp, 4.0_wp])
    pfig => get_global_figure()
    call add_plot(x, y)
    call title("Sine wave")
    call xlabel("x")
    call ylabel("sin(x)")

    anim = FuncAnimation(update_sine, frames=nframes, interval=10, fig=pfig)
    call save_animation(anim, outfile, status=status)

    if (status /= 0) then
        print *, "FAIL: save_animation returned status ", status
        error stop "save_animation .txt failed with non-zero status"
    end if

    inquire(file=outfile, exist=exists)
    if (.not. exists) then
        print *, "FAIL: output file not created: ", outfile
        error stop "save_animation .txt did not create output file"
    end if

    frame_count = 0
    open(newunit=unit, file=outfile, status='old', action='read', iostat=ios)
    if (ios /= 0) then
        print *, "FAIL: could not open output file for reading"
        error stop "cannot open generated .txt file"
    end if

    do
        read(unit, '(A)', iostat=ios) line
        if (ios /= 0) exit
        if (index(line, '=== Frame') > 0) frame_count = frame_count + 1
    end do
    close(unit)

    if (frame_count /= nframes) then
        print *, "FAIL: expected ", nframes, " frames, got ", frame_count
        error stop "frame count mismatch in .txt animation"
    end if

    call delete_file_runtime(outfile, ok)

contains

    subroutine update_sine(frame)
        integer, intent(in) :: frame
        real(wp) :: phase
        phase = real(frame, wp) * 0.3_wp
        y = sin(x * 0.5_wp + phase)
        call pfig%clear()
        call add_plot(x, y)
        call title("Sine wave")
        call xlabel("x")
        call ylabel("sin(x)")
        call pfig%set_rendered(.false.)
    end subroutine update_sine

end program test_animation_txt_save

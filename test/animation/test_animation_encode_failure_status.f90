program test_animation_encode_failure_status
    !! When the ffmpeg encoder fails, save_animation must report failure even
    !! if an older movie already exists at the requested path, and it must
    !! still write the documented PNG frame-sequence fallback.
    !!
    !! The test re-runs itself as a child with a fake `ffmpeg` first on PATH
    !! that exits 1 without writing anything. Oracles: the child's status
    !! (nonzero), the fallback PNG files on disk and their PNG signature.
    use fortplot
    use fortplot_animation
    use fortplot_pipe, only: check_ffmpeg_available
    use fortplot_system_runtime, only: create_directory_runtime, is_windows
    use iso_fortran_env, only: real64
    implicit none

    character(len=*), parameter :: dir = 'build/test/output/anim_encode_failure'
    character(len=*), parameter :: movie = dir//'/movie.mp4'
    character(len=*), parameter :: status_file = dir//'/child_status.txt'
    integer, parameter :: nframes = 3
    type(figure_t), target :: fig
    real(real64) :: x(20), y(20)
    character(len=512) :: self_path, mode
    integer :: i, status, exitstat, cmdstat, unit
    logical :: ok

    x = [(real(i - 1, real64)/19.0_real64, i = 1, 20)]
    y = 0.0_real64
    mode = ''
    if (command_argument_count() >= 1) call get_command_argument(1, mode)
    if (trim(mode) == 'child') then
        call save_movie(status)
        open (newunit=unit, file=status_file, status='replace', action='write')
        write (unit, '(i0)') status
        close (unit)
        stop 0
    end if

    if (is_windows()) then
        print *, 'SKIPPED: fake ffmpeg needs a POSIX shell'
        stop 0
    end if
    if (.not. check_ffmpeg_available()) then
        print *, 'SKIPPED: ffmpeg needed to create the stale movie'
        stop 0
    end if
    call create_directory_runtime(dir//'/fakebin', ok)
    if (.not. ok) error stop 'cannot create test output directory'
    call execute_command_line('rm -f "'//dir//'"/movie*', wait=.true.)

    ! A genuine earlier movie at the requested path.
    call save_movie(status)
    if (status /= 0) error stop 'setup: real ffmpeg could not encode the movie'

    open (newunit=unit, file=dir//'/fakebin/ffmpeg', status='replace', &
          action='write')
    write (unit, '(a)') '#!/bin/sh'
    write (unit, '(a)') 'echo "fake ffmpeg: encoder failure" >&2'
    write (unit, '(a)') 'exit 1'
    close (unit)
    call execute_command_line('chmod +x "'//dir//'/fakebin/ffmpeg"', wait=.true.)

    call get_command_argument(0, self_path)
    call execute_command_line('PATH="$(pwd)/'//dir//'/fakebin:$PATH" "'// &
                              trim(self_path)//'" child', wait=.true., &
                              exitstat=exitstat, cmdstat=cmdstat)
    if (cmdstat /= 0 .or. exitstat /= 0) error stop 'child run failed'

    open (newunit=unit, file=status_file, status='old', action='read')
    read (unit, *) status
    close (unit)
    print '(a,i0)', ' status after failed encode: ', status
    if (status == 0) then
        print *, 'FAIL: save_animation reported success although ffmpeg failed'
        stop 1
    end if
    do i = 0, nframes - 1
        if (.not. is_png(frame_name(i))) then
            print *, 'FAIL: missing PNG fallback frame ', frame_name(i)
            stop 1
        end if
    end do
    print *, 'PASS: failed encode reports an error and keeps the PNG fallback'

contains

    subroutine save_movie(stat)
        integer, intent(out) :: stat
        type(animation_t) :: anim
        call fig%initialize(width=320, height=240)
        call fig%add_plot(x, y)
        call fig%set_ylim(-1.0_real64, 1.0_real64)
        anim = FuncAnimation(update, frames=nframes, interval=100, fig=fig)
        call save_animation(anim, movie, fps=10, status=stat)
    end subroutine save_movie

    subroutine update(frame)
        integer, intent(in) :: frame
        y = sin(6.0_real64*x + real(frame, real64))
        call fig%set_ydata(1, y)
    end subroutine update

    function frame_name(k) result(name)
        integer, intent(in) :: k
        character(len=:), allocatable :: name
        character(len=16) :: buf
        write (buf, '(i0)') k
        name = dir//'/movie_frame_'//trim(buf)//'.png'
    end function frame_name

    logical function is_png(path)
        character(len=*), intent(in) :: path
        character(len=8) :: magic
        integer :: u, ios
        is_png = .false.
        open (newunit=u, file=path, access='stream', form='unformatted', &
              status='old', action='read', iostat=ios)
        if (ios /= 0) return
        read (u, iostat=ios) magic
        close (u)
        is_png = ios == 0 .and. magic == achar(137)//'PNG'//achar(13)// &
                 achar(10)//achar(26)//achar(10)
    end function is_png

end program test_animation_encode_failure_status

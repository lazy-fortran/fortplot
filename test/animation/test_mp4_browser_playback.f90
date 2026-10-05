program test_mp4_browser_playback
    !! MP4 output must play faithfully in browsers, QuickTime and Safari.
    !! ffprobe/ffmpeg and a direct MP4 box scan are the independent oracles:
    !! H.264 yuv420p with even dimensions (an odd figure must still encode),
    !! BT.709 tags, constant frame rate with every written frame present,
    !! one keyframe per second, moov before mdat, and frames in order.
    use fortplot
    use fortplot_animation
    use fortplot_pipe, only: check_ffmpeg_available
    use fortplot_system_runtime, only: create_directory_runtime, is_windows
    use iso_fortran_env, only: real64
    implicit none

    integer, parameter :: nframes = 30, fps = 10
    character(len=*), parameter :: dir = 'build/test/output/mp4_playback'
    character(len=*), parameter :: movie = dir // '/odd_size.mp4'
    type(figure_t), target :: fig
    type(animation_t) :: anim
    real(real64) :: x(50), y(50)
    integer :: i, status
    logical :: ok
    real(real64) :: psnr_same, psnr_other

    if (is_windows()) then
        print *, 'SKIPPED: probe commands use a POSIX shell'
        stop 0
    end if
    if (.not. (check_ffmpeg_available() .and. command_ok('ffprobe -version'))) then
        print *, 'SKIPPED: ffmpeg/ffprobe not available'
        stop 0
    end if
    call create_directory_runtime(dir, ok)
    if (.not. ok) error stop 'cannot create test output directory'

    x = [(real(i - 1, real64)/49.0_real64, i = 1, 50)]
    y = 0.0_real64
    call fig%initialize(width=321, height=241)
    call fig%add_plot(x, y)
    call fig%set_ylim(-1.0_real64, 1.0_real64)
    call update(1)
    call fig%savefig(dir // '/frame_first.png')

    anim = FuncAnimation(update, frames=nframes, interval=100, fig=fig)
    call save_animation(anim, movie, fps=fps, status=status)
    if (status /= 0) error stop 'save_animation failed for an odd-sized figure'
    call fig%savefig(dir // '/frame_last.png')

    call expect(probe('stream=codec_name') == 'h264', 'codec h264')
    call expect(probe('stream=pix_fmt') == 'yuv420p', 'pix_fmt yuv420p')
    call expect(probe('stream=width') == '322', 'width padded to even 322')
    call expect(probe('stream=height') == '242', 'height padded to even 242')
    call expect(probe('stream=r_frame_rate') == '10/1', 'constant 10 fps')
    call expect(probe('stream=nb_frames') == '30', 'all 30 frames encoded')
    call expect(probe('stream=color_space') == 'bt709', 'BT.709 matrix tag')
    call expect(probe('stream=color_primaries') == 'bt709', 'BT.709 primaries tag')
    call expect(probe('stream=color_transfer') == 'bt709', 'BT.709 transfer tag')
    call expect(probe('stream=color_range') == 'tv', 'limited range tag')
    call expect(keyframe_count() == nframes/fps, 'one keyframe per second')
    call expect(moov_before_mdat(movie), 'faststart: moov precedes mdat')

    psnr_same = frame_psnr(nframes - 1, dir // '/frame_last.png')
    psnr_other = frame_psnr(nframes - 1, dir // '/frame_first.png')
    print '(a,f8.2,a,f8.2)', ' last-frame PSNR vs last/first PNG:', psnr_same, ' /', psnr_other
    call expect(psnr_same > 30.0_real64, 'last frame visually lossless')
    call expect(psnr_same > psnr_other + 3.0_real64, 'last frame is the final state')
    psnr_same = frame_psnr(0, dir // '/frame_first.png')
    call expect(psnr_same > 30.0_real64, 'first frame is the initial state')
    print *, 'PASS: MP4 browser playback properties'

contains

    subroutine update(frame)
        integer, intent(in) :: frame
        y = sin(6.283185307179586_real64*(x - real(frame, real64)/nframes))
        call fig%set_ydata(1, y)
    end subroutine update

    subroutine expect(condition, what)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: what
        if (.not. condition) then
            print *, 'FAIL: ', what
            error stop 1
        end if
        print *, 'ok: ', what
    end subroutine expect

    logical function command_ok(command)
        character(len=*), intent(in) :: command
        integer :: exitstat, cmdstat
        call execute_command_line(command // ' >/dev/null 2>&1', wait=.true., &
                                  exitstat=exitstat, cmdstat=cmdstat)
        command_ok = (cmdstat == 0 .and. exitstat == 0)
    end function command_ok

    function capture(command) result(line)
        character(len=*), intent(in) :: command
        character(len=256) :: line
        character(len=*), parameter :: tmp = dir // '/probe.txt'
        integer :: unit, ios, exitstat, cmdstat
        line = ''
        call execute_command_line(command // ' > "' // tmp // '"', wait=.true., &
                                  exitstat=exitstat, cmdstat=cmdstat)
        if (cmdstat /= 0 .or. exitstat /= 0) return
        open(newunit=unit, file=tmp, action='read', iostat=ios)
        if (ios /= 0) return
        read(unit, '(a)', iostat=ios) line
        close(unit)
    end function capture

    function probe(entry) result(value)
        character(len=*), intent(in) :: entry
        character(len=:), allocatable :: value
        value = trim(capture('ffprobe -v error -select_streams v:0 -show_entries ' // &
                             entry // ' -of default=nw=1:nk=1 "' // movie // '"'))
    end function probe

    integer function keyframe_count()
        character(len=256) :: line
        line = capture('ffprobe -v error -select_streams v:0 -show_entries packet=flags' // &
                       ' -of csv=p=0 "' // movie // '" | grep -c K')
        read(line, *, iostat=i) keyframe_count
        if (i /= 0) keyframe_count = -1
    end function keyframe_count

    real(real64) function frame_psnr(index, reference) result(psnr)
        integer, intent(in) :: index
        character(len=*), intent(in) :: reference
        character(len=256) :: line
        character(len=16) :: idx
        integer :: p, ios
        write(idx, '(i0)') index
        line = capture('ffmpeg -nostdin -hide_banner -i "' // movie // '" -i "' // &
            reference // '" -lavfi "[0:v]select=eq(n\,' // trim(idx) // &
            '),scale=flags=accurate_rnd+full_chroma_int,format=rgb24,' // &
            'crop=321:241:0:0[a];[1:v]format=rgb24[b];[a][b]psnr" -frames:v 1 -f null - 2>&1' // &
            ' | grep -o "average:[0-9.]*" | cut -d: -f2')
        psnr = -1.0_real64
        p = len_trim(line)
        if (p > 0) read(line, *, iostat=ios) psnr
    end function frame_psnr

    logical function moov_before_mdat(path)
        !! Walk top-level ISO-BMFF boxes: 32-bit big-endian size, then 4-byte type.
        character(len=*), intent(in) :: path
        integer :: unit, ios, pos, fsize
        integer(1) :: hdr(8)
        integer(8) :: box
        character(len=4) :: kind
        moov_before_mdat = .false.
        open(newunit=unit, file=path, access='stream', form='unformatted', &
             action='read', iostat=ios)
        if (ios /= 0) return
        inquire(unit=unit, size=fsize)
        pos = 1
        do while (pos + 8 <= fsize + 1)
            read(unit, pos=pos, iostat=ios) hdr
            if (ios /= 0) exit
            box = 0
            do i = 1, 4
                box = box*256 + iand(int(hdr(i), 8), 255_8)
            end do
            kind = transfer(hdr(5:8), kind)
            if (kind == 'moov') moov_before_mdat = .true.
            if (kind == 'moov' .or. kind == 'mdat' .or. box < 8) exit
            pos = pos + int(box)
        end do
        close(unit)
    end function moov_before_mdat

end program test_mp4_browser_playback

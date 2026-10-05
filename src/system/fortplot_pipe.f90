module fortplot_pipe
    use fortplot_system_runtime, only: check_command_available_runtime, &
                                       delete_file_runtime, is_windows
    implicit none
    private

    public :: open_ffmpeg_pipe
    public :: write_png_to_pipe
    public :: close_ffmpeg_pipe
    public :: check_ffmpeg_available

    logical, save :: pipe_open = .false.
    integer, save :: frame_index = 0
    integer, save :: output_fps = 0
    character(len=:), allocatable, save :: output_filename
    character(len=:), allocatable, save :: frame_directory

contains

    function open_ffmpeg_pipe(filename, fps) result(status)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: fps
        integer :: status
        logical :: ok

        call reset_pipe_state()

        if (fps <= 0) then
            status = -1
            return
        end if

        if (.not. is_supported_path(filename)) then
            status = -1
            return
        end if

        output_filename = trim(filename)
        output_fps = fps
        call create_private_frame_directory(ok)
        if (.not. ok) then
            status = -1
            call reset_pipe_state()
            return
        end if

        frame_index = 0
        pipe_open = .true.
        status = 0
    end function open_ffmpeg_pipe

    function write_png_to_pipe(png_data) result(status)
        integer(1), intent(in) :: png_data(:)
        integer :: status
        character(len=:), allocatable :: frame_file

        if (.not. pipe_open) then
            status = -2
            return
        end if

        if (size(png_data) == 0) then
            status = -3
            return
        end if

        frame_file = build_frame_filename(frame_index)
        call write_binary_file(frame_file, png_data, status)
        if (status == 0) frame_index = frame_index + 1
    end function write_png_to_pipe

    function close_ffmpeg_pipe() result(status)
        integer :: status
        character(len=:), allocatable :: command
        integer :: cmdstat, exitstat

        if (.not. pipe_open) then
            status = 0
            return
        end if

        if (frame_index == 0) then
            call cleanup_frame_directory()
            call reset_pipe_state()
            status = -1
            return
        end if

        command = build_encode_command(trim(frame_directory) // path_sep() // 'frame_%06d.png', &
                                       output_filename, output_fps)
        call execute_command_line(command, wait=.true., exitstat=exitstat, cmdstat=cmdstat)

        call cleanup_frame_directory()
        call reset_pipe_state()

        if (cmdstat /= 0) then
            status = -1
        else
            status = exitstat
        end if
    end function close_ffmpeg_pipe

    function build_encode_command(frame_pattern, filename, fps) result(command)
        !! H.264 settings that browsers, QuickTime and Safari play back faithfully:
        !! even dimensions padded with white (yuv420p requires them), an explicit
        !! BT.709 conversion that is also tagged in the stream (untagged streams
        !! are decoded with player-dependent matrices), visually lossless CRF,
        !! a keyframe every second for scrubbing, and moov-first MP4 layout.
        character(len=*), intent(in) :: frame_pattern, filename
        integer, intent(in) :: fps
        character(len=:), allocatable :: command

        command = 'ffmpeg -y -nostdin -hide_banner -loglevel error -framerate ' // &
                  trim(int_to_str(fps)) // ' -i "' // frame_pattern // '"' // &
                  ' -vf "pad=ceil(iw/2)*2:ceil(ih/2)*2:color=white,' // &
                  'scale=out_color_matrix=bt709:out_range=tv:' // &
                  'flags=lanczos+accurate_rnd+full_chroma_int,format=yuv420p,' // &
                  'setparams=range=tv:color_primaries=bt709:color_trc=bt709:colorspace=bt709"' // &
                  ' -c:v libx264 -preset slow -crf 18 -g ' // trim(int_to_str(fps)) // &
                  ' -pix_fmt yuv420p'
        if (has_mp4_layout(filename)) command = command // ' -movflags +faststart'
        command = command // ' "' // trim(filename) // '"'
    end function build_encode_command

    logical function has_mp4_layout(filename) result(mp4)
        character(len=*), intent(in) :: filename
        character(len=4) :: tail
        integer :: n, i

        n = len_trim(filename)
        mp4 = .false.
        if (n < 4) return
        tail = filename(n-3:n)
        do i = 1, 4
            if (tail(i:i) >= 'A' .and. tail(i:i) <= 'Z') &
                tail(i:i) = achar(iachar(tail(i:i)) + 32)
        end do
        mp4 = (tail == '.mp4' .or. tail == '.mov' .or. tail == '.m4v')
    end function has_mp4_layout

    function check_ffmpeg_available() result(available)
        logical :: available

        call check_command_available_runtime("ffmpeg", available)
    end function check_ffmpeg_available

    subroutine write_binary_file(filename, data, status)
        character(len=*), intent(in) :: filename
        integer(1), intent(in) :: data(:)
        integer, intent(out) :: status
        integer :: unit_num, ios

        open(newunit=unit_num, file=trim(filename), access='stream', form='unformatted', &
             status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            status = -5
            return
        end if

        write(unit_num, iostat=ios) data
        close(unit_num)
        if (ios /= 0) then
            status = -5
        else
            status = 0
        end if
    end subroutine write_binary_file

    subroutine create_private_frame_directory(ok)
        !! Create a frame directory that no other encoder shares. A plain mkdir
        !! fails on an existing directory, so concurrent processes or stale
        !! frames from an interrupted run can never be mixed into this movie.
        logical, intent(out) :: ok
        character(len=:), allocatable :: base, command
        integer :: count, rate, max_count, attempt, cmdstat, exitstat

        call system_clock(count, rate, max_count)
        base = temp_root() // path_sep() // 'fortplot_ffmpeg_' // trim(int_to_str(count))
        ok = .false.
        do attempt = 0, 15
            frame_directory = base // '_' // trim(int_to_str(attempt))
            if (is_windows()) then
                command = 'mkdir "' // frame_directory // '" >NUL 2>NUL'
            else
                command = 'mkdir "' // frame_directory // '" >/dev/null 2>&1'
            end if
            call execute_command_line(command, wait=.true., exitstat=exitstat, cmdstat=cmdstat)
            if (cmdstat == 0 .and. exitstat == 0) then
                ok = .true.
                return
            end if
        end do
    end subroutine create_private_frame_directory

    function build_frame_filename(index_value) result(path)
        integer, intent(in) :: index_value
        character(len=:), allocatable :: path
        character(len=32) :: counter

        write(counter, '(I6.6)') index_value
        path = trim(frame_directory) // path_sep() // 'frame_' // trim(counter) // '.png'
    end function build_frame_filename

    subroutine cleanup_frame_directory()
        logical :: deleted
        integer :: i, cmdstat, exitstat
        character(len=:), allocatable :: command

        if (.not. allocated(frame_directory)) return

        do i = 0, frame_index - 1
            call delete_file_runtime(build_frame_filename(i), deleted)
        end do

        if (is_windows()) then
            command = 'rmdir "' // trim(frame_directory) // '" >NUL 2>NUL'
        else
            command = 'rmdir "' // trim(frame_directory) // '" >/dev/null 2>&1'
        end if
        call execute_command_line(command, wait=.true., exitstat=exitstat, cmdstat=cmdstat)
    end subroutine cleanup_frame_directory

    subroutine reset_pipe_state()
        pipe_open = .false.
        frame_index = 0
        output_fps = 0
        if (allocated(output_filename)) deallocate(output_filename)
        if (allocated(frame_directory)) deallocate(frame_directory)
    end subroutine reset_pipe_state

    function temp_root() result(path)
        character(len=:), allocatable :: path
        character(len=512) :: env_value
        integer :: status

        if (is_windows()) then
            call get_environment_variable('TEMP', env_value, status=status)
            if (status == 0 .and. len_trim(env_value) > 0) then
                path = trim(env_value)
                return
            end if
            path = '.'
        else
            call get_environment_variable('TMPDIR', env_value, status=status)
            if (status == 0 .and. len_trim(env_value) > 0) then
                path = trim(env_value)
            else
                path = '/tmp'
            end if
        end if
    end function temp_root

    function path_sep() result(sep)
        character(len=1) :: sep

        if (is_windows()) then
            sep = '\'
        else
            sep = '/'
        end if
    end function path_sep

    logical function is_supported_path(path) result(ok)
        character(len=*), intent(in) :: path
        integer :: i

        ok = (len_trim(path) > 0)
        if (.not. ok) return

        do i = 1, len_trim(path)
            select case (path(i:i))
            case ('"', char(10), char(13))
                ok = .false.
                return
            case default
            end select
        end do
    end function is_supported_path

    function int_to_str(value) result(text)
        integer, intent(in) :: value
        character(len=32) :: text

        write(text, '(I0)') value
    end function int_to_str

end module fortplot_pipe

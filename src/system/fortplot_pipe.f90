module fortplot_pipe
    use fortplot_system_runtime, only: check_command_available_runtime, create_directory_runtime, &
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
        frame_directory = build_frame_directory()
        call create_directory_runtime(frame_directory, ok)
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

        command = 'ffmpeg -y -framerate ' // trim(int_to_str(output_fps)) // ' -i "' // &
                  trim(frame_directory) // path_sep() // 'frame_%06d.png" -vcodec libx264 -pix_fmt yuv420p "' // &
                  trim(output_filename) // '"'
        call execute_command_line(command, wait=.true., exitstat=exitstat, cmdstat=cmdstat)

        call cleanup_frame_directory()
        call reset_pipe_state()

        if (cmdstat /= 0) then
            status = -1
        else
            status = exitstat
        end if
    end function close_ffmpeg_pipe

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

    function build_frame_directory() result(path)
        character(len=:), allocatable :: path
        integer :: count, rate, max_count

        call system_clock(count, rate, max_count)
        path = temp_root() // path_sep() // 'fortplot_ffmpeg_' // trim(int_to_str(count))
    end function build_frame_directory

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

module fortplot_pipe
    use iso_c_binding
    use fortplot_system_runtime, only: is_windows
    implicit none
    private
    
    public :: open_ffmpeg_pipe, write_png_to_pipe, close_ffmpeg_pipe
    public :: check_ffmpeg_available
    public :: check_ffmpeg_available_timeout
    
    interface
        function open_ffmpeg_pipe_c(filename, fps) result(status) bind(C, name="open_ffmpeg_pipe_c")
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: filename(*)
            integer(c_int), value, intent(in) :: fps
            integer(c_int) :: status
        end function open_ffmpeg_pipe_c
        
        function write_png_to_pipe_c(png_data, data_size) result(status) bind(C, name="write_png_to_pipe_c")
            import :: c_ptr, c_size_t, c_int
            type(c_ptr), value, intent(in) :: png_data
            integer(c_size_t), value, intent(in) :: data_size
            integer(c_int) :: status
        end function write_png_to_pipe_c
        
        function close_ffmpeg_pipe_c() result(status) bind(C, name="close_ffmpeg_pipe_c")
            import :: c_int
            integer(c_int) :: status
        end function close_ffmpeg_pipe_c
        
        function check_ffmpeg_available_c() result(available) bind(C, name="check_ffmpeg_available_c")
            import :: c_int
            integer(c_int) :: available
        end function check_ffmpeg_available_c
        
        function check_ffmpeg_available_timeout_c() result(available) bind(C, name="check_ffmpeg_available_timeout_c")
            import :: c_int
            integer(c_int) :: available
        end function check_ffmpeg_available_timeout_c
    end interface
    
contains

    function open_ffmpeg_pipe(filename, fps) result(status)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: fps
        integer :: status
        
        character(len=len_trim(filename)+1, kind=c_char) :: c_filename
        
        c_filename = trim(filename) // c_null_char
        status = int(open_ffmpeg_pipe_c(c_filename, int(fps, c_int)))
    end function open_ffmpeg_pipe

    function write_png_to_pipe(png_data) result(status)
        integer(1), intent(in), target :: png_data(:)
        integer :: status
        
        status = int(write_png_to_pipe_c(c_loc(png_data), int(size(png_data), c_size_t)))
    end function write_png_to_pipe

    function close_ffmpeg_pipe() result(status)
        integer :: status
        
        status = int(close_ffmpeg_pipe_c())
    end function close_ffmpeg_pipe

    function check_ffmpeg_available() result(available)
        logical :: available
        
        if (is_windows()) then
            ! Use timeout-protected version on Windows
            available = (check_ffmpeg_available_timeout_c() == 1)
            write(*,'(A,L1)') 'DEBUG: [check_ffmpeg] Windows timeout check result: ', available
        else
            available = (check_ffmpeg_available_c() == 1)
        end if
    end function check_ffmpeg_available

    function check_ffmpeg_available_timeout() result(available)
        !! Timeout-protected FFmpeg availability check for Windows CI
        logical :: available
        
        write(*,'(A)') 'DEBUG: [check_ffmpeg_timeout] Starting timeout-protected check'
        available = (check_ffmpeg_available_timeout_c() == 1)
        write(*,'(A,L1)') 'DEBUG: [check_ffmpeg_timeout] Result: ', available
    end function check_ffmpeg_available_timeout

end module fortplot_pipe
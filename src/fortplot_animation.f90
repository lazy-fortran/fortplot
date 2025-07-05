module fortplot_animation
    use iso_fortran_env, only: real64
    use fortplot_figure_core, only: figure_t
    use fortplot_pipe, only: open_ffmpeg_pipe, write_png_to_pipe, close_ffmpeg_pipe
    implicit none
    private

    ! Animation callback interface
    abstract interface
        subroutine animate_interface(frame)
            integer, intent(in) :: frame
        end subroutine animate_interface
    end interface

    ! Animation type
    type :: animation_t
        procedure(animate_interface), pointer, nopass :: animate_func => null()
        integer :: frames = 0
        integer :: interval_ms = 50
        logical :: save_frames = .false.
        character(len=:), allocatable :: frame_pattern
        type(figure_t), pointer :: fig => null()
    contains
        procedure :: run
        procedure :: save_png_sequence
        procedure :: set_save_frames
        procedure :: save
        procedure :: save_frame_sequence
        procedure :: set_figure
    end type animation_t

    public :: animation_t, FuncAnimation

contains

    function FuncAnimation(animate_func, frames, interval, fig) result(anim)
        procedure(animate_interface) :: animate_func
        integer, intent(in) :: frames
        integer, intent(in), optional :: interval
        type(figure_t), target, intent(in), optional :: fig
        type(animation_t) :: anim

        anim%animate_func => animate_func
        anim%frames = frames
        
        if (present(interval)) then
            anim%interval_ms = interval
        else
            anim%interval_ms = 50  ! Default 50ms between frames
        end if
        
        if (present(fig)) then
            anim%fig => fig
        end if
        
        anim%save_frames = .false.
    end function FuncAnimation

    subroutine run(self)
        class(animation_t), intent(inout) :: self
        integer :: i

        if (.not. associated(self%animate_func)) then
            print *, "Error: Animation callback function not associated"
            return
        end if

        print *, "Running animation with", self%frames, "frames..."

        do i = 1, self%frames
            call self%animate_func(i)
            
            ! Optional: add timing delay
            if (self%interval_ms > 0) then
                call sleep_ms(self%interval_ms)
            end if
        end do

        print *, "Animation completed."
    end subroutine run

    subroutine set_save_frames(self, pattern)
        class(animation_t), intent(inout) :: self
        character(len=*), intent(in) :: pattern
        
        self%save_frames = .true.
        self%frame_pattern = pattern
    end subroutine set_save_frames

    subroutine save_png_sequence(self, filename_pattern)
        class(animation_t), intent(inout) :: self
        character(len=*), intent(in) :: filename_pattern
        
        ! Enable frame saving and run animation
        call self%set_save_frames(filename_pattern)
        call self%run()
    end subroutine save_png_sequence

    subroutine sleep_ms(milliseconds)
        integer, intent(in) :: milliseconds
        ! Simple sleep implementation - platform dependent
        ! For now, just a placeholder
        ! In real implementation, would use system-specific sleep
        continue
    end subroutine sleep_ms

    subroutine save(self, filename, fps, status)
        class(animation_t), intent(inout) :: self
        character(len=*), intent(in) :: filename
        integer, intent(in), optional :: fps
        integer, intent(out), optional :: status
        
        character(len=:), allocatable :: extension
        integer :: actual_fps, stat
        
        extension = get_file_extension(filename)
        
        if (.not. is_video_format(extension)) then
            if (present(status)) status = -3
            print *, "Error: Unsupported file format. Use .mp4, .avi, or .mkv"
            return
        end if
        
        if (.not. check_ffmpeg_available()) then
            if (present(status)) status = -1
            print *, "Error: ffmpeg not found. Please install ffmpeg to save animations."
            return
        end if
        
        actual_fps = get_fps_or_default(fps)
        
        call save_animation_with_ffmpeg_pipe(self, filename, actual_fps, stat)
        
        if (present(status)) status = stat
        
    end subroutine save

    subroutine save_frame_sequence(self, pattern)
        class(animation_t), intent(inout) :: self
        character(len=*), intent(in) :: pattern
        character(len=256) :: filename
        integer :: i
        
        do i = 0, self%frames - 1
            write(filename, '(A,I0,A)') trim(pattern), i, ".png"
            call self%animate_func(i + 1)
            if (associated(self%fig)) then
                call self%fig%savefig(trim(filename))
            end if
        end do
        
    end subroutine save_frame_sequence

    subroutine set_figure(self, fig)
        class(animation_t), intent(inout) :: self
        type(figure_t), target, intent(in) :: fig
        
        self%fig => fig
    end subroutine set_figure

    function get_file_extension(filename) result(ext)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: ext
        integer :: dot_pos
        
        dot_pos = index(filename, ".", back=.true.)
        if (dot_pos > 0) then
            ext = filename(dot_pos+1:)
        else
            ext = ""
        end if
    end function get_file_extension

    function get_timestamp() result(ts)
        character(len=:), allocatable :: ts
        integer :: values(8)
        character(len=20) :: temp
        
        call date_and_time(values=values)
        write(temp, '(I0,I0,I0,I0,I0,I0)') &
            values(1), values(2), values(3), values(5), values(6), values(7)
        ts = trim(temp)
    end function get_timestamp

    function int_to_str(num) result(str)
        integer, intent(in) :: num
        character(len=:), allocatable :: str
        character(len=20) :: temp
        
        write(temp, '(I0)') num
        str = trim(temp)
    end function int_to_str

    function is_video_format(extension) result(is_video)
        character(len=*), intent(in) :: extension
        logical :: is_video
        
        is_video = (extension == "mp4" .or. &
                   extension == "avi" .or. &
                   extension == "mkv")
    end function is_video_format

    function check_ffmpeg_available() result(available)
        use fortplot_pipe, only: check_ffmpeg_available_pipe => check_ffmpeg_available
        logical :: available
        
        available = check_ffmpeg_available_pipe()
    end function check_ffmpeg_available

    function get_fps_or_default(fps) result(actual_fps)
        integer, intent(in), optional :: fps
        integer :: actual_fps
        
        if (present(fps)) then
            actual_fps = fps
        else
            actual_fps = 10
        end if
    end function get_fps_or_default

    subroutine save_animation_with_ffmpeg_pipe(anim, filename, fps, status)
        class(animation_t), intent(inout) :: anim
        character(len=*), intent(in) :: filename
        integer, intent(in) :: fps
        integer, intent(out) :: status
        
        integer :: frame_idx, stat
        integer(1), allocatable :: png_data(:)
        
        stat = open_ffmpeg_pipe(filename, fps)
        if (stat /= 0) then
            status = -4
            print *, "Error: Could not open pipe to ffmpeg"
            return
        end if
        
        do frame_idx = 1, anim%frames
            call generate_png_frame_data(anim, frame_idx, png_data, stat)
            if (stat /= 0) then
                status = -5
                print *, "Error: Failed to generate frame", frame_idx
                stat = close_ffmpeg_pipe()
                return
            end if
            
            stat = write_png_to_pipe(png_data)
            if (stat /= 0) then
                status = -6
                print *, "Error: Failed to write frame to pipe", frame_idx
                stat = close_ffmpeg_pipe()
                return
            end if
            
            if (allocated(png_data)) deallocate(png_data)
        end do
        
        stat = close_ffmpeg_pipe()
        status = 0
    end subroutine save_animation_with_ffmpeg_pipe

    subroutine generate_png_frame_data(anim, frame_idx, png_data, status)
        class(animation_t), intent(inout) :: anim
        integer, intent(in) :: frame_idx
        integer(1), allocatable, intent(out) :: png_data(:)
        integer, intent(out) :: status
        
        character(len=256) :: temp_filename
        integer :: unit_num, file_size, io_stat
        
        if (.not. associated(anim%fig)) then
            status = -1
            return
        end if
        
        call anim%animate_func(frame_idx)
        
        write(temp_filename, '(A,I0,A)') "/tmp/fortplot_frame_", get_current_timestamp(), ".png"
        
        call anim%fig%savefig(temp_filename)
        
        open(newunit=unit_num, file=temp_filename, action='read', &
             form='unformatted', access='stream', iostat=io_stat)
        
        if (io_stat /= 0) then
            status = -1
            return
        end if
        
        inquire(unit=unit_num, size=file_size)
        allocate(png_data(file_size))
        
        read(unit_num, iostat=io_stat) png_data
        close(unit_num)
        
        call execute_command_line("rm -f " // trim(temp_filename))
        
        if (io_stat /= 0) then
            status = -1
            if (allocated(png_data)) deallocate(png_data)
        else
            status = 0
        end if
    end subroutine generate_png_frame_data






    function get_current_timestamp() result(ts)
        integer :: ts
        integer :: values(8)
        
        call date_and_time(values=values)
        ts = values(5) * 10000 + values(6) * 100 + values(7)
    end function get_current_timestamp

end module fortplot_animation
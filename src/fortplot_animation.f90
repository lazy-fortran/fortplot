module fortplot_animation
    use iso_fortran_env, only: real64
    use fortplot_figure_core, only: figure_t
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
        character(len=:), allocatable :: temp_dir
        character(len=256) :: cmd
        integer :: actual_fps
        integer :: i, stat
        logical :: is_video_format
        
        ! Extract file extension
        extension = get_file_extension(filename)
        
        ! Check if video format
        is_video_format = (extension == "mp4" .or. &
                          extension == "avi" .or. &
                          extension == "mkv")
        
        ! Set default fps if not provided
        if (present(fps)) then
            actual_fps = fps
        else
            actual_fps = 10  ! Default 10 fps
        end if
        
        if (is_video_format) then
            ! Check if ffmpeg is available
            call execute_command_line("which ffmpeg > /dev/null 2>&1", exitstat=stat)
            if (stat /= 0) then
                if (present(status)) status = -1
                print *, "Error: ffmpeg not found. Please install ffmpeg to save animations."
                return
            end if
            
            ! Create temporary directory for frames
            temp_dir = "/tmp/fortplot_anim_" // get_timestamp()
            call execute_command_line("mkdir -p " // temp_dir, exitstat=stat)
            if (stat /= 0) then
                if (present(status)) status = -2
                print *, "Error: Could not create temporary directory"
                return
            end if
            
            ! Save frames as PNG sequence
            do i = 1, self%frames
                call self%animate_func(i)
                if (associated(self%fig)) then
                    write(cmd, '(A,A,I4.4,A)') trim(temp_dir), "/frame_", i, ".png"
                    call self%fig%savefig(trim(cmd))
                end if
            end do
            
            ! Use ffmpeg to create video
            write(cmd, '(A,A,A,A,A,A,A,A)') &
                "ffmpeg -r ", trim(adjustl(int_to_str(actual_fps))), &
                " -i ", trim(temp_dir), "/frame_%04d.png", &
                " -c:v libx264 -pix_fmt yuv420p ", &
                trim(filename), " > /dev/null 2>&1"
            
            call execute_command_line(trim(cmd), exitstat=stat)
            
            ! Clean up temporary files
            call execute_command_line("rm -rf " // temp_dir)
            
            if (present(status)) status = stat
            
        else
            ! Not a supported video format
            if (present(status)) status = -3
            print *, "Error: Unsupported file format. Use .mp4, .avi, or .mkv"
        end if
        
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

end module fortplot_animation
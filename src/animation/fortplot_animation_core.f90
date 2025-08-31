module fortplot_animation_core
    use iso_fortran_env, only: real64, wp => real64
    use iso_c_binding, only: c_char, c_int, c_null_char
    use fortplot_constants, only: MILLISECONDS_PER_SECOND
    use fortplot_figure_core, only: figure_t, plot_data_t
    ! savefig is part of figure_t, not rendering module
    use fortplot_pipe, only: open_ffmpeg_pipe, write_png_to_pipe, close_ffmpeg_pipe
    use fortplot_utils, only: initialize_backend, ensure_directory_exists
    use fortplot_logging, only: log_error, log_info, log_warning
    implicit none
    private

    ! Animation configuration constants (consolidated from fortplot_animation_constants)
    integer, parameter, public :: DEFAULT_FRAME_INTERVAL_MS = 50
    integer, parameter, public :: DEFAULT_ANIMATION_FPS = 10
    integer, parameter, public :: MIN_VALID_VIDEO_SIZE = 100
    integer, parameter, public :: MIN_EXPECTED_VIDEO_SIZE = 1000
    integer, parameter, public :: MAX_FILENAME_LENGTH = 255
    
    ! Enhanced recovery constants for exponential backoff
    integer, parameter, public :: MAX_RETRY_ATTEMPTS = 3
    integer, parameter, public :: BASE_RETRY_DELAY_MS = 100

    ! Animation callback interface
    abstract interface
        subroutine animate_interface(frame)
            integer, intent(in) :: frame
        end subroutine animate_interface
    end interface

    ! Animation type
    type, public :: animation_t
        procedure(animate_interface), pointer, nopass :: animate_func => null()
        integer :: frames = 0
        integer :: interval_ms = DEFAULT_FRAME_INTERVAL_MS
        logical :: save_frames = .false.
        character(len=:), allocatable :: frame_pattern
        class(figure_t), pointer :: fig => null()
    contains
        procedure :: run
        procedure :: save_png_sequence
        procedure :: set_save_frames
        procedure :: save_frame_sequence
        procedure :: set_figure
        procedure :: save
    end type animation_t

    ! Animation save interface to avoid circular dependency
    abstract interface
        subroutine save_animation_interface(anim, filename, fps, status)
            import :: animation_t
            class(animation_t), intent(inout) :: anim
            character(len=*), intent(in) :: filename
            integer, intent(in), optional :: fps
            integer, intent(out), optional :: status
        end subroutine save_animation_interface
    end interface

    ! External save implementation procedure pointer
    procedure(save_animation_interface), pointer :: save_animation_impl => null()

    public :: FuncAnimation
    public :: animate_interface
    public :: save_animation_interface
    public :: save_animation_impl

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
            anim%interval_ms = DEFAULT_FRAME_INTERVAL_MS
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
            call log_error_with_remediation("Animation callback function not associated", &
                                           "Ensure FuncAnimation() was called with a valid animation procedure")
            return
        end if

        call log_info("Running animation with frames...")

        do i = 1, self%frames
            call self%animate_func(i)
            
            ! Optional: add timing delay
            if (self%interval_ms > 0) then
                call sleep_ms(self%interval_ms)
            end if
        end do

        call log_info("Animation completed.")
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
        class(figure_t), target, intent(in) :: fig
        
        self%fig => fig
    end subroutine set_figure


    subroutine sleep_ms(milliseconds)
        integer, intent(in) :: milliseconds
        
        ! Platform independent timing delay using Fortran intrinsic
        ! This provides a busy wait that works across platforms
        call cpu_time_delay(real(milliseconds) / MILLISECONDS_PER_SECOND)
    end subroutine sleep_ms

    subroutine cpu_time_delay(seconds)
        real(wp), intent(in) :: seconds
        real(wp) :: start_time, current_time
        
        call cpu_time(start_time)
        do
            call cpu_time(current_time)
            if (current_time - start_time >= seconds) exit
        end do
    end subroutine cpu_time_delay

    subroutine save(self, filename, fps, status)
        !! Save animation to video file - delegates to full pipeline implementation
        class(animation_t), intent(inout) :: self
        character(len=*), intent(in) :: filename
        integer, intent(in), optional :: fps
        integer, intent(out), optional :: status
        
        ! Attempt to register implementation if not done
        call try_register_save_implementation()
        
        if (.not. associated(save_animation_impl)) then
            if (present(status)) status = -1
            call log_error_with_remediation("Animation save implementation not initialized", &
                                           "Import fortplot_animation module to register the save implementation")
            return
        end if
        
        ! Call the facade save_animation wrapper to avoid circular dependency
        call save_animation_impl(self, filename, fps, status)
    end subroutine save

    subroutine try_register_save_implementation()
        !! Attempt to register save implementation (will be set by facade module)
        !! This is a no-op if the facade module hasn't been imported
        continue
    end subroutine try_register_save_implementation

    subroutine log_error_with_remediation(error_msg, remediation_msg)
        character(len=*), intent(in) :: error_msg, remediation_msg
        
        call log_error("ERROR: " // error_msg)
        call log_info("REMEDIATION: " // remediation_msg)
    end subroutine log_error_with_remediation

end module fortplot_animation_core
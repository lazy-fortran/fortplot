! Facade module for animation functionality - maintains backward compatibility
module fortplot_animation
    use fortplot_animation_core
    use fortplot_animation_validation
    use fortplot_animation_rendering
    use fortplot_animation_pipeline
    implicit none

    ! Re-export everything needed for backward compatibility
    public :: animation_t
    public :: FuncAnimation
    public :: animate_interface
    public :: save_animation

    ! Register the save implementation on module initialization
    logical, save :: impl_registered = .false.

contains

    ! Wrapper to maintain backward compatibility for save method
    subroutine save_animation(anim, filename, fps, status)
        type(animation_t), intent(inout) :: anim
        character(len=*), intent(in) :: filename
        integer, intent(in), optional :: fps
        integer, intent(out), optional :: status
        
        call register_save_implementation()
        call save_animation_full(anim, filename, fps, status)
    end subroutine save_animation

    ! Type-bound procedure for animation save method implementation
    subroutine animation_save_impl(anim, filename, fps, status)
        class(animation_t), intent(inout) :: anim
        character(len=*), intent(in) :: filename
        integer, intent(in), optional :: fps
        integer, intent(out), optional :: status
        
        call save_animation_full(anim, filename, fps, status)
    end subroutine animation_save_impl

    ! Register the save implementation pointer
    subroutine register_save_implementation()
        if (.not. impl_registered) then
            save_animation_impl => animation_save_impl
            impl_registered = .true.
        end if
    end subroutine register_save_implementation

end module fortplot_animation
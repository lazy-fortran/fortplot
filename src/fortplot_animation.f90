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

contains

    ! Wrapper to maintain backward compatibility for save method
    subroutine save_animation(anim, filename, fps, status)
        type(animation_t), intent(inout) :: anim
        character(len=*), intent(in) :: filename
        integer, intent(in), optional :: fps
        integer, intent(out), optional :: status
        
        call save_animation_full(anim, filename, fps, status)
    end subroutine save_animation

end module fortplot_animation
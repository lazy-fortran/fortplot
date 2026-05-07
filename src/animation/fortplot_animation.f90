! Facade module for animation functionality - maintains backward compatibility
module fortplot_animation
    use fortplot_animation_core, only: animation_t, animate_interface, &
        save_animation_impl, FuncAnimation_core => FuncAnimation, &
        DEFAULT_FRAME_INTERVAL_MS
    use fortplot_figure_core, only: figure_t
    use fortplot_animation_pipeline, only: save_animation_full
    implicit none
    private

    public :: animation_t
    public :: FuncAnimation
    public :: animate_interface
    public :: save_animation

    logical, save :: impl_registered = .false.

contains

    function FuncAnimation(animate_func, frames, interval, fig) result(anim)
        !! Facade constructor: builds the animation_t and registers the save
        !! implementation so anim%save(...) works directly (per README).
        procedure(animate_interface) :: animate_func
        integer, intent(in) :: frames
        integer, intent(in), optional :: interval
        type(figure_t), target, intent(in), optional :: fig
        type(animation_t) :: anim

        if (present(fig)) then
            anim = FuncAnimation_core(animate_func, frames, interval, fig)
        else
            anim = FuncAnimation_core(animate_func, frames, interval)
        end if
        call register_save_implementation()
    end function FuncAnimation

    subroutine save_animation(anim, filename, fps, status)
        type(animation_t), intent(inout) :: anim
        character(len=*), intent(in) :: filename
        integer, intent(in), optional :: fps
        integer, intent(out), optional :: status

        call register_save_implementation()
        call save_animation_full(anim, filename, fps, status)
    end subroutine save_animation

    subroutine animation_save_impl(anim, filename, fps, status)
        class(animation_t), intent(inout) :: anim
        character(len=*), intent(in) :: filename
        integer, intent(in), optional :: fps
        integer, intent(out), optional :: status

        call save_animation_full(anim, filename, fps, status)
    end subroutine animation_save_impl

    subroutine register_save_implementation()
        if (.not. impl_registered) then
            save_animation_impl => animation_save_impl
            impl_registered = .true.
        end if
    end subroutine register_save_implementation

end module fortplot_animation
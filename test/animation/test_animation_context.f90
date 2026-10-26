module animation_context_cases
    use fortplot_animation, only: animation_callback_t
    implicit none
    type, extends(animation_callback_t) :: frame_counter_t
        integer :: offset = 0, seen(3) = 0
    contains
        procedure :: update => update_counter
    end type
contains
    subroutine update_counter(self, frame)
        class(frame_counter_t), intent(inout) :: self
        integer, intent(in) :: frame
        if (frame < 1 .or. frame > 3) error stop 'frame range'
        self%seen(frame) = self%offset + frame
    end subroutine
    subroutine legacy_frame(frame)
        integer, intent(in) :: frame
        if (frame < 1 .or. frame > 3) error stop 'legacy frame range'
    end subroutine
end module
program test_animation_context
    use animation_context_cases
    use fortplot_animation, only: animation_t, FuncAnimation
    implicit none
    type(frame_counter_t), target :: first, second
    type(animation_t) :: a, b, old
    second%offset = 10
    a = FuncAnimation(first, frames=3, interval=0)
    b = FuncAnimation(second, frames=3, interval=0)
    old = FuncAnimation(legacy_frame, frames=3, interval=0)
    call a%run()
    if (any(first%seen /= [1, 2, 3])) error stop 'first context'
    if (any(second%seen /= 0)) error stop 'independent context'
    call b%run()
    if (any(second%seen /= [11, 12, 13])) error stop 'second context'
    call old%run()
    if (.not. a%has_callback() .or. .not. old%has_callback()) &
        error stop 'callback registration'
    print *, 'animation contexts and legacy callback passed'
end program

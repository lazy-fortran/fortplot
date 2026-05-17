program test_public_api_animation_reexport
    !! FuncAnimation and animation_t must be available through use fortplot.
    use fortplot, only: animation_t, figure_t, FuncAnimation
    implicit none

    type(figure_t), target :: fig
    type(animation_t) :: anim

    call fig%initialize()
    anim = FuncAnimation(update_frame, frames=4, interval=25, fig=fig)

    if (anim%frames /= 4) then
        print *, 'FAIL: FuncAnimation did not preserve frame count'
        stop 1
    end if
    if (anim%interval_ms /= 25) then
        print *, 'FAIL: FuncAnimation did not preserve interval'
        stop 1
    end if
    if (.not. associated(anim%fig)) then
        print *, 'FAIL: FuncAnimation did not preserve figure association'
        stop 1
    end if

    print *, 'Public API animation re-export test passed'

contains

    subroutine update_frame(frame)
        integer, intent(in) :: frame

        if (frame < 0) stop 1
    end subroutine update_frame

end program test_public_api_animation_reexport

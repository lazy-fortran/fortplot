program test_animation
    use fortplot_animation
    implicit none
    
    type(animation_t) :: anim
    integer :: test_frames = 0
    
    ! Test animation creation and basic functionality
    call test_animation_creation()
    call test_animation_execution()
    
    print *, "PASS: Animation tests completed"
    
contains

    subroutine test_animation_creation()
        ! Test creating animation object
        anim = FuncAnimation(test_callback, frames=5, interval=10)
        
        if (anim%frames /= 5) then
            error stop "Animation frames not set correctly"
        end if
        
        if (anim%interval_ms /= 10) then
            error stop "Animation interval not set correctly"
        end if
        
        if (.not. associated(anim%animate_func)) then
            error stop "Animation callback not associated"
        end if
        
        print *, "PASS: Animation creation"
    end subroutine

    subroutine test_animation_execution()
        ! Test running animation
        test_frames = 0
        call anim%run()
        
        if (test_frames /= 5) then
            print *, "Expected 5 frames, got", test_frames
            error stop "Animation did not execute correct number of frames"
        end if
        
        print *, "PASS: Animation execution"
    end subroutine

    subroutine test_callback(frame)
        integer, intent(in) :: frame
        
        test_frames = test_frames + 1
        
        ! Verify frame numbers are sequential
        if (frame /= test_frames) then
            print *, "Expected frame", test_frames, "got", frame
            error stop "Frame numbers not sequential"
        end if
    end subroutine test_callback

end program test_animation
program test_animation_save_failure_status
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure_t, animation_t, FuncAnimation
    use fortplot_animation, only: save_animation
    use fortplot_system_runtime, only: create_directory_runtime, delete_file_runtime
    implicit none
    character(*), parameter :: directory = 'build/test/output/'
    character(*), parameter :: missing = directory//'animation_missing_figure.mp4'
    character(*), parameter :: fallback = directory//'animation_failed_video.mp4'
    character(*), parameter :: png = directory//'animation_failed_video_frame_0.png'
    type(figure_t), target :: fig
    type(animation_t) :: anim
    integer :: status, calls
    logical :: ok, exists

    call create_directory_runtime(directory, ok)
    if (.not. ok) error stop 'test directory creation failed'
    call delete_file_runtime(missing, ok)
    call delete_file_runtime(fallback, ok)
    call delete_file_runtime(png, ok)
    calls = 0
    anim = FuncAnimation(update, frames=1, interval=0)
    call save_animation(anim, missing, status=status)
    inquire (file=missing, exist=exists)
    if (status == 0) error stop 'missing figure reported successful movie save'
    if (exists) error stop 'missing figure created a requested movie'

    call fig%initialize(320, 240)
    call fig%add_plot([0.0_wp, 1.0_wp], [0.0_wp, 1.0_wp])
    anim = FuncAnimation(update, frames=1, interval=0, fig=fig)
    ! Zero fps forces the encoder route to fail before invoking an external
    ! process. With no encoder installed, the same PNG fallback runs directly.
    call save_animation(anim, fallback, fps=0, status=status)
    inquire (file=fallback, exist=exists)
    if (status == 0) error stop 'PNG fallback reported successful requested movie'
    if (exists) error stop 'failed encoder unexpectedly created a movie'
    inquire (file=png, exist=exists)
    if (.not. exists) error stop 'PNG fallback artifact missing'
    if (calls /= 1) error stop 'invalid movie preparation invoked callback'
    call delete_file_runtime(png, ok)
    print '(a)', 'Animation missing-figure and PNG-fallback failure status pass'
contains
    subroutine update(frame)
        integer, intent(in) :: frame
        if (frame /= 1) error stop 'unexpected frame'
        calls = calls + 1
    end subroutine
end program test_animation_save_failure_status

program test_lazy_session_probe
    !! A savefig to a file must not probe for a graphical session.
    !! Fortran does not promise that .and. short-circuits, so the PDF and SVG
    !! backends nest the filename test around has_graphical_session(); that
    !! probe shells out to uname and would otherwise run on every save.
    !! A fake uname placed first in PATH makes any probe observable.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use, intrinsic :: iso_c_binding, only: c_char, c_int
    use fortplot
    use fortplot_system_runtime, only: create_directory_runtime
    use fortplot_system_viewer, only: has_graphical_session
    implicit none

    character(len=*), parameter :: probe_dir = 'build/test/output/lazy_session_probe'
    character(len=*), parameter :: fake_uname = probe_dir//'/uname'
    character(len=*), parameter :: marker = probe_dir//'/probed'

    type :: env_keep_t
        character(kind=c_char), allocatable :: value(:)
    end type env_keep_t

    interface
        function c_putenv(name_value) bind(C, name="putenv") result(rc)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name_value(*)
            integer(c_int) :: rc
        end function c_putenv
    end interface

    character(len=4096) :: path_env, os_env
    character(len=64) :: pdf_name, svg_name
    real(wp) :: xd(2), yd(2)
    logical :: ok
    integer :: unit, ios

    call create_directory_runtime(probe_dir, ok)
    if (.not. ok) then
        print *, 'SKIP: cannot create ', probe_dir
        stop 0
    end if

    call write_fake_uname()
    call execute_command_line('/bin/chmod +x "'//fake_uname//'"', exitstat=ios)
    if (ios /= 0) then
        print *, 'SKIP: cannot make the uname probe observable'
        stop 0
    end if

    call get_environment_variable('PATH', path_env)
    call get_environment_variable('OS', os_env)
    ! Empty OS keeps get_os_type on the uname branch, not the Windows one.
    call set_env('PATH', trim(probe_dir)//':'//trim(path_env))
    call set_env('OS', '')

    pdf_name = 'build/test/output/lazy_session_probe/probe.pdf'
    svg_name = 'build/test/output/lazy_session_probe/probe.svg'
    xd = [0.0_wp, 1.0_wp]
    yd = [0.0_wp, 1.0_wp]

    call figure(figsize=[4.0_wp, 3.0_wp])
    call plot(xd, yd)
    call savefig(pdf_name)
    call savefig(svg_name)

    if (probe_ran()) then
        print *, 'FAIL: file savefig ran the graphical session probe'
        call finish(trim(path_env), trim(os_env))
        stop 1
    end if
    print *, 'PASS: PDF and SVG savefig did not probe for a session'

    ! Positive control: the probe is observable when it is asked for.
    ok = has_graphical_session()
    if (.not. probe_ran()) then
        print *, 'FAIL: probe is not observable, test setup is broken'
        call finish(trim(path_env), trim(os_env))
        stop 1
    end if

    call finish(trim(path_env), trim(os_env))
    print *, 'All lazy session probe tests PASSED'

contains

    subroutine write_fake_uname()
        !! Report a normal Linux kernel and leave a marker when invoked.
        open (newunit=unit, file=fake_uname, action='write', form='formatted', &
            iostat=ios)
        if (ios /= 0) then
            print *, 'FAIL: cannot write ', fake_uname
            stop 1
        end if
        write (unit, '(A)') '#!/bin/sh'
        write (unit, '(A)') 'touch "'//marker//'"'
        write (unit, '(A)') 'echo Linux'
        close (unit)
    end subroutine write_fake_uname

    logical function probe_ran()
        inquire (file=marker, exist=probe_ran)
    end function probe_ran

    subroutine set_env(name, value)
        !! Export NAME=VALUE; putenv keeps the pointer, so each buffer is kept
        !! alive in a saved slot until this test ends.
        character(len=*), intent(in) :: name, value
        type(env_keep_t), save :: keeps(8)
        integer, save :: slot = 0
        character(len=:), allocatable :: raw
        integer :: i
        integer(c_int) :: rc

        raw = trim(name)//'='//trim(value)
        slot = slot + 1
        if (slot > size(keeps)) then
            print *, 'FAIL: no environment slot left for ', name
            stop 1
        end if
        if (allocated(keeps(slot)%value)) deallocate(keeps(slot)%value)
        allocate(keeps(slot)%value(len(raw) + 1))
        do i = 1, len(raw)
            keeps(slot)%value(i) = transfer(raw(i:i), keeps(slot)%value(i))
        end do
        keeps(slot)%value(len(raw) + 1) = achar(0, kind=c_char)
        rc = c_putenv(keeps(slot)%value)
        if (rc /= 0) then
            print *, 'FAIL: putenv failed for ', name
            stop 1
        end if
    end subroutine set_env

    subroutine finish(path, os)
        !! Restore the environment and drop the probe artifacts.
        character(len=*), intent(in) :: path, os

        call set_env('PATH', path)
        call set_env('OS', os)
        call execute_command_line('rm -f "'//marker//'" "'//fake_uname//'"')
    end subroutine finish

end program test_lazy_session_probe

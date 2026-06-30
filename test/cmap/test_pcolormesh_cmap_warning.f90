program test_pcolormesh_cmap_warning
    use iso_fortran_env, only: wp => real64
    use fortplot
    implicit none

    character(len=1024) :: arg0, arg1
    character(len=4096) :: command, output_log
    character(len=512) :: line
    integer :: exitstat, cmdstat, unit, ios
    logical :: found_warning
    real(wp), dimension(11) :: x, y
    real(wp), dimension(10, 10) :: z
    integer :: i, j

    call get_command_argument(1, arg1)
    if (trim(arg1) == '--probe') then
        do i = 1, 11
            x(i) = real(i - 1, wp)
            y(i) = real(i - 1, wp)
        end do

        do i = 1, 10
            do j = 1, 10
                z(i, j) = real(i + j, wp)
            end do
        end do

        call figure()
        call pcolormesh(x, y, z, cmap='plasma')
        stop 0
    end if

    call get_command_argument(0, arg0)
    output_log = 'build/test/output/test_pcolormesh_cmap_warning.log'
    command = 'FORTPLOT_FORCE_WARNINGS=1 "' // trim(arg0) // &
              '" --probe > "' // trim(output_log) // '" 2>&1'

    call execute_command_line(command, wait=.true., exitstat=exitstat, &
                              cmdstat=cmdstat)
    if (cmdstat /= 0 .or. exitstat /= 0) then
        print *, 'ERROR: probe execution failed with cmdstat=', cmdstat, &
                 ' exitstat=', exitstat
        stop 1
    end if

    inquire(file=trim(output_log), exist=found_warning)
    if (.not. found_warning) then
        print *, 'ERROR: missing probe output log'
        stop 2
    end if

    open(newunit=unit, file=trim(output_log), status='old', action='read', &
         iostat=ios)
    if (ios /= 0) then
        print *, 'ERROR: unable to open probe output log'
        stop 3
    end if

    found_warning = .false.
    do
        read(unit, '(A)', iostat=ios) line
        if (ios /= 0) exit
        if (index(line, "register_pcolormesh_plot_data: 'colormap' is deprecated; use 'cmap'") > 0) then
            found_warning = .true.
            exit
        end if
    end do

    close(unit)

    if (found_warning) then
        print *, 'ERROR: unexpected pcolormesh colormap deprecation warning'
        stop 4
    end if

    print *, 'PASS: pcolormesh(cmap=...) emits no colormap deprecation warning'
end program test_pcolormesh_cmap_warning

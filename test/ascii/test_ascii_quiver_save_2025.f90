program test_ascii_quiver_save_2025
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure, quiver, savefig
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none

    real(wp) :: x(3), y(3), u(3), v(3)
    integer :: i, unit, ios, current_line, top_frame, bottom_frame
    integer :: arrow_line, arrow_col
    logical :: dir_ok, file_exists
    character(len=*), parameter :: outfile = &
        'build/test/output/ascii_quiver_save_2025.txt'
    character(len=512) :: line

    call create_directory_runtime('build/test/output', dir_ok)

    do i = 1, 3
        x(i) = 0.2_wp*real(i, wp)
        y(i) = 0.5_wp
        u(i) = 1.0_wp
        v(i) = 0.0_wp
    end do

    call figure(figsize=[8.0_wp, 6.0_wp])
    call quiver(x, y, u, v)
    call savefig(outfile)

    inquire(file=outfile, exist=file_exists)
    if (.not. file_exists) then
        print *, 'FAIL: ASCII quiver file missing'
        stop 1
    end if

    current_line = 0
    top_frame = 0
    bottom_frame = 0
    arrow_line = 0
    arrow_col = 0

    open(newunit=unit, file=outfile, status='old', action='read', iostat=ios)
    if (ios /= 0) then
        print *, 'FAIL: cannot open ASCII quiver output'
        stop 1
    end if
    do
        read(unit, '(A)', iostat=ios) line
        if (ios /= 0) exit
        current_line = current_line + 1
        if (is_frame_line(line)) then
            if (top_frame == 0) then
                top_frame = current_line
            else
                bottom_frame = current_line
            end if
        end if
        if (arrow_line == 0 .and. index(line, '>') > 0) then
            arrow_line = current_line
            arrow_col = index(line, '>')
        end if
    end do
    close(unit)

    if (arrow_line == 0) then
        print *, 'FAIL: ASCII quiver output missing the expected > glyph'
        stop 1
    end if
    if (top_frame == 0 .or. bottom_frame == 0) then
        print *, 'FAIL: could not locate the ASCII frame'
        stop 1
    end if
    if (arrow_line <= top_frame .or. arrow_line >= bottom_frame) then
        print *, 'FAIL: ASCII quiver glyph is not inside the frame'
        stop 1
    end if
    if (arrow_col <= 1) then
        print *, 'FAIL: ASCII quiver glyph landed on the border'
        stop 1
    end if

    print *, 'PASS: ASCII quiver output keeps the expected arrow glyph visible'

contains

    pure logical function is_frame_line(line) result(found)
        character(len=*), intent(in) :: line
        character(len=:), allocatable :: t

        t = trim(adjustl(line))
        found = .false.
        if (len(t) < 3) return
        found = t(1:1) == '+' .and. index(t, '---') > 0
    end function is_frame_line

end program test_ascii_quiver_save_2025

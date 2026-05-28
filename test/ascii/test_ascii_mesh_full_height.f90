program test_ascii_mesh_full_height
    !! Regression test for issue #1967: ASCII pcolormesh must fill the full
    !! plot height. The mesh quad fill previously mapped vertices through the
    !! char-cell-aspect-scaled y range instead of the unscaled range used by
    !! the axis frame and ticks, compressing the fill into the lower ~half of
    !! the canvas. Assert that both the topmost and bottommost interior rows of
    !! the plot area carry mesh fill characters for a full-range mesh.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure, pcolormesh, savefig
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none

    integer, parameter :: nx = 50, ny = 50
    real(wp) :: x_coords(nx + 1), y_coords(ny + 1)
    real(wp) :: c_data(ny, nx)
    integer :: i, j
    character(len=*), parameter :: outfile = &
        'build/test/output/ascii_pcolormesh_full_height.txt'
    logical :: dir_ok

    character(len=512), allocatable :: lines(:)
    integer :: nlines, top_row, bottom_row

    call create_directory_runtime('build/test/output', dir_ok)

    ! Mesh spanning x in [0, 2.0], y in [0, 1.2] (matches pcolormesh_basic),
    ! with a smooth gradient so every quad gets a visible fill character.
    do i = 1, nx + 1
        x_coords(i) = real(i - 1, wp) / real(nx, wp) * 2.0_wp
    end do
    do j = 1, ny + 1
        y_coords(j) = real(j - 1, wp) / real(ny, wp) * 1.2_wp
    end do
    do i = 1, nx
        do j = 1, ny
            c_data(j, i) = real(i + j, wp)
        end do
    end do

    call figure(figsize=[6.4_wp, 4.8_wp])
    call pcolormesh(x_coords, y_coords, c_data, colormap='viridis')
    call savefig(outfile)

    call read_lines(outfile, lines, nlines)
    call find_plot_area_rows(lines, nlines, top_row, bottom_row)

    if (top_row <= 0 .or. bottom_row <= 0) then
        print *, 'FAIL: could not locate plot area frame in ASCII output'
        stop 1
    end if

    ! The very first/last interior rows can hold only a tick label, so accept
    ! fill anywhere within the top three and bottom three interior rows. Before
    ! the fix the entire top half is blank, so this still fails pre-fix.
    if (.not. any_row_has_fill(lines, top_row, min(top_row + 2, bottom_row))) then
        print *, 'FAIL: top of plot area has no mesh fill (issue #1967 regression)'
        do i = top_row, min(top_row + 2, bottom_row)
            print *, 'top row: ', trim(lines(i))
        end do
        stop 1
    end if
    print *, 'PASS: top of plot area contains mesh fill'

    if (.not. any_row_has_fill(lines, max(bottom_row - 2, top_row), bottom_row)) then
        print *, 'FAIL: bottom of plot area has no mesh fill'
        do i = max(bottom_row - 2, top_row), bottom_row
            print *, 'bottom row: ', trim(lines(i))
        end do
        stop 1
    end if
    print *, 'PASS: bottom of plot area contains mesh fill'

    print *, 'ascii pcolormesh fills full plot height'

contains

    subroutine read_lines(path, out_lines, count)
        character(len=*), intent(in) :: path
        character(len=512), allocatable, intent(out) :: out_lines(:)
        integer, intent(out) :: count
        integer :: unit, ios
        character(len=512) :: buf

        count = 0
        allocate (out_lines(0))
        open (newunit=unit, file=path, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, 'FAIL: cannot read ', path
            stop 1
        end if
        do
            read (unit, '(A)', iostat=ios) buf
            if (ios /= 0) exit
            out_lines = [out_lines, buf]
            count = count + 1
        end do
        close (unit)
    end subroutine read_lines

    subroutine find_plot_area_rows(in_lines, count, top_row, bottom_row)
        !! The plot area is bounded by '+---...---+' frame lines. The first
        !! interior row sits just below the top frame; the last just above the
        !! bottom frame.
        character(len=*), intent(in) :: in_lines(:)
        integer, intent(in) :: count
        integer, intent(out) :: top_row, bottom_row
        integer :: i, top_frame, bottom_frame

        top_frame = 0
        bottom_frame = 0
        do i = 1, count
            if (is_frame_line(in_lines(i))) then
                if (top_frame == 0) then
                    top_frame = i
                else
                    bottom_frame = i
                end if
            end if
        end do

        if (top_frame == 0 .or. bottom_frame == 0 .or. &
            bottom_frame <= top_frame + 1) then
            top_row = 0
            bottom_row = 0
            return
        end if
        top_row = top_frame + 1
        bottom_row = bottom_frame - 1
    end subroutine find_plot_area_rows

    pure logical function is_frame_line(line)
        character(len=*), intent(in) :: line
        character(len=:), allocatable :: t
        t = trim(adjustl(line))
        is_frame_line = .false.
        if (len(t) < 3) return
        is_frame_line = t(1:1) == '+' .and. index(t, '---') > 0
    end function is_frame_line

    logical function any_row_has_fill(in_lines, first, last) result(found)
        character(len=*), intent(in) :: in_lines(:)
        integer, intent(in) :: first, last
        integer :: i
        found = .false.
        do i = first, last
            if (row_has_fill(in_lines(i))) then
                found = .true.
                return
            end if
        end do
    end function any_row_has_fill

    pure logical function row_has_fill(line)
        !! A filled mesh row contains gradient fill glyphs in the interior. The
        !! border '|' and tick digits do not count; require a run of fill
        !! characters from the ASCII density ramp.
        character(len=*), intent(in) :: line
        character(len=*), parameter :: fill_chars = '-=+*#%@.:'
        integer :: i, run
        run = 0
        row_has_fill = .false.
        do i = 1, len_trim(line)
            if (index(fill_chars, line(i:i)) > 0) then
                run = run + 1
                if (run >= 5) then
                    row_has_fill = .true.
                    return
                end if
            else
                run = 0
            end if
        end do
    end function row_has_fill

end program test_ascii_mesh_full_height

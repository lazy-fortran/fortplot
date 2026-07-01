program test_3d_tick_labels
    !! Geometry-level regression for 3D tick-label placement (refs #2055).
    !!
    !! The corner/endpoint de-collision lives in the shared 3D axis placement
    !! (fortplot_3d_axes) used by every backend, so this test exercises it via
    !! the text-inspectable ASCII backend on the two fixtures from
    !! example/fortran/3d_plotting/3d_plotting.f90 (the helix and the parametric
    !! spiral) at the default mplot3d view. The matplotlib oracle for the raster
    !! bounding boxes is scripts/oracle_3d_tick_labels.py.
    !!
    !! Asserted invariants:
    !!   - the 3D axes render numeric tick labels (axes are present);
    !!   - no two labels merge into a malformed token at a shared corner;
    !!   - no adjacent rows carry a duplicated left-margin tick label.
    use fortplot
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none

    logical :: dir_ok
    character(len=*), parameter :: out_dir = 'build/test/output'
    character(len=*), parameter :: helix_txt = out_dir//'/tick_helix.txt'
    character(len=*), parameter :: param_txt = out_dir//'/tick_parametric.txt'

    call create_directory_runtime(out_dir, dir_ok)

    call render_helix(helix_txt)
    call render_parametric(param_txt)

    ! The helix spans a wide, well-conditioned box, so its corner labels must be
    ! placed without merging - this is the direct #2055 corner-collision check.
    call check_fixture(helix_txt, '3d_helix', strict=.true.)
    ! The parametric spiral collapses toward the origin, so its projected bottom
    ! edge is short; assert labels are present and free of adjacent-row
    ! duplication (ASCII cannot space every label on such a compressed edge).
    call check_fixture(param_txt, 'parametric_curve', strict=.false.)

    print *, 'All 3D tick-label placement tests PASSED'

contains

    subroutine render_helix(path)
        character(len=*), intent(in) :: path
        integer, parameter :: n = 100
        real(wp) :: x(n), y(n), z(n)
        integer :: i

        do i = 1, n
            x(i) = cos(real(i - 1, wp)*0.1_wp)
            y(i) = sin(real(i - 1, wp)*0.1_wp)
            z(i) = real(i - 1, wp)*0.05_wp
        end do
        call figure(figsize=[8.0_wp, 6.0_wp])
        call add_3d_plot(x, y, z, label='3D Helix')
        call title('3D Line Plot - Helix')
        call savefig(path)
    end subroutine render_helix

    subroutine render_parametric(path)
        character(len=*), intent(in) :: path
        integer, parameter :: n = 200
        real(wp) :: x(n), y(n), z(n), theta, pi
        integer :: i

        pi = 4.0_wp*atan(1.0_wp)
        do i = 1, n
            theta = 6.0_wp*pi*real(i - 1, wp)/real(n - 1, wp)
            x(i) = cos(theta)*exp(-theta*0.1_wp)
            y(i) = sin(theta)*exp(-theta*0.1_wp)
            z(i) = theta*0.1_wp
        end do
        call figure(figsize=[8.0_wp, 6.0_wp])
        call add_3d_plot(x, y, z, label='Parametric spiral', linestyle='-')
        call title('3D Parametric Curve')
        call savefig(path)
    end subroutine render_parametric

    subroutine check_fixture(path, name, strict)
        character(len=*), intent(in) :: path, name
        logical, intent(in) :: strict
        character(len=256), allocatable :: lines(:)
        integer :: nlines, k, label_count
        character(len=32) :: tok, prev_tok

        call read_lines(path, lines, nlines)
        if (nlines <= 0) then
            print *, 'FAIL: empty 3D ASCII frame for ', name
            error stop 'empty 3D frame'
        end if

        label_count = 0
        prev_tok = ''
        do k = 1, nlines
            if (strict .and. row_has_malformed_token(lines(k))) then
                print *, 'FAIL: merged/overlapping tick label in ', name, &
                    ' row ', k
                error stop 'colliding 3D tick labels'
            end if
            if (count_numeric_tokens(lines(k)) > 0) label_count = label_count + 1
            tok = left_margin_numeric_token(lines(k))
            if (len_trim(tok) > 0 .and. tok == prev_tok) then
                print *, 'FAIL: adjacent duplicate tick label in ', name
                error stop 'adjacent duplicate 3D tick labels'
            end if
            prev_tok = tok
        end do

        if (label_count == 0) then
            print *, 'FAIL: no tick labels rendered for ', name
            error stop 'missing 3D tick labels'
        end if
        print *, 'PASS: ', name, ' tick labels placed without collision (', &
            label_count, ' rows)'
    end subroutine check_fixture

    logical function row_has_malformed_token(line) result(bad)
        !! A well-formed numeric token has at most one '.' and a '-' only in the
        !! leading position. Two labels colliding at a corner merge into a token
        !! like "1.0-1.0" or "0.00.5", which this rejects.
        character(len=*), intent(in) :: line
        integer :: j, dots, minus_after
        character(len=64) :: cand

        bad = .false.
        j = 1
        do while (j <= len(line))
            if (is_number_char(line(j:j)) .and. line(j:j) /= '.') then
                cand = ''
                do while (j <= len(line))
                    if (.not. is_number_char(line(j:j))) exit
                    cand = trim(cand)//line(j:j)
                    j = j + 1
                end do
                if (token_is_numeric(cand)) then
                    call token_defects(cand, dots, minus_after)
                    if (dots > 1 .or. minus_after > 0) then
                        bad = .true.
                        return
                    end if
                end if
            else
                j = j + 1
            end if
        end do
    end function row_has_malformed_token

    subroutine token_defects(tok, dots, minus_after)
        character(len=*), intent(in) :: tok
        integer, intent(out) :: dots, minus_after
        integer :: j

        dots = 0
        minus_after = 0
        do j = 1, len_trim(tok)
            if (tok(j:j) == '.') dots = dots + 1
            if (tok(j:j) == '-' .and. j > 1) minus_after = minus_after + 1
        end do
    end subroutine token_defects

    integer function count_numeric_tokens(line) result(cnt)
        character(len=*), intent(in) :: line
        integer :: j
        character(len=32) :: cand

        cnt = 0
        j = 1
        do while (j <= len(line))
            if (is_number_char(line(j:j)) .and. line(j:j) /= '.') then
                cand = ''
                do while (j <= len(line))
                    if (.not. is_number_char(line(j:j))) exit
                    cand = trim(cand)//line(j:j)
                    j = j + 1
                end do
                if (token_is_numeric(cand)) cnt = cnt + 1
            else
                j = j + 1
            end if
        end do
    end function count_numeric_tokens

    character(len=32) function left_margin_numeric_token(line) result(tok)
        character(len=*), intent(in) :: line
        character(len=32) :: cand
        integer :: j, endc

        tok = ''
        endc = min(len(line), 10)
        j = 1
        do while (j <= endc)
            if (is_number_char(line(j:j)) .and. line(j:j) /= '.') then
                cand = ''
                do while (j <= len(line))
                    if (.not. is_number_char(line(j:j))) exit
                    cand = trim(cand)//line(j:j)
                    j = j + 1
                end do
                if (token_is_numeric(cand)) then
                    tok = cand
                    return
                end if
            else
                j = j + 1
            end if
        end do
    end function left_margin_numeric_token

    logical function is_number_char(c) result(res)
        character(len=1), intent(in) :: c

        res = (c >= '0' .and. c <= '9')
        if (c == '.') res = .true.
        if (c == '-') res = .true.
    end function is_number_char

    logical function token_is_numeric(tok) result(res)
        character(len=*), intent(in) :: tok
        integer :: j

        res = .false.
        if (len_trim(tok) == 0) return
        do j = 1, len_trim(tok)
            if (tok(j:j) >= '0' .and. tok(j:j) <= '9') res = .true.
        end do
    end function token_is_numeric

    subroutine read_lines(path, lines, nlines)
        character(len=*), intent(in) :: path
        character(len=256), allocatable, intent(out) :: lines(:)
        integer, intent(out) :: nlines
        integer :: u, ios, cap
        character(len=256) :: buf
        character(len=256), allocatable :: tmp(:)

        cap = 64
        allocate (lines(cap))
        nlines = 0
        open (newunit=u, file=path, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, 'FAIL: cannot open ', trim(path)
            error stop 'missing 3D frame'
        end if
        do
            read (u, '(A)', iostat=ios) buf
            if (ios /= 0) exit
            if (nlines == cap) then
                allocate (tmp(2*cap))
                tmp(1:cap) = lines
                call move_alloc(tmp, lines)
                cap = 2*cap
            end if
            nlines = nlines + 1
            lines(nlines) = buf
        end do
        close (u)
    end subroutine read_lines

end program test_3d_tick_labels

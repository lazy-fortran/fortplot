program test_streamplot_arrows
    !! Verify streamplot arrow rendering via matplotlib-style wrapper and that
    !! the text backend output is readable at terminal-cell resolution:
    !! sparse direction markers, thinned flow lines, reserved axes/labels
    !! (issue #2070).
    use fortplot, only: figure, streamplot, title, savefig, wp
    use fortplot_test_output_helpers, only: ensure_test_output_dir
    implicit none

    real(wp), allocatable :: x(:), y(:)
    real(wp), allocatable :: u(:,:), v(:,:)
    character(len=:), allocatable :: outdir
    integer :: i, j, nx, ny

    call ensure_test_output_dir('streamplot', outdir)

    nx = 20; ny = 15
    allocate(x(nx), y(ny), u(nx, ny), v(nx, ny))

    do i = 1, nx
        x(i) = real(i-1, wp) / real(nx-1, wp)
    end do
    do j = 1, ny
        y(j) = real(j-1, wp) / real(ny-1, wp)
    end do

    ! Simple rotational field
    do j = 1, ny
        do i = 1, nx
            u(i,j) = -(y(j) - 0.5_wp)
            v(i,j) =  (x(i) - 0.5_wp)
        end do
    end do

    call figure()
    call streamplot(x, y, u, v, density=1.0_wp, arrowsize=1.0_wp, arrowstyle='->')
    call title('streamplot arrows smoke test')
    call savefig(trim(outdir)//'streamplot_arrows.png')
    call savefig(trim(outdir)//'streamplot_arrows.txt')

    ! Negative fixture: a dense circular field must still cap visible vector
    ! occupancy and stay within the frame (no runtime warning expected).
    call figure()
    call streamplot(x, y, u, v, density=2.0_wp, arrowsize=1.0_wp, arrowstyle='->')
    call title('streamplot arrows dense')
    call savefig(trim(outdir)//'streamplot_arrows_dense.txt')

    call check_readable_text(trim(outdir)//'streamplot_arrows.txt')
    call check_readable_text(trim(outdir)//'streamplot_arrows_dense.txt')

    deallocate(x, y, u, v)

    print *, 'PASS: streamplot text output is readable'

contains

    subroutine check_readable_text(path)
        !! Assert the streamplot text output is legible: several direction
        !! markers, no flow row flooded with glyphs, no hyphen-fragment runs in
        !! the flow field, intact tick labels, and a rectangular frame.
        character(len=*), intent(in) :: path

        integer, parameter :: max_len = 400
        character(len=max_len) :: line
        integer :: unit, ios, ll, k, c1, c2
        integer :: n_dir, flow_count, hyphen_run, max_flow, max_hyphen
        integer :: frame_width, interior_width
        logical :: row_has_digit, saw_frame, uniform_width, has_x_labels
        character(len=1) :: ch

        n_dir = 0
        max_flow = 0
        max_hyphen = 0
        frame_width = -1
        saw_frame = .false.
        uniform_width = .true.
        has_x_labels = .false.

        open(newunit=unit, file=path, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, 'FAIL: cannot open ', path
            error stop 1
        end if

        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            ll = len_trim(line)

            if (index(line(1:max(1, ll)), '0.0') > 0 .and. &
                index(line(1:max(1, ll)), '1.0') > 0) has_x_labels = .true.

            if (ll < 2) cycle
            if (line(1:1) /= '|') cycle
            if (line(ll:ll) /= '|') cycle

            ! An interior frame row: enforce a rectangular frame and inspect the
            ! interior between the two vertical borders.
            if (frame_width < 0) then
                frame_width = ll
            else if (ll /= frame_width) then
                uniform_width = .false.
            end if

            c1 = 2
            c2 = ll - 1
            row_has_digit = .false.
            flow_count = 0
            hyphen_run = 0
            do k = c1, c2
                ch = line(k:k)
                select case (ch)
                case ('<', '>', '^', 'v', '/', '\')
                    n_dir = n_dir + 1
                case ('.')
                    flow_count = flow_count + 1
                case ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')
                    row_has_digit = .true.
                end select
                if (ch == '-') then
                    hyphen_run = hyphen_run + 1
                    if (hyphen_run > max_hyphen .and. .not. row_has_digit) then
                        max_hyphen = hyphen_run
                    end if
                else
                    hyphen_run = 0
                end if
            end do

            ! Only flow rows (no axis tick labels) are judged for flooding.
            if (.not. row_has_digit) then
                saw_frame = .true.
                if (flow_count > max_flow) max_flow = flow_count
            end if
        end do
        close(unit)

        interior_width = frame_width - 2

        if (.not. saw_frame) then
            print *, 'FAIL: no interior flow rows found in ', path
            error stop 1
        end if
        if (n_dir <= 1) then
            print *, 'FAIL: expected more than one direction marker, got ', n_dir
            error stop 1
        end if
        if (2*max_flow >= interior_width) then
            print *, 'FAIL: a flow row is flooded with glyphs: ', max_flow, &
                ' of ', interior_width
            error stop 1
        end if
        if (max_hyphen > 2) then
            print *, 'FAIL: flow row dominated by hyphen fragments, run ', &
                max_hyphen
            error stop 1
        end if
        if (.not. uniform_width) then
            print *, 'FAIL: frame rows are not a uniform width in ', path
            error stop 1
        end if
        if (.not. has_x_labels) then
            print *, 'FAIL: tick labels missing (0.0/1.0) in ', path
            error stop 1
        end if
    end subroutine check_readable_text

end program test_streamplot_arrows

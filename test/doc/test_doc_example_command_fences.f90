program test_doc_example_command_fences
    use fortplot_directory_listing, only: list_directory_entries
    implicit none

    integer, parameter :: max_entries = 256
    character(len=256) :: entries(max_entries)
    integer :: count, status, i

    call list_directory_entries('doc/examples', entries, count, status)
    if (status /= 0) then
        print *, 'FAIL: cannot list doc/examples'
        stop 1
    end if

    do i = 1, count
        if (.not. is_markdown_file(entries(i))) cycle
        call assert_no_bare_commands('doc/examples/' // trim(entries(i)))
    end do

    print *, 'Doc example command fence tests passed'

contains

    logical function is_markdown_file(name)
        character(len=*), intent(in) :: name
        integer :: n

        n = len_trim(name)
        is_markdown_file = n >= 3 .and. name(n-2:n) == '.md'
    end function is_markdown_file

    subroutine assert_no_bare_commands(path)
        character(len=*), intent(in) :: path
        character(len=1024) :: line
        logical :: in_fence
        integer :: unit, ios, line_no

        in_fence = .false.
        line_no = 0

        open(newunit=unit, file=trim(path), status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, 'FAIL: cannot open ', trim(path)
            stop 1
        end if

        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            line_no = line_no + 1
            call trim_right(line)

            if (len_trim(line) >= 3 .and. line(1:3) == '```') then
                in_fence = .not. in_fence
                cycle
            end if

            if (in_fence) cycle
            if (starts_shell_command(line)) then
                print *, 'FAIL: bare shell command in ', trim(path)
                print *, '  line ', line_no, ': ', trim(line)
                stop 1
            end if
        end do

        close(unit)
    end subroutine assert_no_bare_commands

    logical function starts_shell_command(line)
        character(len=*), intent(in) :: line
        character(len=1024) :: trimmed

        trimmed = adjustl(line)
        starts_shell_command = index(trimmed, 'fpm ') == 1 .or. &
                               index(trimmed, 'make ') == 1 .or. &
                               index(trimmed, 'cmake ') == 1 .or. &
                               index(trimmed, './') == 1
    end function starts_shell_command

    subroutine trim_right(s)
        character(len=*), intent(inout) :: s
        integer :: i

        do i = len(s), 1, -1
            if (s(i:i) /= ' ') then
                if (i < len(s)) s(i+1:) = ''
                return
            end if
        end do
        s = ''
    end subroutine trim_right

end program test_doc_example_command_fences

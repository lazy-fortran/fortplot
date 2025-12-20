program test_docs_index_pages
    implicit none

    call assert_index_with_title("doc/cmake_example/index.md")
    call assert_examples_sorted("doc/examples/index.md")

contains

    subroutine assert_index_with_title(path)
        character(len=*), intent(in) :: path
        logical :: exists
        character(len=256) :: line1, line2
        integer :: ios
        integer :: u

        inquire(file=path, exist=exists)
        if (.not. exists) then
            print *, "Missing ", trim(path)
            stop 1
        end if

        open(newunit=u, file=path, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, "Cannot open ", trim(path)
            stop 1
        end if

        read(u,'(A)', iostat=ios) line1
        if (ios /= 0) then
            print *, "Cannot read first line of ", trim(path)
            close(u)
            stop 1
        end if

        read(u,'(A)', iostat=ios) line2
        if (ios /= 0) then
            print *, "Cannot read second line of ", trim(path)
            close(u)
            stop 1
        end if
        close(u)

        call trim_right(line1)
        call trim_right(line2)

        if (.not. is_setext_header(line1, line2)) then
            print *, "Invalid or missing Setext header in ", trim(path)
            stop 1
        end if
    end subroutine assert_index_with_title

    subroutine assert_examples_sorted(path)
        character(len=*), intent(in) :: path
        integer, parameter :: max_examples = 128
        character(len=1024) :: line
        character(len=128) :: names(max_examples)
        character(len=128) :: entry_name
        logical :: exists
        logical :: in_section
        integer :: unit
        integer :: ios
        integer :: count
        integer :: start_pos
        integer :: end_pos
        integer :: i

        names = ''
        in_section = .false.
        count = 0

        inquire(file=path, exist=exists)
        if (.not. exists) then
            print *, "Missing ", trim(path)
            stop 1
        end if

        open(newunit=unit, file=path, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, "Cannot open ", trim(path)
            stop 1
        end if

        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            call trim_right(line)
            if (.not. in_section) then
                if (trim(line) == '<!-- AUTO_EXAMPLES_START -->') then
                    in_section = .true.
                end if
                cycle
            end if
            if (trim(line) == '<!-- AUTO_EXAMPLES_END -->') exit
            if (len_trim(line) == 0) cycle
            if (len_trim(line) < 2) cycle
            if (line(1:2) /= '- ') cycle
            start_pos = index(line, '[')
            end_pos = index(line, ']')
            if (start_pos <= 0 .or. end_pos <= start_pos) cycle
            if (count >= max_examples) then
                print *, "Example index exceeds test capacity"
                stop 1
            end if
            count = count + 1
            entry_name = adjustl(line(start_pos + 1:end_pos - 1))
            names(count) = trim(entry_name)
        end do
        close(unit)

        if (count <= 1) return

        do i = 2, count
            if (names(i - 1) > names(i)) then
                print *, "Example index not sorted:", trim(names(i - 1)), &
                    ' vs ', trim(names(i))
                stop 1
            end if
        end do
    end subroutine assert_examples_sorted

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

    logical function is_setext_header(l1, l2)
        character(len=*), intent(in) :: l1, l2
        integer :: n, i
        character :: ch

        n = len_trim(l2)
        if (n < 3) then
            is_setext_header = .false.
            return
        end if

        ch = l2(1:1)
        if (ch /= '=' .and. ch /= '-') then
            is_setext_header = .false.
            return
        end if

        do i = 1, n
            if (l2(i:i) /= ch) then
                is_setext_header = .false.
                return
            end if
        end do
        is_setext_header = (len_trim(l1) > 0)
    end function is_setext_header

end program test_docs_index_pages

program test_doc_requirements_file
    implicit none

    call assert_requirements_exists()
    call assert_requirements_contains_ford()

contains

    subroutine assert_requirements_exists()
        character(len=*), parameter :: path = 'doc/requirements-doc.txt'
        logical :: exists

        inquire(file=path, exist=exists)
        if (.not. exists) then
            print *, 'Missing ', trim(path)
            stop 1
        end if
    end subroutine assert_requirements_exists

    subroutine assert_requirements_contains_ford()
        character(len=*), parameter :: path = 'doc/requirements-doc.txt'
        character(len=512) :: line
        integer :: u, ios
        logical :: found

        found = .false.
        open(newunit=u, file=path, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, 'Cannot open ', trim(path)
            stop 1
        end if

        do
            read(u, '(A)', iostat=ios) line
            if (ios /= 0) exit
            if (is_dependency_line(line)) then
                if (index(to_lower(trim(line)), 'ford') > 0) then
                    found = .true.
                    exit
                end if
            end if
        end do
        close(u)

        if (.not. found) then
            print *, 'File does not list ford: ', trim(path)
            stop 1
        end if
    end subroutine assert_requirements_contains_ford

    logical function is_dependency_line(s)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: t
        integer :: n

        t = trim(s)
        n = len_trim(t)
        if (n == 0) then
            is_dependency_line = .false.
            return
        end if
        if (t(1:1) == '#') then
            is_dependency_line = .false.
            return
        end if
        is_dependency_line = .true.
    end function is_dependency_line

    pure function to_lower(s) result(r)
        character(len=*), intent(in) :: s
        character(len=len(s)) :: r
        integer :: i, ia
        do i = 1, len(s)
            ia = iachar(s(i:i))
            if (ia >= iachar('A') .and. ia <= iachar('Z')) then
                r(i:i) = achar(ia + 32)
            else
                r(i:i) = s(i:i)
            end if
        end do
    end function to_lower

end program test_doc_requirements_file


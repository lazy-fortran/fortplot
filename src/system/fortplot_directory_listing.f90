module fortplot_directory_listing
    use fortplot_os_detection, only: is_windows
    implicit none
    private

    public :: list_directory_entries

contains

    subroutine list_directory_entries(path, entries, count, status)
        character(len=*), intent(in) :: path
        character(len=*), intent(out) :: entries(:)
        integer, intent(out) :: count
        integer, intent(out), optional :: status

        character(len=:), allocatable :: listing_file, command
        character(len=512) :: line
        logical :: ok
        integer :: unit_num, ios, local_status

        count = 0
        entries = ''
        local_status = 0

        if (len_trim(path) == 0) then
            local_status = -4
            goto 100
        end if

        if (index(path, '"') > 0) then
            local_status = -7
            goto 100
        end if

        listing_file = temp_listing_file()
        if (is_windows()) then
            command = 'cmd /c dir /b /a "' // trim(path) // '" > "' // trim(listing_file) // '"'
        else
            command = 'LC_ALL=C ls -1A "' // trim(path) // '" > "' // trim(listing_file) // '"'
        end if

        call run_listing_command(command, ok)
        if (.not. ok) then
            local_status = -1
            goto 100
        end if

        open(newunit=unit_num, file=trim(listing_file), status='old', action='read', iostat=ios)
        if (ios /= 0) then
            local_status = -1
            goto 100
        end if

        do
            read(unit_num, '(A)', iostat=ios) line
            if (ios /= 0) exit
            if (len_trim(line) == 0) cycle
            if (count >= size(entries)) then
                local_status = -6
                exit
            end if
            count = count + 1
            entries(count) = trim(line)
        end do
        close(unit_num)

100     continue
        if (allocated(listing_file)) then
            call delete_temp_file(listing_file)
        end if
        if (present(status)) status = local_status
    end subroutine list_directory_entries

    subroutine run_listing_command(command, ok)
        character(len=*), intent(in) :: command
        logical, intent(out) :: ok
        integer :: cmdstat, exitstat

        call execute_command_line(command, wait=.true., exitstat=exitstat, cmdstat=cmdstat)
        ok = (cmdstat == 0 .and. exitstat == 0)
    end subroutine run_listing_command

    function temp_listing_file() result(path)
        !! Reserve a private path for the listing redirect.
        !!
        !! The name used to come from system_clock alone, so two processes that
        !! listed a directory within the same tick chose the same file and
        !! clobbered each other: one saw the other's entries or an empty file
        !! that had already been deleted. That made the documentation tests fail
        !! intermittently under a parallel `fpm test`, since each scans example
        !! output directories.
        !!
        !! Claim each candidate with status='new', which fails if the name is
        !! already taken, and keep the empty file as the reservation until the
        !! shell redirect overwrites it.
        character(len=:), allocatable :: path
        integer, parameter :: MAX_ATTEMPTS = 64
        integer :: count, rate, max_count, attempt, unit_num, ios

        call system_clock(count, rate, max_count)
        do attempt = 0, MAX_ATTEMPTS - 1
            path = candidate_path(count + attempt)
            open(newunit=unit_num, file=path, status='new', action='write', &
                 iostat=ios)
            if (ios == 0) then
                close(unit_num)
                return
            end if
        end do
        ! Every candidate was taken: fall back to the last one rather than fail
        ! outright, restoring the previous best-effort behaviour.
        path = candidate_path(count + MAX_ATTEMPTS)
    end function temp_listing_file

    function candidate_path(tag) result(path)
        integer, intent(in) :: tag
        character(len=:), allocatable :: path

        if (is_windows()) then
            path = 'fortplot_directory_listing_' // trim(int_to_str(tag)) // '.txt'
        else
            path = '/tmp/fortplot_directory_listing_' // trim(int_to_str(tag)) // '.txt'
        end if
    end function candidate_path

    subroutine delete_temp_file(path)
        character(len=*), intent(in) :: path
        integer :: unit_num, ios
        logical :: exists

        inquire(file=trim(path), exist=exists)
        if (.not. exists) return

        open(newunit=unit_num, file=trim(path), status='old', iostat=ios)
        if (ios == 0) close(unit_num, status='delete')
    end subroutine delete_temp_file

    function int_to_str(value) result(text)
        integer, intent(in) :: value
        character(len=32) :: text

        write(text, '(I0)') value
    end function int_to_str

end module fortplot_directory_listing

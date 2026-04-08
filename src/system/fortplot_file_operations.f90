module fortplot_file_operations
    use fortplot_os_detection, only: is_windows
    implicit none
    private

    public :: create_directory_runtime
    public :: delete_file_runtime
    public :: check_directory_exists
    public :: create_directory_recursive
    public :: create_single_directory

contains

    subroutine create_directory_runtime(path, success)
        character(len=*), intent(in) :: path
        logical, intent(out) :: success

        if (.not. is_valid_path(path)) then
            success = .false.
            return
        end if
        call create_directory_recursive(path, success)
    end subroutine create_directory_runtime

    subroutine delete_file_runtime(filename, success)
        character(len=*), intent(in) :: filename
        logical, intent(out) :: success
        integer :: unit_num, ios
        logical :: exists

        if (.not. is_valid_path(filename)) then
            success = .false.
            return
        end if

        inquire(file=trim(filename), exist=exists)
        if (.not. exists) then
            success = .true.
            return
        end if

        open(newunit=unit_num, file=trim(filename), status='old', iostat=ios)
        if (ios /= 0) then
            success = .false.
            return
        end if

        close(unit_num, status='delete', iostat=ios)
        success = (ios == 0)
    end subroutine delete_file_runtime

    subroutine check_directory_exists(path, exists)
        character(len=*), intent(in) :: path
        logical, intent(out) :: exists

        exists = .false.
        if (len_trim(path) == 0) return

        if (is_windows()) then
            inquire(file=trim(path), exist=exists)
            if (.not. exists) inquire(file=trim(path)//"\.", exist=exists)
        else
            inquire(file=trim(path)//"/.", exist=exists)
            if (.not. exists) inquire(file=trim(path), exist=exists)
        end if
    end subroutine check_directory_exists

    subroutine create_single_directory(path, success)
        character(len=*), intent(in) :: path
        logical, intent(out) :: success

        call create_directory_command(path, .false., success)
    end subroutine create_single_directory

    subroutine create_directory_recursive(path, success)
        character(len=*), intent(in) :: path
        logical, intent(out) :: success

        call create_directory_command(path, .true., success)
    end subroutine create_directory_recursive

    subroutine create_directory_command(path, recursive, success)
        character(len=*), intent(in) :: path
        logical, intent(in) :: recursive
        logical, intent(out) :: success
        character(len=:), allocatable :: command
        integer :: cmdstat, exitstat

        call check_directory_exists(path, success)
        if (success) return
        if (len_trim(path) == 0) then
            success = .false.
            return
        end if

        if (is_windows()) then
            command = 'mkdir ' // quote_argument(path) // ' >NUL 2>NUL'
        else if (recursive) then
            command = 'mkdir -p ' // quote_argument(path)
        else
            command = 'mkdir ' // quote_argument(path)
        end if

        call execute_command_line(command, wait=.true., exitstat=exitstat, cmdstat=cmdstat)
        call check_directory_exists(path, success)
        if (.not. success) success = (cmdstat == 0 .and. exitstat == 0)
    end subroutine create_directory_command

    logical function is_valid_path(path) result(valid)
        character(len=*), intent(in) :: path
        integer :: i, n, code

        n = len_trim(path)
        valid = (n > 0)
        if (.not. valid) return

        do i = 1, n
            code = iachar(path(i:i))
            select case (code)
            case (0, 10, 13)
                valid = .false.
                return
            case default
            end select
        end do
    end function is_valid_path

    function quote_argument(path) result(quoted)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: quoted

        if (is_windows()) then
            quoted = '"' // replace_all(trim(path), '"', '""') // '"'
        else
            quoted = "'" // replace_all(trim(path), "'", "'\''") // "'"
        end if
    end function quote_argument

    function replace_all(text, old, new) result(updated)
        character(len=*), intent(in) :: text, old, new
        character(len=:), allocatable :: updated
        integer :: pos, old_len

        updated = text
        old_len = len(old)
        if (old_len == 0) return

        pos = index(updated, old)
        do while (pos > 0)
            updated = updated(:pos - 1) // new // updated(pos + old_len:)
            pos = index(updated, old)
        end do
    end function replace_all

end module fortplot_file_operations

module fortplot_directory_listing
    !! Cross-platform directory listing utilities for documentation tooling
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_size_t, c_null_char
    implicit none
    private

    integer, parameter :: INITIAL_BUFFER_SIZE = 16384
    integer, parameter :: BUFFER_GROWTH_FACTOR = 2
    integer, parameter :: MAX_BUFFER_SIZE = 1048576
    character(kind=c_char), parameter :: C_NEWLINE = char(10, kind=c_char)

    public :: list_directory_entries

    interface
        function fortplot_list_directory(path, buffer, buffer_len) bind(C, name="fortplot_list_directory") result(res)
            import :: c_char, c_int, c_size_t
            character(kind=c_char), dimension(*), intent(in) :: path
            character(kind=c_char), dimension(*), intent(out) :: buffer
            integer(c_size_t), value :: buffer_len
            integer(c_int) :: res
        end function fortplot_list_directory
    end interface

contains

    subroutine list_directory_entries(path, entries, count, status)
        !! List files and direct child directories without recursion
        character(len=*), intent(in) :: path
        character(len=*), intent(out) :: entries(:)
        integer, intent(out) :: count
        integer, intent(out), optional :: status

        character(kind=c_char), allocatable :: c_path(:), buffer(:)
        integer :: buffer_size
        integer(c_int) :: result
        integer :: i, start_idx, max_entries
        integer :: local_status

        count = 0
        entries = ''
        local_status = 0
        max_entries = size(entries)

        if (len_trim(path) == 0) then
            local_status = -4
            goto 100
        end if

        call to_c_string(trim(path), c_path)

        buffer_size = INITIAL_BUFFER_SIZE
        do
            call allocate_buffer(buffer, buffer_size)
            result = fortplot_list_directory(c_path, buffer, int(buffer_size, kind=c_size_t))

            if (result == -2) then
                deallocate(buffer)
                buffer_size = buffer_size * BUFFER_GROWTH_FACTOR
                if (buffer_size > MAX_BUFFER_SIZE) then
                    local_status = -5
                    goto 100
                end if
                cycle
            else
                exit
            end if
        end do

        if (result < 0) then
            local_status = result
            goto 100
        end if

        start_idx = 1
        do i = 1, result
            if (buffer(i) == C_NEWLINE .or. buffer(i) == c_null_char) then
                call append_entry(buffer(start_idx:i-1), entries, count, max_entries, local_status)
                if (local_status /= 0) goto 100
                start_idx = i + 1
            end if
        end do

        if (start_idx <= result) then
            call append_entry(buffer(start_idx:result), entries, count, max_entries, local_status)
        end if

100     continue
        if (allocated(buffer)) deallocate(buffer)
        if (allocated(c_path)) deallocate(c_path)

        if (present(status)) status = local_status
    end subroutine list_directory_entries

    subroutine append_entry(slice, entries, count, max_entries, status)
        character(kind=c_char), intent(in) :: slice(:)
        character(len=*), intent(inout) :: entries(:)
        integer, intent(inout) :: count
        integer, intent(in) :: max_entries
        integer, intent(inout) :: status

        if (status /= 0) return
        if (size(slice) <= 0) return

        if (count >= max_entries) then
            status = -6
            return
        end if

        count = count + 1
        call copy_to_entry(slice, entries(count))
    end subroutine append_entry

    subroutine allocate_buffer(buffer, size)
        character(kind=c_char), allocatable, intent(out) :: buffer(:)
        integer, intent(in) :: size

        if (size <= 0) then
            allocate(buffer(1))
            buffer = c_null_char
        else
            allocate(buffer(size))
            buffer = c_null_char
        end if
    end subroutine allocate_buffer

    subroutine to_c_string(str, c_str)
        character(len=*), intent(in) :: str
        character(kind=c_char), allocatable, intent(out) :: c_str(:)
        integer :: n, i

        n = len_trim(str)
        allocate(c_str(n + 1))
        do i = 1, n
            c_str(i) = char(ichar(str(i:i)), kind=c_char)
        end do
        c_str(n + 1) = c_null_char
    end subroutine to_c_string

    subroutine copy_to_entry(source, dest)
        character(kind=c_char), intent(in) :: source(:)
        character(len=*), intent(inout) :: dest
        integer :: n, i
        character(len=1) :: tmp

        dest = ''
        n = min(len(dest), size(source))

        do i = 1, n
            tmp = transfer(source(i), tmp)
            dest(i:i) = tmp
        end do
    end subroutine copy_to_entry

end module fortplot_directory_listing

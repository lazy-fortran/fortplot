module fortplot_path_operations
    !! Path manipulation and normalization utilities
    !! 
    !! This module handles cross-platform path operations including
    !! path mapping, separator normalization, and path parsing.

    use fortplot_os_detection, only: is_windows

    implicit none
    private

    public :: map_unix_to_windows_path
    public :: normalize_path_separators
    public :: get_parent_directory
    public :: parse_path_segments

contains

    function map_unix_to_windows_path(path) result(mapped_path)
        !! Map Unix-style /tmp paths to Windows-compatible paths
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: mapped_path
        character(len=512) :: temp_dir
        integer :: status
        
        if (is_windows()) then
            if (path == "/tmp") then
                ! Use Windows TEMP directory
                call get_environment_variable("TEMP", temp_dir, status=status)
                if (status == 0 .and. len_trim(temp_dir) > 0) then
                    mapped_path = trim(temp_dir)
                else
                    ! Fallback to local tmp directory
                    mapped_path = "tmp"
                end if
            else if (len(path) >= 5 .and. path(1:5) == "/tmp/") then
                ! Map /tmp/filename to TEMP/filename or tmp/filename
                call get_environment_variable("TEMP", temp_dir, status=status)
                if (status == 0 .and. len_trim(temp_dir) > 0) then
                    mapped_path = trim(temp_dir) // "\" // path(6:)
                else
                    ! Fallback to local tmp directory
                    mapped_path = "tmp" // path(5:)
                end if
            else
                mapped_path = path
            end if
        else
            ! On Unix/Linux, keep paths as-is
            mapped_path = path
        end if
    end function map_unix_to_windows_path

    function normalize_path_separators(path, to_windows) result(normalized)
        !! Normalize path separators for the target platform
        character(len=*), intent(in) :: path
        logical, intent(in) :: to_windows
        character(len=:), allocatable :: normalized
        integer :: i
        
        normalized = path
        
        if (to_windows) then
            ! Convert forward slashes to backslashes
            do i = 1, len(normalized)
                if (normalized(i:i) == '/') then
                    normalized(i:i) = '\'
                end if
            end do
        else
            ! Convert backslashes to forward slashes
            do i = 1, len(normalized)
                if (normalized(i:i) == '\') then
                    normalized(i:i) = '/'
                end if
            end do
        end if
    end function normalize_path_separators

    function get_parent_directory(path) result(parent)
        !! Extract parent directory from a path
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: parent
        integer :: last_sep, i
        
        last_sep = 0
        do i = len_trim(path), 1, -1
            if (path(i:i) == '/' .or. path(i:i) == '\') then
                last_sep = i - 1
                exit
            end if
        end do
        
        if (last_sep > 0) then
            parent = path(1:last_sep)
        else
            parent = ""
        end if
    end function get_parent_directory

    subroutine parse_path_segments(path, segments, n_segments)
        !! Parse a path into directory segments
        character(len=*), intent(in) :: path
        character(len=*), intent(out) :: segments(100)
        integer, intent(out) :: n_segments
        integer :: i, start_pos, path_len
        character :: sep
        
        segments = ""
        n_segments = 0
        path_len = len_trim(path)
        if (path_len == 0) return
        
        ! Determine separator
        if (is_windows()) then
            sep = '\'
        else
            sep = '/'
        end if
        
        ! Handle absolute paths
        start_pos = 1
        if (path(1:1) == '/' .or. path(1:1) == '\') then
            n_segments = 1
            segments(1) = path(1:1)
            start_pos = 2
        else if (path_len >= 2 .and. path(2:2) == ':') then
            ! Windows drive letter
            n_segments = 1
            if (path_len >= 3 .and. (path(3:3) == '\' .or. path(3:3) == '/')) then
                segments(1) = path(1:3)
                start_pos = 4
            else
                segments(1) = path(1:2)
                start_pos = 3
            end if
        end if
        
        ! Parse remaining segments
        i = start_pos
        do while (i <= path_len)
            if (path(i:i) == '/' .or. path(i:i) == '\') then
                if (i > start_pos) then
                    n_segments = n_segments + 1
                    segments(n_segments) = path(start_pos:i-1)
                end if
                start_pos = i + 1
            end if
            i = i + 1
        end do
        
        ! Add final segment
        if (start_pos <= path_len) then
            n_segments = n_segments + 1
            segments(n_segments) = path(start_pos:path_len)
        end if
    end subroutine parse_path_segments

end module fortplot_path_operations
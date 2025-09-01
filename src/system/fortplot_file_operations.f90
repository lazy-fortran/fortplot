module fortplot_file_operations
    !! File system operations module
    !! 
    !! This module handles file and directory operations with security
    !! restrictions to prevent unauthorized filesystem access.

    use fortplot_os_detection, only: is_debug_enabled, is_windows
    use fortplot_logging,      only: log_warning

    implicit none
    private

    ! Interface to C directory creation function
    interface
        function create_directory_windows_c(path) bind(c, name="create_directory_windows_c") result(success)
            use, intrinsic :: iso_c_binding, only: c_char, c_int
            character(c_char), intent(in) :: path(*)
            integer(c_int) :: success
        end function create_directory_windows_c
    end interface

    public :: create_directory_runtime
    public :: delete_file_runtime
    public :: check_directory_exists
    public :: create_directory_recursive
    public :: create_single_directory

contains

    subroutine create_directory_runtime(path, success)
        !! Create directory with comprehensive security validation
        !! Issue #903: Allow legitimate user directories while preserving security
        character(len=*), intent(in) :: path
        logical, intent(out) :: success
        logical :: debug_enabled
        logical :: is_allowed_path
        character(len=512) :: normalized_path
        
        success = .false.
        debug_enabled = is_debug_enabled()
        normalized_path = path
        
        ! SECURITY LAYER 1: Basic path safety validation
        if (.not. is_basic_safe_path(normalized_path)) then
            if (debug_enabled) then
                call log_warning('Security: Unsafe path blocked by validation')
            end if
            success = .false.
            return
        end if
        
        ! SECURITY LAYER 2: Path whitelist for allowed directory creation
        call check_allowed_path(normalized_path, is_allowed_path)
        if (.not. is_allowed_path) then
            if (debug_enabled) then
                call log_warning('Security: Directory creation not allowed for requested path')
            end if
            success = .false.
            return
        end if
        
        ! Try recursive directory creation approach
        call create_directory_recursive(path, success)
        
        if (.not. success .and. debug_enabled) then
            call log_warning('Directory creation failed (check permissions and parent existence)')
        end if
    end subroutine create_directory_runtime

    subroutine delete_file_runtime(filename, success)
        !! SECURITY: File deletion disabled for security compliance
        character(len=*), intent(in) :: filename
        logical, intent(out) :: success
        
        ! SECURITY: External file operations disabled to prevent vulnerabilities
        success = .false.
    end subroutine delete_file_runtime

    subroutine check_directory_exists(path, exists)
        !! Check if a directory exists using inquire
        character(len=*), intent(in) :: path
        logical, intent(out) :: exists
        
        if (is_windows()) then
            ! On Windows, try multiple approaches
            inquire(file=trim(path), exist=exists)
            if (.not. exists) then
                inquire(file=trim(path)//"\.", exist=exists)
            end if
            if (.not. exists) then
                inquire(file=trim(path)//"\\", exist=exists)
            end if
        else
            ! Unix/Linux approach
            inquire(file=trim(path)//"/." , exist=exists)
            if (.not. exists) then
                inquire(file=trim(path), exist=exists)
            end if
        end if
    end subroutine check_directory_exists
    
    subroutine create_single_directory(path, success)
        !! Create a single directory level - robust cross-platform method
        character(len=*), intent(in) :: path
        logical, intent(out) :: success
        logical :: dir_exists, parent_exists
        character(len=512) :: parent_path, test_file
        integer :: i, last_sep, unit, iostat
        
        success = .false.
        
        ! First check if directory already exists
        call check_directory_exists(path, dir_exists)
        if (dir_exists) then
            success = .true.
            return
        end if
        
        ! Find parent directory
        last_sep = 0
        do i = len_trim(path), 1, -1
            if (path(i:i) == '/' .or. path(i:i) == '\') then
                last_sep = i - 1
                exit
            end if
        end do
        
        if (last_sep > 0) then
            parent_path = path(1:last_sep)
            call check_directory_exists(parent_path, parent_exists)
            if (.not. parent_exists) then
                ! Parent doesn't exist, can't create subdirectory
                success = .false.
                return
            end if
        end if
        
        ! Try to actually create or verify the directory exists
        call check_directory_exists(path, dir_exists)
        if (dir_exists) then
            success = .true.
        else
            ! Try a simple test file approach for directory creation
            test_file = trim(path)
            if (is_windows()) then
                test_file = trim(test_file) // "\test_dir_creation.tmp"
            else
                test_file = trim(test_file) // "/test_dir_creation.tmp"
            end if
            
            ! Try to open a file to test if we can create in this directory
            open(newunit=unit, file=test_file, status='unknown', &
                 action='write', iostat=iostat)
            if (iostat == 0) then
                close(unit, status='delete')
                success = .true.
            else
                success = .false.
            end if
        end if
    end subroutine create_single_directory

    recursive subroutine create_directory_recursive(path, success)
        !! Recursively create directory path including parent directories
        use, intrinsic :: iso_c_binding, only: c_null_char
        character(len=*), intent(in) :: path
        logical, intent(out) :: success
        character(len=512) :: parent_path, test_file
        character(len=512) :: path_segments(100)
        character(len=512) :: current_path
        integer :: i, n_segments, last_sep, unit, iostat
        logical :: parent_exists, dir_exists
        
        success = .false.
        
        ! Check if directory already exists
        call check_directory_exists(path, dir_exists)
        if (dir_exists) then
            success = .true.
            return
        end if
        
        ! Find parent directory
        last_sep = 0
        do i = len_trim(path), 1, -1
            if (path(i:i) == '/' .or. path(i:i) == '\') then
                last_sep = i - 1
                exit
            end if
        end do
        
        if (last_sep > 0) then
            parent_path = path(1:last_sep)
            
            ! Recursively create parent
            call check_directory_exists(parent_path, parent_exists)
            if (.not. parent_exists) then
                call create_directory_recursive(parent_path, parent_exists)
                if (.not. parent_exists) then
                    success = .false.
                    return
                end if
            end if
        end if
        
        ! Use C function for robust directory creation on all platforms
        ! Note: create_directory_windows_c has both Windows and Unix implementations
        success = (create_directory_windows_c(trim(path) // c_null_char) == 1)
        
        ! Final check
        call check_directory_exists(path, success)
    end subroutine create_directory_recursive

    subroutine check_allowed_path(path, is_allowed)
        !! Check if directory path is allowed for creation
        !! Issue #903: Intelligent path whitelist for user experience
        character(len=*), intent(in) :: path
        logical, intent(out) :: is_allowed
        character(len=512) :: normalized_path
        
        normalized_path = trim(path)
        is_allowed = .false.
        
        ! EXISTING TEST PATHS (preserve all current functionality)
        if (index(normalized_path, 'build/test') > 0 .or. &
            index(normalized_path, 'build\test') > 0 .or. &
            index(normalized_path, 'fortplot_test_') > 0 .or. &
            index(normalized_path, 'output/example') > 0 .or. &
            index(normalized_path, 'output\example') > 0 .or. &
            index(normalized_path, 'test/output') > 0 .or. &
            index(normalized_path, 'test\output') > 0 .or. &
            trim(normalized_path) == 'test' .or. &
            trim(normalized_path) == 'test/output' .or. &
            trim(normalized_path) == 'test\output' .or. &
            index(normalized_path, '/tmp/fortplot_test_') > 0 .or. &
            index(normalized_path, '\tmp\fortplot_test_') > 0) then
            is_allowed = .true.
            return
        end if
        
        ! ANIMATION OUTPUT PATHS (Issue #938: Enable animation directory creation)
        if (index(normalized_path, 'output/example/fortran/animation') > 0 .or. &
            index(normalized_path, 'output\example\fortran\animation') > 0 .or. &
            index(normalized_path, 'animation') > 0) then
            is_allowed = .true.
            return
        end if
        
        ! COMMON USER DIRECTORIES (Issue #903: Enable basic user workflow)
        call check_user_directory_patterns(normalized_path, is_allowed)
    end subroutine check_allowed_path

    logical function has_parent_segment(path) result(has_parent)
        !! Detect '..' path segments robustly for both '/' and '\\' separators
        character(len=*), intent(in) :: path
        integer :: n
        character(len=:), allocatable :: p
        has_parent = .false.
        n = len_trim(path)
        if (n == 0) return
        p = path(1:n)
        if (trim(p) == '..') then
            has_parent = .true.
            return
        end if
        if (index(p, '../') > 0) then
            has_parent = .true.
            return
        end if
        if (index(p, '..\\') > 0) then
            has_parent = .true.
            return
        end if
        if (index(p, '/..') > 0) then
            has_parent = .true.
            return
        end if
        if (index(p, '\\..') > 0) then
            has_parent = .true.
            return
        end if
    end function has_parent_segment

    subroutine check_user_directory_patterns(path, is_allowed)
        !! Check if path matches common user directory patterns
        !! Issue #903: Support matplotlib-like directory auto-creation
        character(len=*), intent(in) :: path
        logical, intent(out) :: is_allowed
        character(len=512) :: first_component
        integer :: first_slash
        
        is_allowed = .false.
        
        ! Extract first path component for pattern matching
        first_slash = index(path, '/')
        if (first_slash == 0) first_slash = index(path, '\')
        
        if (first_slash > 0) then
            first_component = path(1:first_slash-1)
        else
            first_component = path
        end if
        
        ! SCIENTIFIC/ANALYSIS DIRECTORIES
        if (trim(first_component) == 'results' .or. &
            trim(first_component) == 'plots' .or. &
            trim(first_component) == 'figures' .or. &
            trim(first_component) == 'output' .or. &
            trim(first_component) == 'data' .or. &
            trim(first_component) == 'analysis' .or. &
            trim(first_component) == 'images' .or. &
            trim(first_component) == 'graphics' .or. &
            trim(first_component) == 'visualization' .or. &
            trim(first_component) == 'charts') then
            is_allowed = .true.
            return
        end if
        
        ! RELATIVE SUBDIRECTORIES (no leading slash, no traversal)
        if (len_trim(path) > 0 .and. path(1:1) /= '/' .and. path(1:1) /= '\') then
            ! Allow only if there is no parent directory segment
            if (.not. has_parent_segment(path)) then
                is_allowed = .true.
                return
            end if
        end if
    end subroutine check_user_directory_patterns

    function is_basic_safe_path(path) result(safe)
        !! Basic path safety validation to prevent common security issues
        !! Issue #903: Simplified security validation to avoid circular dependencies
        character(len=*), intent(in) :: path
        logical :: safe
        
        safe = .true.
        
        ! Check for empty path
        if (len_trim(path) == 0) then
            safe = .false.
            return
        end if
        
        ! Check for directory traversal attacks (robust '..' detection)
        if (has_parent_segment(path)) then
            safe = .false.
            return
        end if
        
        ! Check for absolute system paths that should be blocked
        if (index(path, '/etc/') == 1 .or. index(path, '/sys/') == 1 .or. &
            index(path, '/proc/') == 1 .or. index(path, '/dev/') == 1) then
            safe = .false.
            return
        end if
        
        ! Check for dangerous characters that could enable shell injection
        if (index(path, ';') > 0 .or. index(path, '&') > 0 .or. &
            index(path, '|') > 0 .or. index(path, '`') > 0 .or. &
            index(path, '$') > 0 .or. index(path, '(') > 0 .or. &
            index(path, ')') > 0 .or. index(path, '<') > 0 .or. &
            index(path, '>') > 0 .or. index(path, '*') > 0 .or. &
            index(path, '?') > 0 .or. index(path, '!') > 0) then
            safe = .false.
            return
        end if
        
        ! Check for null characters
        if (index(path, char(0)) > 0) then
            safe = .false.
            return
        end if
        
        ! Path is safe
        safe = .true.
    end function is_basic_safe_path

end module fortplot_file_operations

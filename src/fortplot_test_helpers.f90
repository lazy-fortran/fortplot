module fortplot_test_helpers
    !! Unified secure test utilities module
    !! Provides comprehensive test file management with proper cleanup
    !! Replaces all previous test helper modules with secure implementation
    
    use iso_fortran_env, only: int32, int64
    use fortplot_system_runtime, only: is_windows, create_directory_runtime, &
                                       delete_file_runtime, normalize_path_separators
    ! No longer need secure module - validation is built-in
    use fortplot, only: figure_t
    implicit none
    private
    
    public :: test_savefig
    public :: test_initialize_figure
    public :: test_cleanup_all
    public :: test_get_temp_path
    public :: test_cleanup_file
    public :: test_initialize_environment
    public :: test_register_file
    public :: test_get_unique_suffix
    
    character(len=*), parameter :: TEMP_DIR_PREFIX = "fortplot_test_"
    character(len=512), save :: current_test_dir = ""
    logical, save :: test_dir_created = .false.
    character(len=512), save :: registered_files(100) = ""
    integer, save :: file_count = 0
    
contains

    function is_safe_path(path) result(safe)
        !! Validate path for basic security (simplified version)
        character(len=*), intent(in) :: path
        logical :: safe
        integer :: i, len_path
        character :: c
        
        safe = .false.
        len_path = len_trim(path)
        
        ! Check basic constraints
        if (len_path == 0 .or. len_path > 512) return
        
        ! Check for directory traversal
        if (index(path, '..') > 0) return
        
        ! Check for null bytes
        if (index(path, char(0)) > 0) return
        
        ! Check for reasonable characters
        do i = 1, len_path
            c = path(i:i)
            if (.not. ((c >= 'a' .and. c <= 'z') .or. &
                       (c >= 'A' .and. c <= 'Z') .or. &
                       (c >= '0' .and. c <= '9') .or. &
                       c == '-' .or. c == '_' .or. c == '.' .or. &
                       c == '/' .or. c == '\' .or. c == ':' .or. &
                       c == ' ')) then
                return
            end if
        end do
        
        safe = .true.
    end function is_safe_path

    function test_get_unique_suffix() result(suffix)
        !! Generate reliable unique suffix combining multiple entropy sources
        character(len=:), allocatable :: suffix
        character(len=64) :: timestamp_str, pid_str, random_str
        integer(int64) :: timestamp
        integer :: pid, random_val, status
        real :: random_real
        
        ! Get high-resolution timestamp
        call system_clock(timestamp)
        write(timestamp_str, '(I0)') timestamp
        
        ! Try to get real process ID from environment
        call get_environment_variable("PPID", pid_str, status=status)
        if (status == 0 .and. len_trim(pid_str) > 0) then
            read(pid_str, *, iostat=status) pid
            if (status /= 0) pid = int(mod(timestamp, 99991_int64)) + 1000
        else
            ! Use timestamp-based fallback with better distribution
            pid = int(mod(timestamp, 99991_int64)) + 1000
        end if
        write(pid_str, '(I0)') pid
        
        ! Add random component
        call random_number(random_real)
        random_val = int(random_real * 8999) + 1000
        write(random_str, '(I0)') random_val
        
        ! Combine all entropy sources
        suffix = trim(timestamp_str) // "_" // trim(pid_str) // "_" // trim(random_str)
    end function test_get_unique_suffix
    
    subroutine ensure_test_directory(unique_suffix)
        !! Ensure test directory exists with proper error handling
        character(len=*), intent(in), optional :: unique_suffix
        character(len=512) :: base_temp
        character(len=:), allocatable :: suffix
        integer :: status
        logical :: success
        
        if (test_dir_created .and. len_trim(current_test_dir) > 0) return
        
        ! Generate or use provided unique suffix
        if (present(unique_suffix)) then
            suffix = trim(unique_suffix)
        else
            suffix = test_get_unique_suffix()
        end if
        
        ! Build platform-appropriate temp directory path
        if (is_windows()) then
            call get_environment_variable("TEMP", base_temp, status=status)
            if (status /= 0 .or. len_trim(base_temp) == 0) then
                call get_environment_variable("TMP", base_temp, status=status)
                if (status /= 0 .or. len_trim(base_temp) == 0) then
                    ! Use current directory as fallback on Windows
                    base_temp = "."
                end if
            end if
            current_test_dir = trim(base_temp) // "\" // TEMP_DIR_PREFIX // trim(suffix)
            current_test_dir = normalize_path_separators(current_test_dir, .true.)
        else
            ! Always use /tmp on Unix systems - it exists and is writable
            current_test_dir = "/tmp/" // TEMP_DIR_PREFIX // trim(suffix)
        end if
        
        ! Validate the generated path
        if (.not. is_safe_path(current_test_dir)) then
            print *, "ERROR: Generated invalid temp directory path"
            current_test_dir = TEMP_DIR_PREFIX // trim(suffix)
        end if
        
        ! Try to create or use the directory
        call create_directory_runtime(current_test_dir, success)
        if (success) then
            test_dir_created = .true.
        else
            ! Check if directory already exists
            inquire(file=trim(current_test_dir)//"/." , exist=test_dir_created)
            if (.not. test_dir_created) then
                ! Try build/test directory which should exist
                current_test_dir = "build/test/" // TEMP_DIR_PREFIX // trim(suffix)
                if (is_windows()) then
                    current_test_dir = normalize_path_separators(current_test_dir, .true.)
                end if
                call create_directory_runtime(current_test_dir, success)
                test_dir_created = success
                
                if (.not. success) then
                    ! Check if build/test exists
                    inquire(file="build/test/." , exist=test_dir_created)
                    if (test_dir_created) then
                        current_test_dir = "build/test"
                        if (is_windows()) then
                            current_test_dir = normalize_path_separators(current_test_dir, .true.)
                        end if
                    else
                        ! Ultimate fallback: use current directory
                        current_test_dir = "."
                        test_dir_created = .true.
                    end if
                end if
            end if
        end if
    end subroutine ensure_test_directory
    
    function test_get_temp_path(filename) result(full_path)
        !! Get secure temporary file path with validation
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: full_path
        
        ! Validate input filename
        if (.not. is_safe_path(filename)) then
            print *, "ERROR: Invalid filename provided: ", trim(filename)
            full_path = "invalid_filename"
            return
        end if
        
        call ensure_test_directory()
        
        ! If temp directory creation failed, don't create files
        if (.not. test_dir_created .or. len_trim(current_test_dir) == 0) then
            print *, "ERROR: No temp directory available, cannot create test file: ", trim(filename)
            if (is_windows()) then
                full_path = "NUL"  ! Windows null device
            else
                full_path = "/dev/null"  ! Unix null device
            end if
            return
        end if
        
        ! Build path with proper separators
        if (is_windows()) then
            full_path = trim(current_test_dir) // "\" // trim(filename)
            full_path = normalize_path_separators(full_path, .true.)
        else
            full_path = trim(current_test_dir) // "/" // trim(filename)
        end if
        
        ! Final path validation
        if (.not. is_safe_path(full_path)) then
            print *, "ERROR: Generated invalid temp path: ", trim(full_path)
            full_path = trim(filename)  ! Fallback to local file
        end if
    end function test_get_temp_path
    
    subroutine test_initialize_environment(test_name)
        !! Initialize test environment with specific name
        character(len=*), intent(in) :: test_name
        
        ! Reset file registry
        file_count = 0
        registered_files = ""
        
        ! Initialize directory with test name as suffix
        call ensure_test_directory(test_name)
    end subroutine test_initialize_environment
    
    subroutine test_register_file(filename)
        !! Register a file for automatic cleanup
        character(len=*), intent(in) :: filename
        
        if (file_count < size(registered_files)) then
            file_count = file_count + 1
            registered_files(file_count) = filename
        else
            print *, "WARNING: Test file registry full, cannot register: ", trim(filename)
        end if
    end subroutine test_register_file
    
    subroutine test_cleanup_file(filename)
        !! Clean up a specific test file
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: full_path
        logical :: success
        
        full_path = test_get_temp_path(filename)
        call delete_file_runtime(full_path, success)
        
        if (.not. success) then
            print *, "WARNING: Could not delete test file: ", trim(full_path)
        end if
    end subroutine test_cleanup_file
    
    subroutine test_initialize_figure(fig, width, height, backend)
        !! Initialize figure for testing (ensures temp directory exists)
        type(figure_t), intent(out) :: fig
        integer, intent(in) :: width, height
        character(len=*), intent(in) :: backend
        
        call ensure_test_directory()
        call fig%initialize(width, height, backend)
    end subroutine test_initialize_figure
    
    subroutine test_savefig(fig, filename)
        !! Save figure to temporary directory with automatic registration
        type(figure_t), intent(inout) :: fig
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: temp_path
        
        temp_path = test_get_temp_path(filename)
        call test_register_file(filename)
        call fig%savefig(temp_path)
    end subroutine test_savefig
    
    subroutine test_cleanup_all()
        !! Clean up all registered files and test directory securely
        logical :: success
        integer :: i
        character(len=:), allocatable :: file_path
        
        if (.not. test_dir_created .or. len_trim(current_test_dir) == 0) return
        
        ! First, clean up all registered files individually
        do i = 1, file_count
            if (len_trim(registered_files(i)) > 0) then
                file_path = test_get_temp_path(registered_files(i))
                call delete_file_runtime(file_path, success)
                ! Don't warn on individual file cleanup failures
            end if
        end do
        
        ! Try to remove the test directory itself
        ! This is safe since we validate paths and use only our created directories
        call delete_directory_secure(current_test_dir, success)
        
        ! Reset state
        test_dir_created = .false.
        current_test_dir = ""
        file_count = 0
        registered_files = ""
    end subroutine test_cleanup_all
    
    subroutine delete_directory_secure(dir_path, success)
        !! Securely delete a directory using runtime system calls
        character(len=*), intent(in) :: dir_path
        logical, intent(out) :: success
        character(len=:), allocatable :: command
        integer :: exitstat, cmdstat
        character(len=256) :: cmdmsg
        
        success = .false.
        
        ! Validate the directory path before deletion
        if (.not. is_safe_path(dir_path)) then
            return
        end if
        
        ! Only delete directories we created (must contain our prefix)
        if (index(dir_path, TEMP_DIR_PREFIX) == 0) then
            return
        end if
        
        ! SECURITY: Directory deletion requires external command execution
        ! This functionality is disabled for security compliance
        success = .false.
    end subroutine delete_directory_secure

end module fortplot_test_helpers
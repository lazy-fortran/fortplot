module fortplot_test_utilities
    !! Comprehensive utilities for managing test output files and temporary directories
    !! Provides systematic cleanup to prevent test file pollution
    use iso_fortran_env, only: int32, int64
    use fortplot_system_runtime, only: is_windows, create_directory_runtime, &
                                       delete_file_runtime, map_unix_to_windows_path, &
                                       normalize_path_separators
    use fortplot, only: figure_t
    implicit none
    private

    public :: get_test_output_dir
    public :: create_test_output_dir
    public :: cleanup_test_file
    public :: get_test_output_path
    public :: initialize_test_environment
    public :: cleanup_test_environment
    public :: setup_test_figure
    public :: cleanup_all_test_files
    public :: register_test_file
    public :: test_wrapper_t

    character(len=*), parameter :: TEST_OUTPUT_BASE = "fortplot_test_temp"
    
    !> Type for managing test file registry and cleanup
    type :: test_wrapper_t
        character(len=512), allocatable :: registered_files(:)
        integer :: file_count = 0
        character(len=:), allocatable :: test_dir
    contains
        procedure :: initialize => test_wrapper_initialize
        procedure :: register_file => test_wrapper_register_file
        procedure :: cleanup => test_wrapper_cleanup
        procedure :: get_path => test_wrapper_get_path
        procedure :: setup_figure => test_wrapper_setup_figure
    end type test_wrapper_t
    
    ! Global test wrapper for simple usage
    type(test_wrapper_t), save :: global_test_wrapper

contains

    function get_test_output_dir(unique_suffix) result(dir_path)
        !! Get the path to the test output directory with optional unique suffix
        character(len=*), intent(in), optional :: unique_suffix
        character(len=:), allocatable :: dir_path
        character(len=512) :: temp_dir
        character(len=64) :: suffix
        integer :: status
        
        ! Create unique suffix from process ID and timestamp
        if (present(unique_suffix)) then
            suffix = trim(unique_suffix)
        else
            write(suffix, '("_", I0)') get_process_id()
        end if
        
        if (is_windows()) then
            ! Use Windows TEMP directory
            call get_environment_variable("TEMP", temp_dir, status=status)
            if (status == 0 .and. len_trim(temp_dir) > 0) then
                dir_path = trim(temp_dir) // "\" // TEST_OUTPUT_BASE // trim(suffix)
            else
                ! Fallback to local directory
                dir_path = TEST_OUTPUT_BASE // trim(suffix)
            end if
            dir_path = normalize_path_separators(dir_path, .true.)
        else
            ! Use /tmp on Unix-like systems
            dir_path = "/tmp/" // TEST_OUTPUT_BASE // trim(suffix)
        end if
    end function get_test_output_dir
    
    function get_process_id() result(pid)
        !! Get current process ID for unique directory naming
        integer :: pid
        character(len=32) :: pid_str
        integer :: status
        
        ! Try to get PID from environment or use default
        call get_environment_variable("PPID", pid_str, status=status)
        if (status == 0 .and. len_trim(pid_str) > 0) then
            read(pid_str, *, iostat=status) pid
            if (status /= 0) pid = 12345
        else
            ! Fallback: use simple timestamp-based ID
            call system_clock(pid)
            pid = abs(mod(pid, 99999)) + 1000
        end if
    end function get_process_id

    subroutine create_test_output_dir(success, unique_suffix)
        !! Create the test output directory if it doesn't exist
        logical, intent(out) :: success
        character(len=*), intent(in), optional :: unique_suffix
        character(len=:), allocatable :: dir_path
        
        dir_path = get_test_output_dir(unique_suffix)
        call create_directory_runtime(dir_path, success)
    end subroutine create_test_output_dir

    function get_test_output_path(filename, unique_suffix) result(full_path)
        !! Get the full path for a test output file
        character(len=*), intent(in) :: filename
        character(len=*), intent(in), optional :: unique_suffix
        character(len=:), allocatable :: full_path
        character(len=:), allocatable :: dir_path
        
        dir_path = get_test_output_dir(unique_suffix)
        
        if (is_windows()) then
            full_path = trim(dir_path) // "\" // trim(filename)
            full_path = normalize_path_separators(full_path, .true.)
        else
            full_path = trim(dir_path) // "/" // trim(filename)
        end if
    end function get_test_output_path

    subroutine cleanup_test_file(filename, success, unique_suffix)
        !! Delete a test output file
        character(len=*), intent(in) :: filename
        logical, intent(out) :: success
        character(len=*), intent(in), optional :: unique_suffix
        character(len=:), allocatable :: full_path
        
        full_path = get_test_output_path(filename, unique_suffix)
        call delete_file_runtime(full_path, success)
    end subroutine cleanup_test_file
    
    subroutine initialize_test_environment(test_name)
        !! Initialize test environment with unique directory
        character(len=*), intent(in) :: test_name
        logical :: success
        
        call global_test_wrapper%initialize(test_name)
        call create_test_output_dir(success, test_name)
        if (.not. success) then
            print *, "WARNING: Failed to create test directory for ", trim(test_name)
        end if
    end subroutine initialize_test_environment
    
    subroutine cleanup_test_environment()
        !! Clean up all registered test files and directory
        call global_test_wrapper%cleanup()
    end subroutine cleanup_test_environment
    
    subroutine setup_test_figure(fig, width, height, backend, filename)
        !! Set up a figure with automatic file registration
        type(figure_t), intent(out) :: fig
        integer, intent(in) :: width, height
        character(len=*), intent(in) :: backend
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: full_path
        
        full_path = global_test_wrapper%get_path(filename)
        call global_test_wrapper%register_file(filename)
        call fig%initialize(width, height, backend)
    end subroutine setup_test_figure
    
    subroutine cleanup_all_test_files(file_patterns)
        !! Clean up test files matching patterns
        character(len=*), intent(in), optional :: file_patterns(:)
        character(len=512), parameter :: default_patterns(5) = [ &
            "*.png    ", "*.pdf    ", "*.txt    ", "*.ppm    ", "*.gif    "]
        character(len=512) :: pattern
        integer :: i, unit_num, iostat
        logical :: exists, success
        character(len=256) :: filename
        
        ! Use default patterns if none provided
        if (present(file_patterns)) then
            do i = 1, size(file_patterns)
                pattern = trim(file_patterns(i))
                call cleanup_pattern(pattern)
            end do
        else
            do i = 1, size(default_patterns)
                pattern = trim(default_patterns(i))
                call cleanup_pattern(pattern)
            end do
        end if
    end subroutine cleanup_all_test_files
    
    subroutine cleanup_pattern(pattern)
        !! Clean up files matching a specific pattern
        character(len=*), intent(in) :: pattern
        ! Note: This is a simplified implementation
        ! In practice, would need system-specific directory traversal
    end subroutine cleanup_pattern
    
    subroutine register_test_file(filename)
        !! Register a file for cleanup
        character(len=*), intent(in) :: filename
        call global_test_wrapper%register_file(filename)
    end subroutine register_test_file
    
    ! Test wrapper type procedures
    subroutine test_wrapper_initialize(self, test_name)
        !! Initialize test wrapper
        class(test_wrapper_t), intent(inout) :: self
        character(len=*), intent(in) :: test_name
        
        self%test_dir = get_test_output_dir(test_name)
        self%file_count = 0
        if (.not. allocated(self%registered_files)) then
            allocate(self%registered_files(100))  ! Initial capacity
        end if
    end subroutine test_wrapper_initialize
    
    subroutine test_wrapper_register_file(self, filename)
        !! Register a file for cleanup
        class(test_wrapper_t), intent(inout) :: self
        character(len=*), intent(in) :: filename
        
        self%file_count = self%file_count + 1
        if (self%file_count <= size(self%registered_files)) then
            self%registered_files(self%file_count) = filename
        end if
    end subroutine test_wrapper_register_file
    
    subroutine test_wrapper_cleanup(self)
        !! Clean up all registered files
        class(test_wrapper_t), intent(inout) :: self
        integer :: i
        logical :: success
        character(len=:), allocatable :: full_path
        
        do i = 1, self%file_count
            full_path = trim(self%test_dir) // "/" // trim(self%registered_files(i))
            call delete_file_runtime(full_path, success)
        end do
        
        ! Try to remove the test directory (may fail if not empty, which is fine)
        if (allocated(self%test_dir)) then
            call system("rmdir "//trim(self%test_dir)//" 2>/dev/null || rm -rf "//trim(self%test_dir)//" 2>/dev/null || true")
        end if
        
        self%file_count = 0
    end subroutine test_wrapper_cleanup
    
    function test_wrapper_get_path(self, filename) result(full_path)
        !! Get full path for a file
        class(test_wrapper_t), intent(in) :: self
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: full_path
        
        if (is_windows()) then
            full_path = trim(self%test_dir) // "\" // trim(filename)
            full_path = normalize_path_separators(full_path, .true.)
        else
            full_path = trim(self%test_dir) // "/" // trim(filename)
        end if
    end function test_wrapper_get_path
    
    subroutine test_wrapper_setup_figure(self, fig, width, height, backend, filename)
        !! Set up a figure with path management
        class(test_wrapper_t), intent(inout) :: self
        type(figure_t), intent(out) :: fig
        integer, intent(in) :: width, height
        character(len=*), intent(in) :: backend
        character(len=*), intent(in) :: filename
        
        call self%register_file(filename)
        call fig%initialize(width, height, backend)
    end subroutine test_wrapper_setup_figure

end module fortplot_test_utilities
module fortplot_test_helpers
    !! Simple test helper functions to prevent file pollution
    !! Provides automatic cleanup and temporary directory management
    
    use iso_fortran_env, only: int32
    use fortplot_system_runtime, only: is_windows, create_directory_runtime, &
                                       delete_file_runtime, normalize_path_separators
    use fortplot, only: figure_t
    implicit none
    private
    
    public :: test_savefig
    public :: test_initialize_figure
    public :: test_cleanup_all
    public :: test_get_temp_path
    
    character(len=*), parameter :: TEMP_DIR_PREFIX = "fortplot_test_"
    character(len=512), save :: current_test_dir = ""
    logical, save :: test_dir_created = .false.
    
contains

    subroutine ensure_test_directory()
        !! Ensure test directory exists (create if needed)
        character(len=512) :: base_temp, pid_str
        integer :: status, pid_val
        logical :: success
        
        if (test_dir_created) return
        
        ! Get process-specific directory name
        call system_clock(pid_val)
        write(pid_str, '(I0)') abs(mod(pid_val, 99999))
        
        if (is_windows()) then
            call get_environment_variable("TEMP", base_temp, status=status)
            if (status /= 0 .or. len_trim(base_temp) == 0) then
                base_temp = "C:\temp"
            end if
            current_test_dir = trim(base_temp) // "\" // TEMP_DIR_PREFIX // trim(pid_str)
            current_test_dir = normalize_path_separators(current_test_dir, .true.)
        else
            current_test_dir = "/tmp/" // TEMP_DIR_PREFIX // trim(pid_str)
        end if
        
        call create_directory_runtime(current_test_dir, success)
        test_dir_created = success
        
        if (.not. success) then
            ! Fallback to current directory
            current_test_dir = TEMP_DIR_PREFIX // trim(pid_str)
            test_dir_created = .true.
        end if
    end subroutine ensure_test_directory
    
    function test_get_temp_path(filename) result(full_path)
        !! Get temporary file path for test output
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: full_path
        
        call ensure_test_directory()
        
        if (is_windows()) then
            full_path = trim(current_test_dir) // "\" // trim(filename)
            full_path = normalize_path_separators(full_path, .true.)
        else
            full_path = trim(current_test_dir) // "/" // trim(filename)
        end if
    end function test_get_temp_path
    
    subroutine test_initialize_figure(fig, width, height, backend)
        !! Initialize figure for testing (ensures temp directory exists)
        type(figure_t), intent(out) :: fig
        integer, intent(in) :: width, height
        character(len=*), intent(in) :: backend
        
        call ensure_test_directory()
        call fig%initialize(width, height, backend)
    end subroutine test_initialize_figure
    
    subroutine test_savefig(fig, filename)
        !! Save figure to temporary directory
        type(figure_t), intent(inout) :: fig
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: temp_path
        
        temp_path = test_get_temp_path(filename)
        call fig%savefig(temp_path)
    end subroutine test_savefig
    
    subroutine test_cleanup_all()
        !! Clean up all test files and directory
        logical :: success
        character(len=1024) :: command
        
        if (.not. test_dir_created .or. len_trim(current_test_dir) == 0) return
        
        ! Remove directory and all contents
        if (is_windows()) then
            write(command, '("rmdir /S /Q """, A, """ 2>NUL")') trim(current_test_dir)
        else
            write(command, '("rm -rf """, A, """ 2>/dev/null || true")') trim(current_test_dir)
        end if
        
        call system(command)
        test_dir_created = .false.
        current_test_dir = ""
    end subroutine test_cleanup_all

end module fortplot_test_helpers
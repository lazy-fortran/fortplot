module fortplot_windows_performance
    !! Windows CI Performance Optimization Module
    !!
    !! This module provides Windows-specific performance optimizations to address
    !! Issue #188: Slow test execution on Windows CI. It implements strategies
    !! to minimize file I/O bottlenecks and provide alternative high-performance
    !! backends for CI testing.
    
    use iso_fortran_env, only: int32, real64
    use iso_c_binding, only: c_char, c_int, c_null_char
    use fortplot_string_utils, only: parse_boolean_env
    implicit none
    private
    
    public :: get_fast_temp_dir
    public :: setup_windows_performance
    public :: is_ci_environment
    public :: should_use_memory_backend
    public :: get_performance_config
    
    ! Performance configuration type
    type, public :: performance_config_t
        logical :: use_memory_backend = .false.
        logical :: batch_file_operations = .false.
        logical :: minimize_io = .false.
        integer :: max_parallel_files = 1
        character(len=256) :: temp_dir = ""
    end type performance_config_t
    
    ! Module-level configuration
    type(performance_config_t), save :: global_config
    logical, save :: initialized = .false.
    
contains
    
    function get_fast_temp_dir() result(temp_dir)
        !! Get Windows performance-optimized temporary directory
        !! Priority order for Windows performance:
        !! 1. RAMDISK if available (fastest)
        !! 2. Local temp directory
        !! 3. System temp directory
        character(len=256) :: temp_dir
        character(len=256) :: env_value
        integer :: stat
        logical :: dir_exists
        
        ! First check for RAMDISK (some CI environments provide this)
        call get_environment_variable("RAMDISK", env_value, status=stat)
        if (stat == 0 .and. len_trim(env_value) > 0) then
            temp_dir = trim(env_value)
            inquire(file=trim(temp_dir), exist=dir_exists)
            if (dir_exists) return
        end if
        
        ! Check for local temp directory (usually faster than network drives)
        call get_environment_variable("LOCALAPPDATA", env_value, status=stat)
        if (stat == 0 .and. len_trim(env_value) > 0) then
            temp_dir = trim(env_value) // "\Temp"
            inquire(file=trim(temp_dir), exist=dir_exists)
            if (dir_exists) return
        end if
        
        ! Fall back to standard temp directory
        call get_environment_variable("TEMP", env_value, status=stat)
        if (stat == 0 .and. len_trim(env_value) > 0) then
            temp_dir = trim(env_value)
            return
        end if
        
        ! Last resort: current directory
        temp_dir = "."
        
    end function get_fast_temp_dir
    
    subroutine setup_windows_performance()
        !! Configure Windows-specific performance optimizations
        !! This should be called at the start of test suites on Windows CI
        
        character(len=256) :: env_value
        integer :: stat
        
        if (initialized) return
        
        ! Check if we're in CI environment
        global_config%use_memory_backend = is_ci_environment()
        
        ! Check for performance override flags
        call get_environment_variable("FORTPLOT_USE_MEMORY_BACKEND", env_value, status=stat)
        if (stat == 0 .and. len_trim(env_value) > 0) then
            if (parse_boolean_env(env_value)) global_config%use_memory_backend = .true.
        end if
        
        call get_environment_variable("FORTPLOT_BATCH_IO", env_value, status=stat)
        if (stat == 0 .and. len_trim(env_value) > 0) then
            if (parse_boolean_env(env_value)) global_config%batch_file_operations = .true.
        end if
        
        call get_environment_variable("FORTPLOT_MINIMIZE_IO", env_value, status=stat)
        if (stat == 0 .and. len_trim(env_value) > 0) then
            if (parse_boolean_env(env_value)) global_config%minimize_io = .true.
        end if
        
        ! Set optimal temp directory
        global_config%temp_dir = get_fast_temp_dir()
        
        ! Set parallel file limit based on CI environment
        if (is_ci_environment()) then
            global_config%max_parallel_files = 1  ! Sequential for CI stability
        else
            global_config%max_parallel_files = 4  ! Limited parallelism locally
        end if
        
        initialized = .true.
        
        ! Log configuration if in debug mode
        call get_environment_variable("FORTPLOT_DEBUG", env_value, status=stat)
        if (stat == 0 .and. len_trim(env_value) > 0 .and. parse_boolean_env(env_value)) then
            print *, "Windows Performance Configuration:"
            print *, "  Memory backend: ", global_config%use_memory_backend
            print *, "  Batch I/O: ", global_config%batch_file_operations
            print *, "  Minimize I/O: ", global_config%minimize_io
            print *, "  Temp directory: ", trim(global_config%temp_dir)
            print *, "  Max parallel files: ", global_config%max_parallel_files
        end if
        
    end subroutine setup_windows_performance
    
    function is_ci_environment() result(is_ci)
        !! Check if running in CI environment
        logical :: is_ci
        character(len=256) :: env_value
        integer :: stat
        
        ! Check common CI environment variables
        call get_environment_variable("CI", env_value, status=stat)
        if (stat == 0 .and. len_trim(env_value) > 0) then
            is_ci = .true.
            return
        end if
        
        call get_environment_variable("GITHUB_ACTIONS", env_value, status=stat)
        if (stat == 0 .and. len_trim(env_value) > 0) then
            is_ci = .true.
            return
        end if
        
        call get_environment_variable("APPVEYOR", env_value, status=stat)
        if (stat == 0 .and. len_trim(env_value) > 0) then
            is_ci = .true.
            return
        end if
        
        is_ci = .false.
        
    end function is_ci_environment
    
    function should_use_memory_backend() result(use_memory)
        !! Determine if memory backend should be used for current context
        logical :: use_memory
        
        if (.not. initialized) call setup_windows_performance()
        use_memory = global_config%use_memory_backend
        
    end function should_use_memory_backend
    
    function get_performance_config() result(config)
        !! Get current performance configuration
        type(performance_config_t) :: config
        
        if (.not. initialized) call setup_windows_performance()
        config = global_config
        
    end function get_performance_config
    
end module fortplot_windows_performance

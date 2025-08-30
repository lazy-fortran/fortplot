module fortplot_system_runtime
    !! Runtime OS detection and cross-platform system operations orchestrator
    !! 
    !! This module orchestrates system operations by delegating to
    !! specialized system modules for better security and maintainability.

    use fortplot_os_detection
    use fortplot_path_operations
    use fortplot_file_operations
    use fortplot_system_commands

    implicit none
    private

    ! Re-export all public interfaces
    public :: is_windows
    public :: create_directory_runtime
    public :: delete_file_runtime
    public :: open_with_default_app_runtime
    public :: check_command_available_runtime
    public :: map_unix_to_windows_path
    public :: normalize_path_separators

end module fortplot_system_runtime
!! Security module - unified interface to security functionality
!! This module provides a single interface to all security operations
module fortplot_security
    use fortplot_security_core
    use fortplot_security_environment
    implicit none
    
    ! Re-export all public interfaces from the split modules
    public :: safe_create_directory
    public :: safe_remove_file  
    public :: safe_check_program_available
    public :: safe_validate_mpeg_with_ffprobe
    public :: safe_launch_viewer
    public :: sanitize_filename
    public :: is_safe_path
    public :: get_test_output_path
    public :: is_imagemagick_environment_enabled

end module fortplot_security

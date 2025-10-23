!! Security module - unified interface to security functionality
!! This module provides a single interface to all security operations
module fortplot_security
    use fortplot_security_core
    implicit none

    ! Re-export all public interfaces from the core module
    public :: safe_create_directory
    public :: safe_remove_file
    public :: sanitize_filename
    public :: is_safe_path
    public :: get_test_output_path

end module fortplot_security

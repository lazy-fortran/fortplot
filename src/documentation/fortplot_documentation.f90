module fortplot_documentation
    !! Consolidated documentation generation module
    !! Re-exports functionality from specialized modules

    use fortplot_documentation_core, only: &
        PATH_MAX_LEN, FILENAME_MAX_LEN, LINE_MAX_LEN, &
        check_file_exists, get_file_extension, replace_extension, &
        copy_file_content, build_file_path, &
        build_readme_path, build_output_path, build_fortran_url, &
        build_python_path, build_local_fortran_path, &
        title_case, get_output_title, &
        get_fortran_filename, get_example_run_target

    use fortplot_documentation_processing, only: &
        get_example_count, get_example_dir, get_example_name, &
        process_example, MAX_EXAMPLES

    use fortplot_documentation_output, only: &
        write_generated_outputs, scan_directory_for_media, &
        MAX_MEDIA_FILES

    implicit none
    private

    ! Re-export constants
    public :: PATH_MAX_LEN, FILENAME_MAX_LEN, LINE_MAX_LEN
    public :: MAX_EXAMPLES, MAX_MEDIA_FILES

    ! Re-export core utilities
    public :: check_file_exists, get_file_extension, replace_extension
    public :: copy_file_content, build_file_path
    public :: build_readme_path, build_output_path, build_fortran_url
    public :: build_python_path, build_local_fortran_path
    public :: title_case, get_output_title
    public :: get_fortran_filename, get_example_run_target

    ! Re-export processing interface
    public :: get_example_count, get_example_dir, get_example_name
    public :: process_example

    ! Re-export output interface
    public :: write_generated_outputs
    public :: scan_directory_for_media

    ! Additional constants for backward compatibility
    integer, parameter :: VIDEO_WIDTH = 800
    integer, parameter :: VIDEO_HEIGHT = 600
    character(len=*), parameter :: GITHUB_BASE_URL = 'https://github.com/lazy-fortran/fortplot'

    public :: VIDEO_WIDTH, VIDEO_HEIGHT, GITHUB_BASE_URL

end module fortplot_documentation
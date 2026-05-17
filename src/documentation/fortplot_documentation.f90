module fortplot_documentation
    !! Consolidated documentation generation facade.
    !!
    !! Re-exports processing from fortplot_doc_processing and output
    !! generation from fortplot_doc_output. Core utilities remain here.

    use fortplot_directory_listing, only: list_directory_entries
    use fortplot_logging, only: log_warning
    use fortplot_doc_utils, only: &
        build_file_path, check_file_exists, file_exists, get_file_extension, &
        lowercase_string, replace_extension, title_case
    use fortplot_doc_constants, only: PATH_MAX_LEN, FILENAME_MAX_LEN, LINE_MAX_LEN, &
                                      MAX_EXAMPLES, MAX_MEDIA_FILES, &
                                      VIDEO_WIDTH, VIDEO_HEIGHT, &
                                      GITHUB_BASE_URL, OUTPUT_BASE_DIR
    use fortplot_doc_processing, only: get_example_count, get_example_dir, &
                                       get_example_name, process_example
    use fortplot_doc_output, only: write_generated_outputs, scan_directory_for_media

    implicit none
    private

    ! =====================
    ! Public constants
    ! =====================
    public :: PATH_MAX_LEN, FILENAME_MAX_LEN, LINE_MAX_LEN
    public :: MAX_EXAMPLES, MAX_MEDIA_FILES
    public :: VIDEO_WIDTH, VIDEO_HEIGHT
    public :: GITHUB_BASE_URL

    ! =====================
    ! Public utilities
    ! =====================
    public :: check_file_exists, get_file_extension, replace_extension
    public :: copy_file_content, build_file_path
    public :: build_readme_path, build_output_path, build_fortran_url
    public :: build_local_fortran_path
    public :: title_case, get_output_title
    public :: get_fortran_filename, get_example_run_target

    ! =====================
    ! Public processing interface
    ! =====================
    public :: get_example_count, get_example_dir, get_example_name
    public :: process_example

    ! =====================
    ! Public output interface
    ! =====================
    public :: write_generated_outputs
    public :: scan_directory_for_media

contains

    subroutine copy_file_content(input_file, output_file)
        character(len=*), intent(in) :: input_file, output_file
        integer :: input_unit, output_unit, ios
        character(len=LINE_MAX_LEN) :: line

        open(newunit=input_unit, file=input_file, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            call log_warning('Could not open input file: ' // trim(input_file))
            return
        end if

        open(newunit=output_unit, file=output_file, status='replace', action='write')

        do
            read(input_unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            write(output_unit, '(A)') trim(line)
        end do

        close(input_unit)
        close(output_unit)
    end subroutine copy_file_content

    pure subroutine build_readme_path(example_dir, readme_file)
        character(len=*), intent(in) :: example_dir
        character(len=PATH_MAX_LEN), intent(out) :: readme_file

        readme_file = trim(example_dir) // '/README.md'
    end subroutine build_readme_path

    pure subroutine build_output_path(example_name, output_file)
        character(len=*), intent(in) :: example_name
        character(len=PATH_MAX_LEN), intent(out) :: output_file

        output_file = OUTPUT_BASE_DIR // trim(example_name) // '/' // trim(example_name) // '.png'
    end subroutine build_output_path

    pure subroutine build_fortran_url(example_name, fortran_path)
        character(len=*), intent(in) :: example_name
        character(len=PATH_MAX_LEN), intent(out) :: fortran_path
        character(len=PATH_MAX_LEN) :: fortran_file

        call get_fortran_filename(example_name, fortran_file)
        fortran_path = GITHUB_BASE_URL // 'example/fortran/' // &
                      trim(example_name) // '/' // trim(fortran_file)
    end subroutine build_fortran_url

    pure subroutine build_local_fortran_path(example_name, local_path)
        character(len=*), intent(in) :: example_name
        character(len=PATH_MAX_LEN), intent(out) :: local_path

        local_path = 'example/' // trim(example_name) // '.f90'
    end subroutine build_local_fortran_path

    function get_output_title(filename) result(title)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: title
        character(len=:), allocatable :: base_name
        integer :: dot_pos, slash_pos

        slash_pos = index(filename, '/', back=.true.)
        if (slash_pos > 0) then
            base_name = filename(slash_pos+1:)
        else
            base_name = filename
        end if

        dot_pos = index(base_name, '.', back=.true.)
        if (dot_pos > 0) then
            base_name = base_name(1:dot_pos-1)
        end if

        title = title_case(base_name)
    end function get_output_title

    pure subroutine get_fortran_filename(example_name, filename)
        character(len=*), intent(in) :: example_name
        character(len=PATH_MAX_LEN), intent(out) :: filename

        select case(trim(example_name))
        case('animation')
            filename = 'save_animation_demo.f90'
        case default
            filename = trim(example_name) // '.f90'
        end select
    end subroutine get_fortran_filename

    pure subroutine get_example_run_target(example_name, run_target)
        character(len=*), intent(in) :: example_name
        character(len=PATH_MAX_LEN), intent(out) :: run_target

        select case(trim(example_name))
        case('animation')
            run_target = 'save_animation_demo'
        case default
            run_target = trim(example_name)
        end select
    end subroutine get_example_run_target

end module fortplot_documentation

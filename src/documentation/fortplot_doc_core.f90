module fortplot_doc_core
    !! Core utilities and constants for documentation generation
    implicit none
    private

    ! Public constants
    public :: PATH_MAX_LEN, FILENAME_MAX_LEN, LINE_MAX_LEN
    public :: MAX_EXAMPLES, MAX_MEDIA_FILES
    public :: VIDEO_WIDTH, VIDEO_HEIGHT
    public :: GITHUB_BASE_URL

    ! Public file/path helpers
    public :: check_file_exists, get_file_extension, replace_extension
    public :: copy_file_content, build_file_path
    public :: build_readme_path, build_output_path, build_fortran_url
    public :: build_python_path, build_local_fortran_path

    ! Public text helpers used across modules
    public :: title_case, get_output_title

    ! Public example filename helper (kept in core to avoid cycles)
    public :: get_fortran_filename

    ! String length constants
    integer, parameter :: PATH_MAX_LEN = 256
    integer, parameter :: FILENAME_MAX_LEN = 256
    integer, parameter :: LINE_MAX_LEN = 1024

    ! Array size constants
    integer, parameter :: MAX_EXAMPLES = 20
    integer, parameter :: MAX_MEDIA_FILES = 10

    ! Video dimensions
    integer, parameter :: VIDEO_WIDTH = 800
    integer, parameter :: VIDEO_HEIGHT = 600

    ! URL constants
    character(len=*), parameter :: GITHUB_BASE_URL = &
        'https://github.com/lazy-fortran/fortplot/blob/main/'

contains

    ! --------------------
    ! File helper routines
    ! --------------------

    logical function check_file_exists(dir, filename)
        character(len=*), intent(in) :: dir, filename
        character(len=PATH_MAX_LEN) :: full_path

        call build_file_path(dir, filename, full_path)
        inquire(file=trim(full_path), exist=check_file_exists)
    end function check_file_exists

    pure function get_file_extension(filename) result(ext)
        character(len=*), intent(in) :: filename
        character(len=10) :: ext
        integer :: dot_pos

        dot_pos = index(filename, '.', back=.true.)
        if (dot_pos > 0 .and. dot_pos < len_trim(filename)) then
            ext = filename(dot_pos+1:)
        else
            ext = ''
        end if
    end function get_file_extension

    pure function replace_extension(filename, new_ext) result(new_filename)
        character(len=*), intent(in) :: filename, new_ext
        character(len=FILENAME_MAX_LEN) :: new_filename
        integer :: dot_pos

        dot_pos = index(filename, '.', back=.true.)
        if (dot_pos > 0) then
            new_filename = filename(1:dot_pos) // new_ext
        else
            new_filename = trim(filename) // '.' // new_ext
        end if
    end function replace_extension

    pure subroutine build_file_path(dir, filename, full_path)
        character(len=*), intent(in) :: dir, filename
        character(len=PATH_MAX_LEN), intent(out) :: full_path

        full_path = trim(dir) // '/' // trim(filename)
    end subroutine build_file_path

    subroutine copy_file_content(source_file, target_unit)
        character(len=*), intent(in) :: source_file
        integer, intent(in) :: target_unit

        character(len=LINE_MAX_LEN) :: line
        integer :: unit_src, ios

        open(newunit=unit_src, file=trim(source_file), status='old', iostat=ios)
        if (ios == 0) then
            do
                read(unit_src, '(A)', iostat=ios) line
                if (ios /= 0) exit
                write(target_unit, '(A)') trim(line)
            end do
            close(unit_src)
        end if
    end subroutine copy_file_content

    ! --------------------
    ! Path helper routines
    ! --------------------

    pure subroutine build_readme_path(example_dir, readme_file)
        character(len=*), intent(in) :: example_dir
        character(len=PATH_MAX_LEN), intent(out) :: readme_file

        readme_file = trim(example_dir) // "/README.md"
    end subroutine build_readme_path

    pure subroutine build_output_path(example_name, output_file)
        character(len=*), intent(in) :: example_name
        character(len=PATH_MAX_LEN), intent(out) :: output_file

        ! Documentation consolidation - use canonical destination
        output_file = "example/fortran/" // trim(example_name) // "/README.md"
    end subroutine build_output_path

    pure subroutine build_fortran_url(example_name, fortran_path)
        character(len=*), intent(in) :: example_name
        character(len=PATH_MAX_LEN), intent(out) :: fortran_path

        character(len=PATH_MAX_LEN) :: fortran_file

        call get_fortran_filename(example_name, fortran_file)
        fortran_path = GITHUB_BASE_URL // 'example/fortran/' // &
                      trim(example_name) // '/' // trim(fortran_file)
    end subroutine build_fortran_url

    pure subroutine build_python_path(example_name, python_path)
        character(len=*), intent(in) :: example_name
        character(len=PATH_MAX_LEN), intent(out) :: python_path

        python_path = 'example/python/' // trim(example_name) // '/' // &
                     trim(example_name) // '.py'
    end subroutine build_python_path

    pure subroutine build_local_fortran_path(example_name, local_path)
        character(len=*), intent(in) :: example_name
        character(len=PATH_MAX_LEN), intent(out) :: local_path

        character(len=PATH_MAX_LEN) :: fortran_file

        call get_fortran_filename(example_name, fortran_file)
        local_path = 'example/fortran/' // trim(example_name) // '/' // trim(fortran_file)
    end subroutine build_local_fortran_path

    ! --------------------
    ! Text helper routines
    ! --------------------

    pure function title_case(name) result(title)
        character(len=*), intent(in) :: name
        character(len=FILENAME_MAX_LEN) :: title
        integer :: i

        title = name
        call replace_underscores_with_spaces(title)
        call capitalize_first_and_after_spaces(title)
    end function title_case

    pure subroutine replace_underscores_with_spaces(text)
        character(len=*), intent(inout) :: text
        integer :: i

        do i = 1, len_trim(text)
            if (text(i:i) == '_') text(i:i) = ' '
        end do
    end subroutine replace_underscores_with_spaces

    pure subroutine capitalize_first_and_after_spaces(text)
        character(len=*), intent(inout) :: text
        integer :: i

        if (len_trim(text) > 0) then
            call capitalize_if_lowercase(text(1:1))

            do i = 2, len_trim(text)
                if (text(i-1:i-1) == ' ') then
                    call capitalize_if_lowercase(text(i:i))
                end if
            end do
        end if
    end subroutine capitalize_first_and_after_spaces

    pure subroutine capitalize_if_lowercase(char)
        character(len=1), intent(inout) :: char
        if (char >= 'a' .and. char <= 'z') then
            char = achar(ichar(char) - 32)
        end if
    end subroutine capitalize_if_lowercase

    pure function get_output_title(filename) result(title)
        character(len=*), intent(in) :: filename
        character(len=FILENAME_MAX_LEN) :: title
        character(len=FILENAME_MAX_LEN) :: base
        integer :: dot_pos

        dot_pos = index(filename, '.', back=.true.)
        if (dot_pos > 0) then
            base = filename(1:dot_pos-1)
        else
            base = filename
        end if

        title = title_case(base)
    end function get_output_title

    ! --------------------
    ! Example helper
    ! --------------------

    pure subroutine get_fortran_filename(example_name, filename)
        character(len=*), intent(in) :: example_name
        character(len=PATH_MAX_LEN), intent(out) :: filename

        select case(trim(example_name))
        case('animation')
            filename = 'save_animation_demo.f90'
        case('ascii_heatmap')
            filename = 'ascii_heatmap_demo.f90'
        case default
            filename = trim(example_name) // '.f90'
        end select
    end subroutine get_fortran_filename

end module fortplot_doc_core


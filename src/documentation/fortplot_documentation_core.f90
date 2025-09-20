module fortplot_documentation_core
    !! Core utility functions for documentation generation
    !! Contains file handling, path management, and string operations

    implicit none
    private

    public :: PATH_MAX_LEN, FILENAME_MAX_LEN, LINE_MAX_LEN
    public :: check_file_exists, get_file_extension, replace_extension
    public :: copy_file_content, build_file_path
    public :: build_readme_path, build_output_path, build_fortran_url
    public :: build_python_path, build_local_fortran_path
    public :: title_case, get_output_title
    public :: get_fortran_filename, get_example_run_target

    integer, parameter :: PATH_MAX_LEN = 256
    integer, parameter :: FILENAME_MAX_LEN = 256
    integer, parameter :: LINE_MAX_LEN = 1024

contains

    logical function check_file_exists(dir, filename)
        character(len=*), intent(in) :: dir, filename
        character(len=PATH_MAX_LEN) :: full_path
        logical :: exists

        full_path = trim(adjustl(dir)) // '/' // trim(adjustl(filename))
        inquire(file=full_path, exist=exists)
        check_file_exists = exists
    end function check_file_exists

    function get_file_extension(filename) result(extension)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: extension
        integer :: dot_pos

        dot_pos = index(filename, '.', back=.true.)
        if (dot_pos > 0) then
            extension = filename(dot_pos+1:)
        else
            extension = ''
        end if
    end function get_file_extension

    function replace_extension(filename, new_ext) result(new_filename)
        character(len=*), intent(in) :: filename, new_ext
        character(len=:), allocatable :: new_filename
        integer :: dot_pos

        dot_pos = index(filename, '.', back=.true.)
        if (dot_pos > 0) then
            new_filename = filename(1:dot_pos) // trim(new_ext)
        else
            new_filename = trim(filename) // '.' // trim(new_ext)
        end if
    end function replace_extension

    subroutine copy_file_content(input_file, output_file)
        character(len=*), intent(in) :: input_file, output_file
        integer :: input_unit, output_unit, ios
        character(len=LINE_MAX_LEN) :: line

        open(newunit=input_unit, file=input_file, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, 'Warning: Could not open input file: ', trim(input_file)
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

    pure subroutine build_file_path(dir, filename, full_path)
        character(len=*), intent(in) :: dir, filename
        character(len=*), intent(out) :: full_path
        full_path = trim(adjustl(dir)) // '/' // trim(adjustl(filename))
    end subroutine build_file_path

    function build_readme_path(example_dir) result(readme_path)
        character(len=*), intent(in) :: example_dir
        character(len=:), allocatable :: readme_path
        readme_path = trim(example_dir) // '/README.md'
    end function build_readme_path

    function build_output_path(example_dir, fortran_name) result(output_path)
        character(len=*), intent(in) :: example_dir, fortran_name
        character(len=:), allocatable :: output_path
        output_path = trim(example_dir) // '/' // trim(fortran_name)
    end function build_output_path

    function build_fortran_url(example_name, fortran_name) result(url)
        character(len=*), intent(in) :: example_name, fortran_name
        character(len=:), allocatable :: url
        url = 'https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/' &
            // trim(example_name) // '/' // trim(fortran_name)
    end function build_fortran_url

    function build_python_path(example_dir) result(python_path)
        character(len=*), intent(in) :: example_dir
        character(len=:), allocatable :: python_path
        python_path = replace_extension(trim(example_dir), 'py')
    end function build_python_path

    function build_local_fortran_path(example_dir) result(fortran_path)
        character(len=*), intent(in) :: example_dir
        character(len=:), allocatable :: fortran_path
        character(len=:), allocatable :: basename
        integer :: slash_pos

        slash_pos = index(example_dir, '/', back=.true.)
        if (slash_pos > 0) then
            basename = example_dir(slash_pos+1:)
        else
            basename = example_dir
        end if
        fortran_path = trim(basename) // '.f90'
    end function build_local_fortran_path

    function title_case(input) result(output)
        character(len=*), intent(in) :: input
        character(len=:), allocatable :: output
        integer :: i
        logical :: capitalize_next

        output = input
        capitalize_next = .true.

        do i = 1, len(output)
            if (output(i:i) == '_' .or. output(i:i) == ' ') then
                output(i:i) = ' '
                capitalize_next = .true.
            else if (capitalize_next) then
                output(i:i) = char(iachar(output(i:i)) - iachar('a') + iachar('A'))
                capitalize_next = .false.
            end if
        end do
    end function title_case

    function get_output_title(example_name) result(title)
        character(len=*), intent(in) :: example_name
        character(len=:), allocatable :: title
        title = title_case(example_name)
    end function get_output_title

    function get_fortran_filename(example_name) result(filename)
        character(len=*), intent(in) :: example_name
        character(len=:), allocatable :: filename
        filename = trim(example_name) // '.f90'
    end function get_fortran_filename

    function get_example_run_target(example_name) result(target)
        character(len=*), intent(in) :: example_name
        character(len=:), allocatable :: target
        target = 'example/' // trim(example_name)
    end function get_example_run_target

end module fortplot_documentation_core
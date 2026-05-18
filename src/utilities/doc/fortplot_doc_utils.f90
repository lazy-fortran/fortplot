module fortplot_doc_utils
    use fortplot_directory_listing, only: list_directory_entries
    implicit none
    private

    public :: build_file_path
    public :: get_file_extension
    public :: replace_extension
    public :: title_case
    public :: lowercase_string
    public :: file_exists
    public :: check_file_exists
    public :: build_readme_path
    public :: build_output_path
    public :: build_fortran_url
    public :: build_local_fortran_path
    public :: get_output_title
    public :: get_fortran_filename
    public :: get_example_run_target

contains

    pure subroutine build_file_path(dir, filename, full_path)
        character(len=*), intent(in) :: dir, filename
        character(len=*), intent(out) :: full_path

        full_path = trim(adjustl(dir)) // '/' // trim(adjustl(filename))
    end subroutine build_file_path

    pure function get_file_extension(filename) result(extension)
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

    pure function replace_extension(filename, new_ext) result(new_filename)
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

    pure function title_case(value) result(output)
        character(len=*), intent(in) :: value
        character(len=:), allocatable :: output
        character(len=1) :: ch
        integer :: i
        logical :: new_word

        output = ''
        new_word = .true.
        do i = 1, len_trim(value)
            ch = value(i:i)
            select case (ch)
            case ('_', '-', ' ')
                if (len(output) > 0) then
                    if (output(len(output):len(output)) /= ' ') then
                        output = output // ' '
                    end if
                end if
                new_word = .true.
            case default
                if (new_word) then
                    output = output // char_upper(ch)
                    new_word = .false.
                else if (len(output) > 0) then
                    if (is_digit(output(len(output):len(output))) .and. &
                        is_alpha(ch)) then
                        output = output // char_upper(ch)
                    else
                        output = output // char_lower(ch)
                    end if
                else
                    output = output // char_lower(ch)
                end if
            end select
        end do

        if (len(output) > 0) then
            if (output(len(output):len(output)) == ' ') then
                output = output(1:len(output)-1)
            end if
        end if
    end function title_case

    pure function lowercase_string(value) result(output)
        character(len=*), intent(in) :: value
        character(len=:), allocatable :: output
        integer :: i

        output = value
        do i = 1, len(output)
            output(i:i) = char_lower(output(i:i))
        end do
    end function lowercase_string

    logical function file_exists(path)
        character(len=*), intent(in) :: path

        inquire(file=trim(path), exist=file_exists)
    end function file_exists

    logical function check_file_exists(dir, filename)
        character(len=*), intent(in) :: dir, filename
        character(len=:), allocatable :: full_path

        full_path = trim(adjustl(dir)) // '/' // trim(adjustl(filename))
        check_file_exists = file_exists(full_path)
    end function check_file_exists

    pure function char_lower(ch) result(out)
        character(len=1), intent(in) :: ch
        character(len=1) :: out
        integer :: code

        code = iachar(ch)
        if (code >= iachar('A') .and. code <= iachar('Z')) then
            out = achar(code + 32)
        else
            out = ch
        end if
    end function char_lower

    pure function char_upper(ch) result(out)
        character(len=1), intent(in) :: ch
        character(len=1) :: out
        integer :: code

        code = iachar(ch)
        if (code >= iachar('a') .and. code <= iachar('z')) then
            out = achar(code - 32)
        else
            out = ch
        end if
    end function char_upper

    pure logical function is_digit(ch)
        character(len=1), intent(in) :: ch
        integer :: code

        code = iachar(ch)
        is_digit = code >= iachar('0') .and. code <= iachar('9')
    end function is_digit

    pure logical function is_alpha(ch)
        character(len=1), intent(in) :: ch
        integer :: code

        code = iachar(char_lower(ch))
        is_alpha = code >= iachar('a') .and. code <= iachar('z')
    end function is_alpha

    subroutine build_readme_path(base_dir, readme_path)
        character(len=*), intent(in) :: base_dir
        character(len=*), intent(out) :: readme_path

        readme_path = trim(adjustl(base_dir)) // '/README.md'
    end subroutine build_readme_path

    subroutine build_output_path(base_dir, output_path)
        character(len=*), intent(in) :: base_dir
        character(len=*), intent(out) :: output_path

        output_path = 'doc/examples/' // trim(adjustl(base_dir)) // '.md'
    end subroutine build_output_path

    subroutine build_fortran_url(example_name, fortran_url)
        character(len=*), intent(in) :: example_name
        character(len=*), intent(out) :: fortran_url
        character(len=256) :: fortran_filename

        fortran_url = &
            'https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/' // &
            trim(adjustl(example_name)) // '/'
        call get_fortran_filename(example_name, fortran_filename)
        fortran_url = trim(fortran_url) // trim(fortran_filename)
    end subroutine build_fortran_url

    subroutine build_local_fortran_path(example_name, fortran_path)
        character(len=*), intent(in) :: example_name
        character(len=*), intent(out) :: fortran_path
        character(len=256) :: fortran_filename

        call get_fortran_filename(example_name, fortran_filename)
        fortran_path = 'example/fortran/' // trim(adjustl(example_name)) // &
                       '/' // trim(fortran_filename)
    end subroutine build_local_fortran_path

    function get_output_title(example_name) result(title)
        character(len=*), intent(in) :: example_name
        character(len=:), allocatable :: title

        title = title_case(example_name)
    end function get_output_title

    subroutine get_fortran_filename(example_name, fortran_filename)
        character(len=*), intent(in) :: example_name
        character(len=*), intent(out) :: fortran_filename

        call resolve_fortran_filename(example_name, fortran_filename)
    end subroutine get_fortran_filename

    subroutine get_example_run_target(example_name, run_target)
        character(len=*), intent(in) :: example_name
        character(len=*), intent(out) :: run_target
        character(len=256) :: fortran_filename
        integer :: dot_pos

        call get_fortran_filename(example_name, fortran_filename)
        dot_pos = index(trim(fortran_filename), '.', back=.true.)
        if (dot_pos > 1) then
            run_target = fortran_filename(1:dot_pos - 1)
        else
            run_target = trim(fortran_filename)
        end if
    end subroutine get_example_run_target

    subroutine resolve_fortran_filename(example_name, fortran_filename)
        character(len=*), intent(in) :: example_name
        character(len=*), intent(out) :: fortran_filename
        character(len=256) :: entries(64)
        character(len=256) :: candidate
        character(len=512) :: example_dir, path
        integer :: count, status, i, matches

        example_dir = 'example/fortran/' // trim(adjustl(example_name))

        candidate = trim(adjustl(example_name)) // '.f90'
        path = trim(example_dir) // '/' // trim(candidate)
        if (file_exists(path)) then
            fortran_filename = trim(candidate)
            return
        end if

        candidate = 'example.f90'
        path = trim(example_dir) // '/' // trim(candidate)
        if (file_exists(path)) then
            fortran_filename = trim(candidate)
            return
        end if

        call list_directory_entries(trim(example_dir), entries, count, status)
        matches = 0
        candidate = ''
        if (status == 0) then
            do i = 1, count
                if (trim(get_file_extension(trim(entries(i)))) /= 'f90') cycle
                matches = matches + 1
                candidate = trim(entries(i))
            end do
        end if

        if (matches == 1) then
            fortran_filename = trim(candidate)
        else
            fortran_filename = trim(adjustl(example_name)) // '.f90'
        end if
    end subroutine resolve_fortran_filename

end module fortplot_doc_utils

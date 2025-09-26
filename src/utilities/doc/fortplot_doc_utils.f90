module fortplot_doc_utils
    implicit none
    private

    public :: build_file_path
    public :: get_file_extension
    public :: replace_extension
    public :: title_case
    public :: lowercase_string
    public :: file_exists
    public :: check_file_exists

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

end module fortplot_doc_utils

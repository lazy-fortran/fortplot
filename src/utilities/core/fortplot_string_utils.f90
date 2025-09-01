module fortplot_string_utils
    !! Core string utilities with minimal dependencies
    implicit none
    private
    public :: to_lowercase, parse_boolean_env

contains

    function to_lowercase(input) result(output)
        !! Convert string to lowercase (ASCII A-Z)
        character(len=*), intent(in) :: input
        character(len=len(input)) :: output
        integer :: i, c
        do i = 1, len(input)
            c = iachar(input(i:i))
            if (c >= 65 .and. c <= 90) then
                output(i:i) = achar(c + 32)
            else
                output(i:i) = input(i:i)
            end if
        end do
    end function to_lowercase

    logical function parse_boolean_env(env_value) result(val)
        !! Case-insensitive parse for common boolean-ish strings.
        character(len=*), intent(in) :: env_value
        character(len=len_trim(env_value)) :: lower
        val = .false.
        if (len_trim(env_value) == 0) return
        lower = to_lowercase(trim(env_value))
        if (lower == '1' .or. lower == 'true' .or. lower == 'yes' .or. lower == 'on') then
            val = .true.
        else if (lower == '0' .or. lower == 'false' .or. lower == 'no' .or. lower == 'off') then
            val = .false.
        end if
    end function parse_boolean_env

end module fortplot_string_utils


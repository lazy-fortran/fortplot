program test_boolean_env_parsing
    !! Focused test for parse_boolean_env utility
    use fortplot_string_utils, only: parse_boolean_env
    implicit none

    call ensure(parse_boolean_env('1'), '1 => true')
    call ensure(parse_boolean_env('true'), 'true => true')
    call ensure(parse_boolean_env('TRUE'), 'TRUE => true')
    call ensure(parse_boolean_env('Yes'), 'Yes => true')
    call ensure(parse_boolean_env('on'), 'on => true')

    call ensure(.not. parse_boolean_env('0'), '0 => false')
    call ensure(.not. parse_boolean_env('false'), 'false => false')
    call ensure(.not. parse_boolean_env('FALSE'), 'FALSE => false')
    call ensure(.not. parse_boolean_env('No'), 'No => false')
    call ensure(.not. parse_boolean_env('off'), 'off => false')

    call ensure(.not. parse_boolean_env('random'), 'random => false')
    call ensure(.not. parse_boolean_env(''), 'empty => false')

contains

    subroutine ensure(cond, msg)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: msg
        if (.not. cond) then
            print *, 'FAIL: ', trim(msg)
            stop 1
        else
            print *, 'PASS: ', trim(msg)
        end if
    end subroutine ensure

end program test_boolean_env_parsing

program test_spec_unicode
    use fortplot, only: spec_t, json_to_spec
    implicit none
    type(spec_t) :: spec
    integer :: status, i
    character(len=32), parameter :: invalid(*) = [character(len=32) :: &
        '\u123', '\uZZZZ', '\uD800', '\uDC00', '\uD800\u0041', '\q']
    call json_to_spec('{"title":"r\u00b2 |R(r)|\u00b2 and a\u2080"}', spec, status)
    if (status /= 0) error stop 'Cannot decode escaped scientific labels'
    if (spec%title /= 'r² |R(r)|² and a₀') error stop 'Unicode label bytes differ'
    call json_to_spec('{"title":"\uD83D\uDE00"}', spec, status)
    if (status /= 0) error stop 'Cannot decode surrogate pair'
    if (spec%title /= achar(240)//achar(159)//achar(152)//achar(128)) &
        error stop 'Surrogate pair UTF-8 bytes differ'
    call json_to_spec('{"data":{"values":[{"s":"\u03b1"}]}}', spec, status)
    if (status /= 0) error stop 'Cannot decode Unicode data strings'
    if (trim(spec%data%columns(1)%string_values(1)) /= 'α') &
        error stop 'Unicode data string bytes differ'
    do i = 1, size(invalid)
        call json_to_spec('{"title":"'//trim(invalid(i))//'"}', spec, status)
        if (status == 0) error stop 'Malformed escape accepted'
    end do
    call json_to_spec('{"title":"a'//char(1)//'b"}', spec, status)
    if (status == 0) error stop 'Unescaped control character accepted'
    print *, 'PASS: Unicode JSON escapes, surrogate pairs and malformed strings'
end program test_spec_unicode

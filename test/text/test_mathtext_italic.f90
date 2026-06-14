program test_mathtext_italic
    !! Math-mode runs (inside '$...$') must be tagged italic so backends apply
    !! the synthetic oblique shear, matching matplotlib's italic math variables.
    !! Text outside the dollars stays upright.
    use fortplot_text_layout, only: preprocess_math_text
    use fortplot_mathtext, only: parse_mathtext, mathtext_element_t
    implicit none

    type(mathtext_element_t), allocatable :: elements(:)
    character(len=256) :: processed
    integer :: plen, i
    logical :: saw_upright_prefix, saw_italic_base, saw_italic_super

    call preprocess_math_text('f(x) = $x^2$', processed, plen)
    elements = parse_mathtext(processed(1:plen))

    saw_upright_prefix = .false.
    saw_italic_base = .false.
    saw_italic_super = .false.

    do i = 1, size(elements)
        if (trim(elements(i)%text) == 'f(x) = ' .or. &
            trim(elements(i)%text) == 'f(x) =') then
            if (elements(i)%italic) error stop "non-math prefix must stay upright"
            saw_upright_prefix = .true.
        end if
        if (trim(elements(i)%text) == 'x' .and. elements(i)%element_type == 0) then
            if (.not. elements(i)%italic) error stop "math base 'x' must be italic"
            saw_italic_base = .true.
        end if
        if (trim(elements(i)%text) == '2' .and. elements(i)%element_type == 1) then
            if (.not. elements(i)%italic) error stop "math superscript must be italic"
            saw_italic_super = .true.
        end if
    end do

    if (.not. saw_upright_prefix) error stop "upright prefix element missing"
    if (.not. saw_italic_base) error stop "italic math base element missing"
    if (.not. saw_italic_super) error stop "italic superscript element missing"

    ! Bare text without dollars is never italic.
    call preprocess_math_text('plain x text', processed, plen)
    elements = parse_mathtext(processed(1:plen))
    do i = 1, size(elements)
        if (elements(i)%italic) error stop "text without dollars must stay upright"
    end do

    print *, 'PASS: math-mode runs tagged italic, surrounding text upright'
end program test_mathtext_italic

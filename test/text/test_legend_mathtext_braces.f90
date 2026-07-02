program test_legend_mathtext_braces
    !! Issue #2063: legend labels and titles with braced superscripts/subscripts
    !! must render as scripts (raised/lowered content) with the grouping braces
    !! hidden, instead of leaking literal '{' and '}' glyphs. This exercises the
    !! shared latex -> mathtext pipeline that both the raster and PDF backends run
    !! for legend labels (process_latex_in_text -> preprocess_math_text ->
    !! parse_mathtext).

    use fortplot_text_layout, only: has_mathtext, preprocess_math_text
    use fortplot_latex_parser, only: process_latex_in_text
    use fortplot_mathtext, only: parse_mathtext, mathtext_element_t, &
                                 ELEMENT_NORMAL, ELEMENT_SUPERSCRIPT, &
                                 ELEMENT_SUBSCRIPT
    implicit none

    character(len=*), parameter :: s2 = achar(194)//achar(178)  ! superscript two
    character(len=*), parameter :: rho = achar(207)//achar(129)
    character(len=*), parameter :: xi = achar(206)//achar(190)
    character(len=*), parameter :: sigma = achar(207)//achar(131)
    integer :: fail_count

    fail_count = 0

    ! Positive: bare braced exponent renders as a superscript, braces hidden.
    call check_superscript_no_braces('e^{-x/4}', '-x/4', fail_count)

    ! Positive: Greek symbols resolve and the braced exponent stays a script.
    call check_pipeline_no_braces( &
        'Gaussian: \rho(\xi) = e^{-\xi'//s2//'/2\sigma'//s2//'}', fail_count)
    call assert_super_content_present( &
        'Gaussian: \rho(\xi) = e^{-\xi'//s2//'/2\sigma'//s2//'}', &
        '-'//xi//s2//'/2'//sigma//s2, fail_count)

    ! Positive: second example label from unicode_demo.
    call check_pipeline_no_braces('Modified \Gamma: f(\xi) = \xi'//s2// &
                                  ' e^{-\xi}', fail_count)
    call assert_super_content_present('\xi'//s2//' e^{-\xi}', '-'//xi, &
                                      fail_count)

    ! Positive: braced subscript renders as a subscript with braces hidden.
    call check_subscript_no_braces('A_{n}', 'n', fail_count)

    ! Detection: braced scripts are math even without '$...$' delimiters.
    call assert_true(has_mathtext('e^{-x/4}'), &
                     'braced superscript must be math', fail_count)
    call assert_true(has_mathtext('A_{n}'), &
                     'braced subscript must be math', fail_count)

    ! Negative: bare unbraced markup stays literal, not math (no over-broadening).
    call assert_false(has_mathtext('caret^x'), &
                      'bare unbraced caret is literal', fail_count)
    call assert_false(has_mathtext('rate_i value'), &
                      'bare unbraced subscript is literal', fail_count)

    if (fail_count > 0) then
        print *, 'FAIL:', fail_count, 'assertion(s) failed'
        stop 1
    end if

    print *, 'PASS: legend braced scripts render without literal braces'

contains

    subroutine build_elements(label, elements)
        character(len=*), intent(in) :: label
        type(mathtext_element_t), allocatable, intent(out) :: elements(:)
        character(len=512) :: processed, math_ready
        integer :: plen, mlen

        call process_latex_in_text(label, processed, plen)
        call preprocess_math_text(processed(1:plen), math_ready, mlen)
        elements = parse_mathtext(math_ready(1:mlen))
    end subroutine build_elements

    subroutine check_pipeline_no_braces(label, fails)
        character(len=*), intent(in) :: label
        integer, intent(inout) :: fails
        type(mathtext_element_t), allocatable :: elements(:)
        integer :: i

        call build_elements(label, elements)
        if (.not. allocated(elements)) then
            fails = fails + 1
            print *, "FAIL: '"//label//"' produced no elements"
            return
        end if
        do i = 1, size(elements)
            if (index(elements(i)%text, '{') /= 0 .or. &
                index(elements(i)%text, '}') /= 0) then
                fails = fails + 1
                print *, "FAIL: '"//label//"' leaked a literal brace in '"// &
                    trim(elements(i)%text)//"'"
            end if
        end do
    end subroutine check_pipeline_no_braces

    subroutine check_superscript_no_braces(label, expected_super, fails)
        character(len=*), intent(in) :: label, expected_super
        integer, intent(inout) :: fails
        type(mathtext_element_t), allocatable :: elements(:)
        logical :: found
        integer :: i

        call check_pipeline_no_braces(label, fails)
        call build_elements(label, elements)
        found = .false.
        do i = 1, size(elements)
            if (elements(i)%element_type == ELEMENT_SUPERSCRIPT) then
                if (trim(elements(i)%text) == expected_super) found = .true.
            end if
        end do
        if (.not. found) then
            fails = fails + 1
            print *, "FAIL: '"//label//"' missing superscript '"// &
                expected_super//"'"
        end if
    end subroutine check_superscript_no_braces

    subroutine check_subscript_no_braces(label, expected_sub, fails)
        character(len=*), intent(in) :: label, expected_sub
        integer, intent(inout) :: fails
        type(mathtext_element_t), allocatable :: elements(:)
        logical :: found
        integer :: i

        call check_pipeline_no_braces(label, fails)
        call build_elements(label, elements)
        found = .false.
        do i = 1, size(elements)
            if (elements(i)%element_type == ELEMENT_SUBSCRIPT) then
                if (trim(elements(i)%text) == expected_sub) found = .true.
            end if
        end do
        if (.not. found) then
            fails = fails + 1
            print *, "FAIL: '"//label//"' missing subscript '"// &
                expected_sub//"'"
        end if
    end subroutine check_subscript_no_braces

    subroutine assert_super_content_present(label, expected_super, fails)
        character(len=*), intent(in) :: label, expected_super
        integer, intent(inout) :: fails
        type(mathtext_element_t), allocatable :: elements(:)
        logical :: found
        integer :: i

        call build_elements(label, elements)
        found = .false.
        do i = 1, size(elements)
            if (elements(i)%element_type == ELEMENT_SUPERSCRIPT) then
                if (trim(elements(i)%text) == expected_super) found = .true.
            end if
        end do
        if (.not. found) then
            fails = fails + 1
            print *, "FAIL: '"//label//"' missing resolved superscript '"// &
                expected_super//"'"
        end if
    end subroutine assert_super_content_present

    subroutine assert_true(cond, msg, fails)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: msg
        integer, intent(inout) :: fails

        if (.not. cond) then
            fails = fails + 1
            print *, 'FAIL: '//msg
        end if
    end subroutine assert_true

    subroutine assert_false(cond, msg, fails)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: msg
        integer, intent(inout) :: fails

        if (cond) then
            fails = fails + 1
            print *, 'FAIL: '//msg
        end if
    end subroutine assert_false

end program test_legend_mathtext_braces

program test_mathtext_parsing
    !! Unit tests for mathematical text parsing functionality
    use fortplot_mathtext, only: parse_mathtext, mathtext_element_t
    implicit none

    type(mathtext_element_t), allocatable :: elements(:)
    logical :: test_passed

    print *, 'Testing mathematical text parsing...'

    ! Test 1: Simple superscript
    call test_simple_superscript(test_passed)
    if (test_passed) then
        print *, 'PASS: Simple superscript parsing (x^2)'
    else
        print *, 'FAIL: Simple superscript parsing (x^2)'
        stop 1
    end if

    ! Test 2: Simple subscript
    call test_simple_subscript(test_passed)
    if (test_passed) then
        print *, 'PASS: Simple subscript parsing (x_i)'
    else
        print *, 'FAIL: Simple subscript parsing (x_i)'
        stop 1
    end if

    ! Test 3: Multi-character superscript with braces
    call test_multichar_superscript(test_passed)
    if (test_passed) then
        print *, 'PASS: Multi-character superscript parsing (x^{abc})'
    else
        print *, 'FAIL: Multi-character superscript parsing (x^{abc})'
        stop 1
    end if

    ! Test 4: Mixed normal and mathematical text
    call test_mixed_text(test_passed)
    if (test_passed) then
        print *, 'PASS: Mixed text parsing (Hello x^2 world)'
    else
        print *, 'FAIL: Mixed text parsing (Hello x^2 world)'
        stop 1
    end if

    ! Test 5: Combined superscript and subscript
    call test_combined_super_sub(test_passed)
    if (test_passed) then
        print *, 'PASS: Combined super/subscript parsing (x_i^j)'
    else
        print *, 'FAIL: Combined super/subscript parsing (x_i^j)'
        stop 1
    end if

    ! Test 6: Square root
    call test_sqrt(test_passed)
    if (test_passed) then
        print *, 'PASS: Square root parsing (\sqrt{x})'
    else
        print *, 'FAIL: Square root parsing (\sqrt{x})'
        stop 1
    end if

    print *, 'All mathematical text parsing tests passed!'

contains

    subroutine test_simple_superscript(passed)
        logical, intent(out) :: passed

        elements = parse_mathtext('x^2')

        passed = .false.
        if (size(elements) == 2) then
            if (trim(elements(1)%text) == 'x' .and. elements(1)%element_type == 0) then
                if (trim(elements(2)%text) == '2' .and. elements(2)%element_type == 1) then
                    if (abs(elements(2)%font_size_ratio - 0.7) < 0.01) then
                        passed = .true.
                    end if
                end if
            end if
        end if
    end subroutine test_simple_superscript

    subroutine test_simple_subscript(passed)
        logical, intent(out) :: passed

        elements = parse_mathtext('x_i')

        passed = .false.
        if (size(elements) == 2) then
            if (trim(elements(1)%text) == 'x' .and. elements(1)%element_type == 0) then
                if (trim(elements(2)%text) == 'i' .and. elements(2)%element_type == 2) then
                    if (abs(elements(2)%font_size_ratio - 0.7) < 0.01) then
                        passed = .true.
                    end if
                end if
            end if
        end if
    end subroutine test_simple_subscript

    subroutine test_multichar_superscript(passed)
        logical, intent(out) :: passed

        elements = parse_mathtext('x^{abc}')

        passed = .false.
        if (size(elements) == 2) then
            if (trim(elements(1)%text) == 'x' .and. elements(1)%element_type == 0) then
                if (trim(elements(2)%text) == 'abc' .and. elements(2)%element_type == 1) then
                    if (abs(elements(2)%font_size_ratio - 0.7) < 0.01) then
                        passed = .true.
                    end if
                end if
            end if
        end if
    end subroutine test_multichar_superscript

    subroutine test_mixed_text(passed)
        logical, intent(out) :: passed

        elements = parse_mathtext('Hello x^2 world')

        passed = .false.
        if (size(elements) == 4) then
            if (elements(1)%text == 'Hello ' .and. elements(1)%element_type == 0) then
                if (elements(2)%text == 'x' .and. elements(2)%element_type == 0) then
                    if (elements(3)%text == '2' .and. elements(3)%element_type == 1) then
                        if (elements(4)%text == ' world' .and. elements(4)%element_type == 0) then
                            passed = .true.
                        end if
                    end if
                end if
            end if
        end if
    end subroutine test_mixed_text

    subroutine test_combined_super_sub(passed)
        logical, intent(out) :: passed

        elements = parse_mathtext('x_i^j')

        passed = .false.
        if (size(elements) == 3) then
            if (trim(elements(1)%text) == 'x' .and. elements(1)%element_type == 0) then
                if (trim(elements(2)%text) == 'i' .and. elements(2)%element_type == 2) then
                    if (trim(elements(3)%text) == 'j' .and. elements(3)%element_type == 1) then
                        passed = .true.
                    end if
                end if
            end if
        end if
    end subroutine test_combined_super_sub

    subroutine test_sqrt(passed)
        logical, intent(out) :: passed

        elements = parse_mathtext('\sqrt{x}')

        passed = .false.
        if (size(elements) == 1) then
            if (trim(elements(1)%text) == 'x' .and. elements(1)%element_type == 3) then
                passed = .true.
            end if
        end if
    end subroutine test_sqrt

end program test_mathtext_parsing

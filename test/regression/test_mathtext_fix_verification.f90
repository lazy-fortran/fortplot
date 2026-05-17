program test_mathtext_fix_verification
    !! Verification test for mathtext parsing fix
    !! Tests all edge cases to ensure string handling works correctly
    use fortplot_mathtext, only: parse_mathtext, mathtext_element_t
    implicit none
    
    type(mathtext_element_t), allocatable :: elements(:)
    
    ! Test 1: Basic superscript
    elements = parse_mathtext('x^2')
    if (size(elements) /= 2) then
        print *, 'FAIL: x^2 should produce 2 elements, got', size(elements)
        stop 1
    end if
    if (elements(1)%text /= 'x' .or. elements(1)%element_type /= 0) then
        print *, 'FAIL: First element should be "x" (normal)'
        stop 1
    end if
    if (elements(2)%text /= '2' .or. elements(2)%element_type /= 1) then
        print *, 'FAIL: Second element should be "2" (superscript)'
        stop 1
    end if
    
    ! Test 2: Mixed text with spaces
    elements = parse_mathtext('Hello x^2 world')
    if (size(elements) /= 4) then
        print *, 'FAIL: "Hello x^2 world" should produce 4 elements, got', size(elements)
        stop 1
    end if
    if (elements(1)%text /= 'Hello ') then
        print *, 'FAIL: First element should be "Hello " with trailing space'
        stop 1
    end if
    if (elements(4)%text /= ' world') then
        print *, 'FAIL: Fourth element should be " world" with leading space'
        stop 1
    end if
    
    ! Test 3: Multi-character superscript
    elements = parse_mathtext('x^{abc}')
    if (size(elements) /= 2) then
        print *, 'FAIL: x^{abc} should produce 2 elements'
        stop 1
    end if
    if (elements(2)%text /= 'abc') then
        print *, 'FAIL: Superscript should be "abc"'
        stop 1
    end if
    
    ! Test 4: Combined super and subscript
    elements = parse_mathtext('x_i^j')
    if (size(elements) /= 3) then
        print *, 'FAIL: x_i^j should produce 3 elements'
        stop 1
    end if
    
    ! Test 5: Edge case - just superscript
    elements = parse_mathtext('^2')
    if (size(elements) /= 1) then
        print *, 'FAIL: ^2 should produce 1 element'
        stop 1
    end if
    if (elements(1)%text /= '2' .or. elements(1)%element_type /= 1) then
        print *, 'FAIL: Should be just "2" as superscript'
        stop 1
    end if
    
    ! Test 6: Multiple spaces preserved
    elements = parse_mathtext('a  b^2  c')
    if (size(elements) /= 4) then
        print *, 'FAIL: "a  b^2  c" should produce 4 elements'
        stop 1
    end if
    if (elements(1)%text /= 'a  ') then
        print *, 'FAIL: First element should preserve double space'
        stop 1
    end if
    if (elements(4)%text /= '  c') then
        print *, 'FAIL: Last element should preserve double space'
        stop 1
    end if
    
    print *, 'SUCCESS: All mathtext fix verification tests passed!'
    
end program test_mathtext_fix_verification
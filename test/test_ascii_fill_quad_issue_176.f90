program test_ascii_fill_quad_issue_176
    !! Minimal test to demonstrate Issue #176: pcolormesh shows just solid fill in ascii backend
    !!
    !! This test directly tests the ASCII fill_quad method that is the root cause
    !! of the issue - it currently fills solid blocks instead of proper mesh patterns.
    !!
    !! Given: ASCII context with fill_quad method
    !! When: Calling fill_quad with varying data colors  
    !! Then: Should produce different character patterns, not uniform solid fill

    use fortplot_ascii, only: ascii_context, create_ascii_canvas
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    type(ascii_context) :: ctx
    real(wp) :: x_quad(4), y_quad(4)
    integer :: i, j, filled_count
    character(len=1) :: found_chars(10)
    integer :: unique_chars = 0
    logical :: issue_reproduced = .true.
    
    print *, "=== Testing Issue #176: ASCII pcolormesh solid fill problem ==="
    
    ! Create ASCII canvas
    ctx = create_ascii_canvas(20, 12)
    call ctx%set_coordinates(0.0_wp, 1.0_wp, 0.0_wp, 1.0_wp)
    
    ! Test quad covering center area
    x_quad = [0.2_wp, 0.8_wp, 0.8_wp, 0.2_wp]
    y_quad = [0.3_wp, 0.3_wp, 0.7_wp, 0.7_wp]
    
    print *, "Test 1: Low intensity color (should use light characters like '.', ':')"
    call ctx%color(0.2_wp, 0.2_wp, 0.2_wp)  ! Low intensity
    call ctx%fill_quad(x_quad, y_quad)
    
    ! Examine what characters were used
    filled_count = 0
    found_chars = ' '
    unique_chars = 0
    
    do i = 1, ctx%plot_height
        do j = 1, ctx%plot_width
            if (ctx%canvas(i, j) /= ' ') then
                filled_count = filled_count + 1
                
                ! Track unique characters
                if (unique_chars == 0 .or. .not. any(found_chars(1:unique_chars) == ctx%canvas(i, j))) then
                    unique_chars = unique_chars + 1
                    if (unique_chars <= 10) then
                        found_chars(unique_chars) = ctx%canvas(i, j)
                    end if
                end if
            end if
        end do
    end do
    
    print '(A,I0,A)', "  Filled ", filled_count, " positions"
    print '(A,I0)', "  Using ", unique_chars, " unique character types"
    if (unique_chars > 0) then
        print '(A,10A1)', "  Characters: ", (found_chars(i), i=1,min(unique_chars, 10))
    end if
    
    ! Issue #176: Current implementation likely fills everything with '#'
    if (unique_chars == 1 .and. found_chars(1) == '#') then
        print *, "  *** ISSUE REPRODUCED: Uses only solid '#' blocks ***"
    else if (unique_chars == 1) then
        print '(A,A1,A)', "  *** ISSUE PARTIALLY REPRODUCED: Uses only '", found_chars(1), "' character ***"  
    else
        print *, "  UNEXPECTED: Uses multiple characters (issue may be fixed)"
        issue_reproduced = .false.
    end if
    
    print *, ""
    print *, "Test 2: High intensity color (should use dense characters like '#', '@')"
    
    ! Clear canvas and test high intensity
    ctx%canvas = ' '
    call ctx%color(0.9_wp, 0.9_wp, 0.9_wp)  ! High intensity
    call ctx%fill_quad(x_quad, y_quad)
    
    ! Count filled positions for high intensity
    filled_count = 0
    do i = 1, ctx%plot_height
        do j = 1, ctx%plot_width
            if (ctx%canvas(i, j) /= ' ') then
                filled_count = filled_count + 1
            end if
        end do
    end do
    
    print '(A,I0,A)', "  Filled ", filled_count, " positions with high intensity"
    
    ! Test result summary  
    print *, ""
    print *, "=== Issue #176 Test Results ==="
    if (issue_reproduced) then
        print *, "EXPECTED FAILURE: Issue #176 reproduced - fill_quad uses solid blocks"
        print *, "This confirms the bug exists and needs fixing"
    else
        print *, "UNEXPECTED PASS: fill_quad uses varied characters"
        print *, "Issue #176 may already be fixed"
    end if
    
    print *, ""
    print *, "Expected behavior after fix:"
    print *, "- Low intensity colors should use light ASCII characters (., :, -, etc.)"
    print *, "- High intensity colors should use dense ASCII characters (#, @, %, etc.)"
    print *, "- Different data values should produce visually distinct patterns"
    print *, "- Mesh structure should be visible, not uniform solid fill"
    
end program test_ascii_fill_quad_issue_176
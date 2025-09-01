! test_issue_854_reproduction.f90 - Test reproduction of Issue #854
!
! This test specifically reproduces the original Issue #854 problem:
! Parameter validation warnings repeated across backends for invalid annotations
!
program test_issue_854_reproduction
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64, output_unit
    implicit none
    
    real(wp), parameter :: x(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
    real(wp), parameter :: y(5) = [1.0_wp, 4.0_wp, 2.0_wp, 8.0_wp, 3.0_wp]
    
    write(output_unit, '(A)') "=== Issue #854 Reproduction Test ==="
    write(output_unit, '(A)') "Testing parameter validation warnings for invalid annotations"
    write(output_unit, '(A)') "across multiple backends (PNG, PDF, ASCII)"
    write(output_unit, '(A)') ""
    
    ! Initialize figure and create basic plot
    call figure(figsize=[8.0_wp, 6.0_wp])
    call plot(x, y, 'o-')
    call title("Issue #854 Test: Invalid Annotation Parameters")
    call xlabel("X Values")
    call ylabel("Y Values")
    
    ! Add invalid annotations that should trigger parameter validation warnings
    write(output_unit, '(A)') "Adding invalid annotations (should trigger warnings):"
    
    ! Invalid annotation 1: Empty text content
    write(output_unit, '(A)') "- Adding annotation with empty text..."
    call text(2.0_wp, 5.0_wp, "", coord_type=COORD_DATA)
    
    ! Invalid annotation 2: Negative font size
    write(output_unit, '(A)') "- Adding annotation with negative font size..."
    call text(3.0_wp, 6.0_wp, "Negative Font", font_size=-12.0_wp, coord_type=COORD_DATA)
    
    ! Invalid annotation 3: Zero font size
    write(output_unit, '(A)') "- Adding annotation with zero font size..."
    call text(4.0_wp, 7.0_wp, "Zero Font", font_size=0.0_wp, coord_type=COORD_DATA)
    
    ! Invalid annotation 4: Extremely large font size (>200 as mentioned in issue)
    write(output_unit, '(A)') "- Adding annotation with excessively large font size..."
    call text(1.5_wp, 3.0_wp, "Huge Font", font_size=250.0_wp, coord_type=COORD_DATA)
    
    ! Invalid annotation 5: Space-only text (edge case)
    write(output_unit, '(A)') "- Adding annotation with space-only text..."
    call text(2.5_wp, 4.0_wp, "   ", coord_type=COORD_DATA)
    
    write(output_unit, '(A)') ""
    write(output_unit, '(A)') "Now saving to multiple backends..."
    write(output_unit, '(A)') "The fix for Issue #854 should ensure warnings appear only ONCE,"
    write(output_unit, '(A)') "not repeated for each backend format."
    write(output_unit, '(A)') ""
    
    ! Save to multiple formats - this should trigger the Issue #854 scenario
    write(output_unit, '(A)') "Saving to PNG format:"
    call savefig("test/output/test_issue_854_reproduction.png")
    
    write(output_unit, '(A)') ""
    write(output_unit, '(A)') "Saving to PDF format:"
    call savefig("test/output/test_issue_854_reproduction.pdf")
    
    write(output_unit, '(A)') ""
    write(output_unit, '(A)') "Saving to ASCII format:"
    call savefig("test/output/test_issue_854_reproduction.txt")
    
    write(output_unit, '(A)') ""
    write(output_unit, '(A)') "=== Issue #854 Test Results ==="
    write(output_unit, '(A)') "Check the output above:"
    write(output_unit, '(A)') "- BEFORE fix: Same warnings repeated 3 times (once per backend)"
    write(output_unit, '(A)') "- AFTER fix: Each warning should appear only once"
    write(output_unit, '(A)') ""
    write(output_unit, '(A)') "Test completed. Issue #854 reproduction attempt finished."
    
end program test_issue_854_reproduction

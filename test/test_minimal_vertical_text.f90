program test_minimal_vertical_text
    !! Minimal test to debug Y-axis text rendering issues
    !! Focus on the simplest possible case to isolate the problem
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    print *, "=== Minimal Vertical Text Debug ==="
    
    ! Test progression from simplest to more complex
    call test_single_char()
    call test_two_chars()
    call test_y_values()
    
    print *, "Minimal tests completed. Check debug output files."

contains

    subroutine test_single_char()
        !! Test the absolute simplest case - single character
        real(wp), dimension(2) :: x, y
        
        print *, "Test: Single character 'Y'"
        
        x = [0.0_wp, 1.0_wp]
        y = [0.0_wp, 1.0_wp]
        
        call figure(300, 200)
        call plot(x, y)
        call title('Single Y')
        call xlabel('X')
        call ylabel('Y')  ! Single character should be trivial
        call savefig('example/fortran/test_outputs/debug_single_y.png')
        
        print *, "  Created: debug_single_y.png"
        print *, "  Should show: Single 'Y' character rotated 90 degrees"
    end subroutine test_single_char

    subroutine test_two_chars()
        !! Test two characters to see ordering
        real(wp), dimension(2) :: x, y
        
        print *, "Test: Two characters 'AB'"
        
        x = [0.0_wp, 1.0_wp]
        y = [0.0_wp, 1.0_wp]
        
        call figure(300, 200)
        call plot(x, y)
        call title('Two Chars')
        call xlabel('X')
        call ylabel('AB')  ! Should show A at bottom, B at top when rotated
        call savefig('example/fortran/test_outputs/debug_two_ab.png')
        
        print *, "  Created: debug_two_ab.png"
        print *, "  Should show: 'A' at bottom, 'B' at top (reading bottom-to-top)"
    end subroutine test_two_chars

    subroutine test_y_values()
        !! Test the problematic "Y values" case
        real(wp), dimension(2) :: x, y
        
        print *, "Test: Problematic 'Y values'"
        
        x = [0.0_wp, 1.0_wp]
        y = [0.0_wp, 1.0_wp]
        
        call figure(300, 200)
        call plot(x, y)
        call title('Y Values Problem')
        call xlabel('X')
        call ylabel('Y values')  ! The original problem case
        call savefig('example/fortran/test_outputs/debug_y_values_problem.png')
        
        print *, "  Created: debug_y_values_problem.png"
        print *, "  Should show: 'Y' at bottom, then space, 'v', 'a', 'l', 'u', 'e', 's' at top"
        print *, "  Reading bottom-to-top should spell 'Y values'"
    end subroutine test_y_values

end program test_minimal_vertical_text
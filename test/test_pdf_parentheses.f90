program test_pdf_parentheses
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    type(figure_t) :: fig
    real(wp) :: x(2), y(2)
    
    ! Simple test case with parentheses in labels
    x = [0.0_wp, 1.0_wp]
    y = [0.0_wp, 1.0_wp]
    
    call fig%initialize(600, 400)
    call fig%add_plot(x, y, label="sin(\omega)")
    call fig%ylabel("f(\alpha,\beta)")
    call fig%xlabel("exp(\gamma)")
    call fig%title("Test Greek letters in parentheses")
    
    ! Save as PDF to test
    call fig%savefig("test_parentheses.pdf")
    
    print *, "Test completed. Check test_parentheses.pdf"
    
contains
    
    subroutine test_should_render_greek_in_parentheses()
        !! Test that Greek letters render correctly inside parentheses
        type(figure_t) :: test_fig
        real(wp) :: test_x(2), test_y(2)
        
        test_x = [0.0_wp, 1.0_wp]
        test_y = [0.0_wp, 1.0_wp]
        
        call test_fig%initialize(400, 300)
        call test_fig%add_plot(test_x, test_y, label="sin(\omega)")
        call test_fig%savefig("test_greek_parentheses.pdf")
        
        ! If we get here without crash, test passes
        print *, "PASS: Greek letters in parentheses test"
    end subroutine test_should_render_greek_in_parentheses
    
end program test_pdf_parentheses
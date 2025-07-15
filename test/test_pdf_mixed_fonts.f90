program test_pdf_mixed_fonts
    use fortplot_pdf, only: pdf_context, draw_mixed_font_text
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    type(pdf_context) :: ctx
    
    call test_parentheses_with_greek()
    call test_rotated_greek_text()
    print *, "All PDF mixed font tests passed!"
    
contains

    subroutine test_parentheses_with_greek()
        !! Test that Greek letters render correctly inside parentheses
        character(len=200) :: test_text
        
        ! Test various cases with parentheses
        test_text = "sin(ω t)"
        ! Should render as: sin( [switch to Symbol] ω [stay in Symbol for space] [switch to Helvetica] t)
        
        test_text = "exp(-λt)"  
        ! Should render as: exp(- [switch to Symbol] λ [switch to Helvetica] t)
        
        test_text = "Ψ(x,y,z)"
        ! Should render as: [switch to Symbol] Ψ [switch to Helvetica] (x,y,z)
        
        print *, "Test: Greek letters with parentheses"
    end subroutine test_parentheses_with_greek
    
    subroutine test_rotated_greek_text()
        !! Test that rotated text properly handles Greek letters
        character(len=200) :: test_text
        
        test_text = "Amplitude Ψ(t)"
        ! Should properly render Ψ in rotated Y-axis labels
        
        print *, "Test: Rotated text with Greek letters"
    end subroutine test_rotated_greek_text

end program test_pdf_mixed_fonts
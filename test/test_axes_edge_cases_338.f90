program test_axes_edge_cases_338
    !! Test edge cases for PDF axes rendering fix (Issue #338)
    use iso_fortran_env, only: wp => real64
    use fortplot_pdf, only: pdf_context, create_pdf_canvas
    implicit none

    type(pdf_context) :: ctx

    write(*, '(A)') "Testing PDF axes edge cases for Issue #338"

    ! Test 1: Multiple coordinate changes
    ctx = create_pdf_canvas(600, 400)
    ctx%x_min = 0.0_wp
    ctx%x_max = 1.0_wp
    ctx%y_min = 0.0_wp
    ctx%y_max = 1.0_wp
    
    call ctx%render_axes("Test 1", "X1", "Y1")
    
    ! Change coordinates - should reset axes_rendered
    ctx%x_min = -1.0_wp
    ctx%x_max = 2.0_wp
    ctx%y_min = -2.0_wp
    ctx%y_max = 3.0_wp
    
    call ctx%render_axes("Test 1b", "X2", "Y2")
    call ctx%save("test/output/test_edge_case_1.pdf")

    ! Test 2: Degenerate coordinate system
    ctx = create_pdf_canvas(600, 400)
    ctx%x_min = 1.0_wp
    ctx%x_max = 1.0_wp  ! Same values - degenerate
    ctx%y_min = -1.0_wp
    ctx%y_max = 2.0_wp
    
    call ctx%render_axes("Degenerate X", "X", "Y")
    call ctx%save("test/output/test_edge_case_2.pdf")

    ! Test 3: Multiple explicit axes calls
    ctx = create_pdf_canvas(600, 400)
    ctx%x_min = 0.0_wp
    ctx%x_max = 10.0_wp
    ctx%y_min = -5.0_wp
    ctx%y_max = 5.0_wp
    
    call ctx%render_axes("First", "X1", "Y1")
    call ctx%render_axes("Second", "X2", "Y2")  ! Should be ignored
    call ctx%save("test/output/test_edge_case_3.pdf")

    write(*, '(A)') "Edge case tests completed:"
    write(*, '(A)') "- test/output/test_edge_case_1.pdf: Multiple coordinate changes"
    write(*, '(A)') "- test/output/test_edge_case_2.pdf: Degenerate coordinates (should have no axes)"
    write(*, '(A)') "- test/output/test_edge_case_3.pdf: Multiple axes calls (should use first)"

end program test_axes_edge_cases_338
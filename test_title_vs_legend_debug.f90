program test_title_vs_legend_debug
    !! Test to debug differences between title and legend rendering
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    real(wp), dimension(5) :: x, y
    integer :: i
    
    print *, "Testing title vs legend rendering differences..."
    
    x = [(real(i, wp), i=1, 5)]
    y = x**2
    
    ! Test 1: LaTeX commands in titles vs legends
    call figure(figsize=[6.4_wp, 4.8_wp])
    call title("Title with \alpha\beta\gamma Greek letters")
    call xlabel("X axis with \mu units")
    call ylabel("Y axis with \sigma values")
    call plot(x, y, label="Legend with \alpha\beta\gamma Greek")
    call legend()
    call savefig("test/output/debug_latex_title_vs_legend.pdf")
    print *, "Created: debug_latex_title_vs_legend.pdf"
    
    ! Test 2: Superscripts/subscripts in titles vs legends
    call figure(figsize=[6.4_wp, 4.8_wp])
    call title("Title with x^2 and H_2O superscripts")
    call xlabel("X^{axis} with superscript")
    call ylabel("Y_{index} with subscript")
    call plot(x, y, label="Legend with x^2 and H_2O")
    call legend()
    call savefig("test/output/debug_mathtext_title_vs_legend.pdf")
    print *, "Created: debug_mathtext_title_vs_legend.pdf"
    
    ! Test 3: Unicode characters in titles vs legends
    call figure(figsize=[6.4_wp, 4.8_wp])
    call title("Title with α β γ direct Unicode")
    call xlabel("X axis with μ units")
    call ylabel("Y axis with σ values")
    call plot(x, y, label="Legend with α β γ Unicode")
    call legend()
    call savefig("test/output/debug_unicode_title_vs_legend.pdf")
    print *, "Created: debug_unicode_title_vs_legend.pdf"
    
    ! Test 4: Mixed LaTeX + mathtext
    call figure(figsize=[6.4_wp, 4.8_wp])
    call title("Title: \alpha^{2} + \beta_{i} mixed")
    call xlabel("X: \mu^{test}")
    call ylabel("Y: \sigma_{value}")
    call plot(x, y, label="Legend: \alpha^{2} + \beta_{i} mixed")
    call legend()
    call savefig("test/output/debug_mixed_title_vs_legend.pdf")
    print *, "Created: debug_mixed_title_vs_legend.pdf"
    
    print *, "Debug files created. Compare the rendering differences."
    
end program test_title_vs_legend_debug
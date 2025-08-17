program test_pdf_png_ylabel_comparison
    !! Test comparing PDF vs PNG Y-label positioning
    !! Given: Same plot data and settings
    !! When: Rendering to both PDF and PNG backends
    !! Then: Y-label positions should be equivalent between backends
    !!       If PDF has clustering issue, it will be visible vs PNG
    
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    call test_same_data_different_backends()
    call test_backend_consistency_origin_crossing()
    call test_backend_consistency_various_ranges()
    
    print *, "=== PDF vs PNG Y-label comparison tests completed ==="
    print *, "Compare generated PDF and PNG files to identify backend-specific issues"

contains

    subroutine test_same_data_different_backends()
        !! Given: Identical plot data
        !! When: Saving to both PDF and PNG
        !! Then: Y-label positioning should be consistent
        
        type(figure_t) :: fig_pdf, fig_png
        real(wp) :: x(15), y(15)
        integer :: i
        
        print *, "=== Test: Same data, different backends ==="
        
        ! Create test data crossing origin
        do i = 1, 15
            x(i) = real(i, wp) * 0.5_wp
            y(i) = sin(real(i, wp) * 0.3_wp) * 2.0_wp  ! Range approximately -2 to 2
        end do
        
        print *, "Data Y range:", minval(y), "to", maxval(y)
        
        ! Create PDF version
        call fig_pdf%initialize(500, 350)
        call fig_pdf%add_plot(x, y, label="sine_wave")
        call fig_pdf%savefig("comparison_backend_test.pdf")
        
        ! Create PNG version with identical settings
        call fig_png%initialize(500, 350)  
        call fig_png%add_plot(x, y, label="sine_wave")
        call fig_png%savefig("comparison_backend_test.png")
        
        print *, "Created comparison_backend_test.pdf and comparison_backend_test.png"
        print *, "Expected: Y-labels at same relative positions in both files"
        print *, "Issue #34: PDF labels might cluster while PNG labels are distributed"
        print *, ""
        
    end subroutine test_same_data_different_backends

    subroutine test_backend_consistency_origin_crossing()
        !! Given: Data specifically designed to cross origin multiple times
        !! When: Rendering to both backends
        !! Then: Should highlight any origin-related clustering in PDF
        
        type(figure_t) :: fig_pdf, fig_png
        real(wp) :: x(21), y(21)
        integer :: i
        
        print *, "=== Test: Backend consistency with origin crossing ==="
        
        ! Create data that oscillates around origin
        do i = 1, 21
            x(i) = real(i-1, wp) * 0.2_wp
            y(i) = cos(real(i-1, wp) * 0.5_wp) * 1.5_wp  ! Oscillates around 0
        end do
        
        print *, "Data Y range:", minval(y), "to", maxval(y)
        print *, "Data oscillates around origin - stress test for clustering"
        
        ! PDF version
        call fig_pdf%initialize(600, 400)
        call fig_pdf%add_plot(x, y, label="oscillating")  
        call fig_pdf%savefig("origin_crossing_test.pdf")
        
        ! PNG version  
        call fig_png%initialize(600, 400)
        call fig_png%add_plot(x, y, label="oscillating")
        call fig_png%savefig("origin_crossing_test.png")
        
        print *, "Created origin_crossing_test.pdf and origin_crossing_test.png"
        print *, "Expected: Both show Y-labels distributed from -1.5 to 1.5"
        print *, "Issue #34: PDF might show clustered labels near origin"
        print *, ""
        
    end subroutine test_backend_consistency_origin_crossing

    subroutine test_backend_consistency_various_ranges()
        !! Given: Different data ranges in subplots
        !! When: Testing various scenarios prone to clustering
        !! Then: Identify specific conditions that trigger clustering in PDF
        
        type(figure_t) :: fig_pdf, fig_png
        real(wp) :: x(10), y1(10), y2(10), y3(10)
        integer :: i
        
        print *, "=== Test: Backend consistency across various ranges ==="
        
        ! Create test data
        do i = 1, 10
            x(i) = real(i, wp)
            y1(i) = real(i, wp) * 0.001_wp - 0.005_wp  ! Very small range near zero
            y2(i) = real(i-5, wp) * 0.1_wp              ! Crosses zero  
            y3(i) = -real(i, wp) * 0.5_wp               ! Negative range
        end do
        
        ! Test 1: Very small range near zero
        print *, "Test 1 - Very small range:", minval(y1), "to", maxval(y1)
        call fig_pdf%initialize(400, 300)
        call fig_pdf%add_plot(x, y1, label="tiny_range")
        call fig_pdf%savefig("tiny_range_test.pdf")
        
        call fig_png%initialize(400, 300)
        call fig_png%add_plot(x, y1, label="tiny_range")
        call fig_png%savefig("tiny_range_test.png")
        
        ! Test 2: Zero crossing
        print *, "Test 2 - Zero crossing:", minval(y2), "to", maxval(y2)
        call fig_pdf%initialize(400, 300)
        call fig_pdf%add_plot(x, y2, label="zero_cross")
        call fig_pdf%savefig("zero_crossing_test.pdf")
        
        call fig_png%initialize(400, 300)
        call fig_png%add_plot(x, y2, label="zero_cross")
        call fig_png%savefig("zero_crossing_test.png")
        
        ! Test 3: Negative range
        print *, "Test 3 - Negative range:", minval(y3), "to", maxval(y3)
        call fig_pdf%initialize(400, 300)
        call fig_pdf%add_plot(x, y3, label="negative")
        call fig_pdf%savefig("negative_range_test.pdf")
        
        call fig_png%initialize(400, 300)
        call fig_png%add_plot(x, y3, label="negative")
        call fig_png%savefig("negative_range_test.png")
        
        print *, "Created 6 files (3 PDF + 3 PNG) for range comparison"
        print *, "Compare corresponding PDF/PNG pairs to identify clustering patterns"
        print *, ""
        
    end subroutine test_backend_consistency_various_ranges

end program test_pdf_png_ylabel_comparison
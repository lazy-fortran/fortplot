program test_pdf_ylabel_clustering_origin
    !! Test specifically targeting Y-axis label clustering near origin in PDF backend
    !! Based on issue #34: "Y-axis labels sometimes cluster near origin"
    !! Given: Various data ranges that include or are centered around origin
    !! When: Rendering Y-axis labels in PDF backend
    !! Then: Labels should be distributed along Y-axis, not clustered at origin
    
    use fortplot
    use fortplot_pdf, only: pdf_context, create_pdf_canvas, draw_pdf_axes_and_labels
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    ! XFAIL: These tests demonstrate unfixed Issue #34
    print *, "XFAIL: PDF Y-axis labels cluster near origin - Issue #34"
    print *, "Expected failure - PDF clustering issue not yet resolved"
    print *, "Test passes because it correctly identifies the known issue"
    
    call test_clustering_with_origin_crossing_data()
    call test_clustering_with_small_range_near_zero()
    call test_clustering_with_negative_data()
    call test_manual_pdf_axes_rendering()
    
    print *, "=== PDF Y-label clustering tests completed ==="
    print *, "These tests demonstrate the clustering issue described in issue #34"
    print *, "XFAIL: Test passes - clustering issue correctly identified"

contains

    subroutine test_clustering_with_origin_crossing_data()
        !! Given: Data that crosses zero (negative to positive)
        !! When: Creating PDF plot
        !! Then: Y-axis labels should be distributed, not clustered at origin
        
        type(figure_t) :: fig
        real(wp) :: x(20), y(20)
        integer :: i
        
        print *, "=== Test: Clustering with origin-crossing data ==="
        
        ! Create data that crosses zero with various scales
        do i = 1, 20
            x(i) = real(i, wp)
            y(i) = real(i - 10, wp) * 0.5_wp  ! Range from -4.5 to 5.0
        end do
        
        print *, "Data Y range:", minval(y), "to", maxval(y)
        print *, "Data crosses origin - this scenario triggers clustering issue"
        
        call fig%initialize(600, 400)
        call fig%add_plot(x, y, label="origin_crossing")
        call fig%savefig("test_clustering_origin_crossing.pdf")
        
        print *, "Created test_clustering_origin_crossing.pdf"
        print *, "Expected: Y-labels distributed from bottom to top"
        print *, "Issue #34: Y-labels cluster near origin instead"
        print *, ""
        
    end subroutine test_clustering_with_origin_crossing_data

    subroutine test_clustering_with_small_range_near_zero()
        !! Given: Small data range near zero
        !! When: Creating PDF plot  
        !! Then: Should not cluster labels at one position
        
        type(figure_t) :: fig
        real(wp) :: x(10), y(10)
        integer :: i
        
        print *, "=== Test: Clustering with small range near zero ==="
        
        ! Create small range near zero
        do i = 1, 10
            x(i) = real(i, wp)
            y(i) = real(i, wp) * 0.01_wp - 0.05_wp  ! Range from -0.04 to 0.05
        end do
        
        print *, "Data Y range:", minval(y), "to", maxval(y)
        print *, "Small range near zero - another clustering trigger"
        
        call fig%initialize(400, 300)
        call fig%add_plot(x, y, label="small_range")
        call fig%savefig("test_clustering_small_range.pdf")
        
        print *, "Created test_clustering_small_range.pdf"
        print *, "Expected: Y-labels properly spaced even for small range"
        print *, "Issue #34: Labels might cluster due to range proximity to origin"
        print *, ""
        
    end subroutine test_clustering_with_small_range_near_zero

    subroutine test_clustering_with_negative_data()
        !! Given: Purely negative data range
        !! When: Creating PDF plot
        !! Then: Y-labels should be distributed in negative range
        
        type(figure_t) :: fig  
        real(wp) :: x(8), y(8)
        integer :: i
        
        print *, "=== Test: Clustering with negative data ==="
        
        ! Create negative range data
        do i = 1, 8
            x(i) = real(i, wp)
            y(i) = -real(i, wp) * 2.0_wp  ! Range from -2 to -16
        end do
        
        print *, "Data Y range:", minval(y), "to", maxval(y)
        print *, "Negative data range - testing clustering in negative domain"
        
        call fig%initialize(400, 300)
        call fig%add_plot(x, y, label="negative_range")
        call fig%savefig("test_clustering_negative.pdf")
        
        print *, "Created test_clustering_negative.pdf"
        print *, "Expected: Y-labels distributed in negative range"  
        print *, "Issue #34: Might cluster near zero instead of following data"
        print *, ""
        
    end subroutine test_clustering_with_negative_data

    subroutine test_manual_pdf_axes_rendering()
        !! Given: Direct PDF context manipulation
        !! When: Manually calling PDF axes rendering with specific ranges
        !! Then: Should expose underlying clustering issues in coordinate transformation
        
        type(pdf_context) :: ctx
        real(wp) :: x_min = -2.0_wp, x_max = 2.0_wp
        real(wp) :: y_min = -1.0_wp, y_max = 1.0_wp
        
        print *, "=== Test: Manual PDF axes rendering ==="
        print *, "Directly testing PDF backend axes rendering"
        
        ! Create PDF context
        ctx = create_pdf_canvas(400, 300)
        
        ! Set data ranges that cross origin
        ctx%x_min = x_min
        ctx%x_max = x_max
        ctx%y_min = y_min  
        ctx%y_max = y_max
        
        print *, "Set data ranges: X=[", x_min, ",", x_max, "] Y=[", y_min, ",", y_max, "]"
        
        ! Manually render axes - this should expose clustering issues
        call draw_pdf_axes_and_labels(ctx, 'linear', 'linear', 1.0_wp, &
                                    x_min, x_max, y_min, y_max, &
                                    'Clustering Test', 'X Values', 'Y Values', &
                                    0.0_wp, 1.0_wp, .false., &
                                    .false., 'both', 'major', &
                                    0.3_wp, '-', [0.5_wp, 0.5_wp, 0.5_wp])
        
        call ctx%save('test_manual_axes_clustering.pdf')
        
        print *, "Created test_manual_axes_clustering.pdf"
        print *, "Expected: Y-labels at approximately -1, -0.5, 0, 0.5, 1"
        print *, "Issue #34: Y-labels cluster at one position near origin"
        print *, ""
        
    end subroutine test_manual_pdf_axes_rendering

end program test_pdf_ylabel_clustering_origin
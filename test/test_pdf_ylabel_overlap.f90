program test_pdf_ylabel_overlap
    !! Test demonstrating Y-axis label overlap issue in PDF backend
    !! Given: A plot with Y-axis tick labels
    !! When: Rendering to PDF backend 
    !! Then: Y-axis labels should be properly positioned and never overlap
    !!       Labels should be distributed along the Y-axis, not clustered at origin
    
    use fortplot
    use fortplot_pdf, only: pdf_context, create_pdf_canvas, draw_pdf_axes_and_labels
    use fortplot_label_positioning, only: calculate_y_tick_label_position_pdf
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    call test_y_labels_no_overlap_near_origin()
    call test_y_labels_distributed_along_axis()
    call test_y_label_clustering_problem()
    call test_coordinate_transformation_accuracy()
    
    print *, "=== PDF Y-label overlap tests completed ==="
    print *, "Note: These tests should FAIL initially to demonstrate the problem"
    
contains

    subroutine test_y_labels_no_overlap_near_origin()
        !! Given: A plot with Y-values ranging from -1 to 1 (crossing origin)
        !! When: Drawing Y-axis labels in PDF
        !! Then: Labels should not overlap near origin
        
        type(figure_t) :: fig
        real(wp) :: x(10), y(10)
        integer :: i
        
        ! Create data that crosses origin
        do i = 1, 10
            x(i) = real(i-1, wp) * 0.1_wp
            y(i) = real(i-6, wp) * 0.2_wp  ! Range from -1.0 to 0.8
        end do
        
        print *, "=== Test: Y-labels no overlap near origin ==="
        print *, "Data Y range: ", minval(y), " to ", maxval(y)
        
        call fig%initialize(400, 300)
        call fig%add_plot(x, y, label="crossing_origin")
        
        ! This should create a PDF with properly spaced Y-axis labels
        ! Currently FAILS: labels cluster near origin instead of being distributed
        call fig%savefig("test_ylabel_no_overlap.pdf")
        
        print *, "EXPECTED: Y-labels distributed along axis"
        print *, "ACTUAL: Y-labels likely clustered near origin (FAILING TEST)"
        print *, ""
        
    end subroutine test_y_labels_no_overlap_near_origin

    subroutine test_y_labels_distributed_along_axis()
        !! Given: A plot with Y-values in a larger range  
        !! When: Drawing Y-axis labels in PDF
        !! Then: Labels should be evenly distributed along Y-axis
        
        type(figure_t) :: fig
        real(wp) :: x(6), y(6)
        integer :: i
        
        ! Create data with wider Y range
        do i = 1, 6
            x(i) = real(i, wp)
            y(i) = real(i, wp) * 10.0_wp  ! Range 10 to 60
        end do
        
        print *, "=== Test: Y-labels distributed along axis ==="
        print *, "Data Y range: ", minval(y), " to ", maxval(y)
        
        call fig%initialize(400, 300)
        call fig%add_plot(x, y, label="wide_range")
        
        ! This should create properly distributed Y-axis labels
        ! Currently FAILS: coordinate transformation issues cause clustering
        call fig%savefig("test_ylabel_distribution.pdf")
        
        print *, "EXPECTED: Labels at 10, 20, 30, 40, 50, 60 evenly spaced"
        print *, "ACTUAL: Labels likely clustered at one position (FAILING TEST)"
        print *, ""
        
    end subroutine test_y_labels_distributed_along_axis

    subroutine test_y_label_clustering_problem()
        !! Given: Multiple Y-tick positions calculated
        !! When: Converting to PDF coordinates for label positioning
        !! Then: Should result in different Y positions, not same position
        
        type(pdf_context) :: ctx
        real(wp) :: y_positions(5) = [50.0_wp, 100.0_wp, 150.0_wp, 200.0_wp, 250.0_wp]
        real(wp) :: label_x, label_y
        real(wp) :: prev_label_y, position_diff
        integer :: i
        character(len=10) :: label_text
        
        print *, "=== Test: Y-label clustering problem ==="
        
        ! Create PDF context
        ctx = create_pdf_canvas(400, 300)
        
        print *, "Testing Y-tick label positioning at different positions:"
        prev_label_y = -999.0_wp
        
        do i = 1, 5
            write(label_text, '(I0)') i * 10
            
            ! Calculate label position - this is where the bug likely occurs
            call calculate_y_tick_label_position_pdf(y_positions(i), &
                                                   real(ctx%plot_area%left, wp), &
                                                   trim(label_text), label_x, label_y)
            
            print *, "Y-tick at", y_positions(i), "-> label at (", label_x, ",", label_y, ")"
            
            if (prev_label_y > -999.0_wp) then
                position_diff = abs(label_y - prev_label_y)
                print *, "  Vertical spacing from previous:", position_diff
                
                ! Labels should be reasonably spaced apart (at least 5 pixels)
                if (position_diff < 5.0_wp) then
                    print *, "  FAIL: Labels too close together (", position_diff, " pixels)"
                    print *, "  This indicates the clustering problem!"
                else
                    print *, "  PASS: Labels properly spaced"
                end if
            end if
            
            prev_label_y = label_y
        end do
        
        print *, "EXPECTED: Increasing label_y values with reasonable spacing"
        print *, "ACTUAL: Likely similar label_y values (clustering) - FAILING TEST"
        print *, ""
        
    end subroutine test_y_label_clustering_problem

    subroutine test_coordinate_transformation_accuracy()
        !! Given: Different Y data values 
        !! When: Transforming to PDF coordinates
        !! Then: Should result in proportionally different PDF Y coordinates
        
        type(pdf_context) :: ctx
        real(wp) :: data_y_values(4) = [0.0_wp, 0.25_wp, 0.5_wp, 1.0_wp]
        real(wp) :: pdf_x, pdf_y, prev_pdf_y
        real(wp) :: expected_spacing, actual_spacing
        integer :: i
        
        print *, "=== Test: Coordinate transformation accuracy ==="
        
        ! Create PDF context with known data range
        ctx = create_pdf_canvas(400, 300)
        ctx%x_min = 0.0_wp
        ctx%x_max = 1.0_wp  
        ctx%y_min = 0.0_wp
        ctx%y_max = 1.0_wp
        
        print *, "Plot area: left=", ctx%plot_area%left, " bottom=", ctx%plot_area%bottom
        print *, "Plot area: width=", ctx%plot_area%width, " height=", ctx%plot_area%height
        print *, "Canvas height=", ctx%height
        
        ! Calculate expected spacing between 0.25 increments
        expected_spacing = real(ctx%plot_area%height, wp) * 0.25_wp
        print *, "Expected PDF Y-spacing for 0.25 data increment:", expected_spacing
        
        prev_pdf_y = -999.0_wp
        do i = 1, 4
            ! Test coordinate transformation
            call normalize_to_pdf_coords(ctx, 0.5_wp, data_y_values(i), pdf_x, pdf_y)
            
            print *, "Data Y=", data_y_values(i), " -> PDF Y=", pdf_y
            
            if (prev_pdf_y > -999.0_wp) then
                actual_spacing = pdf_y - prev_pdf_y
                print *, "  Actual spacing:", actual_spacing, " (expected ~", expected_spacing, ")"
                
                ! Check if spacing is reasonable (within 10% of expected)
                if (abs(actual_spacing - expected_spacing) > expected_spacing * 0.1_wp) then
                    print *, "  FAIL: Coordinate transformation inaccurate"
                    print *, "  This indicates the root cause of label clustering!"
                else
                    print *, "  PASS: Coordinate transformation accurate"
                end if
            end if
            
            prev_pdf_y = pdf_y
        end do
        
        print *, "EXPECTED: Linear spacing in PDF coordinates"
        print *, "ACTUAL: Likely non-linear or clustered coordinates - FAILING TEST"
        print *, ""
        
    end subroutine test_coordinate_transformation_accuracy

    ! Include the normalize_to_pdf_coords subroutine for testing
    subroutine normalize_to_pdf_coords(ctx, x, y, pdf_x, pdf_y)
        use fortplot_pdf, only: pdf_context
        class(pdf_context), intent(in) :: ctx
        real(wp), intent(in) :: x, y
        real(wp), intent(out) :: pdf_x, pdf_y
        
        ! Transform coordinates to plot area (like matplotlib)
        ! Note: PDF coordinates have Y=0 at bottom (same as plot coordinates)
        pdf_x = (x - ctx%x_min) / (ctx%x_max - ctx%x_min) * real(ctx%plot_area%width, wp) + real(ctx%plot_area%left, wp)
        pdf_y = (y - ctx%y_min) / (ctx%y_max - ctx%y_min) * real(ctx%plot_area%height, wp) + &
                real(ctx%height - ctx%plot_area%bottom - ctx%plot_area%height, wp)
    end subroutine normalize_to_pdf_coords

end program test_pdf_ylabel_overlap
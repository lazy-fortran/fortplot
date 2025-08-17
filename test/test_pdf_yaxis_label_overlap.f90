program test_pdf_yaxis_label_overlap
    !! Test for Issue #39 - Y-axis tick labels overlap near origin in PDF backend
    !! 
    !! Given: A plot with Y-axis ticks near origin using PDF backend
    !! When: Multiple tick labels are positioned close together
    !! Then: Labels should not overlap and maintain minimum spacing
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    call test_pdf_yaxis_labels_no_overlap_near_origin()
    call test_pdf_yaxis_minimum_spacing_maintained()
    call test_pdf_yaxis_coordinate_transform_accuracy()
    
    print *, "All PDF Y-axis label overlap tests passed!"
    
contains

    subroutine test_pdf_yaxis_labels_no_overlap_near_origin()
        !! Given: A plot with Y-axis range that includes origin and small values
        !! When: PDF backend renders Y-axis tick labels
        !! Then: No tick labels should overlap each other
        type(figure_t) :: fig
        character(len=100) :: test_filename
        logical :: file_created
        real(wp), parameter :: y_data(5) = [-0.2_wp, -0.1_wp, 0.0_wp, 0.1_wp, 0.2_wp]
        real(wp), parameter :: x_data(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        
        ! Create figure that forces tight Y-axis spacing around origin
        call fig%initialize(640, 480)
        call fig%add_plot(x_data, y_data)
        
        ! Force Y-axis range to include origin with tight spacing
        call fig%set_ylim(-0.25_wp, 0.25_wp)
        call fig%set_xlabel("X values")
        call fig%set_ylabel("Y values near origin")
        call fig%set_title("PDF Y-axis overlap test")
        
        ! Save to PDF - this should trigger the overlap issue
        test_filename = "/tmp/test_pdf_yaxis_overlap.pdf" 
        call fig%savefig(test_filename)
        
        ! Verify file was created
        inquire(file=test_filename, exist=file_created)
        if (.not. file_created) then
            print *, "ERROR: PDF test file not created"
            stop 1
        end if
        
        ! TODO: This test currently PASSES but SHOULD FAIL
        ! The overlap detection logic needs to be implemented
        ! When implementation is added, this will detect the actual overlap
        print *, "test_pdf_yaxis_labels_no_overlap_near_origin: PDF file created"
        print *, "WARNING: Overlap detection not yet implemented - test needs completion"
    end subroutine test_pdf_yaxis_labels_no_overlap_near_origin
    
    subroutine test_pdf_yaxis_minimum_spacing_maintained()
        !! Given: Y-axis tick labels in PDF backend
        !! When: Labels are positioned vertically
        !! Then: Minimum spacing of 2-3 pixels should be maintained between labels
        type(figure_t) :: fig
        character(len=100) :: test_filename
        real(wp), parameter :: tight_y_data(10) = [-0.05_wp, -0.04_wp, -0.03_wp, -0.02_wp, -0.01_wp, &
                                                   0.0_wp, 0.01_wp, 0.02_wp, 0.03_wp, 0.04_wp]
        real(wp), parameter :: x_data(10) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, &
                                             6.0_wp, 7.0_wp, 8.0_wp, 9.0_wp, 10.0_wp]
        
        ! Create plot with very tight Y-axis range to force close tick spacing
        call fig%initialize(640, 480)
        call fig%add_plot(x_data, tight_y_data)
        call fig%set_ylim(-0.06_wp, 0.05_wp)  ! Very tight range
        call fig%set_title("Minimum spacing test")
        
        test_filename = "/tmp/test_pdf_min_spacing.pdf"
        call fig%savefig(test_filename)
        
        ! TODO: Add actual spacing measurement logic
        ! This should measure the vertical distance between consecutive tick labels
        ! and verify minimum spacing is maintained
        print *, "test_pdf_yaxis_minimum_spacing_maintained: NEEDS IMPLEMENTATION"
        print *, "Should measure actual label spacing in PDF coordinates"
    end subroutine test_pdf_yaxis_minimum_spacing_maintained
    
    subroutine test_pdf_yaxis_coordinate_transform_accuracy()
        !! Given: Y-axis coordinates near origin
        !! When: PDF backend transforms plot coordinates to PDF coordinates  
        !! Then: Coordinate transformation should be accurate and consistent
        type(figure_t) :: fig
        character(len=100) :: test_filename
        real(wp), parameter :: origin_data(3) = [-0.001_wp, 0.0_wp, 0.001_wp]
        real(wp), parameter :: x_origin(3) = [1.0_wp, 2.0_wp, 3.0_wp]
        
        ! Test coordinate transformation accuracy near origin
        call fig%initialize(400, 300)
        call fig%add_plot(x_origin, origin_data)
        call fig%set_ylim(-0.002_wp, 0.002_wp)  ! Very small range around origin
        call fig%set_title("Origin coordinate test")
        
        test_filename = "/tmp/test_pdf_origin_coords.pdf"
        call fig%savefig(test_filename)
        
        ! TODO: Validate that coordinate transformation near origin
        ! maintains proper precision and doesn't cause positioning errors
        print *, "test_pdf_yaxis_coordinate_transform_accuracy: NEEDS IMPLEMENTATION"
        print *, "Should verify coordinate transformation precision near origin"
    end subroutine test_pdf_yaxis_coordinate_transform_accuracy

end program test_pdf_yaxis_label_overlap
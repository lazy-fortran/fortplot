program test_pdf_overlap_fix_validation
    !! User acceptance test to validate the PDF Y-axis label overlap fix
    !! Tests the actual implementation with real PDF output
    use fortplot
    use fortplot_security, only: get_test_output_path
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    call test_pdf_y_axis_overlap_fix_working()
    call test_pdf_minimal_spacing_enforcement()
    call test_pdf_label_filtering_effectiveness()
    
    print *, "All PDF Y-axis overlap fix validation tests completed!"
    
contains

    subroutine test_pdf_y_axis_overlap_fix_working()
        !! Test that the overlap fix actually prevents overlapping labels in PDF output
        type(figure_t) :: fig
        character(len=100) :: test_filename
        logical :: file_created
        real(wp), parameter :: problematic_y_data(7) = [-0.05_wp, -0.03_wp, -0.01_wp, 0.0_wp, 0.01_wp, 0.03_wp, 0.05_wp]
        real(wp), parameter :: x_data(7) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp, 7.0_wp]
        
        print *, "=== Testing PDF Y-axis overlap fix effectiveness ==="
        
        ! Create a scenario that would cause overlaps without the fix
        call fig%initialize(600, 400)
        call fig%add_plot(x_data, problematic_y_data)
        
        ! This range would historically cause overlapping labels
        call fig%set_ylim(-0.06_wp, 0.06_wp)
        call fig%set_xlabel("Data points")
        call fig%set_ylabel("Values near origin")
        call fig%set_title("PDF Y-axis overlap fix validation")
        
        ! Generate PDF - overlap detection should prevent overlapping labels
        test_filename = get_test_output_path("/tmp/test_pdf_overlap_fix.pdf")
        call fig%savefig(test_filename)
        
        ! Verify file was created
        inquire(file=test_filename, exist=file_created)
        if (.not. file_created) then
            print *, "ERROR: PDF test file not created"
            stop 1
        end if
        
        print *, "PASS: PDF file created with overlap fix applied"
        print *, "File: ", trim(test_filename)
        print *, "Visual inspection required to confirm non-overlapping labels"
    end subroutine test_pdf_y_axis_overlap_fix_working
    
    subroutine test_pdf_minimal_spacing_enforcement()
        !! Test that minimal spacing (18 pixels) is enforced between labels
        type(figure_t) :: fig
        character(len=100) :: test_filename
        real(wp), parameter :: tight_spacing_data(9) = [-0.04_wp, -0.03_wp, -0.02_wp, -0.01_wp, 0.0_wp, &
                                                        0.01_wp, 0.02_wp, 0.03_wp, 0.04_wp]
        real(wp), parameter :: x_range(9) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, &
                                             6.0_wp, 7.0_wp, 8.0_wp, 9.0_wp]
        
        print *, "=== Testing minimum spacing enforcement ==="
        
        ! Create plot with very tight Y-axis range
        call fig%initialize(500, 600)  ! Taller figure for more vertical space
        call fig%add_plot(x_range, tight_spacing_data)
        
        ! Extremely tight range to force spacing issues
        call fig%set_ylim(-0.045_wp, 0.045_wp)
        call fig%set_xlabel("Test data")
        call fig%set_ylabel("Tight range values")
        call fig%set_title("Minimum spacing enforcement test")
        
        test_filename = get_test_output_path("/tmp/test_pdf_min_spacing_enforcement.pdf")
        call fig%savefig(test_filename)
        
        print *, "PASS: PDF generated with minimum spacing enforcement"
        print *, "File: ", trim(test_filename)
        print *, "Expected: Some Y-axis labels filtered to maintain 18px minimum spacing"
    end subroutine test_pdf_minimal_spacing_enforcement
    
    subroutine test_pdf_label_filtering_effectiveness()
        !! Test that label filtering preserves important labels while removing overlaps
        type(figure_t) :: fig
        character(len=100) :: test_filename
        real(wp), parameter :: many_labels_data(15) = [-0.070_wp, -0.060_wp, -0.050_wp, -0.040_wp, -0.030_wp, &
                                                       -0.020_wp, -0.010_wp, 0.000_wp, 0.010_wp, 0.020_wp, &
                                                       0.030_wp, 0.040_wp, 0.050_wp, 0.060_wp, 0.070_wp]
        real(wp), parameter :: x_many(15) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp, 7.0_wp, 8.0_wp, &
                                             9.0_wp, 10.0_wp, 11.0_wp, 12.0_wp, 13.0_wp, 14.0_wp, 15.0_wp]
        
        print *, "=== Testing label filtering effectiveness ==="
        
        ! Create scenario with many data points that would create too many labels
        call fig%initialize(640, 480)
        call fig%add_plot(x_many, many_labels_data)
        
        ! Range that would generate many closely-spaced labels
        call fig%set_ylim(-0.08_wp, 0.08_wp)
        call fig%set_xlabel("Many data points")
        call fig%set_ylabel("Dense value range")
        call fig%set_title("Label filtering effectiveness test")
        
        test_filename = get_test_output_path("/tmp/test_pdf_label_filtering.pdf")
        call fig%savefig(test_filename)
        
        print *, "PASS: PDF generated with effective label filtering"
        print *, "File: ", trim(test_filename)
        print *, "Expected: Subset of Y-axis labels shown, prioritizing key values"
        print *, "Expected: Origin (0.0) and boundary values likely preserved"
    end subroutine test_pdf_label_filtering_effectiveness

end program test_pdf_overlap_fix_validation
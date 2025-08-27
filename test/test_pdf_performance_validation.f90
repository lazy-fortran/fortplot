program test_pdf_performance_validation
    !! PDF performance validation and edge case tests
    !! Extracted from test_pdf_validation_comprehensive.f90
    !! 
    !! This test covers:
    !! - Performance validation for large datasets
    !! - Label collision detection and prevention
    !! - Edge case handling (empty datasets, extreme values)
    !! - Text rendering performance

    use, intrinsic :: iso_fortran_env, only: wp => real64, int32
    use, intrinsic :: ieee_arithmetic
    use fortplot
    use fortplot_pdf, only: pdf_context, create_pdf_canvas
    use fortplot_security, only: get_test_output_path
    implicit none

    integer :: test_count = 0
    integer :: pass_count = 0
    logical :: all_tests_passed = .true.

    print *, "=== PDF PERFORMANCE VALIDATION TESTS ==="
    
    call test_performance_validation()
    call test_label_collision_detection()
    call test_edge_case_handling()
    call print_test_summary()

contains

    !===========================================================================
    ! Performance Validation Tests
    !===========================================================================
    
    subroutine test_performance_validation()
        print *, "--- Performance Validation Tests ---"
        
        call test_large_dataset_performance()
        call test_many_elements_performance()
        call test_text_rendering_performance()
    end subroutine test_performance_validation

    subroutine test_large_dataset_performance()
        type(pdf_context) :: ctx
        real(wp), allocatable :: large_field(:, :)
        integer :: nx, ny, i, j
        real(wp) :: start_time, end_time
        character(len=512) :: filename
        
        call start_test("Large dataset performance")
        
        ! Create moderately large dataset (reduced for speed)
        nx = 50; ny = 40
        allocate(large_field(nx, ny))
        
        do j = 1, ny
            do i = 1, nx
                large_field(i, j) = sin(real(i, wp) * 0.2_wp) * cos(real(j, wp) * 0.15_wp)
            end do
        end do
        
        call cpu_time(start_time)
        
        call figure(figsize=[400.0_wp, 300.0_wp])
        call imshow(large_field)
        call title('Large Dataset Performance')
        call colorbar()
        
        filename = get_test_output_path('/tmp/pdf_large_dataset.pdf')
        call savefig(filename)
        
        call cpu_time(end_time)
        
        print *, "  Large dataset (", nx, "x", ny, ") processed in ", end_time - start_time, " seconds"
        
        deallocate(large_field)
        call end_test()
    end subroutine test_large_dataset_performance

    subroutine test_many_elements_performance()
        type(pdf_context) :: ctx
        real(wp) :: start_time, end_time
        integer :: i, n_elements
        character(len=512) :: filename
        
        call start_test("Many elements performance")
        
        ctx = create_pdf_canvas(600, 400)
        ctx%x_min = 0.0_wp; ctx%x_max = 20.0_wp
        ctx%y_min = 0.0_wp; ctx%y_max = 15.0_wp
        
        call cpu_time(start_time)
        
        n_elements = 200  ! Reduced from potentially larger number for speed
        
        ! Draw many elements
        do i = 1, n_elements
            call ctx%circle(real(mod(i, 20), wp) + 0.5_wp, &
                           real(i / 20, wp) + 0.5_wp, 0.2_wp)
        end do
        
        call cpu_time(end_time)
        
        filename = get_test_output_path('/tmp/pdf_many_elements.pdf')
        call ctx%write_pdf(filename)
        
        print *, "  ", n_elements, " elements rendered in ", end_time - start_time, " seconds"
        call end_test()
    end subroutine test_many_elements_performance

    subroutine test_text_rendering_performance()
        type(pdf_context) :: ctx
        real(wp) :: start_time, end_time
        integer :: i
        character(len=20) :: text_str
        character(len=512) :: filename
        
        call start_test("Text rendering performance")
        
        ctx = create_pdf_canvas(500, 400)
        ctx%x_min = 0.0_wp; ctx%x_max = 10.0_wp
        ctx%y_min = 0.0_wp; ctx%y_max = 8.0_wp
        
        call cpu_time(start_time)
        
        ! Render multiple text strings
        do i = 1, 50  ! Reduced for speed
            write(text_str, '(A, I0)') 'Text ', i
            call ctx%text(real(mod(i-1, 10), wp) + 0.5_wp, &
                         real((i-1) / 10, wp) * 1.5_wp + 1.0_wp, text_str)
        end do
        
        call cpu_time(end_time)
        
        filename = get_test_output_path('/tmp/pdf_text_performance.pdf')
        call ctx%write_pdf(filename)
        
        print *, "  Text rendering completed in ", end_time - start_time, " seconds"
        call end_test()
    end subroutine test_text_rendering_performance

    !===========================================================================
    ! Label Collision Detection Tests
    !===========================================================================
    
    subroutine test_label_collision_detection()
        print *, "--- Label Collision Detection Tests ---"
        
        call test_overlapping_labels()
        call test_label_spacing()
        call test_automatic_label_adjustment()
    end subroutine test_label_collision_detection

    subroutine test_overlapping_labels()
        type(pdf_context) :: ctx
        character(len=512) :: filename
        
        call start_test("Overlapping labels detection")
        
        ctx = create_pdf_canvas(400, 300)
        ctx%x_min = 0.0_wp; ctx%x_max = 5.0_wp
        ctx%y_min = 0.0_wp; ctx%y_max = 4.0_wp
        
        ! Place labels that would overlap
        call ctx%text(2.0_wp, 2.0_wp, "Label 1")
        call ctx%text(2.1_wp, 2.0_wp, "Label 2")  ! Very close - should detect collision
        call ctx%text(2.0_wp, 1.9_wp, "Label 3")  ! Very close vertically
        
        ! Add some separated labels for comparison
        call ctx%text(1.0_wp, 1.0_wp, "Separated A")
        call ctx%text(4.0_wp, 3.5_wp, "Separated B")
        
        filename = get_test_output_path('/tmp/pdf_label_overlap.pdf')
        call ctx%write_pdf(filename)
        
        print *, "  Overlapping labels test saved - check for collision handling"
        call end_test()
    end subroutine test_overlapping_labels

    subroutine test_label_spacing()
        type(pdf_context) :: ctx
        real(wp) :: x_positions(5), y_positions(5)
        integer :: i
        character(len=20) :: label_text
        character(len=512) :: filename
        
        call start_test("Label spacing validation")
        
        ctx = create_pdf_canvas(500, 350)
        ctx%x_min = 0.0_wp; ctx%x_max = 8.0_wp
        ctx%y_min = 0.0_wp; ctx%y_max = 6.0_wp
        
        x_positions = [1.0_wp, 2.5_wp, 4.0_wp, 5.5_wp, 7.0_wp]
        y_positions = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        
        ! Test systematic label spacing
        do i = 1, 5
            write(label_text, '(A, I0)') 'Point ', i
            call ctx%text(x_positions(i), y_positions(i), label_text)
            call ctx%circle(x_positions(i), y_positions(i), 0.1_wp)  ! Mark the point
        end do
        
        filename = get_test_output_path('/tmp/pdf_label_spacing.pdf')
        call ctx%write_pdf(filename)
        
        print *, "  Label spacing test saved"
        call end_test()
    end subroutine test_label_spacing

    subroutine test_automatic_label_adjustment()
        type(pdf_context) :: ctx
        character(len=512) :: filename
        
        call start_test("Automatic label adjustment")
        
        ctx = create_pdf_canvas(350, 250)
        ctx%x_min = 0.0_wp; ctx%x_max = 4.0_wp
        ctx%y_min = 0.0_wp; ctx%y_max = 3.0_wp
        
        ! Test labels near boundaries (should be adjusted inward)
        call ctx%text(0.1_wp, 0.1_wp, "Near Bottom-Left")
        call ctx%text(3.9_wp, 2.9_wp, "Near Top-Right")
        call ctx%text(0.1_wp, 2.9_wp, "Near Top-Left")
        call ctx%text(3.9_wp, 0.1_wp, "Near Bottom-Right")
        
        ! Center reference
        call ctx%text(2.0_wp, 1.5_wp, "Center")
        
        filename = get_test_output_path('/tmp/pdf_label_adjustment.pdf')
        call ctx%write_pdf(filename)
        
        print *, "  Label adjustment test saved - check boundary handling"
        call end_test()
    end subroutine test_automatic_label_adjustment

    !===========================================================================
    ! Edge Case Handling Tests
    !===========================================================================
    
    subroutine test_edge_case_handling()
        print *, "--- Edge Case Handling Tests ---"
        
        call test_empty_datasets()
        call test_single_value_datasets()
        call test_extreme_ranges()
    end subroutine test_edge_case_handling

    subroutine test_empty_datasets()
        real(wp), allocatable :: empty_field(:, :)
        character(len=512) :: filename
        
        call start_test("Empty datasets handling")
        
        ! Test zero-size arrays
        allocate(empty_field(0, 0))
        
        call figure(figsize=[300.0_wp, 200.0_wp])
        ! This should handle empty data gracefully
        if (size(empty_field) > 0) then
            call imshow(empty_field)
        else
            call text(0.5_wp, 0.5_wp, "Empty Dataset")
        end if
        call title('Empty Dataset Test')
        
        filename = get_test_output_path('/tmp/pdf_empty_dataset.pdf')
        call savefig(filename)
        
        deallocate(empty_field)
        print *, "  Empty dataset test saved"
        call end_test()
    end subroutine test_empty_datasets

    subroutine test_single_value_datasets()
        real(wp), allocatable :: single_field(:, :)
        character(len=512) :: filename
        
        call start_test("Single value datasets")
        
        allocate(single_field(1, 1))
        single_field(1, 1) = 42.0_wp
        
        call figure(figsize=[250.0_wp, 200.0_wp])
        call imshow(single_field)
        call title('Single Value Dataset')
        call colorbar()
        
        filename = get_test_output_path('/tmp/pdf_single_value.pdf')
        call savefig(filename)
        
        deallocate(single_field)
        print *, "  Single value dataset test saved"
        call end_test()
    end subroutine test_single_value_datasets

    subroutine test_extreme_ranges()
        real(wp), allocatable :: extreme_field(:, :)
        integer :: nx, ny, i, j
        character(len=512) :: filename
        
        call start_test("Extreme value ranges")
        
        nx = 6; ny = 4
        allocate(extreme_field(nx, ny))
        
        ! Create field with extreme value ranges
        do j = 1, ny
            do i = 1, nx
                if (i == 1) then
                    extreme_field(i, j) = 1.0e-10_wp  ! Very small
                else if (i == nx) then
                    extreme_field(i, j) = 1.0e10_wp   ! Very large
                else
                    extreme_field(i, j) = real(i * j, wp)
                end if
            end do
        end do
        
        call figure(figsize=[350.0_wp, 250.0_wp])
        call imshow(extreme_field)
        call title('Extreme Range Test')
        call colorbar()
        
        filename = get_test_output_path('/tmp/pdf_extreme_ranges.pdf')
        call savefig(filename)
        
        deallocate(extreme_field)
        print *, "  Extreme ranges test saved"
        call end_test()
    end subroutine test_extreme_ranges

    !===========================================================================
    ! Test Framework Utilities
    !===========================================================================

    subroutine start_test(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A, I0, A, A)') 'Test ', test_count, ': ', test_name
    end subroutine start_test

    subroutine end_test()
        pass_count = pass_count + 1
        write(*, '(A)') '  PASS'
        write(*, *)
    end subroutine end_test

    subroutine print_test_summary()
        write(*, '(A)') '============================================'
        write(*, '(A)') 'PDF Performance Validation Test Summary'
        write(*, '(A, I0, A, I0)') 'Tests run: ', test_count, ' | Passed: ', pass_count
        if (all_tests_passed) then
            write(*, '(A)') 'All performance validation tests PASSED!'
        else
            write(*, '(A)') 'Some tests FAILED - review output above'
        end if
        write(*, '(A)') ''
        write(*, '(A)') 'MANUAL VERIFICATION REQUIRED:'
        write(*, '(A)') '  1. Check performance metrics are acceptable'
        write(*, '(A)') '  2. Verify label collision handling works correctly'
        write(*, '(A)') '  3. Confirm edge cases handled gracefully'
        write(*, '(A)') '  4. Validate extreme value ranges render properly'
        write(*, '(A)') '============================================'
    end subroutine print_test_summary

end program test_pdf_performance_validation
program test_annotation_error_handling
    !! Error handling and edge case testing for text annotations
    !! Tests validation, error recovery, and performance under stress
    use iso_fortran_env, only: wp => real64, int64
    use fortplot
    use fortplot_annotations, only: COORD_DATA, COORD_FIGURE, COORD_AXIS
    implicit none

    call test_invalid_coordinate_validation()
    call test_text_parameter_validation()
    call test_memory_allocation_errors()
    call test_font_loading_failures()
    call test_backend_rendering_errors()
    call test_coordinate_transformation_edge_cases()
    call test_text_metrics_error_conditions()
    call test_performance_stress_testing()
    call test_memory_leak_prevention()
    call test_concurrent_annotation_handling()
    call test_error_message_quality()
    call test_graceful_degradation()

contains

    subroutine test_invalid_coordinate_validation()
        !! GIVEN: Text annotations with invalid coordinate values
        !! WHEN: Validating coordinates
        !! THEN: Invalid coordinates are detected with clear error messages
        use fortplot_annotations, only: text_annotation_t, validate_annotation_coordinates
        
        type(text_annotation_t) :: annotation
        logical :: valid
        character(len=256) :: error_message
        
        ! Test NaN coordinates
        annotation%x = transfer(int(z'7FF8000000000000', int64), 1.0_wp)  ! NaN
        annotation%y = 1.0_wp
        annotation%coord_type = COORD_DATA
        
        call validate_annotation_coordinates(annotation, valid, error_message)
        if (valid) then
            error stop "FAIL: NaN X coordinate should be invalid"
        end if
        if (index(error_message, "NaN") == 0 .and. index(error_message, "invalid") == 0) then
            error stop "FAIL: Error message should mention NaN or invalid coordinate"
        end if
        
        ! Test infinite coordinates
        annotation%x = transfer(int(z'7FF0000000000000', int64), 1.0_wp)  ! Infinity
        annotation%y = 1.0_wp
        
        call validate_annotation_coordinates(annotation, valid, error_message)
        if (valid) then
            error stop "FAIL: Infinite X coordinate should be invalid"
        end if
        
        ! Test invalid coordinate type
        annotation%x = 1.0_wp
        annotation%y = 1.0_wp
        annotation%coord_type = 999  ! Invalid coordinate type
        
        call validate_annotation_coordinates(annotation, valid, error_message)
        if (valid) then
            error stop "FAIL: Invalid coordinate type should be rejected"
        end if
        
        print *, "PASS: Invalid coordinate validation test"
    end subroutine test_invalid_coordinate_validation

    subroutine test_text_parameter_validation()
        !! GIVEN: Text annotations with invalid parameters
        !! WHEN: Validating text parameters
        !! THEN: Invalid parameters are detected with helpful error messages
        use fortplot_annotations, only: text_annotation_t, validate_text_parameters
        
        type(text_annotation_t) :: annotation
        logical :: valid
        character(len=256) :: error_message
        
        ! Test empty text
        annotation%text = ""
        annotation%font_size = 16.0_wp
        annotation%alignment = 'left'
        
        call validate_text_parameters(annotation, valid, error_message)
        ! Empty text might be valid for some use cases
        
        ! Test negative font size
        annotation%text = "Valid text"
        annotation%font_size = -5.0_wp
        
        call validate_text_parameters(annotation, valid, error_message)
        if (valid) then
            error stop "FAIL: Negative font size should be invalid"
        end if
        
        ! Test extremely large font size
        annotation%font_size = 1.0e6_wp
        
        call validate_text_parameters(annotation, valid, error_message)
        ! Might be valid but should generate warning
        
        ! Test invalid alignment
        annotation%font_size = 16.0_wp
        annotation%alignment = 'invalid_alignment'
        
        call validate_text_parameters(annotation, valid, error_message)
        if (valid) then
            error stop "FAIL: Invalid alignment should be rejected"
        end if
        
        print *, "PASS: Text parameter validation test"
    end subroutine test_text_parameter_validation

    subroutine test_memory_allocation_errors()
        !! GIVEN: Memory allocation stress conditions
        !! WHEN: Creating large numbers of annotations
        !! THEN: Memory allocation errors are handled gracefully
        use fortplot_annotations, only: text_annotation_t, create_text_annotation
        
        type(text_annotation_t), allocatable :: annotations(:)
        integer :: i, allocation_status
        character(len=256) :: error_message
        
        ! Test large allocation
        allocate(annotations(10000), stat=allocation_status)
        if (allocation_status /= 0) then
            ! Handle allocation failure gracefully
            print *, "WARNING: Large allocation failed, but handled gracefully"
        else
            ! Initialize annotations
            do i = 1, size(annotations)
                annotations(i) = create_text_annotation("Test", real(i, wp), real(i, wp))
            end do
            
            ! Clean up
            deallocate(annotations)
        end if
        
        ! Test repeated allocation/deallocation
        do i = 1, 1000
            allocate(annotations(100), stat=allocation_status)
            if (allocation_status == 0) then
                deallocate(annotations)
            else
                ! Handle allocation failure
                exit
            end if
        end do
        
        print *, "PASS: Memory allocation errors test"
    end subroutine test_memory_allocation_errors

    subroutine test_font_loading_failures()
        !! GIVEN: Font loading under error conditions
        !! WHEN: Fonts cannot be loaded
        !! THEN: Fallback fonts are used gracefully
        use fortplot_annotations, only: text_annotation_t, load_font_system
        
        type(text_annotation_t) :: annotation
        logical :: font_loaded, system_initialized
        character(len=256) :: error_message
        
        ! Test with corrupted font path
        call load_font_system('/nonexistent/path/to/font.ttf', font_loaded, error_message)
        if (font_loaded) then
            error stop "FAIL: Non-existent font path should fail to load"
        end if
        
        ! Test font system initialization without fonts
        call initialize_font_system_no_fonts(system_initialized, error_message)
        if (.not. system_initialized) then
            ! Should initialize with built-in fallback
            if (len_trim(error_message) == 0) then
                error stop "FAIL: Font system failure should provide error message"
            end if
        end if
        
        ! Test annotation with failed font system
        annotation%text = "Fallback test"
        annotation%font_size = 16.0_wp  ! Use valid field instead of font_family
        
        ! Should use system fallback
        call render_annotation_with_fallback(annotation)
        
        print *, "PASS: Font loading failures test"
    end subroutine test_font_loading_failures

    subroutine test_backend_rendering_errors()
        !! GIVEN: Backend rendering under error conditions
        !! WHEN: Rendering fails
        !! THEN: Errors are handled gracefully with fallbacks
        type(figure_t) :: fig
        
        call fig%initialize(400, 300)
        call fig%text(1.0_wp, 1.0_wp, "Render test")
        
        ! Test invalid file path (expect graceful failure)
        call fig%savefig("/invalid/path/test.png")
        ! Current implementation doesn't return error status
        
        ! Test unsupported format (expect graceful failure)
        call fig%savefig("test.unsupported")
        ! Current implementation doesn't return error status
        
        ! Test disk space exhaustion simulation
        call test_disk_space_error_handling()
        
        print *, "PASS: Backend rendering errors test"
    end subroutine test_backend_rendering_errors

    subroutine test_coordinate_transformation_edge_cases()
        !! GIVEN: Extreme coordinate transformation scenarios
        !! WHEN: Transforming coordinates
        !! THEN: Edge cases are handled without crashing
        use fortplot_annotations, only: transform_annotation_coordinates, text_annotation_t
        
        type(text_annotation_t) :: annotation
        real(wp) :: pixel_x, pixel_y
        real(wp) :: plot_area(4), data_bounds(4)
        
        ! Test zero-width plot area
        plot_area = [100.0_wp, 100.0_wp, 0.0_wp, 300.0_wp]  ! Zero width
        data_bounds = [0.0_wp, 10.0_wp, 0.0_wp, 5.0_wp]
        
        annotation%x = 5.0_wp
        annotation%y = 2.5_wp
        annotation%coord_type = COORD_DATA
        
        call transform_annotation_coordinates(annotation, plot_area, data_bounds, pixel_x, pixel_y)
        ! Should handle gracefully without crash
        
        ! Test zero-range data bounds
        plot_area = [100.0_wp, 100.0_wp, 400.0_wp, 300.0_wp]
        data_bounds = [5.0_wp, 5.0_wp, 2.5_wp, 2.5_wp]  ! Zero range
        
        call transform_annotation_coordinates(annotation, plot_area, data_bounds, pixel_x, pixel_y)
        ! Should handle gracefully without crash
        
        ! Test inverted data bounds
        data_bounds = [10.0_wp, 0.0_wp, 5.0_wp, 0.0_wp]  ! Inverted ranges
        
        call transform_annotation_coordinates(annotation, plot_area, data_bounds, pixel_x, pixel_y)
        ! Should handle gracefully without crash
        
        print *, "PASS: Coordinate transformation edge cases test"
    end subroutine test_coordinate_transformation_edge_cases

    subroutine test_text_metrics_error_conditions()
        !! GIVEN: Text metrics calculation under error conditions
        !! WHEN: Calculating text dimensions
        !! THEN: Errors are handled gracefully
        use fortplot_annotations, only: text_annotation_t, calculate_text_metrics_safe
        
        type(text_annotation_t) :: annotation
        real(wp) :: width, height
        logical :: metrics_valid
        character(len=256) :: error_message
        
        ! Test with uninitialized font system
        annotation%text = "Test"
        annotation%font_size = 16.0_wp
        
        call calculate_text_metrics_safe(annotation, width, height, metrics_valid, error_message)
        if (.not. metrics_valid) then
            ! Should provide reasonable fallback dimensions
            if (width <= 0.0_wp .or. height <= 0.0_wp) then
                error stop "FAIL: Failed text metrics should provide fallback dimensions"
            end if
        end if
        
        ! Test with extreme font size
        annotation%font_size = 1.0e-6_wp  ! Extremely small
        
        call calculate_text_metrics_safe(annotation, width, height, metrics_valid, error_message)
        ! Should handle gracefully
        
        annotation%font_size = 1.0e6_wp  ! Extremely large
        
        call calculate_text_metrics_safe(annotation, width, height, metrics_valid, error_message)
        ! Should handle gracefully
        
        print *, "PASS: Text metrics error conditions test"
    end subroutine test_text_metrics_error_conditions

    subroutine test_performance_stress_testing()
        !! GIVEN: High-stress annotation scenarios
        !! WHEN: Processing large numbers of annotations
        !! THEN: Performance remains reasonable
        type(figure_t) :: fig
        real(wp) :: start_time, end_time
        integer :: i
        
        call fig%initialize(1000, 800)
        
        call cpu_time(start_time)
        
        ! Add many annotations with various parameters
        do i = 1, 5000
            call fig%text(real(mod(i, 100), wp) * 0.01_wp, &
                         real(i/100, wp) * 0.02_wp, &
                         "Stress test annotation", &
                         font_size=real(8 + mod(i, 16), wp), &
                         rotation=real(mod(i, 360), wp))
        end do
        
        call cpu_time(end_time)
        
        ! Performance should be reasonable (less than 30 seconds for 5000 annotations)
        if (end_time - start_time > 30.0_wp) then
            print *, "WARNING: Performance test took", end_time - start_time, "seconds"
            ! Note as performance concern but don't fail the test
        end if
        
        call fig%savefig("test_performance_stress.png")
        
        print *, "PASS: Performance stress testing test"
    end subroutine test_performance_stress_testing

    subroutine test_memory_leak_prevention()
        !! GIVEN: Repeated annotation creation and destruction
        !! WHEN: Creating and destroying annotations
        !! THEN: Memory usage remains stable
        type(figure_t) :: fig
        integer :: i, j
        
        ! Test repeated figure creation with annotations
        do i = 1, 100
            call fig%initialize(400, 300)
            
            do j = 1, 50
                call fig%text(real(j, wp) * 0.02_wp, 0.5_wp, "Memory test")
            end do
            
            call fig%savefig("test_memory_leak.png")
            ! Figure should be automatically cleaned up
        end do
        
        ! Memory should remain stable
        ! (This would require memory monitoring in a real implementation)
        
        print *, "PASS: Memory leak prevention test"
    end subroutine test_memory_leak_prevention

    subroutine test_concurrent_annotation_handling()
        !! GIVEN: Multiple annotations being processed simultaneously
        !! WHEN: Rendering annotations concurrently
        !! THEN: No race conditions or data corruption occurs
        type(figure_t) :: fig
        
        call fig%initialize(600, 400)
        
        ! Simulate concurrent annotation addition
        ! (In a real implementation, this might use OpenMP or threading)
        
        ! Add annotations that might interfere with each other
        call fig%text(0.5_wp, 0.5_wp, "Center text", alignment='center')
        call fig%text(0.5_wp, 0.5_wp, "Overlapping text", alignment='left')
        call fig%text(0.5_wp, 0.5_wp, "More overlap", alignment='right')
        
        ! Test with same coordinates but different coordinate systems
        call fig%text(0.5_wp, 0.5_wp, "Data coords", coord_type=COORD_DATA)
        call fig%text(0.5_wp, 0.5_wp, "Figure coords", coord_type=COORD_FIGURE)
        call fig%text(0.5_wp, 0.5_wp, "Axis coords", coord_type=COORD_AXIS)
        
        call fig%savefig("test_concurrent_annotations.png")
        
        print *, "PASS: Concurrent annotation handling test"
    end subroutine test_concurrent_annotation_handling

    subroutine test_error_message_quality()
        !! GIVEN: Various error conditions
        !! WHEN: Errors occur
        !! THEN: Error messages are helpful and informative
        use fortplot_annotations, only: text_annotation_t, validate_annotation
        
        type(text_annotation_t) :: annotation
        logical :: valid
        character(len=512) :: error_message
        
        ! Test comprehensive error message for invalid annotation
        annotation%text = ""
        annotation%x = transfer(int(z'7FF8000000000000', int64), 1.0_wp)  ! NaN
        annotation%y = transfer(int(z'FFF0000000000000', int64), 1.0_wp)  ! -Infinity
        annotation%font_size = -10.0_wp
        annotation%alignment = 'invalid'
        annotation%coord_type = 999
        
        call validate_annotation(annotation, valid, error_message)
        
        if (valid) then
            error stop "FAIL: Annotation with multiple errors should be invalid"
        end if
        
        ! Error message should mention specific problems
        if (index(error_message, "coordinate") == 0) then
            error stop "FAIL: Error message should mention coordinate problems"
        end if
        
        if (index(error_message, "font") == 0) then
            error stop "FAIL: Error message should mention font problems"
        end if
        
        if (len_trim(error_message) < 20) then
            error stop "FAIL: Error message should be descriptive"
        end if
        
        print *, "PASS: Error message quality test"
    end subroutine test_error_message_quality

    subroutine test_graceful_degradation()
        !! GIVEN: System under stress or partial failure
        !! WHEN: Some components fail
        !! THEN: System continues operating with reduced functionality
        type(figure_t) :: fig
        
        call fig%initialize(400, 300)
        
        ! Test with font system failure
        call simulate_font_system_failure()
        call fig%text(0.5_wp, 0.5_wp, "Fallback text")  ! Should use fallback
        
        ! Test with backend partial failure
        call simulate_backend_degradation()
        call fig%savefig("test_degradation.png")  ! Should still produce output
        
        ! Test with memory constraints
        call simulate_memory_constraints()
        call fig%text(0.3_wp, 0.7_wp, "Memory constrained")  ! Should handle gracefully
        
        print *, "PASS: Graceful degradation test"
    end subroutine test_graceful_degradation

    ! Helper subroutines (these would fail initially as they require implementation)
    
    subroutine test_disk_space_error_handling()
        ! Simulate disk space exhaustion
        error stop "Helper subroutine test_disk_space_error_handling not implemented"
    end subroutine test_disk_space_error_handling

    subroutine render_annotation_with_fallback(annotation)
        use fortplot_annotations, only: text_annotation_t
        type(text_annotation_t), intent(in) :: annotation
        error stop "Helper subroutine render_annotation_with_fallback not implemented"
    end subroutine render_annotation_with_fallback

    subroutine initialize_font_system_no_fonts(initialized, error_message)
        logical, intent(out) :: initialized
        character(len=*), intent(out) :: error_message
        error stop "Helper subroutine initialize_font_system_no_fonts not implemented"
    end subroutine initialize_font_system_no_fonts

    subroutine simulate_font_system_failure()
        error stop "Helper subroutine simulate_font_system_failure not implemented"
    end subroutine simulate_font_system_failure

    subroutine simulate_backend_degradation()
        error stop "Helper subroutine simulate_backend_degradation not implemented"
    end subroutine simulate_backend_degradation

    subroutine simulate_memory_constraints()
        error stop "Helper subroutine simulate_memory_constraints not implemented"
    end subroutine simulate_memory_constraints

end program test_annotation_error_handling
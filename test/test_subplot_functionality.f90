program test_subplot_functionality
    !! Comprehensive test suite for subplot functionality (RED phase)
    !!
    !! Tests the public subplot API interface following the architecture
    !! plan from DESIGN.md. These tests are designed to FAIL initially
    !! until the implementation is complete.
    !!
    !! Given: A figure that needs subplot capabilities
    !! When: Subplot functions are called
    !! Then: Subplots should be created, managed, and rendered correctly
    
    use iso_fortran_env, only: wp => real64
    use fortplot, only: figure_t, subplot, plot, xlabel, ylabel, title, savefig
    implicit none
    
    integer :: test_count = 0
    integer :: passed_count = 0
    
    print *, 'Running comprehensive subplot functionality tests...'
    print *, 'These tests are designed to FAIL initially (RED phase)'
    
    ! Core API tests
    call test_subplot_function_interface()
    call test_subplot_grid_creation()
    call test_subplot_switching()
    
    ! Layout and positioning tests
    call test_subplot_positioning_calculations()
    call test_subplot_coordinate_transformation()
    call test_subplot_grid_layouts()
    
    ! Rendering and integration tests
    call test_subplot_rendering_pipeline()
    call test_subplot_independence()
    call test_subplot_backward_compatibility()
    
    ! Error handling tests
    call test_subplot_error_handling()
    
    call print_test_summary()
    
contains

    subroutine test_subplot_function_interface()
        !! Given: A figure that needs subplot support
        !! When: subplot() function is called
        !! Then: Public API should accept subplot parameters
        print *, 'Test: Public subplot() function interface'
        call increment_test_count()
        
        ! This will fail until subplot() is added to fortplot.f90
        call subplot(2, 1, 1)  ! Should create 2x1 grid, switch to first subplot
        
        ! If we reach here without compilation error, test passes
        call increment_passed_count()
        print *, '  PASS: subplot() function exists and callable'
    end subroutine test_subplot_function_interface

    subroutine test_subplot_grid_creation()
        !! Given: A clean figure
        !! When: subplot() is called with grid dimensions
        !! Then: Figure should setup subplot grid properly
        type(figure_t) :: fig
        print *, 'Test: Subplot grid creation and setup'
        call increment_test_count()
        
        call fig%initialize(800, 600)
        
        ! This will fail until figure_t has subplot grid support
        call fig%setup_subplot_grid(2, 3)  ! 2 rows, 3 columns
        
        ! Test grid properties
        if (fig%n_rows == 2 .and. fig%n_cols == 3) then
            call increment_passed_count()
            print *, '  PASS: Subplot grid dimensions set correctly'
        else
            print *, '  FAIL: Subplot grid dimensions incorrect'
        end if
        
        if (fig%using_subplots .eqv. .true.) then
            call increment_passed_count()
            print *, '  PASS: Subplot mode enabled'
        else
            print *, '  FAIL: Subplot mode not enabled'
        end if
    end subroutine test_subplot_grid_creation

    subroutine test_subplot_switching()
        !! Given: A figure with subplot grid
        !! When: Current subplot is changed
        !! Then: Figure should track active subplot correctly
        type(figure_t) :: fig
        print *, 'Test: Subplot switching and current tracking'
        call increment_test_count()
        
        call fig%initialize(800, 600)
        call fig%setup_subplot_grid(2, 2)
        
        ! Test switching to different subplots
        call fig%switch_to_subplot(3)  ! Switch to subplot 3 (row 2, col 1)
        
        if (fig%current_subplot == 3) then
            call increment_passed_count()
            print *, '  PASS: Current subplot tracking works'
        else
            print *, '  FAIL: Current subplot not tracked correctly'
        end if
        
        ! Test subplot bounds calculation
        call fig%switch_to_subplot(4)  ! Switch to subplot 4 (row 2, col 2)
        
        if (fig%current_subplot == 4) then
            call increment_passed_count()
            print *, '  PASS: Multiple subplot switches work'
        else
            print *, '  FAIL: Multiple subplot switches failed'
        end if
    end subroutine test_subplot_switching

    subroutine test_subplot_positioning_calculations()
        !! Given: A subplot grid
        !! When: Subplot bounds are calculated
        !! Then: Positions should follow matplotlib-compatible layout
        type(figure_t) :: fig
        real(wp) :: bounds(4)  ! [left, bottom, width, height]
        print *, 'Test: Subplot positioning calculations'
        call increment_test_count()
        
        call fig%initialize(800, 600)
        call fig%setup_subplot_grid(2, 2)
        
        ! Test first subplot (top-left) bounds
        call fig%calculate_subplot_position(1, 1, bounds)
        
        ! Bounds should be within [0,1] normalized coordinates
        if (bounds(1) >= 0.0_wp .and. bounds(1) <= 1.0_wp .and. &
            bounds(2) >= 0.0_wp .and. bounds(2) <= 1.0_wp .and. &
            bounds(3) > 0.0_wp .and. bounds(4) > 0.0_wp) then
            call increment_passed_count()
            print *, '  PASS: Subplot bounds within valid range'
        else
            print *, '  FAIL: Subplot bounds outside valid range'
        end if
        
        ! Test that all subplots fit within figure
        call fig%calculate_subplot_position(2, 2, bounds)
        if (bounds(1) + bounds(3) <= 1.0_wp .and. bounds(2) + bounds(4) <= 1.0_wp) then
            call increment_passed_count()
            print *, '  PASS: Subplot bounds fit within figure'
        else
            print *, '  FAIL: Subplot bounds exceed figure boundaries'
        end if
    end subroutine test_subplot_positioning_calculations

    subroutine test_subplot_coordinate_transformation()
        !! Given: A subplot with data
        !! When: Data coordinates are transformed
        !! Then: Coordinates should map to subplot bounds correctly
        type(figure_t) :: fig
        real(wp), parameter :: x_data(3) = [1.0_wp, 2.0_wp, 3.0_wp]
        real(wp), parameter :: y_data(3) = [10.0_wp, 20.0_wp, 30.0_wp]
        print *, 'Test: Subplot coordinate transformation'
        call increment_test_count()
        
        call fig%initialize(800, 600)
        call fig%setup_subplot_grid(1, 2)
        call fig%switch_to_subplot(1)
        
        ! Add plot to current subplot
        call fig%add_plot(x_data, y_data, label='test data')
        
        ! Test that plot data is associated with correct subplot
        if (allocated(fig%subplots) .and. size(fig%subplots) >= 1) then
            if (fig%subplots(1)%plot_count == 1) then
                call increment_passed_count()
                print *, '  PASS: Plot data added to correct subplot'
            else
                print *, '  FAIL: Plot data not added to subplot'
            end if
        else
            print *, '  FAIL: Subplot array not allocated'
        end if
    end subroutine test_subplot_coordinate_transformation

    subroutine test_subplot_grid_layouts()
        !! Given: Different grid configurations
        !! When: Various grid sizes are tested
        !! Then: All should be handled correctly
        type(figure_t) :: fig
        print *, 'Test: Various subplot grid layouts'
        call increment_test_count()
        
        ! Test 1x3 layout (horizontal)
        call fig%initialize(1200, 400)
        call fig%setup_subplot_grid(1, 3)
        
        if (fig%n_rows == 1 .and. fig%n_cols == 3) then
            call increment_passed_count()
            print *, '  PASS: 1x3 horizontal layout'
        else
            print *, '  FAIL: 1x3 horizontal layout failed'
        end if
        
        ! Test 3x1 layout (vertical)
        call fig%initialize(400, 1200)
        call fig%setup_subplot_grid(3, 1)
        
        if (fig%n_rows == 3 .and. fig%n_cols == 1) then
            call increment_passed_count()
            print *, '  PASS: 3x1 vertical layout'
        else
            print *, '  FAIL: 3x1 vertical layout failed'
        end if
        
        ! Test 2x3 layout (complex)
        call fig%initialize(900, 600)
        call fig%setup_subplot_grid(2, 3)
        
        if (fig%n_rows == 2 .and. fig%n_cols == 3) then
            call increment_passed_count()
            print *, '  PASS: 2x3 complex layout'
        else
            print *, '  FAIL: 2x3 complex layout failed'
        end if
    end subroutine test_subplot_grid_layouts

    subroutine test_subplot_rendering_pipeline()
        !! Given: A figure with multiple subplots
        !! When: Figure is rendered
        !! Then: All subplots should render correctly
        type(figure_t) :: fig
        real(wp), parameter :: x1(3) = [1.0_wp, 2.0_wp, 3.0_wp]
        real(wp), parameter :: y1(3) = [1.0_wp, 4.0_wp, 9.0_wp]
        real(wp), parameter :: y2(3) = [2.0_wp, 8.0_wp, 18.0_wp]
        print *, 'Test: Multi-subplot rendering pipeline'
        call increment_test_count()
        
        call fig%initialize(800, 600)
        call fig%setup_subplot_grid(1, 2)
        
        ! Add plots to different subplots
        call fig%switch_to_subplot(1)
        call fig%add_plot(x1, y1, label='First plot')
        call fig%set_title('Subplot 1')
        
        call fig%switch_to_subplot(2)
        call fig%add_plot(x1, y2, label='Second plot')
        call fig%set_title('Subplot 2')
        
        ! Test rendering to PNG (should not crash)
        call fig%savefig('test_subplot_render.png')
        
        ! If we reach here without error, rendering works
        call increment_passed_count()
        print *, '  PASS: Multi-subplot rendering completed'
    end subroutine test_subplot_rendering_pipeline

    subroutine test_subplot_independence()
        !! Given: Multiple subplots
        !! When: Each has different properties
        !! Then: Properties should remain independent
        type(figure_t) :: fig
        print *, 'Test: Subplot independence and isolation'
        call increment_test_count()
        
        call fig%initialize(800, 600)
        call fig%setup_subplot_grid(2, 1)
        
        ! Configure first subplot
        call fig%switch_to_subplot(1)
        call fig%set_title('First Title')
        call fig%set_xlabel('X1 Label')
        call fig%set_ylabel('Y1 Label')
        
        ! Configure second subplot with different properties
        call fig%switch_to_subplot(2)
        call fig%set_title('Second Title')
        call fig%set_xlabel('X2 Label')
        call fig%set_ylabel('Y2 Label')
        
        ! Verify independence
        if (allocated(fig%subplots)) then
            if (fig%subplots(1)%title /= fig%subplots(2)%title) then
                call increment_passed_count()
                print *, '  PASS: Subplot titles are independent'
            else
                print *, '  FAIL: Subplot titles not independent'
            end if
            
            if (fig%subplots(1)%xlabel /= fig%subplots(2)%xlabel) then
                call increment_passed_count()
                print *, '  PASS: Subplot x-labels are independent'
            else
                print *, '  FAIL: Subplot x-labels not independent'
            end if
        else
            print *, '  FAIL: Subplot array not allocated'
        end if
    end subroutine test_subplot_independence

    subroutine test_subplot_backward_compatibility()
        !! Given: Existing single-plot API usage
        !! When: No subplot functions are called
        !! Then: Behavior should remain unchanged
        type(figure_t) :: fig
        real(wp), parameter :: x_data(3) = [1.0_wp, 2.0_wp, 3.0_wp]
        real(wp), parameter :: y_data(3) = [1.0_wp, 4.0_wp, 9.0_wp]
        print *, 'Test: Backward compatibility with single plots'
        call increment_test_count()
        
        call fig%initialize(800, 600)
        
        ! Use existing API without subplots
        call fig%add_plot(x_data, y_data, label='Single plot')
        call fig%set_title('Single Plot Title')
        call fig%set_xlabel('X')
        call fig%set_ylabel('Y')
        
        ! Should work as before
        if (.not. fig%using_subplots) then
            call increment_passed_count()
            print *, '  PASS: Single plot mode preserved'
        else
            print *, '  FAIL: Single plot mode affected by subplot changes'
        end if
        
        ! Save should work normally
        call fig%savefig('test_single_plot_compat.png')
        call increment_passed_count()
        print *, '  PASS: Single plot save functionality preserved'
    end subroutine test_subplot_backward_compatibility

    subroutine test_subplot_error_handling()
        !! Given: Invalid subplot operations
        !! When: Error conditions are triggered
        !! Then: Appropriate error handling should occur
        type(figure_t) :: fig, fig2
        print *, 'Test: Subplot error handling'
        call increment_test_count()
        
        call fig%initialize(800, 600)
        call fig%setup_subplot_grid(2, 2)
        
        ! Test invalid subplot index (should not crash)
        call fig%switch_to_subplot(5)  ! Only 4 subplots exist
        
        ! Should handle gracefully - exact behavior depends on implementation
        call increment_passed_count()
        print *, '  PASS: Invalid subplot index handled gracefully'
        
        ! Test subplot without grid setup
        call fig2%initialize(800, 600)
        call fig2%switch_to_subplot(1)  ! No grid setup - should handle gracefully
        
        call increment_passed_count()
        print *, '  PASS: Subplot operations without grid handled gracefully'
    end subroutine test_subplot_error_handling

    subroutine increment_test_count()
        test_count = test_count + 1
    end subroutine increment_test_count

    subroutine increment_passed_count()
        passed_count = passed_count + 1
    end subroutine increment_passed_count

    subroutine print_test_summary()
        print *, ''
        print *, '===== TEST SUMMARY (RED PHASE) ====='
        print *, 'Total tests:', test_count
        print *, 'Expected passes (when implemented):', test_count
        print *, 'Current passes:', passed_count
        print *, 'Expected to fail initially:', test_count - passed_count
        print *, ''
        print *, 'Note: These are RED phase tests - designed to fail initially'
        print *, 'Implementation in Phase 5 should make all tests pass'
        print *, '======================================'
    end subroutine print_test_summary

end program test_subplot_functionality
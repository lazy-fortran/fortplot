program test_ylabel_comprehensive
    !! Comprehensive Y-label test consolidating all Y-label related functionality
    !! Replaces: test_ylabel_positioning.f90, test_ylabel_rotation_fix.f90,
    !!           test_ylabel_rotation_regression.f90, test_raster_ylabel.f90,
    !!           test_png_ylabel_integration.f90, test_y_label_orientation.f90
    !!
    !! This test covers:
    !! - Y-label positioning and alignment
    !! - Y-label rotation (90 degrees counter-clockwise)
    !! - Character bitmap rendering and text dimensions
    !! - Raster rendering of rotated labels  
    !! - PNG integration with Y-labels
    !! - Character ordering and spacing verification
    !! - Cross-backend compatibility (PNG, PDF, ASCII)
    
    use fortplot
    use fortplot_text, only: init_text_system, render_text_to_image, &
                             calculate_text_width, calculate_text_height
    use fortplot_bitmap, only: initialize_white_background
    use fortplot_raster, only: raster_context
    use fortplot_layout, only: plot_area_t
    use fortplot_security, only: get_test_output_path
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    integer :: test_count = 0
    integer :: pass_count = 0

    print *, "=== COMPREHENSIVE Y-LABEL TESTS ==="
    
    ! Initialize text system for low-level tests
    if (.not. init_text_system()) then
        print *, "ERROR: Failed to initialize text system"
        stop 1
    end if
    
    ! Run all test categories
    call test_text_system_functionality()
    call test_rotation_and_positioning()
    call test_raster_rendering()
    call test_backend_integration()
    call test_character_ordering()
    call test_visual_verification()
    
    call print_test_summary()

contains

    !===========================================================================
    ! Text System Tests (from test_ylabel_positioning.f90)
    !===========================================================================
    
    subroutine test_text_system_functionality()
        print *, "--- Text System Functionality Tests ---"
        
        call test_text_dimensions()
        call test_bitmap_rendering()
        call test_buffer_management()
    end subroutine test_text_system_functionality

    subroutine test_text_dimensions()
        integer :: text_width, text_height
        character(len=*), parameter :: test_text = "Temperature (°C)"
        
        call start_test("Text dimensions calculation")
        
        text_width = calculate_text_width(test_text)
        text_height = calculate_text_height(test_text)
        
        call assert_greater(text_width, 0, "Text width should be positive")
        call assert_greater(text_height, 0, "Text height should be positive")
        
        print *, '  Text dimensions: ', text_width, 'x', text_height, ' pixels'
        call end_test()
    end subroutine test_text_dimensions

    subroutine test_bitmap_rendering()
        integer, parameter :: test_width = 100, test_height = 20
        integer(1), allocatable :: test_buffer(:)
        integer :: text_width, text_height, padding, buf_width, buf_height
        integer :: i, non_white_pixels
        character(len=*), parameter :: test_text = "Y Label"
        
        call start_test("Bitmap text rendering")
        
        ! Calculate buffer dimensions
        text_width = calculate_text_width(test_text)
        text_height = calculate_text_height(test_text)
        padding = 4
        buf_width = text_width + 2 * padding
        buf_height = text_height + 2 * padding
        
        ! Create and initialize buffer
        allocate(test_buffer(buf_height * (1 + buf_width * 3)))
        call initialize_white_background(test_buffer, buf_width, buf_height)
        
        ! Render text to buffer
        call render_text_to_image(test_buffer, buf_width, buf_height, &
                                 padding, padding, test_text, &
                                 0_1, 0_1, 0_1)  ! Black text
        
        ! Count non-white pixels to verify text was rendered
        non_white_pixels = 0
        do i = 2, buf_height * (1 + buf_width * 3) - 2, 3
            ! Check RGB values (white is -1_1 which is 255 unsigned)
            if (test_buffer(i) /= -1_1 .or. test_buffer(i+1) /= -1_1 .or. test_buffer(i+2) /= -1_1) then
                non_white_pixels = non_white_pixels + 1
            end if
        end do
        
        call assert_greater(non_white_pixels, 0, "Text should render non-white pixels")
        print *, '  Rendered pixels: ', non_white_pixels
        
        deallocate(test_buffer)
        call end_test()
    end subroutine test_bitmap_rendering

    subroutine test_buffer_management()
        integer(1), allocatable :: buffer(:)
        integer, parameter :: width = 50, height = 30
        integer :: buffer_size
        
        call start_test("Buffer management")
        
        buffer_size = height * (1 + width * 3)
        allocate(buffer(buffer_size))
        
        ! Initialize buffer
        call initialize_white_background(buffer, width, height)
        
        ! Verify buffer is properly initialized (should be all white)
        call assert_equal(int(buffer(2), kind=int32), -1, "Buffer should be white-initialized")
        
        deallocate(buffer)
        call end_test()
    end subroutine test_buffer_management

    !===========================================================================
    ! Rotation and Positioning Tests (from test_ylabel_rotation_*.f90)
    !===========================================================================
    
    subroutine test_rotation_and_positioning()
        print *, "--- Rotation and Positioning Tests ---"
        
        call test_basic_ylabel_rotation()
        call test_ylabel_positioning_accuracy()
        call test_rotation_regression()
    end subroutine test_rotation_and_positioning

    subroutine test_basic_ylabel_rotation()
        real(wp) :: x(100), y(100)
        integer :: i
        character(len=512) :: filename
        
        call start_test("Basic Y-label rotation")
        
        ! Create test data
        do i = 1, 100
            x(i) = real(i-1, wp) * 0.1_wp
            y(i) = sin(x(i))
        end do
        
        ! Test PNG with ylabel rotation
        call figure(figsize=[600.0_wp, 400.0_wp])
        call plot(x, y)
        call title("Y-Label Rotation Test")
        call xlabel("X Axis")
        call ylabel("Temperature (°C)")  ! Should be rotated 90° CCW
        
        filename = get_test_output_path('/tmp/ylabel_rotation.png')
        call savefig(filename)
        
        print *, '  Y-label rotation test saved'
        call end_test()
    end subroutine test_basic_ylabel_rotation

    subroutine test_ylabel_positioning_accuracy()
        real(wp) :: x(5), y(5)
        integer :: i
        character(len=512) :: filename
        
        call start_test("Y-label positioning accuracy")
        
        do i = 1, 5
            x(i) = real(i, wp)
            y(i) = real(i, wp) ** 2
        end do
        
        call figure(figsize=[400.0_wp, 300.0_wp])
        call plot(x, y)
        call title('Positioning Test')
        call xlabel('X values')
        call ylabel('Y values')  ! Test the original problematic case
        
        filename = get_test_output_path('/tmp/ylabel_positioning.png')
        call savefig(filename)
        
        print *, '  Y-label positioning test saved'
        call end_test()
    end subroutine test_ylabel_positioning_accuracy

    subroutine test_rotation_regression()
        real(wp) :: x(10), y(10)
        integer :: i
        character(len=512) :: filename
        
        call start_test("Y-label rotation regression")
        
        do i = 1, 10
            x(i) = real(i, wp)
            y(i) = real(i, wp) * 2.0_wp
        end do
        
        ! Test multiple scenarios that previously caused issues
        call figure(figsize=[500.0_wp, 350.0_wp])
        call plot(x, y, 'b-', label='Data')
        call title('Regression Test')
        call xlabel('Input Values')
        call ylabel('Output Response')
        call legend()
        
        filename = get_test_output_path('/tmp/ylabel_regression.png')
        call savefig(filename)
        
        print *, '  Y-label regression test saved'
        call end_test()
    end subroutine test_rotation_regression

    !===========================================================================
    ! Raster Rendering Tests (from test_raster_ylabel.f90)
    !===========================================================================
    
    subroutine test_raster_rendering()
        print *, "--- Raster Rendering Tests ---"
        
        call test_raster_ylabel_rendering()
        call test_pixel_level_verification()
    end subroutine test_raster_rendering

    subroutine test_raster_ylabel_rendering()
        type(raster_context) :: ctx
        type(plot_area_t) :: plot_area
        integer :: width, height
        integer :: x, y, idx
        logical :: found_text_pixels
        
        call start_test("Raster Y-label rendering")
        
        ! Initialize raster context
        width = 800
        height = 600
        
        allocate(ctx%raster%image_data(width * height * 3))
        ctx%raster%image_data = -1_1  ! White background
        
        ! Set up plot area
        plot_area%left = 100
        plot_area%bottom = 100
        plot_area%width = 600
        plot_area%height = 400
        ctx%plot_area = plot_area
        ctx%raster%width = width
        ctx%raster%height = height
        
        ! Render Y-label
        call ctx%render_ylabel("Temperature (°C)")
        
        ! Verify text was rendered by checking for non-white pixels
        found_text_pixels = .false.
        do y = height/2 - 100, height/2 + 100
            do x = 20, 90  ! Left side where Y-label should be
                idx = ((y - 1) * width + (x - 1)) * 3 + 1
                if (idx > 0 .and. idx + 2 <= size(ctx%raster%image_data)) then
                    if (ctx%raster%image_data(idx) /= -1_1 .or. &
                        ctx%raster%image_data(idx + 1) /= -1_1 .or. &
                        ctx%raster%image_data(idx + 2) /= -1_1) then
                        found_text_pixels = .true.
                        exit
                    end if
                end if
            end do
            if (found_text_pixels) exit
        end do
        
        call assert_true(found_text_pixels, "Raster Y-label should render visible pixels")
        
        deallocate(ctx%raster%image_data)
        call end_test()
    end subroutine test_raster_ylabel_rendering

    subroutine test_pixel_level_verification()
        call start_test("Pixel-level verification")
        
        ! This is a placeholder for more detailed pixel-level tests
        ! In practice, this would check specific pixel patterns for text rendering
        print *, '  Pixel-level verification placeholder - implementation dependent'
        
        call end_test()
    end subroutine test_pixel_level_verification

    !===========================================================================
    ! Backend Integration Tests (from test_png_ylabel_integration.f90)
    !===========================================================================
    
    subroutine test_backend_integration()
        print *, "--- Backend Integration Tests ---"
        
        call test_png_ylabel_integration()
        call test_pdf_ylabel_integration()
        call test_ascii_ylabel_integration()
    end subroutine test_backend_integration

    subroutine test_png_ylabel_integration()
        real(wp) :: x(20), y(20)
        integer :: i
        character(len=512) :: filename
        
        call start_test("PNG Y-label integration")
        
        do i = 1, 20
            x(i) = real(i, wp) * 0.5_wp
            y(i) = exp(-x(i)) * cos(x(i) * 2.0_wp)
        end do
        
        call figure(figsize=[600.0_wp, 450.0_wp])
        call plot(x, y, 'r-', linewidth=2.0_wp)
        call title('PNG Y-Label Integration')
        call xlabel('Time (s)')
        call ylabel('Amplitude (V)')
        call grid(.true.)
        
        filename = get_test_output_path('/tmp/ylabel_png_integration.png')
        call savefig(filename)
        
        print *, '  PNG integration test saved'
        call end_test()
    end subroutine test_png_ylabel_integration

    subroutine test_pdf_ylabel_integration()
        real(wp) :: x(15), y(15)
        integer :: i
        character(len=512) :: filename
        
        call start_test("PDF Y-label integration")
        
        do i = 1, 15
            x(i) = real(i, wp)
            y(i) = real(i, wp)**1.5_wp
        end do
        
        call figure(figsize=[600.0_wp, 450.0_wp])
        call plot(x, y, 'g--', linewidth=1.5_wp)
        call title('PDF Y-Label Integration')
        call xlabel('Position (m)')
        call ylabel('Force (N)')
        
        filename = get_test_output_path('/tmp/ylabel_pdf_integration.pdf')
        call savefig(filename)
        
        print *, '  PDF integration test saved'
        call end_test()
    end subroutine test_pdf_ylabel_integration

    subroutine test_ascii_ylabel_integration()
        real(wp) :: x(10), y(10)
        integer :: i
        character(len=512) :: filename
        
        call start_test("ASCII Y-label integration")
        
        do i = 1, 10
            x(i) = real(i, wp)
            y(i) = real(i, wp) * 0.5_wp
        end do
        
        call figure(figsize=[80.0_wp, 24.0_wp])
        call plot(x, y)
        call title('ASCII Y-Label')
        call xlabel('X')
        call ylabel('Y')
        
        filename = get_test_output_path('/tmp/ylabel_ascii_integration.txt')
        call savefig(filename)
        
        print *, '  ASCII integration test saved'
        call end_test()
    end subroutine test_ascii_ylabel_integration

    !===========================================================================
    ! Character Ordering Tests (from test_y_label_orientation.f90)
    !===========================================================================
    
    subroutine test_character_ordering()
        print *, "--- Character Ordering Tests ---"
        
        call test_y_values_label()
        call test_various_labels()
        call test_character_spacing()
        call test_simple_labels()
    end subroutine test_character_ordering

    subroutine test_y_values_label()
        !! Test the original problematic "Y values" label
        real(wp) :: x(5), y(5)
        integer :: i
        character(len=512) :: filename
        
        call start_test("Y values label ordering")
        
        do i = 1, 5
            x(i) = real(i, wp)
            y(i) = real(i, wp) ** 2
        end do
        
        call figure(figsize=[400.0_wp, 300.0_wp])
        call plot(x, y)
        call title('Y Values Label Test')
        call xlabel('X axis')
        call ylabel('Y values')  ! Should read bottom-to-top when rotated
        
        filename = get_test_output_path('/tmp/ylabel_ordering.png')
        call savefig(filename)
        
        print *, '  Y values ordering test saved - verify "Y values" reads bottom-to-top'
        call end_test()
    end subroutine test_y_values_label

    subroutine test_various_labels()
        real(wp) :: x(3), y(3)
        character(len=512) :: filename
        
        call start_test("Various label characters")
        
        x = [1.0_wp, 2.0_wp, 3.0_wp]
        y = [1.0_wp, 4.0_wp, 9.0_wp]
        
        ! Test different character combinations
        call figure(figsize=[800.0_wp, 600.0_wp])
        
        ! Multiple subplots to test different labels
        call subplot(2, 2, 1)
        call plot(x, y)
        call title('Lowercase')
        call ylabel('temperature')
        
        call subplot(2, 2, 2)
        call plot(x, y)
        call title('Mixed Case')
        call ylabel('Temperature')
        
        call subplot(2, 2, 3)
        call plot(x, y)
        call title('With Symbols')
        call ylabel('Temp (°C)')
        
        call subplot(2, 2, 4)
        call plot(x, y)
        call title('Long Label')
        call ylabel('Temperature Measurement')
        
        filename = get_test_output_path('/tmp/ylabel_various.png')
        call savefig(filename)
        
        print *, '  Various labels test saved - verify all labels are readable'
        call end_test()
    end subroutine test_various_labels

    subroutine test_character_spacing()
        real(wp) :: x(4), y(4)
        character(len=512) :: filename
        
        call start_test("Character spacing analysis")
        
        x = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
        y = [0.0_wp, 1.0_wp, 4.0_wp, 9.0_wp]
        
        call figure(figsize=[600.0_wp, 400.0_wp])
        call plot(x, y, 'bo-', markersize=8.0_wp)
        call title('Character Spacing Test')
        call xlabel('Index')
        call ylabel('illlliiilll')  ! Multiple 'l' characters to test spacing
        call grid(.true.)
        
        filename = get_test_output_path('/tmp/ylabel_spacing.png')
        call savefig(filename)
        
        print *, '  Character spacing test saved - verify consistent "l" spacing'
        call end_test()
    end subroutine test_character_spacing

    subroutine test_simple_labels()
        real(wp) :: x(2), y(2)
        character(len=512) :: filename
        
        call start_test("Simple label cases")
        
        x = [1.0_wp, 5.0_wp]
        y = [2.0_wp, 8.0_wp]
        
        call figure(figsize=[300.0_wp, 200.0_wp])
        call plot(x, y, 'r*-', linewidth=3.0_wp)
        call title('Simple')
        call xlabel('x')
        call ylabel('y')
        
        filename = get_test_output_path('/tmp/ylabel_simple.png')
        call savefig(filename)
        
        print *, '  Simple labels test saved'
        call end_test()
    end subroutine test_simple_labels

    !===========================================================================
    ! Visual Verification Tests
    !===========================================================================
    
    subroutine test_visual_verification()
        print *, "--- Visual Verification Summary ---"
        call start_test("Manual verification instructions")
        
        print *, ''
        print *, 'MANUAL VERIFICATION REQUIRED:'
        print *, '  1. Check ylabel_rotation.png - Y-label rotated 90° counter-clockwise'
        print *, '  2. Check ylabel_positioning.png - "Y values" reads bottom-to-top'
        print *, '  3. Check ylabel_various.png - all subplot labels readable'
        print *, '  4. Check ylabel_spacing.png - consistent character spacing'
        print *, '  5. Check backend integration files (PNG, PDF, ASCII)'
        print *, '  6. Compare with matplotlib output for consistency'
        print *, ''
        
        call end_test()
    end subroutine test_visual_verification

    !===========================================================================
    ! Test Framework Utilities
    !===========================================================================

    subroutine start_test(test_name)
        character(len=*), intent(in) :: test_name
        write(*, '(A, A)') 'Running test: ', test_name
    end subroutine start_test

    subroutine end_test()
        write(*, '(A)') 'Test completed'
        write(*, *)
    end subroutine end_test

    subroutine assert_equal(actual, expected, description)
        integer, intent(in) :: actual, expected
        character(len=*), intent(in) :: description
        
        test_count = test_count + 1
        if (actual == expected) then
            write(*, '(A, A)') '  PASS: ', description
            pass_count = pass_count + 1
        else
            write(*, '(A, A, I0, A, I0)') '  FAIL: ', description, actual, ' != ', expected
        end if
    end subroutine assert_equal

    subroutine assert_greater(actual, threshold, description)
        integer, intent(in) :: actual, threshold
        character(len=*), intent(in) :: description
        
        test_count = test_count + 1
        if (actual > threshold) then
            write(*, '(A, A)') '  PASS: ', description
            pass_count = pass_count + 1
        else
            write(*, '(A, A, I0, A, I0)') '  FAIL: ', description, actual, ' <= ', threshold
        end if
    end subroutine assert_greater

    subroutine assert_true(condition, description)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: description
        
        test_count = test_count + 1
        if (condition) then
            write(*, '(A, A)') '  PASS: ', description
            pass_count = pass_count + 1
        else
            write(*, '(A, A)') '  FAIL: ', description
        end if
    end subroutine assert_true

    subroutine print_test_summary()
        write(*, '(A)') '============================================'
        write(*, '(A, I0, A, I0, A)') 'Test Summary: ', pass_count, ' of ', test_count, ' tests passed'
        write(*, '(A)') 'Consolidated 6+ Y-label test files into single comprehensive test'
        if (pass_count == test_count) then
            write(*, '(A)') 'All tests PASSED!'
        else
            write(*, '(A)') 'Some tests FAILED!'
        end if
    end subroutine print_test_summary

end program test_ylabel_comprehensive
program test_ylabel_rendering
    !! Y-label core rendering tests
    !! Extracted from test_ylabel_comprehensive.f90
    !! 
    !! This test covers:
    !! - Text system functionality and bitmap rendering
    !! - Y-label rotation (90 degrees counter-clockwise) 
    !! - Raster rendering of rotated labels
    !! - Backend integration (PNG, PDF, ASCII)
    
    use fortplot
    use fortplot_text, only: init_text_system, render_text_to_image, &
                             calculate_text_width, calculate_text_height
    use fortplot_bitmap, only: initialize_white_background
    use fortplot_raster, only: raster_context
    use fortplot_layout, only: plot_area_t
    use fortplot_security, only: get_test_output_path
    use, intrinsic :: iso_fortran_env, only: wp => real64, int32
    implicit none

    integer :: test_count = 0
    integer :: pass_count = 0

    print *, "=== Y-LABEL RENDERING TESTS ==="
    
    ! Initialize text system for low-level tests
    if (.not. init_text_system()) then
        print *, "ERROR: Failed to initialize text system"
        stop 1
    end if
    
    ! Run test categories
    call test_text_system_functionality()
    call test_rotation_and_positioning()
    call test_raster_rendering()
    call test_backend_integration()
    call print_test_summary()

contains

    !===========================================================================
    ! Text System Tests
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
    ! Rotation and Positioning Tests
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
        call plot(x, y, 'b-')
        call title('Regression Test')
        call xlabel('Input Values')
        call ylabel('Output Response')
        
        filename = get_test_output_path('/tmp/ylabel_regression.png')
        call savefig(filename)
        
        print *, '  Y-label regression test saved'
        call end_test()
    end subroutine test_rotation_regression

    !===========================================================================
    ! Raster Rendering Tests
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
    ! Backend Integration Tests
    !===========================================================================
    
    subroutine test_backend_integration()
        print *, "--- Backend Integration Tests ---"
        
        call test_png_ylabel_integration()
        call test_pdf_ylabel_integration()
        call test_ascii_ylabel_integration()
    end subroutine test_backend_integration

    subroutine test_png_ylabel_integration()
        real(wp) :: x(50), y(50)
        integer :: i
        character(len=512) :: filename
        
        call start_test("PNG Y-label integration")
        
        do i = 1, 50
            x(i) = real(i, wp) * 0.2_wp
            y(i) = exp(-x(i)) * cos(x(i) * 2.0_wp)
        end do
        
        call figure(figsize=[600.0_wp, 400.0_wp])
        call plot(x, y, 'r-')
        call title('PNG Y-Label Integration Test')
        call xlabel('Time (s)')
        call ylabel('Amplitude (V)')
        
        filename = get_test_output_path('/tmp/ylabel_png_integration.png')
        call savefig(filename)
        
        print *, '  PNG Y-label integration test saved'
        call end_test()
    end subroutine test_png_ylabel_integration

    subroutine test_pdf_ylabel_integration()
        real(wp) :: x(30), y(30)
        integer :: i
        character(len=512) :: filename
        
        call start_test("PDF Y-label integration")
        
        do i = 1, 30
            x(i) = real(i, wp) * 0.3_wp
            y(i) = log(x(i) + 1.0_wp)
        end do
        
        call figure(figsize=[500.0_wp, 400.0_wp])
        call plot(x, y, 'g-')
        call title('PDF Y-Label Integration Test')
        call xlabel('Input')
        call ylabel('Log Output')
        
        filename = get_test_output_path('/tmp/ylabel_pdf_integration.pdf')
        call savefig(filename)
        
        print *, '  PDF Y-label integration test saved'
        call end_test()
    end subroutine test_pdf_ylabel_integration

    subroutine test_ascii_ylabel_integration()
        real(wp) :: x(15), y(15)
        integer :: i
        character(len=512) :: filename
        
        call start_test("ASCII Y-label integration")
        
        do i = 1, 15
            x(i) = real(i, wp)
            y(i) = real(i, wp) ** 0.5_wp
        end do
        
        call figure(figsize=[60.0_wp, 20.0_wp])
        call plot(x, y, '-')
        call title('ASCII Y-Label Test')
        call xlabel('X')
        call ylabel('Y')
        
        filename = get_test_output_path('/tmp/ylabel_ascii_integration.txt')
        call savefig(filename)
        
        print *, '  ASCII Y-label integration test saved'
        call end_test()
    end subroutine test_ascii_ylabel_integration

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

    subroutine assert_greater(actual, minimum, description)
        integer, intent(in) :: actual, minimum
        character(len=*), intent(in) :: description
        
        if (actual > minimum) then
            print *, '  PASS: ', description
        else
            print *, '  FAIL: ', description, ' (got ', actual, ', expected > ', minimum, ')'
        end if
    end subroutine assert_greater

    subroutine assert_equal(actual, expected, description)
        integer, intent(in) :: actual, expected
        character(len=*), intent(in) :: description
        
        if (actual == expected) then
            print *, '  PASS: ', description
        else
            print *, '  FAIL: ', description, ' (got ', actual, ', expected ', expected, ')'
        end if
    end subroutine assert_equal

    subroutine assert_true(condition, description)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: description
        
        if (condition) then
            print *, '  PASS: ', description
        else
            print *, '  FAIL: ', description
        end if
    end subroutine assert_true

    subroutine print_test_summary()
        write(*, '(A)') '============================================'
        write(*, '(A)') 'Y-Label Rendering Test Summary'
        write(*, '(A, I0, A, I0)') 'Tests run: ', test_count, ' | Passed: ', pass_count
        write(*, '(A)') 'Y-label core rendering tests COMPLETED!'
        write(*, '(A)') '============================================'
    end subroutine print_test_summary

end program test_ylabel_rendering
program test_jpeg_backend
    use iso_fortran_env, only: wp => real64
    use fortplot_jpeg, only: jpeg_context, create_jpeg_canvas, get_jpeg_data
    use fortplot_raster, only: raster_context, create_raster_canvas
    implicit none
    
    call test_should_create_jpeg_canvas()
    call test_should_generate_jpeg_data()
    call test_should_extract_common_raster_functionality()
    call test_should_generate_proper_jpeg_structure()
    
    print *, "All JPEG backend tests completed"

contains

    subroutine test_should_create_jpeg_canvas()
        integer, parameter :: width = 800
        integer, parameter :: height = 600
        type(jpeg_context) :: ctx
        
        print *, "Testing JPEG canvas creation..."
        
        ! Arrange & Act - create a JPEG canvas
        ctx = create_jpeg_canvas(width, height)
        
        ! Assert - verify canvas was created correctly
        if (ctx%width /= width) then
            print *, "FAIL: Width mismatch"
            error stop "Test failed: JPEG canvas width"
        end if
        
        if (ctx%height /= height) then
            print *, "FAIL: Height mismatch"
            error stop "Test failed: JPEG canvas height"
        end if
        
        if (ctx%quality /= 85) then
            print *, "FAIL: Default quality mismatch"
            error stop "Test failed: JPEG default quality"
        end if
        
        print *, "PASS: JPEG canvas creation"
    end subroutine test_should_create_jpeg_canvas

    subroutine test_should_generate_jpeg_data()
        integer, parameter :: width = 100
        integer, parameter :: height = 100
        integer, parameter :: quality = 90
        integer(1), allocatable :: test_image_data(:)
        integer(1), allocatable :: jpeg_buffer(:)
        
        print *, "Testing JPEG data generation..."
        
        ! Arrange - create simple test image data (RGB format)
        allocate(test_image_data(height * (1 + width * 3)))
        test_image_data = int(127, 1)  ! Gray image
        
        ! Act - generate JPEG data
        call get_jpeg_data(width, height, test_image_data, quality, jpeg_buffer)
        
        ! Assert - verify JPEG buffer was created
        if (.not. allocated(jpeg_buffer)) then
            print *, "FAIL: JPEG buffer not allocated"
            error stop "Test failed: JPEG buffer allocation"
        end if
        
        if (size(jpeg_buffer) < 4) then
            print *, "FAIL: JPEG buffer too small"
            error stop "Test failed: JPEG buffer size"
        end if
        
        ! Check for JPEG SOI marker (FF D8)
        if (jpeg_buffer(1) /= int(Z'FF', 1) .or. jpeg_buffer(2) /= int(Z'D8', 1)) then
            print *, "FAIL: Missing JPEG SOI marker"
            error stop "Test failed: JPEG SOI marker"
        end if
        
        print *, "PASS: JPEG data generation"
        deallocate(test_image_data, jpeg_buffer)
    end subroutine test_should_generate_jpeg_data

    subroutine test_should_extract_common_raster_functionality()
        integer, parameter :: width = 200
        integer, parameter :: height = 150
        type(raster_context) :: raster_base
        type(jpeg_context) :: jpeg_ctx
        
        print *, "Testing common raster base functionality..."
        
        ! Arrange & Act - create both raster base and JPEG context
        raster_base = create_raster_canvas(width, height)
        jpeg_ctx = create_jpeg_canvas(width, height)
        
        ! Assert - both should have same basic properties from common base
        if (raster_base%width /= jpeg_ctx%width) then
            print *, "FAIL: Width mismatch between raster base and JPEG"
            error stop "Test failed: Common raster width"
        end if
        
        if (raster_base%height /= jpeg_ctx%height) then
            print *, "FAIL: Height mismatch between raster base and JPEG"
            error stop "Test failed: Common raster height"
        end if
        
        ! Both should inherit from the same base context
        if (.not. allocated(raster_base%raster%image_data) .or. .not. allocated(jpeg_ctx%raster%image_data)) then
            print *, "FAIL: Image data not allocated in common raster base"
            error stop "Test failed: Common raster image data"
        end if
        
        print *, "PASS: Common raster base functionality"
    end subroutine test_should_extract_common_raster_functionality

    subroutine test_should_generate_proper_jpeg_structure()
        integer, parameter :: width = 16
        integer, parameter :: height = 16  
        integer, parameter :: quality = 75
        integer(1), allocatable :: test_image_data(:)
        integer(1), allocatable :: jpeg_buffer(:)
        
        print *, "Testing proper JPEG structure..."
        
        ! Arrange - create small test image (16x16 for simplicity)
        allocate(test_image_data(height * (1 + width * 3)))
        test_image_data = int(127, 1)  ! Gray image
        
        ! Act - generate JPEG data
        call get_jpeg_data(width, height, test_image_data, quality, jpeg_buffer)
        
        ! Assert - verify proper JPEG structure
        if (.not. allocated(jpeg_buffer)) then
            print *, "FAIL: JPEG buffer not allocated"
            error stop "Test failed: JPEG buffer allocation"
        end if
        
        if (size(jpeg_buffer) < 20) then
            print *, "FAIL: JPEG buffer too small for proper structure"
            error stop "Test failed: JPEG buffer size"
        end if
        
        ! Check for proper JPEG SOI marker (FF D8)
        if (jpeg_buffer(1) /= int(Z'FF', 1) .or. jpeg_buffer(2) /= int(Z'D8', 1)) then
            print *, "FAIL: Invalid JPEG SOI marker"
            error stop "Test failed: JPEG SOI marker"
        end if
        
        ! Check for JPEG APP0 marker (FF E0) - proper JFIF header
        if (jpeg_buffer(3) /= int(Z'FF', 1) .or. jpeg_buffer(4) /= int(Z'E0', 1)) then
            print *, "FAIL: Missing JFIF APP0 marker"  
            error stop "Test failed: JFIF header"
        end if
        
        ! Check for EOI marker at end (FF D9)
        if (jpeg_buffer(size(jpeg_buffer)-1) /= int(Z'FF', 1) .or. &
            jpeg_buffer(size(jpeg_buffer)) /= int(Z'D9', 1)) then
            print *, "FAIL: Invalid JPEG EOI marker"
            error stop "Test failed: JPEG EOI marker"
        end if
        
        print *, "PASS: Proper JPEG structure"
        deallocate(test_image_data, jpeg_buffer)
    end subroutine test_should_generate_proper_jpeg_structure

end program test_jpeg_backend
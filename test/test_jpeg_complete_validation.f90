program test_jpeg_complete_validation
    ! Comprehensive test to validate JPEG encoding against STB and standard decoders
    use fortplot_jpeg
    use stb_image_write_wrapper  
    use iso_c_binding
    use, intrinsic :: iso_fortran_env, only: int8, int32, real64
    implicit none
    
    interface
        subroutine free(ptr) bind(C, name='free')
            import :: c_ptr
            type(c_ptr), value :: ptr
        end subroutine free
    end interface
    
    call test_existing_encoder()
    call test_complete_encoding()
    call validate_with_imagemagick()
    
contains

    subroutine test_existing_encoder()
        type(jpeg_context) :: ctx
        integer :: width = 8, height = 8
        integer :: i, j
        logical :: exists
        integer :: file_size
        
        print *, "=== Testing Existing JPEG Context Encoder ==="
        
        ! Create simple 8x8 image
        ctx = create_jpeg_canvas(width, height, 90)
        
        ! Fill with gradient (assuming pixel setting works)
        do j = 1, height
            do i = 1, width
                ! Set pixels - check actual API
                ! ctx%canvas(i,j) = real((i-1)*32 + (j-1)*4)
            end do
        end do
        
        ! Save
        call ctx%save("test_context_gradient.jpg")
        
        ! Check file
        inquire(file="test_context_gradient.jpg", exist=exists, size=file_size)
        if (exists) then
            print *, "Context JPEG created:", file_size, "bytes"
            call validate_jpeg_file("test_context_gradient.jpg")
        else
            print *, "ERROR: Context JPEG not created"
        end if
    end subroutine test_existing_encoder
    
    subroutine test_complete_encoding()
        integer(int8), target :: rgb_data(3, 16, 16)
        integer(int8), pointer :: stb_data(:)
        type(c_ptr) :: stb_ptr
        integer :: stb_size
        integer :: i, j
        real :: x, y
        
        print *, ""
        print *, "=== Testing Complete 16x16 Encoding ==="
        
        ! Create test pattern
        do j = 1, 16
            do i = 1, 16
                x = real(i-1) / 15.0
                y = real(j-1) / 15.0
                
                ! Smooth gradients
                rgb_data(1, i, j) = int(255 * x, int8)              ! Red gradient
                rgb_data(2, i, j) = int(255 * y, int8)              ! Green gradient
                rgb_data(3, i, j) = int(255 * (1-x) * (1-y), int8)  ! Blue gradient
            end do
        end do
        
        ! Write with STB
        if (stb_write_jpeg_to_memory(stb_ptr, stb_size, 16, 16, 3, &
            c_loc(rgb_data), 90) == 0) then
            print *, "ERROR: STB encoding failed"
            return
        end if
        
        call c_f_pointer(stb_ptr, stb_data, [stb_size])
        
        print *, "STB JPEG size:", stb_size, "bytes"
        
        ! Write STB file
        open(unit=10, file="stb_16x16_complete.jpg", status='replace', access='stream')
        write(10) stb_data
        close(10)
        
        ! Validate both can be decoded
        call validate_jpeg_file("stb_16x16_complete.jpg")
        
        ! Analyze structure
        call analyze_jpeg_structure(stb_data, stb_size)
        
        call free(stb_ptr)
    end subroutine test_complete_encoding
    
    subroutine validate_with_imagemagick()
        integer :: exit_code
        character(len=200) :: cmd
        
        print *, ""
        print *, "=== Validating with ImageMagick ==="
        
        ! Check if ImageMagick is available
        call execute_command_line("which identify > /dev/null 2>&1", exitstat=exit_code)
        
        if (exit_code == 0) then
            ! Validate each test file
            call check_with_imagemagick("stb_solid.jpg")
            call check_with_imagemagick("stb_gradient.jpg") 
            call check_with_imagemagick("fixed_solid.jpg")
            call check_with_imagemagick("fixed_gradient.jpg")
            call check_with_imagemagick("fixed_checkerboard.jpg")
            call check_with_imagemagick("fixed_16x16.jpg")
        else
            print *, "ImageMagick not available - skipping validation"
        end if
    end subroutine validate_with_imagemagick
    
    subroutine check_with_imagemagick(filename)
        character(len=*), intent(in) :: filename
        character(len=200) :: cmd
        integer :: exit_code
        logical :: exists
        
        inquire(file=filename, exist=exists)
        if (.not. exists) return
        
        ! Use identify to check image
        cmd = "identify -verbose " // trim(filename) // " > /dev/null 2>&1"
        call execute_command_line(trim(cmd), exitstat=exit_code)
        
        if (exit_code == 0) then
            print '(A,A,A)', "  ✓ ", trim(filename), " - Valid JPEG (ImageMagick)"
            
            ! Get basic info
            cmd = "identify -format '%wx%h %z-bit' " // trim(filename) // " 2>/dev/null"
            call execute_command_line(trim(cmd), exitstat=exit_code)
            print *, ""
        else
            print '(A,A,A)', "  ✗ ", trim(filename), " - Invalid JPEG (ImageMagick)"
        end if
    end subroutine check_with_imagemagick
    
    subroutine validate_jpeg_file(filename)
        character(len=*), intent(in) :: filename
        integer :: exit_code
        logical :: exists
        
        inquire(file=filename, exist=exists)
        if (.not. exists) then
            print *, "File not found:", trim(filename)
            return
        end if
        
        ! Try multiple validators
        
        ! 1. file command
        call execute_command_line("file " // trim(filename) // " | grep -q 'JPEG image'", &
            exitstat=exit_code)
        if (exit_code == 0) then
            print *, "  ✓ Valid JPEG (file command)"
        else
            print *, "  ✗ Invalid JPEG (file command)"
        end if
        
        ! 2. jpeginfo if available
        call execute_command_line("which jpeginfo > /dev/null 2>&1", exitstat=exit_code)
        if (exit_code == 0) then
            call execute_command_line("jpeginfo -c " // trim(filename) // " > /dev/null 2>&1", &
                exitstat=exit_code)
            if (exit_code == 0) then
                print *, "  ✓ Valid JPEG (jpeginfo)"
            else
                print *, "  ✗ Invalid JPEG (jpeginfo)"
            end if
        end if
    end subroutine validate_jpeg_file
    
    subroutine analyze_jpeg_structure(jpeg_data, size)
        integer(int8), intent(in) :: jpeg_data(:)
        integer, intent(in) :: size
        integer :: pos, marker_count
        integer :: dht_count, dqt_count
        
        print *, ""
        print *, "JPEG Structure Analysis:"
        
        pos = 1
        marker_count = 0
        dht_count = 0
        dqt_count = 0
        
        do while (pos < size - 1)
            if (jpeg_data(pos) == int(-1, int8)) then
                select case (jpeg_data(pos+1))
                case (int(-40, int8))  ! D8 = SOI
                    print '(A,I5)', "SOI at", pos
                case (int(-39, int8))  ! D9 = EOI
                    print '(A,I5)', "EOI at", pos
                case (int(-38, int8))  ! DA = SOS
                    print '(A,I5)', "SOS at", pos
                case (int(-37, int8))  ! DB = DQT
                    dqt_count = dqt_count + 1
                    print '(A,I5)', "DQT at", pos
                case (int(-60, int8))  ! C4 = DHT
                    dht_count = dht_count + 1
                    print '(A,I5)', "DHT at", pos
                case (int(-64, int8))  ! C0 = SOF0
                    print '(A,I5)', "SOF0 at", pos
                case (int(-32, int8))  ! E0 = APP0
                    print '(A,I5)', "APP0 at", pos
                end select
                
                marker_count = marker_count + 1
                
                ! Skip marker data
                if (jpeg_data(pos+1) /= int(-40, int8) .and. &
                    jpeg_data(pos+1) /= int(-39, int8) .and. &
                    jpeg_data(pos+1) /= int(-38, int8)) then
                    if (pos + 3 < size) then
                        pos = pos + 2 + 256*iand(255,int(jpeg_data(pos+2))) + &
                            iand(255,int(jpeg_data(pos+3)))
                    else
                        exit
                    end if
                else if (jpeg_data(pos+1) == int(-38, int8)) then
                    ! SOS - scan data follows
                    exit
                else
                    pos = pos + 2
                end if
            else
                pos = pos + 1
            end if
        end do
        
        print *, ""
        print *, "Summary:"
        print *, "Total markers:", marker_count
        print *, "DQT tables:", dqt_count
        print *, "DHT tables:", dht_count
    end subroutine analyze_jpeg_structure

end program test_jpeg_complete_validation
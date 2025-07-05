program test_jpeg_fixed_complete
    ! Test the fixed JPEG encoder
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
    
    call test_fixed_encoder()
    
contains

    subroutine test_fixed_encoder()
        integer(int8), allocatable :: rgb_data(:)
        integer(int8), allocatable :: jpeg_data(:)
        integer(int8), target :: rgb_array(3, 8, 8)
        integer(int8), pointer :: stb_data(:)
        type(c_ptr) :: stb_ptr
        integer :: stb_size
        integer :: width = 8, height = 8
        integer :: i, j, idx
        logical :: valid_jpeg
        integer :: exit_code
        
        print *, "=== Testing Fixed JPEG Encoder ==="
        
        ! Create test gradient
        do j = 1, 8
            do i = 1, 8
                rgb_array(1, i, j) = int(min(255, (i-1) * 36), int8)
                rgb_array(2, i, j) = int(min(255, (j-1) * 36), int8) 
                rgb_array(3, i, j) = 0_int8
            end do
        end do
        
        ! Flatten for our encoder
        allocate(rgb_data(width * height * 3))
        idx = 1
        do j = 1, height
            do i = 1, width
                rgb_data(idx) = rgb_array(1, i, j)
                rgb_data(idx+1) = rgb_array(2, i, j)
                rgb_data(idx+2) = rgb_array(3, i, j)
                idx = idx + 3
            end do
        end do
        
        ! Test with our encoder
        call get_jpeg_data(width, height, rgb_data, 90, jpeg_data)
        
        print *, "Our JPEG size:", size(jpeg_data), "bytes"
        
        ! Write to file
        open(unit=10, file="test_fixed_final.jpg", status='replace', access='stream')
        write(10) jpeg_data
        close(10)
        
        ! Validate with multiple tools
        print *, ""
        print *, "Validation:"
        
        ! 1. file command
        call execute_command_line("file test_fixed_final.jpg | grep -q 'JPEG image'", &
            exitstat=exit_code)
        if (exit_code == 0) then
            print *, "  ✓ Valid JPEG (file command)"
        else
            print *, "  ✗ Invalid JPEG (file command)"
        end if
        
        ! 2. ImageMagick identify
        call execute_command_line("which identify > /dev/null 2>&1", exitstat=exit_code)
        if (exit_code == 0) then
            call execute_command_line("identify test_fixed_final.jpg > /dev/null 2>&1", &
                exitstat=exit_code)
            if (exit_code == 0) then
                print *, "  ✓ Valid JPEG (ImageMagick)"
            else
                print *, "  ✗ Invalid JPEG (ImageMagick)"
            end if
        end if
        
        ! 3. Python PIL
        call execute_command_line( &
            "python3 -c 'from PIL import Image; img=Image.open(""test_fixed_final.jpg""); print(""  Size:"", img.size)' 2>&1", &
            exitstat=exit_code)
        if (exit_code == 0) then
            print *, "  ✓ Valid JPEG (Python PIL)"
        else
            print *, "  ✗ Invalid JPEG (Python PIL)"
        end if
        
        ! Compare with STB
        if (stb_write_jpeg_to_memory(stb_ptr, stb_size, 8, 8, 3, &
            c_loc(rgb_array), 90) == 0) then
            print *, "ERROR: STB encoding failed"
            return
        end if
        
        call c_f_pointer(stb_ptr, stb_data, [stb_size])
        
        print *, ""
        print *, "Size comparison:"
        print *, "  Our JPEG:", size(jpeg_data), "bytes"
        print *, "  STB JPEG:", stb_size, "bytes"
        print '(A,F6.2,A)', "  Ratio: ", real(size(jpeg_data)) / real(stb_size) * 100.0, "%"
        
        ! Analyze scan data size
        call analyze_scan_data_size(jpeg_data, stb_data, stb_size)
        
        call free(stb_ptr)
        deallocate(rgb_data, jpeg_data)
    end subroutine test_fixed_encoder
    
    subroutine analyze_scan_data_size(our_data, stb_data, stb_size)
        integer(int8), intent(in) :: our_data(:), stb_data(:)
        integer, intent(in) :: stb_size
        integer :: our_sos, stb_sos, our_eoi, stb_eoi
        integer :: our_scan, stb_scan
        
        ! Find SOS positions
        our_sos = find_marker(our_data, int(-38, int8))
        stb_sos = find_marker(stb_data(1:stb_size), int(-38, int8))
        
        ! Find EOI positions  
        our_eoi = find_marker(our_data, int(-39, int8))
        stb_eoi = find_marker(stb_data(1:stb_size), int(-39, int8))
        
        if (our_sos > 0 .and. our_eoi > 0) then
            our_scan = our_eoi - our_sos - 14
        else
            our_scan = 0
        end if
        
        if (stb_sos > 0 .and. stb_eoi > 0) then
            stb_scan = stb_eoi - stb_sos - 14
        else
            stb_scan = 0
        end if
        
        print *, ""
        print *, "Scan data analysis:"
        print *, "  Our scan data:", our_scan, "bytes"
        print *, "  STB scan data:", stb_scan, "bytes"
        
        if (our_scan > 0 .and. stb_scan > 0) then
            print '(A,F6.2,A)', "  Scan ratio: ", &
                real(our_scan) / real(stb_scan) * 100.0, "%"
        end if
    end subroutine analyze_scan_data_size
    
    function find_marker(data, marker) result(pos)
        integer(int8), intent(in) :: data(:)
        integer(int8), intent(in) :: marker
        integer :: pos, i
        
        pos = 0
        do i = 1, size(data) - 1
            if (data(i) == int(-1, int8) .and. data(i+1) == marker) then
                pos = i
                return
            end if
        end do
    end function find_marker

end program test_jpeg_fixed_complete
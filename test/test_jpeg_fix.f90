program test_jpeg_fix
    use fortplot_jpeg
    use stb_image_write_wrapper
    use iso_c_binding
    use, intrinsic :: iso_fortran_env, only: int8, int32, real64
    implicit none
    
    call test_direct_encoding()
    call test_with_context()
    
contains

    subroutine test_direct_encoding()
        integer(int8), allocatable :: flat_rgb(:)
        integer(int8), allocatable :: jpeg_data(:)
        integer :: width = 8, height = 8
        integer :: i, j, idx
        character(len=100) :: filename
        
        print *, "=== Testing direct JPEG encoding ==="
        
        ! Create flat RGB data (not PNG format)
        allocate(flat_rgb(width * height * 3))
        
        ! Fill with gradient
        idx = 1
        do j = 1, height
            do i = 1, width
                flat_rgb(idx) = int(min(255, (i-1) * 36), int8)     ! R
                flat_rgb(idx+1) = int(min(255, (j-1) * 36), int8)   ! G
                flat_rgb(idx+2) = 0_int8                             ! B
                idx = idx + 3
            end do
        end do
        
        print *, "Created RGB data, first few bytes:"
        print '(16Z3)', (flat_rgb(i), i=1,min(48,size(flat_rgb)))
        
        ! Call get_jpeg_data directly
        call get_jpeg_data(width, height, flat_rgb, 90, jpeg_data)
        
        print *, ""
        print *, "JPEG size:", size(jpeg_data), "bytes"
        
        ! Write to file
        filename = "test_direct.jpg"
        open(unit=10, file=filename, status='replace', access='stream')
        write(10) jpeg_data
        close(10)
        
        print *, "Written to:", filename
        
        ! Hexdump first part
        print *, ""
        print *, "First 64 bytes:"
        call print_hex(jpeg_data, 1, min(64, size(jpeg_data)))
        
        ! Find and show scan data
        call show_scan_data(jpeg_data)
        
        deallocate(flat_rgb, jpeg_data)
    end subroutine test_direct_encoding
    
    subroutine test_with_context()
        type(jpeg_context) :: ctx
        integer :: width = 8, height = 8
        integer :: i, j
        character(len=100) :: filename
        
        print *, ""
        print *, "=== Testing JPEG context encoding ==="
        
        ! Create context
        ctx = create_jpeg_canvas(width, height, 90)
        
        ! Set pixels (assuming context has pixel data)
        ! This depends on the actual API
        
        ! Save
        filename = "test_context.jpg"
        call ctx%save(filename)
        
        print *, "Saved via context to:", filename
    end subroutine test_with_context
    
    subroutine show_scan_data(jpeg_data)
        integer(int8), intent(in) :: jpeg_data(:)
        integer :: i, sos_pos
        
        ! Find SOS marker (FF DA)
        sos_pos = 0
        do i = 1, size(jpeg_data) - 1
            if (jpeg_data(i) == int(-1, int8) .and. jpeg_data(i+1) == int(-38, int8)) then
                sos_pos = i
                exit
            end if
        end do
        
        if (sos_pos > 0) then
            print *, ""
            print *, "SOS marker found at position:", sos_pos
            print *, "SOS header (12 bytes):"
            call print_hex(jpeg_data, sos_pos, min(sos_pos + 12, size(jpeg_data)))
            
            ! Show scan data
            print *, ""
            print *, "Scan data starts at:", sos_pos + 14
            print *, "First 32 bytes of scan data:"
            call print_hex(jpeg_data, sos_pos + 14, min(sos_pos + 46, size(jpeg_data)))
            
            ! Count bytes until EOI
            do i = sos_pos + 14, size(jpeg_data) - 1
                if (jpeg_data(i) == int(-1, int8) .and. jpeg_data(i+1) == int(-39, int8)) then
                    print *, ""
                    print *, "EOI marker at:", i
                    print *, "Scan data size:", i - sos_pos - 14, "bytes"
                    exit
                end if
            end do
        else
            print *, "ERROR: No SOS marker found!"
        end if
    end subroutine show_scan_data
    
    subroutine print_hex(data, start_pos, end_pos)
        integer(int8), intent(in) :: data(:)
        integer, intent(in) :: start_pos, end_pos
        integer :: i
        
        do i = start_pos, end_pos
            write(*, '(Z2.2,1X)', advance='no') iand(255, int(data(i)))
            if (mod(i - start_pos + 1, 16) == 0) write(*,*)
        end do
        if (mod(end_pos - start_pos + 1, 16) /= 0) write(*,*)
    end subroutine print_hex

end program test_jpeg_fix
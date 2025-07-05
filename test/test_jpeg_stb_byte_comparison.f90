program test_jpeg_stb_byte_comparison
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
    
    call test_minimal_jpeg()
    call test_single_block_jpeg()
    call test_gradient_jpeg()
    
contains

    subroutine test_minimal_jpeg()
        integer(int8), target :: rgb_data(3, 8, 8)
        integer(int8), pointer :: stb_data(:)
        type(c_ptr) :: stb_ptr
        integer :: stb_size, i, j
        character(len=100) :: filename
        integer(int8), allocatable :: our_data(:)
        integer :: our_size
        
        print *, "=== Testing minimal 8x8 JPEG ==="
        
        ! Create solid gray 8x8 image
        rgb_data = 127_int8
        
        ! Get STB JPEG in memory
        if (stb_write_jpeg_to_memory(stb_ptr, stb_size, 8, 8, 3, &
            c_loc(rgb_data), 90) == 0) then
            print *, "ERROR: STB JPEG encoding failed"
            return
        end if
        
        ! Convert C pointer to Fortran pointer
        call c_f_pointer(stb_ptr, stb_data, [stb_size])
        
        print *, "STB JPEG size:", stb_size, "bytes"
        
        ! Write STB output to file for reference
        filename = "stb_minimal.jpg"
        open(unit=10, file=filename, status='replace', access='stream')
        write(10) stb_data
        close(10)
        
        ! Get our JPEG implementation
        call encode_jpeg_to_memory(rgb_data, 8, 8, 90, our_data, our_size)
        
        print *, "Our JPEG size:", our_size, "bytes"
        
        ! Write our output for comparison
        filename = "our_minimal.jpg"
        open(unit=10, file=filename, status='replace', access='stream')
        write(10) our_data
        close(10)
        
        ! Compare headers
        call compare_jpeg_headers(stb_data, stb_size, our_data, our_size)
        
        ! Hexdump first 64 bytes
        print *, ""
        print *, "First 64 bytes comparison:"
        print *, "STB:"
        call print_hex_line(stb_data, 1, min(64, stb_size))
        print *, "Ours:"
        call print_hex_line(our_data, 1, min(64, our_size))
        
        ! Free STB memory
        call free(stb_ptr)
        deallocate(our_data)
        
        print *, ""
    end subroutine test_minimal_jpeg
    
    subroutine test_single_block_jpeg()
        integer(int8), target :: rgb_data(3, 8, 8)
        integer(int8), pointer :: stb_data(:)
        type(c_ptr) :: stb_ptr
        integer :: stb_size, i, j
        integer(int8), allocatable :: our_data(:)
        integer :: our_size
        
        print *, "=== Testing single block with gradient ==="
        
        ! Create horizontal gradient
        do j = 1, 8
            do i = 1, 8
                rgb_data(:, i, j) = int((i-1) * 32, int8)
            end do
        end do
        
        ! Get STB JPEG
        if (stb_write_jpeg_to_memory(stb_ptr, stb_size, 8, 8, 3, &
            c_loc(rgb_data), 90) == 0) then
            print *, "ERROR: STB JPEG encoding failed"
            return
        end if
        
        call c_f_pointer(stb_ptr, stb_data, [stb_size])
        print *, "STB gradient JPEG size:", stb_size, "bytes"
        
        ! Get our JPEG
        call encode_jpeg_to_memory(rgb_data, 8, 8, 90, our_data, our_size)
        print *, "Our gradient JPEG size:", our_size, "bytes"
        
        ! Compare SOS segment and scan data
        call compare_sos_segment(stb_data, stb_size, our_data, our_size)
        
        call free(stb_ptr)
        deallocate(our_data)
        
        print *, ""
    end subroutine test_single_block_jpeg
    
    subroutine test_gradient_jpeg()
        integer(int8), target :: rgb_data(3, 16, 16)
        integer(int8), pointer :: stb_data(:)
        type(c_ptr) :: stb_ptr
        integer :: stb_size, i, j
        integer(int8), allocatable :: our_data(:)
        integer :: our_size
        
        print *, "=== Testing 16x16 gradient (4 blocks) ==="
        
        ! Create diagonal gradient
        do j = 1, 16
            do i = 1, 16
                rgb_data(:, i, j) = int(min(255, (i + j - 2) * 8), int8)
            end do
        end do
        
        ! Get STB JPEG
        if (stb_write_jpeg_to_memory(stb_ptr, stb_size, 16, 16, 3, &
            c_loc(rgb_data), 90) == 0) then
            print *, "ERROR: STB JPEG encoding failed"
            return
        end if
        
        call c_f_pointer(stb_ptr, stb_data, [stb_size])
        print *, "STB 16x16 JPEG size:", stb_size, "bytes"
        
        ! Get our JPEG
        call encode_jpeg_to_memory(rgb_data, 16, 16, 90, our_data, our_size)
        print *, "Our 16x16 JPEG size:", our_size, "bytes"
        
        ! Find and compare quantization tables
        call compare_quantization_tables(stb_data, stb_size, our_data, our_size)
        
        call free(stb_ptr)
        deallocate(our_data)
        
        print *, ""
    end subroutine test_gradient_jpeg
    
    subroutine encode_jpeg_to_memory(rgb_data, width, height, quality, jpeg_data, jpeg_size)
        integer(int8), intent(in) :: rgb_data(:,:,:)
        integer, intent(in) :: width, height, quality
        integer(int8), allocatable, intent(out) :: jpeg_data(:)
        integer, intent(out) :: jpeg_size
        integer(int8), allocatable :: flat_data(:)
        integer :: i, j, k, idx
        
        ! Flatten RGB data to match expected format
        allocate(flat_data(width * height * 3))
        idx = 1
        do j = 1, height
            do i = 1, width
                do k = 1, 3
                    flat_data(idx) = rgb_data(k, i, j)
                    idx = idx + 1
                end do
            end do
        end do
        
        ! Call the actual JPEG encoder
        call get_jpeg_data(width, height, flat_data, quality, jpeg_data)
        jpeg_size = size(jpeg_data)
        
        deallocate(flat_data)
    end subroutine encode_jpeg_to_memory
    
    subroutine compare_jpeg_headers(stb_data, stb_size, our_data, our_size)
        integer(int8), intent(in) :: stb_data(:), our_data(:)
        integer, intent(in) :: stb_size, our_size
        integer :: pos
        
        print *, ""
        print *, "Header comparison:"
        
        ! Check SOI
        if (stb_data(1) == int(-1, int8) .and. stb_data(2) == int(-40, int8)) then
            print *, "STB SOI: OK (FFD8)"
        end if
        if (our_data(1) == int(-1, int8) .and. our_data(2) == int(-40, int8)) then
            print *, "Our SOI: OK (FFD8)"
        end if
        
        ! Find APP0
        pos = find_marker(stb_data, stb_size, int(-32, int8))  ! FFE0
        if (pos > 0) then
            print '(A,I4,A)', "STB APP0 at position ", pos, &
                ", length = ", 256*iand(255,int(stb_data(pos+2))) + iand(255,int(stb_data(pos+3)))
        end if
        
        pos = find_marker(our_data, our_size, int(-32, int8))
        if (pos > 0) then
            print '(A,I4,A)', "Our APP0 at position ", pos, &
                ", length = ", 256*iand(255,int(our_data(pos+2))) + iand(255,int(our_data(pos+3)))
        end if
    end subroutine compare_jpeg_headers
    
    subroutine compare_sos_segment(stb_data, stb_size, our_data, our_size)
        integer(int8), intent(in) :: stb_data(:), our_data(:)
        integer, intent(in) :: stb_size, our_size
        integer :: stb_sos, our_sos
        
        print *, ""
        print *, "SOS segment comparison:"
        
        ! Find SOS marker (FFDA)
        stb_sos = find_marker(stb_data, stb_size, int(-38, int8))
        our_sos = find_marker(our_data, our_size, int(-38, int8))
        
        if (stb_sos > 0) then
            print '(A,I4)', "STB SOS at position ", stb_sos
            print *, "STB SOS header:"
            call print_hex_line(stb_data, stb_sos, min(stb_sos + 16, stb_size))
        end if
        
        if (our_sos > 0) then
            print '(A,I4)', "Our SOS at position ", our_sos
            print *, "Our SOS header:"
            call print_hex_line(our_data, our_sos, min(our_sos + 16, our_size))
        end if
    end subroutine compare_sos_segment
    
    subroutine compare_quantization_tables(stb_data, stb_size, our_data, our_size)
        integer(int8), intent(in) :: stb_data(:), our_data(:)
        integer, intent(in) :: stb_size, our_size
        integer :: pos, length, i
        
        print *, ""
        print *, "Quantization table comparison:"
        
        ! Find DQT marker (FFDB)
        pos = find_marker(stb_data, stb_size, int(-37, int8))
        if (pos > 0) then
            length = 256*iand(255,int(stb_data(pos+2))) + iand(255,int(stb_data(pos+3)))
            print '(A,I4,A,I4)', "STB DQT at position ", pos, ", length = ", length
            print *, "First 16 quant values:"
            call print_hex_line(stb_data, pos+5, min(pos+20, stb_size))
        end if
        
        pos = find_marker(our_data, our_size, int(-37, int8))
        if (pos > 0) then
            length = 256*iand(255,int(our_data(pos+2))) + iand(255,int(our_data(pos+3)))
            print '(A,I4,A,I4)', "Our DQT at position ", pos, ", length = ", length
            print *, "First 16 quant values:"
            call print_hex_line(our_data, pos+5, min(pos+20, our_size))
        end if
    end subroutine compare_quantization_tables
    
    function find_marker(data, size, marker) result(pos)
        integer(int8), intent(in) :: data(:)
        integer, intent(in) :: size
        integer(int8), intent(in) :: marker
        integer :: pos, i
        
        pos = 0
        do i = 1, size - 1
            if (data(i) == int(-1, int8) .and. data(i+1) == marker) then
                pos = i
                return
            end if
        end do
    end function find_marker
    
    subroutine print_hex_line(data, start_pos, end_pos)
        integer(int8), intent(in) :: data(:)
        integer, intent(in) :: start_pos, end_pos
        integer :: i
        
        do i = start_pos, end_pos
            write(*, '(Z2.2,1X)', advance='no') iand(255, int(data(i)))
            if (mod(i - start_pos + 1, 16) == 0) write(*,*)
        end do
        if (mod(end_pos - start_pos + 1, 16) /= 0) write(*,*)
    end subroutine print_hex_line

end program test_jpeg_stb_byte_comparison
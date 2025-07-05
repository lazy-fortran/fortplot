program test_jpeg_fixed_validation
    use fortplot_jpeg_fixed
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
    
    call validate_against_stb()
    call test_solid_color()
    call test_gradient()
    call test_checkerboard()
    call test_real_image()
    
contains

    subroutine test_solid_color()
        integer(int8), target :: rgb_data(3, 8, 8)
        integer(int8), allocatable :: flat_rgb(:)
        integer(int8), allocatable :: our_jpeg(:), flat_stb(:)
        integer(int8), pointer :: stb_jpeg(:)
        type(c_ptr) :: stb_ptr
        integer :: stb_size, i, j, idx
        character(len=100) :: filename
        
        print *, ""
        print *, "=== Test 1: Solid Color (Gray) ==="
        
        ! Create solid gray image
        rgb_data = 127_int8
        
        ! Flatten for our encoder
        allocate(flat_rgb(8 * 8 * 3))
        idx = 1
        do j = 1, 8
            do i = 1, 8
                flat_rgb(idx) = rgb_data(1, i, j)
                flat_rgb(idx+1) = rgb_data(2, i, j)
                flat_rgb(idx+2) = rgb_data(3, i, j)
                idx = idx + 3
            end do
        end do
        
        ! Encode with our fixed encoder
        call encode_jpeg_fixed(flat_rgb, 8, 8, 90, our_jpeg)
        
        ! Encode with STB
        if (stb_write_jpeg_to_memory(stb_ptr, stb_size, 8, 8, 3, &
            c_loc(rgb_data), 90) == 0) then
            print *, "ERROR: STB encoding failed"
            return
        end if
        
        call c_f_pointer(stb_ptr, stb_jpeg, [stb_size])
        
        print *, "Our size:", size(our_jpeg), "bytes"
        print *, "STB size:", stb_size, "bytes"
        
        ! Write files
        filename = "fixed_solid.jpg"
        open(unit=10, file=filename, status='replace', access='stream')
        write(10) our_jpeg
        close(10)
        
        filename = "stb_solid.jpg"
        open(unit=10, file=filename, status='replace', access='stream')
        write(10) stb_jpeg
        close(10)
        
        ! Compare structure
        call compare_jpeg_structure(our_jpeg, stb_jpeg, min(size(our_jpeg), stb_size))
        
        ! Test if decodable
        call test_decode("fixed_solid.jpg")
        
        call free(stb_ptr)
        deallocate(flat_rgb, our_jpeg)
    end subroutine test_solid_color
    
    subroutine test_gradient()
        integer(int8), target :: rgb_data(3, 8, 8)
        integer(int8), allocatable :: flat_rgb(:)
        integer(int8), allocatable :: our_jpeg(:)
        integer(int8), pointer :: stb_jpeg(:)
        type(c_ptr) :: stb_ptr
        integer :: stb_size, i, j, idx
        
        print *, ""
        print *, "=== Test 2: Gradient Pattern ==="
        
        ! Create gradient
        do j = 1, 8
            do i = 1, 8
                rgb_data(1, i, j) = int(min(255, (i-1) * 36), int8)
                rgb_data(2, i, j) = int(min(255, (j-1) * 36), int8)
                rgb_data(3, i, j) = 0_int8
            end do
        end do
        
        ! Flatten
        allocate(flat_rgb(8 * 8 * 3))
        idx = 1
        do j = 1, 8
            do i = 1, 8
                flat_rgb(idx) = rgb_data(1, i, j)
                flat_rgb(idx+1) = rgb_data(2, i, j)
                flat_rgb(idx+2) = rgb_data(3, i, j)
                idx = idx + 3
            end do
        end do
        
        ! Encode
        call encode_jpeg_fixed(flat_rgb, 8, 8, 90, our_jpeg)
        
        if (stb_write_jpeg_to_memory(stb_ptr, stb_size, 8, 8, 3, &
            c_loc(rgb_data), 90) == 0) then
            print *, "ERROR: STB encoding failed"
            return
        end if
        
        call c_f_pointer(stb_ptr, stb_jpeg, [stb_size])
        
        print *, "Our size:", size(our_jpeg), "bytes"
        print *, "STB size:", stb_size, "bytes"
        
        ! Write files
        open(unit=10, file="fixed_gradient.jpg", status='replace', access='stream')
        write(10) our_jpeg
        close(10)
        
        open(unit=10, file="stb_gradient.jpg", status='replace', access='stream')
        write(10) stb_jpeg
        close(10)
        
        ! Compare scan data specifically
        call compare_scan_data(our_jpeg, stb_jpeg)
        
        ! Test decode
        call test_decode("fixed_gradient.jpg")
        
        call free(stb_ptr)
        deallocate(flat_rgb, our_jpeg)
    end subroutine test_gradient
    
    subroutine test_checkerboard()
        integer(int8), target :: rgb_data(3, 8, 8)
        integer(int8), allocatable :: flat_rgb(:)
        integer(int8), allocatable :: our_jpeg(:)
        integer :: i, j, idx
        
        print *, ""
        print *, "=== Test 3: Checkerboard Pattern ==="
        
        ! Create checkerboard
        do j = 1, 8
            do i = 1, 8
                if (mod(i+j, 2) == 0) then
                    rgb_data(:, i, j) = int(-1, int8)  ! White
                else
                    rgb_data(:, i, j) = 0_int8  ! Black
                end if
            end do
        end do
        
        ! Flatten
        allocate(flat_rgb(8 * 8 * 3))
        idx = 1
        do j = 1, 8
            do i = 1, 8
                flat_rgb(idx) = rgb_data(1, i, j)
                flat_rgb(idx+1) = rgb_data(2, i, j)
                flat_rgb(idx+2) = rgb_data(3, i, j)
                idx = idx + 3
            end do
        end do
        
        ! Encode
        call encode_jpeg_fixed(flat_rgb, 8, 8, 90, our_jpeg)
        
        print *, "Checkerboard JPEG size:", size(our_jpeg), "bytes"
        
        ! Write file
        open(unit=10, file="fixed_checkerboard.jpg", status='replace', access='stream')
        write(10) our_jpeg
        close(10)
        
        ! Test decode
        call test_decode("fixed_checkerboard.jpg")
        
        deallocate(flat_rgb, our_jpeg)
    end subroutine test_checkerboard
    
    subroutine test_real_image()
        integer(int8), allocatable :: flat_rgb(:)
        integer(int8), allocatable :: our_jpeg(:)
        integer :: width = 16, height = 16
        integer :: i, j, idx
        real :: x, y, r, g, b
        
        print *, ""
        print *, "=== Test 4: 16x16 Color Pattern ==="
        
        ! Create color pattern
        allocate(flat_rgb(width * height * 3))
        idx = 1
        
        do j = 1, height
            do i = 1, width
                x = real(i-1) / real(width-1)
                y = real(j-1) / real(height-1)
                
                r = x
                g = y
                b = 0.5 * (1.0 - x) * (1.0 - y)
                
                flat_rgb(idx) = int(r * 255, int8)
                flat_rgb(idx+1) = int(g * 255, int8)
                flat_rgb(idx+2) = int(b * 255, int8)
                idx = idx + 3
            end do
        end do
        
        ! Encode
        call encode_jpeg_fixed(flat_rgb, width, height, 85, our_jpeg)
        
        print *, "16x16 JPEG size:", size(our_jpeg), "bytes"
        
        ! Write file
        open(unit=10, file="fixed_16x16.jpg", status='replace', access='stream')
        write(10) our_jpeg
        close(10)
        
        ! Test decode
        call test_decode("fixed_16x16.jpg")
        
        deallocate(flat_rgb, our_jpeg)
    end subroutine test_real_image
    
    subroutine compare_jpeg_structure(our_jpeg, stb_jpeg, max_size)
        integer(int8), intent(in) :: our_jpeg(:), stb_jpeg(:)
        integer, intent(in) :: max_size
        integer :: i, diff_count
        
        print *, ""
        print *, "Structure comparison:"
        
        ! Compare markers
        print *, "SOI: Our =", our_jpeg(1:2), "STB =", stb_jpeg(1:2)
        
        ! Find key markers
        call find_and_compare_marker(our_jpeg, stb_jpeg, int(-32, int8), "APP0")
        call find_and_compare_marker(our_jpeg, stb_jpeg, int(-37, int8), "DQT")
        call find_and_compare_marker(our_jpeg, stb_jpeg, int(-64, int8), "SOF0")
        call find_and_compare_marker(our_jpeg, stb_jpeg, int(-60, int8), "DHT")
        call find_and_compare_marker(our_jpeg, stb_jpeg, int(-38, int8), "SOS")
        
        ! Count differences in first 100 bytes
        diff_count = 0
        do i = 1, min(100, max_size)
            if (our_jpeg(i) /= stb_jpeg(i)) diff_count = diff_count + 1
        end do
        
        print '(A,I3,A)', "Header differences in first 100 bytes: ", diff_count, "%"
    end subroutine compare_jpeg_structure
    
    subroutine find_and_compare_marker(our_jpeg, stb_jpeg, marker, name)
        integer(int8), intent(in) :: our_jpeg(:), stb_jpeg(:)
        integer(int8), intent(in) :: marker
        character(len=*), intent(in) :: name
        integer :: our_pos, stb_pos
        
        our_pos = find_marker(our_jpeg, marker)
        stb_pos = find_marker(stb_jpeg, marker)
        
        print '(A,A,I5,A,I5)', name, ": Our pos =", our_pos, ", STB pos =", stb_pos
    end subroutine find_and_compare_marker
    
    function find_marker(jpeg_data, marker) result(pos)
        integer(int8), intent(in) :: jpeg_data(:)
        integer(int8), intent(in) :: marker
        integer :: pos, i
        
        pos = 0
        do i = 1, size(jpeg_data) - 1
            if (jpeg_data(i) == int(-1, int8) .and. jpeg_data(i+1) == marker) then
                pos = i
                return
            end if
        end do
    end function find_marker
    
    subroutine compare_scan_data(our_jpeg, stb_jpeg)
        integer(int8), intent(in) :: our_jpeg(:), stb_jpeg(:)
        integer :: our_sos, stb_sos
        integer :: our_eoi, stb_eoi
        integer :: our_scan_size, stb_scan_size
        
        print *, ""
        print *, "Scan data comparison:"
        
        ! Find SOS markers
        our_sos = find_marker(our_jpeg, int(-38, int8))
        stb_sos = find_marker(stb_jpeg, int(-38, int8))
        
        ! Find EOI markers
        our_eoi = find_marker(our_jpeg, int(-39, int8))
        stb_eoi = find_marker(stb_jpeg, int(-39, int8))
        
        if (our_sos > 0 .and. our_eoi > 0) then
            our_scan_size = our_eoi - our_sos - 14
            print *, "Our scan data size:", our_scan_size, "bytes"
        end if
        
        if (stb_sos > 0 .and. stb_eoi > 0) then
            stb_scan_size = stb_eoi - stb_sos - 14
            print *, "STB scan data size:", stb_scan_size, "bytes"
        end if
        
        if (our_scan_size > 0 .and. stb_scan_size > 0) then
            print '(A,F6.2,A)', "Size ratio: ", &
                real(our_scan_size) / real(stb_scan_size) * 100.0, "%"
        end if
    end subroutine compare_scan_data
    
    subroutine test_decode(filename)
        character(len=*), intent(in) :: filename
        logical :: exists
        integer :: size_bytes
        
        inquire(file=filename, exist=exists, size=size_bytes)
        
        if (exists) then
            print '(A,A,I6,A)', "File ", trim(filename), size_bytes, " bytes - checking with file command..."
            call execute_command_line("file " // trim(filename) // " | grep -q 'JPEG image'", exitstat=size_bytes)
            if (size_bytes == 0) then
                print *, "  ✓ Valid JPEG file"
            else
                print *, "  ✗ Invalid JPEG file!"
            end if
        else
            print *, "ERROR: File not found:", trim(filename)
        end if
    end subroutine test_decode

end program test_jpeg_fixed_validation
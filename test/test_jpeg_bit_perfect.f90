program test_jpeg_bit_perfect
    use fortplot_jpeg
    use, intrinsic :: iso_fortran_env, only: int8
    implicit none
    
    call test_matches_stb_output_exactly()
    
contains
    
    subroutine test_matches_stb_output_exactly()
        integer(1), allocatable :: our_data(:), stb_data(:)
        integer(1) :: pixels(3, 8, 8)  ! RGB 8x8 test image to match STB
        integer :: i, j, k
        integer(1), parameter :: gray_value = 127_int8
        
        ! Create simple test pattern
        pixels = gray_value
        
        ! Generate with our implementation
        call write_jpeg_file("test_our.jpg", 8, 8, reshape(pixels, [192]), 90)
        
        ! Read both files
        call read_binary_file("test_our.jpg", our_data)
        call read_binary_file("stb_minimal.jpg", stb_data)  ! Use existing STB reference
        
        ! Compare sizes
        if (size(our_data) /= size(stb_data)) then
            print *, "FAIL: Size mismatch - our:", size(our_data), "stb:", size(stb_data)
            error stop
        end if
        
        ! Compare byte by byte
        do i = 1, size(our_data)
            if (our_data(i) /= stb_data(i)) then
                print '(A,I5,A,Z2.2,A,Z2.2)', "FAIL: Byte ", i, " differs - our: 0x", &
                    our_data(i), " stb: 0x", stb_data(i)
                error stop
            end if
        end do
        
        print *, "PASS: Output matches STB exactly!"
        
    end subroutine
    
    subroutine read_binary_file(filename, data)
        character(*), intent(in) :: filename
        integer(1), allocatable, intent(out) :: data(:)
        integer :: unit, filesize, iostat
        
        open(newunit=unit, file=filename, form='unformatted', access='stream', &
             status='old', action='read')
        inquire(unit, size=filesize)
        allocate(data(filesize))
        read(unit, iostat=iostat) data
        close(unit)
    end subroutine
    
end program test_jpeg_bit_perfect
program debug_fillbits
    use fortplot_jpeg, only: get_jpeg_data
    use, intrinsic :: iso_fortran_env, only: int8
    implicit none
    
    integer(int8), allocatable :: our_data(:)
    integer(int8), allocatable :: rgb_data(:)
    integer :: i
    
    ! Create 8x8 uniform gray test data
    allocate(rgb_data(192))  ! 8*8*3 = 192
    do i = 1, 192
        rgb_data(i) = int(127, int8)
    end do
    
    ! Generate JPEG
    call get_jpeg_data(8, 8, rgb_data, 90, our_data)
    
    print *, "=== Final bit analysis ==="
    print *, "Total bytes:", size(our_data)
    print *, "Last 20 bytes (hex):"
    do i = max(1, size(our_data)-19), size(our_data)
        if (mod(i-1, 10) == 0) print *, ""
        write(*,'(1X,Z2.2)',advance='no') our_data(i)
    end do
    print *, ""
    print *, ""
    
    ! Focus on the critical bytes
    print *, "Critical 2-byte difference:"
    print *, "Expected STB: 40 1F"
    print *, "Our output:  ", our_data(size(our_data)-4), our_data(size(our_data)-3)
    
    ! Show bit analysis
    print *, ""
    print *, "Bit analysis:"
    print *, "STB 0x40 = 01000000 (bits 6=0, others=0)"
    print *, "STB 0x1F = 00011111 (bits 4-0=1, others=0)"
    print *, ""
    write(*,'(A,Z2.2,A)') "Our first byte: ", our_data(size(our_data)-4), " = "
    call print_bits(our_data(size(our_data)-4))
    write(*,'(A,Z2.2,A)') "Our second byte: ", our_data(size(our_data)-3), " = "
    call print_bits(our_data(size(our_data)-3))
    
    print *, ""
    print *, "This suggests our fillBits (0x7F, 7) is being"
    print *, "positioned differently than STB's implementation."
    
contains
    
    subroutine print_bits(byte_val)
        integer(int8), intent(in) :: byte_val
        integer :: i, bit_val
        
        do i = 7, 0, -1
            bit_val = iand(ishft(int(byte_val), -i), 1)
            write(*,'(I1)',advance='no') bit_val
        end do
        print *
    end subroutine print_bits
    
end program debug_fillbits
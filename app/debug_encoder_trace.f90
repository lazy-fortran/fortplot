program debug_encoder_trace
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
    
    print *, "=== Tracing encoder with uniform gray ==="
    print *, "RGB: (127,127,127) for all pixels"
    print *, ""
    
    ! Generate JPEG and trace what happens
    call get_jpeg_data(8, 8, rgb_data, 90, our_data)
    
    print *, "Our scan data (last 10 bytes):"
    do i = max(1, size(our_data)-9), size(our_data)
        write(*,'(1X,Z2.2)',advance='no') our_data(i)
    end do
    print *, ""
    print *, ""
    
    ! The key insight: let's compare the exact structure
    ! Expected for uniform gray:
    ! - Y component: -1.0 (after RGB->YCbCr conversion)
    ! - U component: 0.0
    ! - V component: ~0.0
    ! - After DCT: Y DC = -64, U DC = 0, V DC = 0
    ! - After quantization: Y DC = -21, U DC = 0, V DC = 0
    
    print *, "Expected bit sequence:"
    print *, "1. Y DC: category 5 (code=6, 3 bits) + value -21 (01010, 5 bits)"
    print *, "2. U DC: category 0 (code=0, 2 bits)"
    print *, "3. V DC: category 0 (code=0, 2 bits)"
    print *, "4. Y AC: EOB (code=10, 4 bits)"
    print *, "5. U AC: EOB (code=0, 2 bits)"
    print *, "6. V AC: EOB (code=0, 2 bits)"
    print *, "7. fillBits: (code=0x7F, 7 bits)"
    print *, ""
    print *, "Total: 3+5+2+2+4+2+2+7 = 27 bits"
    print *, ""
    print *, "The difference between STB (40 1F) and ours (45 15) suggests:"
    print *, "- Different bit packing or alignment"
    print *, "- Different fillBits handling"
    print *, "- Different final byte extraction"
    
end program debug_encoder_trace
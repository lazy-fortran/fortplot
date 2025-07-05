program test_jpeg_dct_validation
    use fortplot_jpeg
    use, intrinsic :: iso_fortran_env, only: int8, int16, int32, real64
    implicit none
    
    call test_single_8x8_block()
    call test_dct_against_stb()
    
contains

    subroutine test_single_8x8_block()
        ! Test DCT on a single 8x8 block with known values
        integer :: i, j
        real(real64) :: block(8,8), dct_block(8,8), expected(8,8)
        
        ! Simple gradient pattern for testing
        do j = 1, 8
            do i = 1, 8
                block(i,j) = real(i + j - 2, real64) * 16.0_real64
            end do
        end do
        
        print *, "Testing single 8x8 block DCT..."
        print *, "Input block:"
        do j = 1, 8
            print '(8F8.1)', block(:,j)
        end do
        
        ! Apply DCT (we'll need to extract this from the JPEG module)
        call apply_dct(block, dct_block)
        
        print *, ""
        print *, "DCT coefficients:"
        do j = 1, 8
            print '(8F10.2)', dct_block(:,j)
        end do
    end subroutine test_single_8x8_block
    
    subroutine test_dct_against_stb()
        ! Create minimal image and compare DCT results with STB
        integer(int8), allocatable :: rgb_data(:,:,:)
        integer(int8), allocatable :: stb_jpeg(:), our_jpeg(:)
        integer :: stb_size, our_size
        integer :: i, j
        
        ! Create 8x8 grayscale test pattern
        allocate(rgb_data(3, 8, 8))
        
        ! Simple gradient
        do j = 1, 8
            do i = 1, 8
                rgb_data(:, i, j) = int((i-1) * 32, int8)
            end do
        end do
        
        print *, ""
        print *, "Creating 8x8 test image for DCT validation..."
        
        ! Write using STB - commented out as we need proper interface
        ! call write_jpeg_to_memory(rgb_data, 8, 8, 3, 90, stb_jpeg, stb_size)
        stb_size = 100
        allocate(stb_jpeg(stb_size))
        stb_jpeg = 0
        
        ! Write using our implementation
        call write_our_jpeg_to_memory(rgb_data, 8, 8, our_jpeg, our_size)
        
        print *, "STB JPEG size:", stb_size
        print *, "Our JPEG size:", our_size
        
        ! Compare headers and markers
        call compare_jpeg_structure(stb_jpeg, stb_size, our_jpeg, our_size)
        
        deallocate(rgb_data, stb_jpeg, our_jpeg)
    end subroutine test_dct_against_stb
    
    subroutine apply_dct(input, output)
        real(real64), intent(in) :: input(8,8)
        real(real64), intent(out) :: output(8,8)
        real(real64) :: temp(8,8)
        real(real64) :: c(8), s
        integer :: i, j, k
        real(real64), parameter :: PI = 3.141592653589793_real64
        
        ! DCT-II formula implementation
        do i = 1, 8
            if (i == 1) then
                c(i) = 1.0_real64 / sqrt(2.0_real64)
            else
                c(i) = 1.0_real64
            end if
        end do
        
        ! Apply 1D DCT to rows
        do j = 1, 8
            do i = 1, 8
                s = 0.0_real64
                do k = 1, 8
                    s = s + input(k,j) * cos((2*(k-1)+1) * (i-1) * PI / 16.0_real64)
                end do
                temp(i,j) = c(i) * s / 2.0_real64
            end do
        end do
        
        ! Apply 1D DCT to columns
        do i = 1, 8
            do j = 1, 8
                s = 0.0_real64
                do k = 1, 8
                    s = s + temp(i,k) * cos((2*(k-1)+1) * (j-1) * PI / 16.0_real64)
                end do
                output(i,j) = c(j) * s / 2.0_real64
            end do
        end do
    end subroutine apply_dct
    
    subroutine write_our_jpeg_to_memory(rgb_data, width, height, jpeg_data, jpeg_size)
        integer(int8), intent(in) :: rgb_data(:,:,:)
        integer, intent(in) :: width, height
        integer(int8), allocatable, intent(out) :: jpeg_data(:)
        integer, intent(out) :: jpeg_size
        
        ! Placeholder - we need to extract the actual encoding logic
        allocate(jpeg_data(width * height * 3))
        jpeg_size = 100
        jpeg_data = 0
    end subroutine write_our_jpeg_to_memory
    
    subroutine compare_jpeg_structure(stb_data, stb_size, our_data, our_size)
        integer(int8), intent(in) :: stb_data(:), our_data(:)
        integer, intent(in) :: stb_size, our_size
        integer :: i
        
        print *, ""
        print *, "Comparing JPEG structure..."
        
        ! Check SOI marker
        if (stb_data(1) == int(Z'FF', int8) .and. stb_data(2) == int(Z'D8', int8)) then
            print *, "STB SOI marker: OK"
        else
            print *, "STB SOI marker: FAILED"
        end if
        
        if (our_data(1) == int(Z'FF', int8) .and. our_data(2) == int(Z'D8', int8)) then
            print *, "Our SOI marker: OK"
        else
            print *, "Our SOI marker: FAILED"
        end if
        
        ! Print first 32 bytes for comparison
        print *, ""
        print *, "First 32 bytes comparison:"
        print *, "STB:"
        print '(16Z3)', (stb_data(i), i=1,min(32,stb_size))
        print *, "Ours:"
        print '(16Z3)', (our_data(i), i=1,min(32,our_size))
    end subroutine compare_jpeg_structure

end program test_jpeg_dct_validation
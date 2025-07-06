program debug_ycbcr_storage
    use fortplot_jpeg
    implicit none
    
    integer, parameter :: width = 8, height = 8
    integer(1) :: rgb_data(width * height * 3)
    real, allocatable :: ycbcr_data(:,:,:)
    integer :: x, y, idx
    real :: r, g, b
    
    ! Create a simple test pattern
    do y = 0, height - 1
        do x = 0, width - 1
            idx = (y * width + x) * 3 + 1
            rgb_data(idx) = int(x * 32, 1)      ! R varies with x
            rgb_data(idx + 1) = int(y * 32, 1)  ! G varies with y
            rgb_data(idx + 2) = int(64, 1)      ! B constant
        end do
    end do
    
    ! Call internal rgb_to_ycbcr
    call test_rgb_to_ycbcr(rgb_data, ycbcr_data)
    
    print *, "YCbCr data shape:", shape(ycbcr_data)
    print *, "First row of Y values:"
    do x = 1, width
        write(*, '(F7.2)', advance='no') ycbcr_data(x, 1, 1)
    end do
    print *, ""
    
contains
    
    subroutine test_rgb_to_ycbcr(rgb_data, ycbcr_data)
        integer(1), intent(in) :: rgb_data(:)
        real, allocatable, intent(out) :: ycbcr_data(:,:,:)
        
        integer :: x, y, idx
        real :: r, g, b
        
        allocate(ycbcr_data(width, height, 3))
        
        do y = 1, height
            do x = 1, width
                ! Extract RGB values
                idx = ((y - 1) * width + (x - 1)) * 3 + 1
                r = real(iand(int(rgb_data(idx)), 255))
                g = real(iand(int(rgb_data(idx + 1)), 255))  
                b = real(iand(int(rgb_data(idx + 2)), 255))
                
                ! Convert to YCbCr
                ycbcr_data(x, y, 1) = 0.299*r + 0.587*g + 0.114*b - 128.0
                ycbcr_data(x, y, 2) = -0.16874*r - 0.33126*g + 0.5*b
                ycbcr_data(x, y, 3) = 0.5*r - 0.41869*g - 0.08131*b
            end do
        end do
    end subroutine

end program
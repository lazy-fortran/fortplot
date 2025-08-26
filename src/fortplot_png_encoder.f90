module fortplot_png_encoder
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: bitmap_to_png_buffer

contains

    subroutine bitmap_to_png_buffer(bitmap, width, height, buffer)
        !! Convert 3D RGB bitmap to PNG buffer format with filter bytes
        integer(1), intent(in) :: bitmap(:,:,:)
        integer, intent(in) :: width, height
        integer(1), intent(out) :: buffer(:)
        integer :: i, j, buf_idx, row_start
        
        ! PNG format: each row starts with filter byte (0 = no filter) followed by RGB data
        do j = 1, height
            row_start = (j - 1) * (1 + width * 3) + 1
            buffer(row_start) = 0_1  ! PNG filter byte: 0 = no filter
            
            do i = 1, width
                buf_idx = row_start + 1 + (i - 1) * 3
                buffer(buf_idx)     = bitmap(i, j, 1) ! R
                buffer(buf_idx + 1) = bitmap(i, j, 2) ! G
                buffer(buf_idx + 2) = bitmap(i, j, 3) ! B
            end do
        end do
    end subroutine bitmap_to_png_buffer

end module fortplot_png_encoder
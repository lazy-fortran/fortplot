program debug_stride_extraction
    implicit none
    
    real :: y_16x16(256)
    real :: extracted_8x8(8, 8)
    integer :: i, j, pos, src_pos
    integer :: offset, stride
    
    print *, "Testing stride extraction logic"
    print *, "==============================="
    
    ! Fill 16x16 array with a pattern so we can verify extraction
    do i = 1, 256
        y_16x16(i) = real(i)
    end do
    
    print *, "16x16 array (first row):"
    do j = 1, 16
        write(*, '(F4.0, 1X)', advance='no') y_16x16(j)
    end do
    print *
    print *, "16x16 array (9th row start):"
    do j = 129, 144
        write(*, '(F4.0, 1X)', advance='no') y_16x16(j)
    end do
    print *
    print *
    
    ! Test extraction for Y+0 (offset=1, stride=16)
    offset = 1
    stride = 16
    print *, "Extracting Y+0 (offset=", offset, ", stride=", stride, "):"
    
    do i = 1, 8  ! row
        do j = 1, 8  ! col
            src_pos = offset + (i-1)*stride + (j-1)
            extracted_8x8(j, i) = y_16x16(src_pos)
        end do
    end do
    
    print *, "Extracted 8x8 block (should be top-left of 16x16):"
    do i = 1, 8
        do j = 1, 8
            write(*, '(F4.0, 1X)', advance='no') extracted_8x8(j, i)
        end do
        print *
    end do
    print *
    
    ! Test extraction for Y+8 (offset=9, stride=16)
    offset = 9
    stride = 16
    print *, "Extracting Y+8 (offset=", offset, ", stride=", stride, "):"
    
    do i = 1, 8  ! row
        do j = 1, 8  ! col
            src_pos = offset + (i-1)*stride + (j-1)
            extracted_8x8(j, i) = y_16x16(src_pos)
        end do
    end do
    
    print *, "Extracted 8x8 block (should be top-right of 16x16):"
    do i = 1, 8
        do j = 1, 8
            write(*, '(F4.0, 1X)', advance='no') extracted_8x8(j, i)
        end do
        print *
    end do
    print *
    
    ! Test extraction for Y+128 (offset=129, stride=16)
    offset = 129
    stride = 16
    print *, "Extracting Y+128 (offset=", offset, ", stride=", stride, "):"
    
    do i = 1, 8  ! row
        do j = 1, 8  ! col
            src_pos = offset + (i-1)*stride + (j-1)
            extracted_8x8(j, i) = y_16x16(src_pos)
        end do
    end do
    
    print *, "Extracted 8x8 block (should be bottom-left of 16x16):"
    do i = 1, 8
        do j = 1, 8
            write(*, '(F4.0, 1X)', advance='no') extracted_8x8(j, i)
        end do
        print *
    end do

end program debug_stride_extraction
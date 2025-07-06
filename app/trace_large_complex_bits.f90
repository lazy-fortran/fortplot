program trace_large_complex_bits
    use fortplot_jpeg
    use, intrinsic :: iso_fortran_env, only: int8, int32
    implicit none
    
    ! Exactly replicate the test pattern from test_jpeg_large_complex.f90
    integer(1), allocatable :: image_data(:,:,:)
    integer :: x, y, idx
    real :: value, base_val
    
    allocate(image_data(64, 64, 3))
    
    ! Create complex pattern
    do y = 1, 64
        do x = 1, 64
            idx = mod(x + y, 17)
            base_val = real(idx) / 17.0
            
            ! R channel: sinusoidal pattern based on x
            value = 0.5 + 0.5 * sin(real(x) * 0.1)
            value = value * base_val
            image_data(x, y, 1) = int(value * 255, int8)
            
            ! G channel: sinusoidal pattern based on y
            value = 0.5 + 0.5 * sin(real(y) * 0.1)
            value = value * base_val
            image_data(x, y, 2) = int(value * 255, int8)
            
            ! B channel: combined pattern
            value = 0.5 + 0.5 * sin(real(x + y) * 0.05)
            value = value * base_val
            image_data(x, y, 3) = int(value * 255, int8)
        end do
    end do
    
    ! Initialize Huffman tables first
    call initialize_huffman_tables()
    
    ! Trace encoding the first MCU
    call trace_first_mcu_encoding(image_data)
    
contains

    subroutine trace_first_mcu_encoding(img)
        integer(1), intent(in) :: img(:,:,:)
        integer :: i, j, k, x, y
        real :: Y_blocks(8,8,4), Cb_block(8,8), Cr_block(8,8)
        real :: r, g, b, y_val, cb_val, cr_val
        integer :: dct_Y(8,8,4), dct_Cb(8,8), dct_Cr(8,8)
        integer :: zigzag_Y(64,4), zigzag_Cb(64), zigzag_Cr(64)
        integer :: dc_Y(4), dc_Cb, dc_Cr
        integer :: prev_dc_Y, prev_dc_Cb, prev_dc_Cr
        integer :: dc_diff
        integer :: category, value_bits, num_bits
        integer :: huffman_code, huffman_bits
        
        print *, "=== Tracing first MCU encoding ==="
        print *, ""
        
        ! Initialize previous DC values
        prev_dc_Y = 0
        prev_dc_Cb = 0
        prev_dc_Cr = 0
        
        ! Extract Y blocks (2x2 for 4:2:0)
        do j = 0, 1
            do i = 0, 1
                k = j * 2 + i + 1
                print '(A,I0,A,I0,A,I0,A)', "Y Block ", k, " (", i, ",", j, "):"
                
                ! Extract 8x8 Y block
                do y = 1, 8
                    do x = 1, 8
                        r = real(img(i*8+x, j*8+y, 1))
                        g = real(img(i*8+x, j*8+y, 2))
                        b = real(img(i*8+x, j*8+y, 3))
                        
                        ! RGB to Y conversion
                        y_val = 0.299 * r + 0.587 * g + 0.114 * b
                        Y_blocks(x, y, k) = y_val - 128.0
                    end do
                end do
                
                ! Apply DCT and quantization
                call apply_dct_and_quantize(Y_blocks(:,:,k), dct_Y(:,:,k), 0)
                
                ! Convert to zigzag
                call to_zigzag(dct_Y(:,:,k), zigzag_Y(:,k))
                
                ! Get DC coefficient
                dc_Y(k) = zigzag_Y(1,k)
                
                ! Compute DC difference
                dc_diff = dc_Y(k) - prev_dc_Y
                
                print '(A,I5)', "  DC value: ", dc_Y(k)
                print '(A,I5)', "  DC diff:  ", dc_diff
                
                ! Get encoding info
                call get_dc_encoding_info(dc_diff, category, value_bits, num_bits)
                
                print '(A,I3)', "  Category: ", category
                
                if (category > 0) then
                    ! Get Huffman code
                    huffman_code = YDC_HT(category+1, 1)
                    huffman_bits = YDC_HT(category+1, 2)
                    
                    print '(A,B0)', "  Huffman code: ", huffman_code
                    print '(A,I2)', "  Huffman bits: ", huffman_bits
                    print '(A,B0)', "  Value bits:   ", value_bits
                    print '(A,I2)', "  Value length: ", num_bits
                    print '(A,I3)', "  Total bits:   ", huffman_bits + num_bits
                else
                    huffman_code = YDC_HT(1, 1)
                    huffman_bits = YDC_HT(1, 2)
                    print '(A,B0)', "  Huffman code: ", huffman_code
                    print '(A,I2)', "  Huffman bits: ", huffman_bits
                    print '(A,I3)', "  Total bits:   ", huffman_bits
                end if
                
                ! Show first few AC coefficients
                print *, "  First 5 AC coefficients:"
                do idx = 2, min(6, 64)
                    if (zigzag_Y(idx,k) /= 0) then
                        print '(A,I2,A,I5)', "    AC[", idx-1, "] = ", zigzag_Y(idx,k)
                    end if
                end do
                
                prev_dc_Y = dc_Y(k)
                print *, ""
            end do
        end do
        
        ! Extract Cb and Cr blocks (single 8x8 each, subsampled)
        print *, "Cb Block:"
        do y = 1, 8
            do x = 1, 8
                ! Average 2x2 region
                r = 0.0
                g = 0.0
                b = 0.0
                do j = 0, 1
                    do i = 0, 1
                        r = r + real(img((x-1)*2+i+1, (y-1)*2+j+1, 1))
                        g = g + real(img((x-1)*2+i+1, (y-1)*2+j+1, 2))
                        b = b + real(img((x-1)*2+i+1, (y-1)*2+j+1, 3))
                    end do
                end do
                r = r / 4.0
                g = g / 4.0
                b = b / 4.0
                
                cb_val = -0.16874 * r - 0.33126 * g + 0.5 * b + 128.0
                Cb_block(x, y) = cb_val - 128.0
            end do
        end do
        
        call apply_dct_and_quantize(Cb_block, dct_Cb, 1)
        call to_zigzag(dct_Cb, zigzag_Cb)
        dc_Cb = zigzag_Cb(1)
        dc_diff = dc_Cb - prev_dc_Cb
        
        print '(A,I5)', "  DC value: ", dc_Cb
        print '(A,I5)', "  DC diff:  ", dc_diff
        
        call get_dc_encoding_info(dc_diff, category, value_bits, num_bits)
        print '(A,I3)', "  Category: ", category
        
        print *, ""
        print *, "Cr Block:"
        ! Similar for Cr...
        
    end subroutine trace_first_mcu_encoding
    
    subroutine get_dc_encoding_info(dc_diff, category, value_bits, num_bits)
        integer, intent(in) :: dc_diff
        integer, intent(out) :: category, value_bits, num_bits
        integer :: abs_val
        
        abs_val = abs(dc_diff)
        
        ! Get category
        if (abs_val == 0) then
            category = 0
            num_bits = 0
            value_bits = 0
        else
            category = 1
            do while (ishft(1, category) <= abs_val)
                category = category + 1
            end do
            num_bits = category
            
            ! Get value bits
            if (dc_diff >= 0) then
                value_bits = dc_diff
            else
                ! One's complement for negative values
                value_bits = dc_diff + ishft(1, category) - 1
            end if
        end if
    end subroutine get_dc_encoding_info
    
    subroutine apply_dct_and_quantize(input_block, output_block, table_idx)
        real, intent(in) :: input_block(8,8)
        integer, intent(out) :: output_block(8,8)
        integer, intent(in) :: table_idx
        real :: dct_result(8,8)
        integer :: i, j, k, l
        real :: sum, cu, cv
        real, parameter :: PI = 3.14159265358979323846
        integer :: quant_table(8,8)
        
        ! Standard luminance quantization table
        integer, parameter :: std_lum_quant(8,8) = reshape([ &
            16, 11, 10, 16,  24,  40,  51,  61, &
            12, 12, 14, 19,  26,  58,  60,  55, &
            14, 13, 16, 24,  40,  57,  69,  56, &
            14, 17, 22, 29,  51,  87,  80,  62, &
            18, 22, 37, 56,  68, 109, 103,  77, &
            24, 35, 55, 64,  81, 104, 113,  92, &
            49, 64, 78, 87, 103, 121, 120, 101, &
            72, 92, 95, 98, 112, 100, 103,  99], [8,8])
        
        ! Standard chrominance quantization table
        integer, parameter :: std_chr_quant(8,8) = reshape([ &
            17, 18, 24, 47, 99, 99, 99, 99, &
            18, 21, 26, 66, 99, 99, 99, 99, &
            24, 26, 56, 99, 99, 99, 99, 99, &
            47, 66, 99, 99, 99, 99, 99, 99, &
            99, 99, 99, 99, 99, 99, 99, 99, &
            99, 99, 99, 99, 99, 99, 99, 99, &
            99, 99, 99, 99, 99, 99, 99, 99, &
            99, 99, 99, 99, 99, 99, 99, 99], [8,8])
        
        if (table_idx == 0) then
            quant_table = std_lum_quant
        else
            quant_table = std_chr_quant
        end if
        
        ! Apply DCT
        do j = 1, 8
            do i = 1, 8
                sum = 0.0
                do l = 1, 8
                    do k = 1, 8
                        sum = sum + input_block(k,l) * &
                              cos((2.0*(k-1)+1.0)*(i-1)*PI/16.0) * &
                              cos((2.0*(l-1)+1.0)*(j-1)*PI/16.0)
                    end do
                end do
                
                cu = merge(1.0/sqrt(2.0), 1.0, i == 1)
                cv = merge(1.0/sqrt(2.0), 1.0, j == 1)
                dct_result(i,j) = 0.25 * cu * cv * sum
            end do
        end do
        
        ! Quantize
        do j = 1, 8
            do i = 1, 8
                output_block(i,j) = nint(dct_result(i,j) / real(quant_table(i,j)))
            end do
        end do
    end subroutine apply_dct_and_quantize
    
    subroutine to_zigzag(block, zigzag)
        integer, intent(in) :: block(8,8)
        integer, intent(out) :: zigzag(64)
        integer :: zigzag_order(2,64)
        integer :: i
        
        ! Define zigzag order
        zigzag_order = reshape([ &
            1,1, 1,2, 2,1, 3,1, 2,2, 1,3, 1,4, 2,3, 3,2, 4,1, 5,1, 4,2, 3,3, 2,4, 1,5, 1,6, &
            2,5, 3,4, 4,3, 5,2, 6,1, 7,1, 6,2, 5,3, 4,4, 3,5, 2,6, 1,7, 1,8, 2,7, 3,6, 4,5, &
            5,4, 6,3, 7,2, 8,1, 8,2, 7,3, 6,4, 5,5, 4,6, 3,7, 2,8, 3,8, 4,7, 5,6, 6,5, 7,4, &
            8,3, 8,4, 7,5, 6,6, 5,7, 4,8, 5,8, 6,7, 7,6, 8,5, 8,6, 7,7, 6,8, 7,8, 8,7, 8,8], [2,64])
        
        do i = 1, 64
            zigzag(i) = block(zigzag_order(1,i), zigzag_order(2,i))
        end do
    end subroutine to_zigzag

end program trace_large_complex_bits
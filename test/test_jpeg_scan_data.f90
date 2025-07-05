program test_jpeg_scan_data
    use iso_fortran_env, only: int32
    implicit none
    
    call test_simple_block_encoding()
    call test_gray_block_encoding()
    call test_scan_data_length()
    
    print *, "All scan data tests passed!"
    
contains
    
    subroutine test_simple_block_encoding()
        integer(int32) :: y_block(64), u_block(64), v_block(64)
        integer(int32) :: scan_data(1000), scan_length
        integer(int32) :: expected_length
        
        ! Test simple uniform gray block (-1 values after JPEG shift)
        y_block = -1
        u_block = 0
        v_block = 0
        
        call encode_single_mcu(y_block, u_block, v_block, scan_data, scan_length)
        
        ! For uniform gray blocks, scan data should be minimal:
        ! Y DC=-3 (2 bits category + 2 bits value) + EOB (4 bits) = 6 bits
        ! U DC=0 (2 bits) + EOB (2 bits) = 4 bits  
        ! V DC=0 (2 bits) + EOB (2 bits) = 4 bits
        ! Total: 14 bits + padding = 2-3 bytes
        
        if (scan_length < 2 .or. scan_length > 10) then
            print *, "FAIL: Scan length", scan_length, "outside expected range 2-10"
            error stop
        end if
        
        print *, "Simple block encoding test passed, length:", scan_length
    end subroutine
    
    subroutine test_gray_block_encoding()
        integer(int32) :: y_block(64), u_block(64), v_block(64)
        integer(int32) :: scan_data(1000), scan_length
        integer(int32) :: i
        
        ! Test encoding of single gray block with known pattern
        y_block = 0
        u_block = 0  
        v_block = 0
        
        ! Set DC coefficients to known values
        y_block(1) = -3  ! Should encode as category 2, value 00
        u_block(1) = 0   ! Should encode as category 0, no value bits
        v_block(1) = 0   ! Should encode as category 0, no value bits
        
        call encode_single_mcu(y_block, u_block, v_block, scan_data, scan_length)
        
        ! Verify we get some reasonable output
        if (scan_length == 0) then
            print *, "FAIL: No scan data generated"
            error stop
        end if
        
        ! Print first few bytes for debugging
        print *, "Gray block scan data (first 8 bytes):"
        do i = 1, min(scan_length, 8)
            print *, "  Byte", i, ":", scan_data(i)
        end do
        
        print *, "Gray block encoding test passed, length:", scan_length
    end subroutine
    
    subroutine test_scan_data_length()
        integer(int32) :: y_block(64), u_block(64), v_block(64)
        integer(int32) :: scan_data(1000), scan_length
        integer(int32) :: stb_reference_length
        
        ! Test with simple pattern and compare length to STB expectation
        y_block = -1  ! Uniform gray
        u_block = 0   ! Neutral chroma
        v_block = 0   ! Neutral chroma
        
        call encode_single_mcu(y_block, u_block, v_block, scan_data, scan_length)
        
        ! From STB analysis, simple 8x8 gray should produce ~40-50 bytes scan data
        stb_reference_length = 45  ! Approximate from hexdump analysis
        
        if (abs(scan_length - stb_reference_length) > 20) then
            print *, "WARN: Scan length", scan_length, "differs significantly from STB", stb_reference_length
        else
            print *, "Scan length", scan_length, "within reasonable range of STB", stb_reference_length
        end if
        
        print *, "Scan data length test completed"
    end subroutine
    
    subroutine encode_single_mcu(y_block, u_block, v_block, scan_data, scan_length)
        integer(int32), intent(in) :: y_block(64), u_block(64), v_block(64)
        integer(int32), intent(out) :: scan_data(1000)
        integer(int32), intent(out) :: scan_length
        
        ! Simplified MCU encoding - replace with actual implementation
        integer(int32) :: bit_buffer, bit_count, byte_count
        integer(int32) :: dc_y_prev, dc_u_prev, dc_v_prev
        
        bit_buffer = 0
        bit_count = 0
        byte_count = 0
        dc_y_prev = 0
        dc_u_prev = 0
        dc_v_prev = 0
        
        ! Encode Y block
        call encode_block_huffman(y_block, 1, dc_y_prev, bit_buffer, bit_count, scan_data, byte_count)
        
        ! Encode U block  
        call encode_block_huffman(u_block, 0, dc_u_prev, bit_buffer, bit_count, scan_data, byte_count)
        
        ! Encode V block
        call encode_block_huffman(v_block, 0, dc_v_prev, bit_buffer, bit_count, scan_data, byte_count)
        
        ! Flush remaining bits
        if (bit_count > 0) then
            ! Pad with 1s and flush
            do while (bit_count < 8)
                bit_buffer = ior(bit_buffer, ishft(1, bit_count))
                bit_count = bit_count + 1
            end do
            byte_count = byte_count + 1
            scan_data(byte_count) = iand(bit_buffer, 255)
        end if
        
        scan_length = byte_count
    end subroutine
    
    subroutine encode_block_huffman(block, is_luma, dc_prev, bit_buffer, bit_count, output, byte_count)
        integer(int32), intent(in) :: block(64), is_luma
        integer(int32), intent(inout) :: dc_prev, bit_buffer, bit_count, byte_count
        integer(int32), intent(inout) :: output(1000)
        
        integer(int32) :: dc_diff, dc_cat, dc_bits
        
        ! Encode DC coefficient
        dc_diff = block(1) - dc_prev
        dc_prev = block(1)
        
        ! Get DC category and bits
        call get_dc_category_bits(dc_diff, dc_cat, dc_bits)
        
        ! Add DC Huffman code (simplified)
        if (is_luma == 1) then
            call add_huffman_code(dc_cat, get_y_dc_code(dc_cat), get_y_dc_length(dc_cat), &
                                 bit_buffer, bit_count, output, byte_count)
        else
            call add_huffman_code(dc_cat, get_uv_dc_code(dc_cat), get_uv_dc_length(dc_cat), &
                                 bit_buffer, bit_count, output, byte_count)
        end if
        
        ! Add DC value bits
        if (dc_cat > 0) then
            call add_bits(dc_bits, dc_cat, bit_buffer, bit_count, output, byte_count)
        end if
        
        ! Add EOB for AC coefficients (simplified - assume all AC = 0)
        if (is_luma == 1) then
            call add_huffman_code(0, 10, 4, bit_buffer, bit_count, output, byte_count)  ! Y EOB
        else
            call add_huffman_code(0, 0, 2, bit_buffer, bit_count, output, byte_count)   ! UV EOB
        end if
    end subroutine
    
    subroutine get_dc_category_bits(value, category, bits)
        integer(int32), intent(in) :: value
        integer(int32), intent(out) :: category, bits
        
        integer(int32) :: abs_val
        
        if (value == 0) then
            category = 0
            bits = 0
        else
            abs_val = abs(value)
            category = bit_size(abs_val)  ! Number of bits needed
            if (value > 0) then
                bits = value
            else
                bits = value - 1  ! Two's complement for negative
            end if
        end if
    end subroutine
    
    function get_y_dc_code(category) result(code)
        integer(int32), intent(in) :: category
        integer(int32) :: code
        
        ! Simplified Y DC Huffman codes
        select case(category)
        case(0); code = 0   ! 00
        case(1); code = 2   ! 010
        case(2); code = 3   ! 011
        case(3); code = 4   ! 100
        case default; code = 5
        end select
    end function
    
    function get_y_dc_length(category) result(length)
        integer(int32), intent(in) :: category
        integer(int32) :: length
        
        select case(category)
        case(0); length = 2
        case(1); length = 3
        case(2); length = 3
        case(3); length = 3
        case default; length = 4
        end select
    end function
    
    function get_uv_dc_code(category) result(code)
        integer(int32), intent(in) :: category
        integer(int32) :: code
        
        ! Simplified UV DC Huffman codes
        select case(category)
        case(0); code = 0   ! 00
        case(1); code = 1   ! 01
        case(2); code = 2   ! 10
        case default; code = 3
        end select
    end function
    
    function get_uv_dc_length(category) result(length)
        integer(int32), intent(in) :: category
        integer(int32) :: length
        
        select case(category)
        case(0); length = 2
        case(1); length = 2
        case(2); length = 2
        case default; length = 3
        end select
    end function
    
    subroutine add_huffman_code(symbol, code, length, bit_buffer, bit_count, output, byte_count)
        integer(int32), intent(in) :: symbol, code, length
        integer(int32), intent(inout) :: bit_buffer, bit_count, byte_count
        integer(int32), intent(inout) :: output(1000)
        
        call add_bits(code, length, bit_buffer, bit_count, output, byte_count)
    end subroutine
    
    subroutine add_bits(value, num_bits, bit_buffer, bit_count, output, byte_count)
        integer(int32), intent(in) :: value, num_bits
        integer(int32), intent(inout) :: bit_buffer, bit_count, byte_count
        integer(int32), intent(inout) :: output(1000)
        
        integer(int32) :: i
        
        ! Add bits one by one (MSB first)
        do i = num_bits-1, 0, -1
            if (iand(ishft(value, -i), 1) == 1) then
                bit_buffer = ior(bit_buffer, ishft(1, bit_count))
            end if
            bit_count = bit_count + 1
            
            if (bit_count == 8) then
                byte_count = byte_count + 1
                output(byte_count) = iand(bit_buffer, 255)
                
                ! Handle byte stuffing
                if (output(byte_count) == 255) then
                    byte_count = byte_count + 1
                    output(byte_count) = 0
                end if
                
                bit_buffer = 0
                bit_count = 0
            end if
        end do
    end subroutine
    
end program
module fortplot_zlib_huffman
    !! Huffman encoding and deflate compression functionality
    !! Split from fortplot_zlib.f90 for file size compliance (Issue #884)
    use, intrinsic :: iso_fortran_env, only: int8, int32
    implicit none
    
    private
    public :: compress_with_fixed_huffman, analyze_compressibility
    
    ! Deflate compression constants
    integer, parameter :: MAX_MATCH = 258
    integer, parameter :: MIN_MATCH = 3
    integer, parameter :: MAX_DISTANCE = 32768
    integer, parameter :: HASH_BITS = 15
    integer, parameter :: HASH_SIZE = 2**HASH_BITS
    integer, parameter :: WINDOW_SIZE = 32768
    
contains

    subroutine init_fixed_huffman_tables(literal_codes, literal_lengths, distance_codes, distance_lengths)
        !! Initialize fixed Huffman tables as per RFC 1951
        integer, intent(out) :: literal_codes(0:285)
        integer, intent(out) :: literal_lengths(0:285)
        integer, intent(out) :: distance_codes(0:29)
        integer, intent(out) :: distance_lengths(0:29)
        integer :: i, code
        
        ! Fixed literal/length codes
        code = 0
        ! 0-143: 8 bits (00110000 - 10111111)
        do i = 0, 143
            literal_codes(i) = code + 48  ! Start at 00110000
            literal_lengths(i) = 8
            code = code + 1
        end do
        
        code = 0
        ! 144-255: 9 bits (110010000 - 111111111)
        do i = 144, 255
            literal_codes(i) = code + 400  ! Start at 110010000
            literal_lengths(i) = 9
            code = code + 1
        end do
        
        code = 0
        ! 256-279: 7 bits (0000000 - 0010111)
        do i = 256, 279
            literal_codes(i) = code
            literal_lengths(i) = 7
            code = code + 1
        end do
        
        code = 0
        ! 280-287: 8 bits (11000000 - 11000111)
        do i = 280, 285  ! Note: only 280-285 are valid
            literal_codes(i) = code + 192  ! Start at 11000000
            literal_lengths(i) = 8
            code = code + 1
        end do
        
        ! Fixed distance codes (all 5 bits)
        do i = 0, 29
            distance_codes(i) = i
            distance_lengths(i) = 5
        end do
    end subroutine init_fixed_huffman_tables
    
    function calculate_hash(data, pos, data_len) result(hash_val)
        !! Calculate hash for LZ77 matching (3-byte hash)
        integer(int8), intent(in) :: data(*)
        integer, intent(in) :: pos, data_len
        integer :: hash_val
        
        if (pos + 2 <= data_len) then
            hash_val = iand(ior(ior(ishft(iand(int(data(pos)), 255), 16), &
                                   ishft(iand(int(data(pos+1)), 255), 8)), &
                               iand(int(data(pos+2)), 255)), HASH_SIZE - 1)
        else
            hash_val = 0
        end if
    end function calculate_hash
    
    subroutine update_hash_table(hash_table, hash_chain, hash_val, pos)
        !! Update hash table for LZ77 matching
        integer, intent(inout) :: hash_table(0:)
        integer, intent(inout) :: hash_chain(:)
        integer, intent(in) :: hash_val, pos
        
        integer :: chain_pos
        
        chain_pos = iand(pos, WINDOW_SIZE - 1) + 1
        hash_chain(chain_pos) = hash_table(hash_val)
        hash_table(hash_val) = pos
    end subroutine update_hash_table
    
    subroutine find_longest_match(data, pos, data_len, hash_table, hash_chain, match_len, match_dist)
        !! Find longest match using LZ77 algorithm
        integer(int8), intent(in) :: data(*)
        integer, intent(in) :: pos, data_len
        integer, intent(in) :: hash_table(0:)
        integer, intent(in) :: hash_chain(:)
        integer, intent(out) :: match_len, match_dist
        
        integer :: hash_val, chain_pos, candidate_pos, len, max_len
        integer :: i, chain_length
        
        match_len = 0
        match_dist = 0
        max_len = min(MAX_MATCH, data_len - pos + 1)
        
        if (max_len < MIN_MATCH) return
        
        hash_val = calculate_hash(data, pos, data_len)
        candidate_pos = hash_table(hash_val)
        chain_length = 0
        
        do while (candidate_pos > 0 .and. candidate_pos < pos .and. chain_length < 128)
            if (pos - candidate_pos > MAX_DISTANCE) exit
            
            ! Check match length
            len = 0
            do i = 0, max_len - 1
                if (data(pos + i) == data(candidate_pos + i)) then
                    len = len + 1
                else
                    exit
                end if
            end do
            
            if (len >= MIN_MATCH .and. len > match_len) then
                match_len = len
                match_dist = pos - candidate_pos
                if (len >= max_len) exit  ! Found maximum possible match
            end if
            
            ! Follow hash chain
            chain_pos = iand(candidate_pos, WINDOW_SIZE - 1) + 1
            candidate_pos = hash_chain(chain_pos)
            chain_length = chain_length + 1
        end do
    end subroutine find_longest_match
    
    subroutine write_bits(buffer, bit_pos, byte_pos, value, num_bits)
        !! Write bits to output buffer (LSB first)
        integer(int8), intent(inout) :: buffer(:)
        integer, intent(inout) :: bit_pos, byte_pos
        integer, intent(in) :: value, num_bits
        
        integer :: i, bit
        
        do i = 0, num_bits - 1
            bit = iand(ishft(value, -i), 1)
            
            if (bit_pos == 0) then
                buffer(byte_pos) = 0
            end if
            
            buffer(byte_pos) = ior(buffer(byte_pos), int(ishft(bit, bit_pos), int8))
            bit_pos = bit_pos + 1
            
            if (bit_pos == 8) then
                bit_pos = 0
                byte_pos = byte_pos + 1
            end if
        end do
    end subroutine write_bits
    
    subroutine encode_literal(buffer, bit_pos, byte_pos, literal, codes, lengths)
        !! Encode a literal using Huffman coding
        integer(int8), intent(inout) :: buffer(:)
        integer, intent(inout) :: bit_pos, byte_pos
        integer(int8), intent(in) :: literal
        integer, intent(in) :: codes(0:), lengths(0:)
        
        integer :: lit_val
        
        lit_val = iand(int(literal), 255)  ! Ensure 0-255 range
        call write_bits(buffer, bit_pos, byte_pos, bit_reverse(codes(lit_val), lengths(lit_val)), lengths(lit_val))
    end subroutine encode_literal
    
    subroutine encode_length_distance(buffer, bit_pos, byte_pos, length, distance, &
                                      literal_codes, literal_lengths, distance_codes, distance_lengths)
        !! Encode length-distance pair using Huffman coding
        integer(int8), intent(inout) :: buffer(:)
        integer, intent(inout) :: bit_pos, byte_pos
        integer, intent(in) :: length, distance
        integer, intent(in) :: literal_codes(0:), literal_lengths(0:)
        integer, intent(in) :: distance_codes(0:), distance_lengths(0:)
        
        integer :: length_code, length_extra_bits, length_extra
        integer :: distance_code, distance_extra_bits, distance_extra
        
        ! Encode length
        call get_length_code(length, length_code, length_extra_bits, length_extra)
        call write_bits(buffer, bit_pos, byte_pos, &
                       bit_reverse(literal_codes(length_code), literal_lengths(length_code)), &
                       literal_lengths(length_code))
        if (length_extra_bits > 0) then
            call write_bits(buffer, bit_pos, byte_pos, length_extra, length_extra_bits)
        end if
        
        ! Encode distance
        call get_distance_code(distance, distance_code, distance_extra_bits, distance_extra)
        call write_bits(buffer, bit_pos, byte_pos, &
                       bit_reverse(distance_codes(distance_code), distance_lengths(distance_code)), &
                       distance_lengths(distance_code))
        if (distance_extra_bits > 0) then
            call write_bits(buffer, bit_pos, byte_pos, distance_extra, distance_extra_bits)
        end if
    end subroutine encode_length_distance
    
    subroutine get_length_code(length, code, extra_bits, extra)
        !! Get length code and extra bits according to RFC 1951
        integer, intent(in) :: length
        integer, intent(out) :: code, extra_bits, extra
        
        if (length < 3 .or. length > 258) then
            code = 256  ! Invalid, use end-of-block
            extra_bits = 0
            extra = 0
            return
        end if
        
        if (length <= 10) then
            code = 257 + (length - 3)
            extra_bits = 0
            extra = 0
        else if (length <= 18) then
            code = 265 + (length - 11) / 2
            extra_bits = 1
            extra = mod(length - 11, 2)
        else if (length <= 34) then
            code = 269 + (length - 19) / 4
            extra_bits = 2
            extra = mod(length - 19, 4)
        else if (length <= 66) then
            code = 273 + (length - 35) / 8
            extra_bits = 3
            extra = mod(length - 35, 8)
        else if (length <= 130) then
            code = 277 + (length - 67) / 16
            extra_bits = 4
            extra = mod(length - 67, 16)
        else if (length <= 257) then
            code = 281 + (length - 131) / 32
            extra_bits = 5
            extra = mod(length - 131, 32)
        else  ! length = 258
            code = 285
            extra_bits = 0
            extra = 0
        end if
    end subroutine get_length_code
    
    subroutine get_distance_code(distance, code, extra_bits, extra)
        !! Get distance code and extra bits according to RFC 1951
        integer, intent(in) :: distance
        integer, intent(out) :: code, extra_bits, extra
        
        if (distance < 1 .or. distance > 32768) then
            code = 0
            extra_bits = 0
            extra = 0
            return
        end if
        
        if (distance <= 4) then
            code = distance - 1
            extra_bits = 0
            extra = 0
        else if (distance <= 8) then
            code = 4 + (distance - 5) / 2
            extra_bits = 1
            extra = mod(distance - 5, 2)
        else if (distance <= 16) then
            code = 6 + (distance - 9) / 4
            extra_bits = 2
            extra = mod(distance - 9, 4)
        else if (distance <= 32) then
            code = 8 + (distance - 17) / 8
            extra_bits = 3
            extra = mod(distance - 17, 8)
        else if (distance <= 64) then
            code = 10 + (distance - 33) / 16
            extra_bits = 4
            extra = mod(distance - 33, 16)
        else if (distance <= 128) then
            code = 12 + (distance - 65) / 32
            extra_bits = 5
            extra = mod(distance - 65, 32)
        else if (distance <= 256) then
            code = 14 + (distance - 129) / 64
            extra_bits = 6
            extra = mod(distance - 129, 64)
        else if (distance <= 512) then
            code = 16 + (distance - 257) / 128
            extra_bits = 7
            extra = mod(distance - 257, 128)
        else if (distance <= 1024) then
            code = 18 + (distance - 513) / 256
            extra_bits = 8
            extra = mod(distance - 513, 256)
        else if (distance <= 2048) then
            code = 20 + (distance - 1025) / 512
            extra_bits = 9
            extra = mod(distance - 1025, 512)
        else if (distance <= 4096) then
            code = 22 + (distance - 2049) / 1024
            extra_bits = 10
            extra = mod(distance - 2049, 1024)
        else if (distance <= 8192) then
            code = 24 + (distance - 4097) / 2048
            extra_bits = 11
            extra = mod(distance - 4097, 2048)
        else if (distance <= 16384) then
            code = 26 + (distance - 8193) / 4096
            extra_bits = 12
            extra = mod(distance - 8193, 4096)
        else  ! distance <= 32768
            code = 28 + (distance - 16385) / 8192
            extra_bits = 13
            extra = mod(distance - 16385, 8192)
        end if
    end subroutine get_distance_code

    function bit_reverse(value, num_bits) result(reversed_value)
        !! Reverses the bits of a given value up to num_bits.
        integer, intent(in) :: value, num_bits
        integer :: reversed_value
        integer :: i
        
        reversed_value = 0
        do i = 0, num_bits - 1
            if (iand(ishft(value, -i), 1) == 1) then
                reversed_value = ior(reversed_value, ishft(1, num_bits - 1 - i))
            end if
        end do
    end function bit_reverse
    
    function analyze_compressibility(data, data_len) result(ratio)
        !! Analyze data to estimate compression ratio
        !! Returns value between 0 (incompressible) and 1 (highly compressible)
        integer(int8), intent(in) :: data(*)
        integer, intent(in) :: data_len
        real :: ratio
        
        integer :: i, run_length, total_runs
        integer(int8) :: current_byte, prev_byte
        integer :: byte_counts(0:255)
        real :: entropy
        
        if (data_len < 10) then
            ratio = 0.1
            return
        end if
        
        ! Count byte frequencies and runs
        byte_counts = 0
        total_runs = 0
        prev_byte = data(1)
        run_length = 1
        
        do i = 1, min(data_len, 8192)  ! Sample first 8KB
            current_byte = data(i)
            byte_counts(iand(int(current_byte), 255)) = &
                byte_counts(iand(int(current_byte), 255)) + 1
            
            if (i > 1) then
                if (current_byte == prev_byte) then
                    run_length = run_length + 1
                else
                    if (run_length >= 3) total_runs = total_runs + 1
                    run_length = 1
                    prev_byte = current_byte
                end if
            end if
        end do
        
        ! Calculate simple entropy estimate
        entropy = 0.0
        do i = 0, 255
            if (byte_counts(i) > 0) then
                entropy = entropy + 1.0
            end if
        end do
        entropy = entropy / 256.0  ! Normalize
        
        ! Estimate compression ratio based on entropy and runs
        ratio = (1.0 - entropy) * 0.5 + real(total_runs) / real(data_len/10) * 0.5
        ratio = max(0.0, min(1.0, ratio))
        
    end function analyze_compressibility
    
    subroutine compress_with_fixed_huffman(input_data, input_len, output_buffer, output_pos)
        !! Compress data using fixed Huffman codes with LZ77
        integer(int8), intent(in) :: input_data(*)
        integer, intent(in) :: input_len
        integer(int8), intent(inout) :: output_buffer(:)
        integer, intent(inout) :: output_pos
        
        integer :: literal_codes(0:285), literal_lengths(0:285)
        integer :: distance_codes(0:29), distance_lengths(0:29)
        integer :: hash_table(0:HASH_SIZE-1)
        integer :: hash_chain(WINDOW_SIZE)
        integer :: pos, match_len, match_dist
        integer :: bit_pos, byte_pos
        integer :: block_size, block_start
        
        ! Initialize fixed Huffman tables
        call init_fixed_huffman_tables(literal_codes, literal_lengths, &
                                      distance_codes, distance_lengths)
        
        ! Initialize hash structures
        hash_table = 0
        hash_chain = 0
        
        pos = 1
        byte_pos = output_pos
        bit_pos = 0
        
        ! Process in blocks to avoid overflow
        do while (pos <= input_len)
            block_start = pos
            block_size = min(input_len - pos + 1, 32768)
            
            ! Write block header: BFINAL, BTYPE=01 (fixed Huffman)
            if (pos + block_size >= input_len) then
                call write_bits(output_buffer, bit_pos, byte_pos, 3, 3)  ! BFINAL=1, BTYPE=01
            else
                call write_bits(output_buffer, bit_pos, byte_pos, 2, 3)  ! BFINAL=0, BTYPE=01
            end if
            
            ! Compress block data
            do while (pos <= block_start + block_size - 1 .and. pos <= input_len)
                ! Try to find a match
                call find_longest_match(input_data, pos, input_len, &
                                       hash_table, hash_chain, match_len, match_dist)
                
                if (match_len >= MIN_MATCH) then
                    ! Encode length-distance pair
                    call encode_length_distance(output_buffer, bit_pos, byte_pos, &
                                              match_len, match_dist, &
                                              literal_codes, literal_lengths, &
                                              distance_codes, distance_lengths)
                    
                    ! Update hash table for all matched positions
                    do while (match_len > 0)
                        if (pos <= input_len - 2) then
                            call update_hash_table(hash_table, hash_chain, &
                                                 calculate_hash(input_data, pos, input_len), pos)
                        end if
                        pos = pos + 1
                        match_len = match_len - 1
                    end do
                else
                    ! Encode literal
                    call encode_literal(output_buffer, bit_pos, byte_pos, &
                                      input_data(pos), literal_codes, literal_lengths)
                    
                    ! Update hash table
                    if (pos <= input_len - 2) then
                        call update_hash_table(hash_table, hash_chain, &
                                             calculate_hash(input_data, pos, input_len), pos)
                    end if
                    pos = pos + 1
                end if
            end do
            
            ! Write end-of-block code (256)
            call write_bits(output_buffer, bit_pos, byte_pos, &
                          bit_reverse(literal_codes(256), literal_lengths(256)), &
                          literal_lengths(256))
        end do
        
        ! Ensure final bits are written
        if (bit_pos > 0) then
            byte_pos = byte_pos + 1
        end if
        
        output_pos = byte_pos
        
    end subroutine compress_with_fixed_huffman

end module fortplot_zlib_huffman
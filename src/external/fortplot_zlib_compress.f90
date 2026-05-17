module fortplot_zlib_compress
    !! Deflate compression, CRC32, and Adler-32 checksum implementation
    !! Extracted from fortplot_zlib_core for size compliance (Issue #1747)
    use, intrinsic :: iso_fortran_env, only: int8, int32
    use iso_c_binding, only: c_ptr, c_loc, c_f_pointer, c_associated
    use fortplot_zlib_checksums, only: crc32_calculate, calculate_adler32, &
                                       initialize_zlib_debug
    implicit none

    private
    public :: crc32_calculate, deflate_compress, calculate_adler32
    public :: initialize_zlib_debug
    public :: init_fixed_huffman_tables
    public :: bit_reverse
    public :: zlib_compress, zlib_compress_into

    integer, parameter :: MAX_MATCH = 258
    integer, parameter :: MIN_MATCH = 3
    integer, parameter :: MAX_DISTANCE = 32768
    integer, parameter :: HASH_BITS = 15
    integer, parameter :: HASH_SIZE = 2**HASH_BITS
    integer, parameter :: WINDOW_SIZE = 32768

contains

    subroutine deflate_compress(input_data, input_len, output_data, output_len)
        !! Full deflate compression implementation with LZ77 and Huffman coding
        integer(int8), intent(in) :: input_data(*)
        integer, intent(in) :: input_len
        integer(int8), allocatable, intent(out) :: output_data(:)
        integer, intent(out) :: output_len

        integer :: hash_table(0:HASH_SIZE - 1)
        integer :: hash_chain(WINDOW_SIZE)

        integer :: literal_codes(0:285)
        integer :: literal_lengths(0:285)
        integer :: distance_codes(0:29)
        integer :: distance_lengths(0:29)

        integer(int8), allocatable :: bit_buffer(:)
        integer :: bit_pos, byte_pos
        integer :: i, pos, match_len, match_dist
        integer :: hash_val

        call init_fixed_huffman_tables(literal_codes, literal_lengths, distance_codes, distance_lengths)

        hash_table = -1
        hash_chain = -1

        allocate (bit_buffer(max(64, input_len*2)))
        bit_pos = 0
        byte_pos = 1

        call write_bits(bit_buffer, bit_pos, byte_pos, 1, 1)
        call write_bits(bit_buffer, bit_pos, byte_pos, 1, 2)

        pos = 1
        do while (pos <= input_len)
            call find_longest_match(input_data, pos, input_len, hash_table, hash_chain, match_len, match_dist)

            if (match_len >= MIN_MATCH) then
     call encode_length_distance(bit_buffer, bit_pos, byte_pos, match_len, match_dist, &
                       literal_codes, literal_lengths, distance_codes, distance_lengths)

                do i = 0, match_len - 1
                    if (pos + i <= input_len) then
                        hash_val = calculate_hash(input_data, pos + i, input_len)
                       call update_hash_table(hash_table, hash_chain, hash_val, pos + i)
                    end if
                end do
                pos = pos + match_len
            else
                call encode_literal(bit_buffer, bit_pos, byte_pos, input_data(pos), literal_codes, literal_lengths)

                if (pos <= input_len) then
                    hash_val = calculate_hash(input_data, pos, input_len)
                    call update_hash_table(hash_table, hash_chain, hash_val, pos)
                end if
                pos = pos + 1
            end if
        end do

        call write_bits(bit_buffer, bit_pos, byte_pos, bit_reverse(literal_codes(256), literal_lengths(256)), literal_lengths(256))

        if (bit_pos > 0) then
            byte_pos = byte_pos + 1
        end if

        output_len = byte_pos - 1
        allocate (output_data(output_len))
        output_data(1:output_len) = bit_buffer(1:output_len)

    end subroutine deflate_compress

    subroutine init_fixed_huffman_tables(literal_codes, literal_lengths, distance_codes, distance_lengths)
        !! Initialize fixed Huffman tables as per RFC 1951
        integer, intent(out) :: literal_codes(0:285)
        integer, intent(out) :: literal_lengths(0:285)
        integer, intent(out) :: distance_codes(0:29)
        integer, intent(out) :: distance_lengths(0:29)
        integer :: i, code

        code = 0
        do i = 0, 143
            literal_codes(i) = code + 48
            literal_lengths(i) = 8
            code = code + 1
        end do

        code = 0
        do i = 144, 255
            literal_codes(i) = code + 400
            literal_lengths(i) = 9
            code = code + 1
        end do

        code = 0
        do i = 256, 279
            literal_codes(i) = code
            literal_lengths(i) = 7
            code = code + 1
        end do

        code = 0
        do i = 280, 285
            literal_codes(i) = code + 192
            literal_lengths(i) = 8
            code = code + 1
        end do

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
                                    ishft(iand(int(data(pos + 1)), 255), 8)), &
                                iand(int(data(pos + 2)), 255)), HASH_SIZE - 1)
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
                if (len >= max_len) exit
            end if

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

        lit_val = iand(int(literal), 255)
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

        call get_length_code(length, length_code, length_extra_bits, length_extra)
        call write_bits(buffer, bit_pos, byte_pos, &
                bit_reverse(literal_codes(length_code), literal_lengths(length_code)), &
                        literal_lengths(length_code))
        if (length_extra_bits > 0) then
            call write_bits(buffer, bit_pos, byte_pos, length_extra, length_extra_bits)
        end if

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
            code = 256
            extra_bits = 0
            extra = 0
            return
        end if

        if (length <= 10) then
            code = 257 + (length - 3)
            extra_bits = 0
            extra = 0
        else if (length <= 18) then
            code = 265 + (length - 11)/2
            extra_bits = 1
            extra = mod(length - 11, 2)
        else if (length <= 34) then
            code = 269 + (length - 19)/4
            extra_bits = 2
            extra = mod(length - 19, 4)
        else if (length <= 66) then
            code = 273 + (length - 35)/8
            extra_bits = 3
            extra = mod(length - 35, 8)
        else if (length <= 130) then
            code = 277 + (length - 67)/16
            extra_bits = 4
            extra = mod(length - 67, 16)
        else if (length <= 257) then
            code = 281 + (length - 131)/32
            extra_bits = 5
            extra = mod(length - 131, 32)
        else
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
            code = 4 + (distance - 5)/2
            extra_bits = 1
            extra = mod(distance - 5, 2)
        else if (distance <= 16) then
            code = 6 + (distance - 9)/4
            extra_bits = 2
            extra = mod(distance - 9, 4)
        else if (distance <= 32) then
            code = 8 + (distance - 17)/8
            extra_bits = 3
            extra = mod(distance - 17, 8)
        else if (distance <= 64) then
            code = 10 + (distance - 33)/16
            extra_bits = 4
            extra = mod(distance - 33, 16)
        else if (distance <= 128) then
            code = 12 + (distance - 65)/32
            extra_bits = 5
            extra = mod(distance - 65, 32)
        else if (distance <= 256) then
            code = 14 + (distance - 129)/64
            extra_bits = 6
            extra = mod(distance - 129, 64)
        else if (distance <= 512) then
            code = 16 + (distance - 257)/128
            extra_bits = 7
            extra = mod(distance - 257, 128)
        else if (distance <= 1024) then
            code = 18 + (distance - 513)/256
            extra_bits = 8
            extra = mod(distance - 513, 256)
        else if (distance <= 2048) then
            code = 20 + (distance - 1025)/512
            extra_bits = 9
            extra = mod(distance - 1025, 512)
        else if (distance <= 4096) then
            code = 22 + (distance - 2049)/1024
            extra_bits = 10
            extra = mod(distance - 2049, 1024)
        else if (distance <= 8192) then
            code = 24 + (distance - 4097)/2048
            extra_bits = 11
            extra = mod(distance - 4097, 2048)
        else if (distance <= 16384) then
            code = 26 + (distance - 8193)/4096
            extra_bits = 12
            extra = mod(distance - 8193, 4096)
        else
            code = 28 + (distance - 16385)/8192
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

    subroutine zlib_compress_into(input_data, input_len, output_data, output_len)
        !! Compress data into a newly allocated buffer with zlib wrapper
        integer(int8), intent(in) :: input_data(*)
        integer, intent(in) :: input_len
        integer(int8), allocatable, intent(out) :: output_data(:)
        integer, intent(out) :: output_len

        integer(int8), allocatable :: compressed_block(:)
        integer :: compressed_block_len
        integer(int32) :: adler32_checksum
        integer :: pos

        call deflate_compress(input_data, input_len, compressed_block, compressed_block_len)

        output_len = 2 + compressed_block_len + 4
        allocate (output_data(output_len))

        pos = 1
        output_data(pos) = int(z'78', int8)
        pos = pos + 1
        output_data(pos) = int(z'5E', int8)
        pos = pos + 1
        output_data(pos:pos + compressed_block_len - 1) = &
            compressed_block(1:compressed_block_len)
        pos = pos + compressed_block_len

        adler32_checksum = calculate_adler32(input_data, input_len)
        output_data(pos) = int(iand(ishft(adler32_checksum, -24), 255), int8)
        pos = pos + 1
        output_data(pos) = int(iand(ishft(adler32_checksum, -16), 255), int8)
        pos = pos + 1
        output_data(pos) = int(iand(ishft(adler32_checksum, -8), 255), int8)
        pos = pos + 1
        output_data(pos) = int(iand(adler32_checksum, 255), int8)

    end subroutine zlib_compress_into

    function zlib_compress(input_data, input_len, output_len) result(output_data)
        !! Backwards-compatible wrapper returning an allocatable result
        integer(int8), intent(in) :: input_data(*)
        integer, intent(in) :: input_len
        integer, intent(out) :: output_len
        integer(int8), allocatable :: output_data(:)

        call zlib_compress_into(input_data, input_len, output_data, output_len)
    end function zlib_compress

end module fortplot_zlib_compress

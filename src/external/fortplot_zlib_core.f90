module fortplot_zlib_core
    !! Pure Fortran implementation of zlib compression, decompression, and CRC32
    !! Ported from STB image libraries for self-contained PNG support

    use, intrinsic :: iso_fortran_env, only: int8, int32
    use iso_c_binding, only: c_ptr, c_loc, c_f_pointer, c_associated
    use fortplot_zlib_utils, only: crc32_calculate, calculate_adler32, bit_reverse, &
                                  write_bits, encode_literal, encode_length_distance
    implicit none

    private
    public :: zlib_compress, zlib_decompress, crc32_calculate

    ! Deflate compression constants
    integer, parameter :: MAX_MATCH = 258
    integer, parameter :: MIN_MATCH = 3
    integer, parameter :: MAX_DISTANCE = 32768
    integer, parameter :: HASH_BITS = 15
    integer, parameter :: HASH_SIZE = 2**HASH_BITS
    integer, parameter :: WINDOW_SIZE = 32768

    ! Decode table parameters for fixed Huffman streams
    integer, parameter :: LITERAL_TABLE_BITS = 9
    integer, parameter :: DISTANCE_TABLE_BITS = 5
    integer, parameter :: length_base(257:285) = [ &
        3, 4, 5, 6, 7, 8, 9, 10, &
        11, 13, 15, 17, 19, 23, 27, 31, &
        35, 43, 51, 59, 67, 83, 99, 115, &
        131, 163, 195, 227, 258 ]
    integer, parameter :: length_extra_bits(257:285) = [ &
        0, 0, 0, 0, 0, 0, 0, 0, &
        1, 1, 1, 1, 2, 2, 2, 2, &
        3, 3, 3, 3, 4, 4, 4, 4, &
        5, 5, 5, 5, 0 ]
    integer, parameter :: distance_base(0:29) = [ &
        1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193, &
        257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145, &
        8193, 12289, 16385, 24577 ]
    integer, parameter :: distance_extra_bits(0:29) = [ &
        0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, &
        7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13 ]

contains

    function zlib_compress(input_data, input_len, output_len) result(output_data)
        integer(int8), intent(in) :: input_data(:)
        integer, intent(in) :: input_len
        integer, intent(out) :: output_len
        integer(int8), allocatable :: output_data(:)

        integer, parameter :: MAX_OUTPUT_SIZE = 65536
        integer(int8) :: temp_buffer(MAX_OUTPUT_SIZE)
        integer(int32) :: adler32_checksum
        integer :: header_size, compressed_size

        if (input_len <= 0) then
            output_len = 0
            allocate(output_data(1))
            return
        end if

        ! Create zlib header (2 bytes)
        temp_buffer(1) = int(z'78', int8)  ! CMF: Compression method + info
        temp_buffer(2) = int(z'9C', int8)  ! FLG: Flags

        header_size = 2

        ! Compress data using deflate
        call deflate_compress(input_data, input_len, temp_buffer(header_size+1:), compressed_size)

        ! Calculate and append Adler32 checksum (4 bytes, big endian)
        adler32_checksum = calculate_adler32(input_data, input_len)
        temp_buffer(header_size + compressed_size + 1) = int(ishft(adler32_checksum, -24), int8)
        temp_buffer(header_size + compressed_size + 2) = int(ishft(adler32_checksum, -16), int8)
        temp_buffer(header_size + compressed_size + 3) = int(ishft(adler32_checksum, -8), int8)
        temp_buffer(header_size + compressed_size + 4) = int(adler32_checksum, int8)

        output_len = header_size + compressed_size + 4

        allocate(output_data(output_len))
        output_data(1:output_len) = temp_buffer(1:output_len)
    end function zlib_compress

    function zlib_decompress(input_data, input_len, status, verify_checksum) result(output_data)
        integer(int8), intent(in) :: input_data(:)
        integer, intent(in) :: input_len
        integer, intent(out) :: status
        logical, intent(in), optional :: verify_checksum
        integer(int8), allocatable :: output_data(:)

        integer, parameter :: MAX_OUTPUT_SIZE = 1048576
        integer(int8) :: temp_buffer(MAX_OUTPUT_SIZE)
        integer :: cmf, flg, output_pos, bit_pos, byte_pos
        integer(int32) :: stored_adler32, calculated_adler32
        logical :: verify_check

        status = 0
        verify_check = .true.
        if (present(verify_checksum)) verify_check = verify_checksum

        if (input_len < 6) then
            status = -1
            allocate(output_data(1))
            return
        end if

        ! Check zlib header
        cmf = input_data(1)
        flg = input_data(2)

        if (iand(cmf, 15) /= 8) then  ! Only deflate method supported
            status = -2
            allocate(output_data(1))
            return
        end if

        if (mod(cmf * 256 + flg, 31) /= 0) then  ! Header checksum
            status = -3
            allocate(output_data(1))
            return
        end if

        ! Decompress deflate stream
        bit_pos = 0
        byte_pos = 3
        call inflate_decompress(input_data, input_len, byte_pos, bit_pos, &
                               temp_buffer, output_pos, status)

        if (status /= 0) then
            allocate(output_data(1))
            return
        end if

        ! Verify Adler32 checksum if requested
        if (verify_check) then
            if (byte_pos + 3 >= input_len) then
                status = -4
                allocate(output_data(1))
                return
            end if

            stored_adler32 = ior(ior(ior(ishft(int(input_data(byte_pos), int32), 24), &
                                         ishft(int(input_data(byte_pos+1), int32), 16)), &
                                     ishft(int(input_data(byte_pos+2), int32), 8)), &
                                 int(input_data(byte_pos+3), int32))

            calculated_adler32 = calculate_adler32(temp_buffer, output_pos)

            if (stored_adler32 /= calculated_adler32) then
                status = -5
                allocate(output_data(1))
                return
            end if
        end if

        allocate(output_data(output_pos))
        output_data(1:output_pos) = temp_buffer(1:output_pos)
    end function zlib_decompress

    subroutine deflate_compress(input_data, input_len, output_data, output_len)
        integer(int8), intent(in) :: input_data(:)
        integer, intent(in) :: input_len
        integer(int8), intent(out) :: output_data(:)
        integer, intent(out) :: output_len

        integer :: bit_pos, byte_pos
        integer :: literal_codes(0:287), literal_lengths(0:287)
        integer :: distance_codes(0:31), distance_lengths(0:31)
        integer :: i

        bit_pos = 0
        byte_pos = 1
        output_data = 0

        ! Initialize fixed Huffman tables
        call init_fixed_huffman_tables(literal_codes, literal_lengths, distance_codes, distance_lengths)

        ! Write deflate block header (final block, fixed Huffman)
        call write_bits(output_data, bit_pos, byte_pos, 1, 1)  ! BFINAL = 1
        call write_bits(output_data, bit_pos, byte_pos, 1, 2)  ! BTYPE = 01 (fixed Huffman)

        ! Encode all input as literals (simple implementation)
        do i = 1, input_len
            call encode_literal(output_data, bit_pos, byte_pos, int(input_data(i)), &
                               literal_codes, literal_lengths)
        end do

        ! Write end of block symbol (256)
        call encode_literal(output_data, bit_pos, byte_pos, 256, literal_codes, literal_lengths)

        ! Byte align
        if (bit_pos > 0) then
            byte_pos = byte_pos + 1
        end if

        output_len = byte_pos - 1
    end subroutine deflate_compress

    subroutine init_fixed_huffman_tables(literal_codes, literal_lengths, distance_codes, distance_lengths)
        integer, intent(out) :: literal_codes(0:287), literal_lengths(0:287)
        integer, intent(out) :: distance_codes(0:31), distance_lengths(0:31)
        integer :: i

        ! Fixed literal/length codes
        do i = 0, 143
            literal_lengths(i) = 8
            literal_codes(i) = bit_reverse(48 + i, 8)
        end do
        do i = 144, 255
            literal_lengths(i) = 9
            literal_codes(i) = bit_reverse(400 + i - 144, 9)
        end do
        do i = 256, 279
            literal_lengths(i) = 7
            literal_codes(i) = bit_reverse(i - 256, 7)
        end do
        do i = 280, 287
            literal_lengths(i) = 8
            literal_codes(i) = bit_reverse(192 + i - 280, 8)
        end do

        ! Fixed distance codes (all length 5)
        do i = 0, 31
            distance_lengths(i) = 5
            distance_codes(i) = bit_reverse(i, 5)
        end do
    end subroutine init_fixed_huffman_tables

    subroutine inflate_decompress(input_data, input_len, byte_pos, bit_pos, &
                                 output_data, output_pos, status)
        integer(int8), intent(in) :: input_data(:)
        integer, intent(in) :: input_len
        integer, intent(inout) :: byte_pos, bit_pos
        integer(int8), intent(out) :: output_data(:)
        integer, intent(out) :: output_pos, status

        integer :: bfinal, btype
        logical :: final_block

        output_pos = 0
        status = 0
        final_block = .false.

        do while (.not. final_block)
            if (byte_pos > input_len) then
                status = -1
                return
            end if

            bfinal = read_bits(input_data, byte_pos, bit_pos, 1)
            btype = read_bits(input_data, byte_pos, bit_pos, 2)

            final_block = (bfinal == 1)

            select case(btype)
            case(0)  ! No compression
                call inflate_stored_block(input_data, input_len, byte_pos, bit_pos, &
                                        output_data, output_pos, status)
            case(1)  ! Fixed Huffman
                call inflate_fixed_block(input_data, input_len, byte_pos, bit_pos, &
                                        output_data, output_pos, status)
            case(2)  ! Dynamic Huffman
                call inflate_dynamic_block(input_data, input_len, byte_pos, bit_pos, &
                                          output_data, output_pos, status)
            case default
                status = -2
                return
            end select

            if (status /= 0) return
        end do
    end subroutine inflate_decompress

    function read_bits(input_data, byte_pos, bit_pos, num_bits) result(value)
        integer(int8), intent(in) :: input_data(:)
        integer, intent(inout) :: byte_pos, bit_pos
        integer, intent(in) :: num_bits
        integer :: value
        integer :: bits_read, bits_available

        value = 0
        bits_read = 0

        do while (bits_read < num_bits)
            if (byte_pos > size(input_data)) exit

            bits_available = min(8 - bit_pos, num_bits - bits_read)
            value = ior(value, ishft(iand(ishft(int(input_data(byte_pos)), -bit_pos), &
                                         ishft(1, bits_available) - 1), bits_read))

            bit_pos = bit_pos + bits_available
            bits_read = bits_read + bits_available

            if (bit_pos >= 8) then
                bit_pos = 0
                byte_pos = byte_pos + 1
            end if
        end do
    end function read_bits

    subroutine inflate_stored_block(input_data, input_len, byte_pos, bit_pos, &
                                   output_data, output_pos, status)
        integer(int8), intent(in) :: input_data(:)
        integer, intent(in) :: input_len
        integer, intent(inout) :: byte_pos, bit_pos
        integer(int8), intent(inout) :: output_data(:)
        integer, intent(inout) :: output_pos
        integer, intent(out) :: status

        integer :: len, nlen, i

        status = 0

        ! Byte align
        if (bit_pos > 0) then
            bit_pos = 0
            byte_pos = byte_pos + 1
        end if

        if (byte_pos + 3 >= input_len) then
            status = -1
            return
        end if

        len = ior(int(input_data(byte_pos)), ishft(int(input_data(byte_pos+1)), 8))
        nlen = ior(int(input_data(byte_pos+2)), ishft(int(input_data(byte_pos+3)), 8))

        if (len /= iand(not(nlen), 65535)) then
            status = -2
            return
        end if

        byte_pos = byte_pos + 4

        if (byte_pos + len - 1 > input_len) then
            status = -3
            return
        end if

        do i = 1, len
            if (output_pos >= size(output_data)) then
                status = -4
                return
            end if
            output_pos = output_pos + 1
            output_data(output_pos) = input_data(byte_pos + i - 1)
        end do

        byte_pos = byte_pos + len
    end subroutine inflate_stored_block

    subroutine inflate_fixed_block(input_data, input_len, byte_pos, bit_pos, &
                                  output_data, output_pos, status)
        integer(int8), intent(in) :: input_data(:)
        integer, intent(in) :: input_len
        integer, intent(inout) :: byte_pos, bit_pos
        integer(int8), intent(inout) :: output_data(:)
        integer, intent(inout) :: output_pos
        integer, intent(out) :: status

        integer :: symbol

        status = 0

        do
            symbol = decode_literal_length_symbol(input_data, byte_pos, bit_pos)

            if (symbol < 0) then
                status = -1
                return
            else if (symbol < 256) then
                ! Literal byte
                if (output_pos >= size(output_data)) then
                    status = -2
                    return
                end if
                output_pos = output_pos + 1
                output_data(output_pos) = int(symbol, int8)
            else if (symbol == 256) then
                ! End of block
                exit
            else
                ! Length/distance pair (simplified - just treat as error for now)
                status = -3
                return
            end if
        end do
    end subroutine inflate_fixed_block

    subroutine inflate_dynamic_block(input_data, input_len, byte_pos, bit_pos, &
                                    output_data, output_pos, status)
        integer(int8), intent(in) :: input_data(:)
        integer, intent(in) :: input_len
        integer, intent(inout) :: byte_pos, bit_pos
        integer(int8), intent(inout) :: output_data(:)
        integer, intent(inout) :: output_pos
        integer, intent(out) :: status

        ! Simplified implementation - just return error for now
        status = -1
    end subroutine inflate_dynamic_block

    function decode_literal_length_symbol(input_data, byte_pos, bit_pos) result(symbol)
        integer(int8), intent(in) :: input_data(:)
        integer, intent(inout) :: byte_pos, bit_pos
        integer :: symbol

        ! Simplified fixed Huffman decoding
        integer :: code, bits

        code = read_bits(input_data, byte_pos, bit_pos, 7)
        bits = 7

        if (code <= 23) then
            symbol = 256 + code
        else
            code = ior(ishft(code, 1), read_bits(input_data, byte_pos, bit_pos, 1))
            bits = 8
            if (code >= 48 .and. code <= 191) then
                symbol = code - 48
            else if (code >= 192 .and. code <= 199) then
                symbol = 280 + code - 192
            else
                code = ior(ishft(code, 1), read_bits(input_data, byte_pos, bit_pos, 1))
                bits = 9
                if (code >= 400 .and. code <= 511) then
                    symbol = 144 + code - 400
                else
                    symbol = -1  ! Error
                end if
            end if
        end if
    end function decode_literal_length_symbol

end module fortplot_zlib_core
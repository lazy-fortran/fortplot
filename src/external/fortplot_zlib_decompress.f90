module fortplot_zlib_decompress
    !! Pure Fortran implementation of zlib decompression and CRC32.
    !!
    !! Extracted from fortplot_zlib_core for size compliance (Issue #1694).
    !! Compression functions live in fortplot_zlib_compress.

    use, intrinsic :: iso_fortran_env, only: int8, int32
    use iso_c_binding, only: c_ptr, c_loc, c_f_pointer, c_associated
    use fortplot_zlib_compress, only: init_fixed_huffman_tables, bit_reverse
    use fortplot_zlib_checksums, only: crc32_calculate, calculate_adler32
    implicit none

    private
    public :: zlib_decompress

    private :: build_fixed_decode_table
    private :: zlib_init_decompress_state
    private :: zlib_decompress_blocks
    private :: zlib_decompress_stored_block
    private :: zlib_decode_compressed_block
    private :: zlib_decode_length_distance
    private :: zlib_read_bits
    private :: zlib_ensure_bits
    private :: zlib_drop_bits
    private :: zlib_align_to_byte
    private :: zlib_read_aligned_byte
    private :: zlib_reserve_output
    private :: zlib_append_byte
    private :: zlib_decode_literal_symbol
    private :: zlib_decode_distance_symbol
    private :: zlib_trim_output

    ! Decompression state for internal procedures
    private :: zlib_decompress_state_t
    type zlib_decompress_state_t
        integer :: idx = 1
        integer :: deflate_end_idx = 0
        integer(int32) :: bit_buffer = 0
        integer :: bit_count = 0
        integer :: status = 0
        integer :: out_size = 0
        integer :: input_len = 0
        integer(int8), allocatable :: output_data(:)
        integer(int8), allocatable :: input_data(:)
        integer :: literal_table(0:2**9 - 1)
        integer :: literal_table_len(0:2**9 - 1)
        integer :: distance_table(0:2**5 - 1)
        integer :: distance_table_len(0:2**5 - 1)
        integer :: mask_literal = 511
        integer :: mask_distance = 31
    end type zlib_decompress_state_t

    ! Decode table parameters for fixed Huffman streams
    integer, parameter :: LITERAL_TABLE_BITS = 9
    integer, parameter :: DISTANCE_TABLE_BITS = 5
    integer, parameter :: length_base(257:285) = [ &
                          3, 4, 5, 6, 7, 8, 9, 10, &
                          11, 13, 15, 17, 19, 23, 27, 31, &
                          35, 43, 51, 59, 67, 83, 99, 115, &
                          131, 163, 195, 227, 258]
    integer, parameter :: length_extra_bits(257:285) = [ &
                          0, 0, 0, 0, 0, 0, 0, 0, &
                          1, 1, 1, 1, 2, 2, 2, 2, &
                          3, 3, 3, 3, 4, 4, 4, 4, &
                          5, 5, 5, 5, 0]
    integer, parameter :: distance_base(0:29) = [ &
                          1, 2, 3, 4, 5, 7, 9, 13, &
                          17, 25, 33, 49, 65, 97, 129, 193, &
                          257, 385, 513, 769, 1025, 1537, 2049, 3073, &
                          4097, 6145, 8193, 12289, 16385, 24577]
    integer, parameter :: distance_extra_bits(0:29) = [ &
                          0, 0, 0, 0, 1, 1, 2, 2, &
                          3, 3, 4, 4, 5, 5, 6, 6, &
                          7, 7, 8, 8, 9, 9, 10, 10, &
                          11, 11, 12, 12, 13, 13]

contains

   function zlib_decompress(input_data, input_len, status, verify_checksum) result(output_data)
        !! Decompress zlib (deflate) data using fixed Huffman tables
        integer(int8), intent(in) :: input_data(*)
        integer, intent(in) :: input_len
        integer, intent(out), optional :: status
        logical, intent(in), optional :: verify_checksum
        integer(int8), allocatable :: output_data(:)

        type(zlib_decompress_state_t) :: state
        integer :: cmf, flg
        integer :: status_local
        logical :: do_verify
        integer(int32) :: adler_expected, adler_calc

        status_local = 0
        do_verify = .true.
        if (present(verify_checksum)) do_verify = verify_checksum

        if (input_len < 6) then
            status_local = -1
            goto 900
        end if

        cmf = iand(int(input_data(1)), 255)
        flg = iand(int(input_data(2)), 255)

        if (iand(cmf, 15) /= 8 .or. mod(cmf*256 + flg, 31) /= 0) then
            status_local = -2
            goto 900
        end if
        if (iand(flg, 32) /= 0) then
            status_local = -3
            goto 900
        end if

        state%idx = 3
        state%deflate_end_idx = input_len - 4
        if (state%deflate_end_idx < state%idx) then
            status_local = -4
            goto 900
        end if

        call zlib_init_decompress_state(state, input_data, input_len)

        allocate (output_data(max(256, input_len*4)))
        state%output_data = output_data
        state%out_size = 0

        call zlib_decompress_blocks(state, input_data)

        if (state%status == 0) call zlib_align_to_byte(state)

        if (state%status == 0) then
            if (state%idx + 3 > input_len) then
                state%status = -8
            else
                adler_expected = ishft(iand(int(input_data(state%idx)), 255), 24) + &
                                ishft(iand(int(input_data(state%idx + 1)), 255), 16) + &
                                 ishft(iand(int(input_data(state%idx + 2)), 255), 8) + &
                                  iand(int(input_data(state%idx + 3)), 255)
                if (do_verify) then
                    adler_calc = calculate_adler32(state%output_data, state%out_size)
                    if (adler_expected /= adler_calc) state%status = -9
                end if
            end if
        end if

        if (state%status == 0) then
            call zlib_trim_output(state)
            output_data = state%output_data
        else
            output_data = state%output_data(1:0)
        end if

900     continue
        if (.not. allocated(output_data)) allocate (output_data(0))
        if (present(status)) status = state%status
    end function zlib_decompress

    subroutine build_fixed_decode_table(codes, lengths, max_bits, table, table_len)
        !! Build Huffman decode table from codes and lengths
        integer, intent(in) :: codes(0:)
        integer, intent(in) :: lengths(0:)
        integer, intent(in) :: max_bits
        integer, intent(out) :: table(0:)
        integer, intent(out) :: table_len(0:)
        integer :: symbol, len, rev_code, repeats, step, base_idx, limit

        limit = size(table) - 1
        table = -1
        table_len = 0
        do symbol = 0, size(codes) - 1
            len = lengths(symbol)
            if (len == 0) cycle
            rev_code = bit_reverse(codes(symbol), len)
            step = 2**len
            repeats = 2**(max_bits - len)
            do base_idx = 0, repeats - 1
                if (rev_code + base_idx*step <= limit) then
                    table(rev_code + base_idx*step) = symbol
                    table_len(rev_code + base_idx*step) = len
                end if
            end do
        end do
    end subroutine build_fixed_decode_table

    subroutine zlib_init_decompress_state(state, input_data, input_len)
        !! Initialize decompression state and build Huffman tables
        type(zlib_decompress_state_t), intent(inout) :: state
        integer(int8), intent(in) :: input_data(*)
        integer, intent(in) :: input_len
        integer :: literal_codes(0:285), literal_lengths(0:285)
        integer :: distance_codes(0:29), distance_lengths(0:29)

        state%bit_buffer = 0
        state%bit_count = 0
        state%status = 0
        state%input_len = input_len
        allocate (state%input_data(input_len))
        state%input_data(1:input_len) = input_data(1:input_len)

        call init_fixed_huffman_tables(literal_codes, literal_lengths, distance_codes, distance_lengths)
     call build_fixed_decode_table(literal_codes, literal_lengths, LITERAL_TABLE_BITS, &
                                          state%literal_table, state%literal_table_len)
  call build_fixed_decode_table(distance_codes, distance_lengths, DISTANCE_TABLE_BITS, &
                                          state%distance_table, state%distance_table_len)
        state%mask_literal = 2**LITERAL_TABLE_BITS - 1
        state%mask_distance = 2**DISTANCE_TABLE_BITS - 1
    end subroutine zlib_init_decompress_state

    subroutine zlib_decompress_blocks(state, input_data)
        !! Main decompression loop processing all blocks
        type(zlib_decompress_state_t), intent(inout) :: state
        integer(int8), intent(in) :: input_data(*)
        logical :: last_block
        integer :: btype

        last_block = .false.
        do while (.not. last_block .and. state%status == 0)
            last_block = (zlib_read_bits(state, 1) == 1)
            btype = zlib_read_bits(state, 2)
            if (state%status /= 0) exit

            select case (btype)
            case (0)
                call zlib_decompress_stored_block(state)
            case (1)
                call zlib_decode_compressed_block(state, input_data)
            case default
                state%status = -7
            end select
        end do
    end subroutine zlib_decompress_blocks

    subroutine zlib_decompress_stored_block(state)
        !! Decompress uncompressed stored block
        type(zlib_decompress_state_t), intent(inout) :: state
        integer :: len, nlen, i

        call zlib_align_to_byte(state)
        if (state%status /= 0) return
        if (state%idx + 3 > state%deflate_end_idx) then
            state%status = -5
            return
        end if
        len = zlib_read_aligned_byte(state)
        if (state%status /= 0) return
        nlen = zlib_read_aligned_byte(state)
        if (state%status /= 0) return
        if (len /= iand(65535 - nlen, 65535)) then
            state%status = -6
            return
        end if
        call zlib_reserve_output(state, len)
        if (state%status /= 0) return
        do i = 1, len
            call zlib_append_byte(state, zlib_read_aligned_byte(state))
            if (state%status /= 0) return
        end do
    end subroutine zlib_decompress_stored_block

    subroutine zlib_decode_compressed_block(state, input_data)
        !! Decode compressed block using Huffman coding
        type(zlib_decompress_state_t), intent(inout) :: state
        integer(int8), intent(in) :: input_data(*)
        integer :: lit_symbol

        do
            lit_symbol = zlib_decode_literal_symbol(state)
            if (state%status /= 0) return
            if (lit_symbol < 256) then
                call zlib_append_byte(state, lit_symbol)
                if (state%status /= 0) return
            else if (lit_symbol == 256) then
                exit
            else
                call zlib_decode_length_distance(state, lit_symbol)
                if (state%status /= 0) return
            end if
        end do
    end subroutine zlib_decode_compressed_block

    subroutine zlib_decode_length_distance(state, length_code)
        !! Decode length-distance pair and copy from window
        type(zlib_decompress_state_t), intent(inout) :: state
        integer, intent(in) :: length_code
        integer :: distance_code
        integer :: length, distance, extra_bits, extra_val
        integer :: i

        extra_bits = length_extra_bits(length_code)
        extra_val = 0
        if (extra_bits > 0) extra_val = zlib_read_bits(state, extra_bits)
        length = length_base(length_code) + extra_val

        distance_code = zlib_decode_distance_symbol(state)
        if (state%status /= 0) return
        extra_bits = distance_extra_bits(distance_code)
        extra_val = 0
        if (extra_bits > 0) extra_val = zlib_read_bits(state, extra_bits)
        distance = distance_base(distance_code) + extra_val

        if (distance <= 0 .or. distance > state%out_size) then
            state%status = -12
            return
        end if

        call zlib_reserve_output(state, length)
        if (state%status /= 0) return
        do i = 1, length
            call zlib_append_byte(state, iand(int(state%output_data(state%out_size - distance + 1)), 255))
            if (state%status /= 0) return
        end do
    end subroutine zlib_decode_length_distance

    integer function zlib_read_bits(state, num_bits)
        !! Read num_bits from bit buffer
        type(zlib_decompress_state_t), intent(inout) :: state
        integer, intent(in) :: num_bits
        integer :: mask

        if (state%status /= 0) then
            zlib_read_bits = 0
            return
        end if
        call zlib_ensure_bits(state, num_bits)
        if (state%status /= 0) then
            zlib_read_bits = 0
            return
        end if
        mask = 2**num_bits - 1
        zlib_read_bits = iand(state%bit_buffer, mask)
        call zlib_drop_bits(state, num_bits)
    end function zlib_read_bits

    subroutine zlib_ensure_bits(state, required)
        !! Ensure at least required bits in buffer
        type(zlib_decompress_state_t), intent(inout) :: state
        integer, intent(in) :: required
        integer :: byte_val

        do while (state%bit_count < required .and. state%status == 0)
            if (state%idx > state%deflate_end_idx) then
                state%status = -5
                return
            end if
            byte_val = iand(int(state%input_data(state%idx)), 255)
            state%bit_buffer = state%bit_buffer + ishft(byte_val, state%bit_count)
            state%bit_count = state%bit_count + 8
            state%idx = state%idx + 1
        end do
    end subroutine zlib_ensure_bits

    subroutine zlib_drop_bits(state, num_bits)
        !! Drop num_bits from bit buffer
        type(zlib_decompress_state_t), intent(inout) :: state
        integer, intent(in) :: num_bits

        state%bit_buffer = ishft(state%bit_buffer, -num_bits)
        state%bit_count = state%bit_count - num_bits
        if (state%bit_count < 0) state%bit_count = 0
    end subroutine zlib_drop_bits

    subroutine zlib_align_to_byte(state)
        !! Align bit buffer to byte boundary
        type(zlib_decompress_state_t), intent(inout) :: state
        integer :: remainder

        remainder = mod(state%bit_count, 8)
        if (remainder > 0) call zlib_drop_bits(state, remainder)
    end subroutine zlib_align_to_byte

    integer function zlib_read_aligned_byte(state)
        !! Read byte aligned to byte boundary
        type(zlib_decompress_state_t), intent(inout) :: state

        if (state%status /= 0) then
            zlib_read_aligned_byte = 0
            return
        end if
        if (state%bit_count /= 0) then
            state%status = -11
            zlib_read_aligned_byte = 0
            return
        end if
        if (state%idx > state%deflate_end_idx) then
            state%status = -5
            zlib_read_aligned_byte = 0
            return
        end if
        zlib_read_aligned_byte = iand(int(state%input_data(state%idx)), 255)
        state%idx = state%idx + 1
    end function zlib_read_aligned_byte

    subroutine zlib_reserve_output(state, extra)
        !! Reserve extra bytes in output buffer
        type(zlib_decompress_state_t), intent(inout) :: state
        integer, intent(in) :: extra
        integer :: required, new_capacity
        integer(int8), allocatable :: grow(:)

        if (state%status /= 0) return
        required = state%out_size + extra
        if (required <= size(state%output_data)) return
        new_capacity = max(required, max(256, size(state%output_data)*2))
        allocate (grow(new_capacity))
    if (state%out_size > 0) grow(1:state%out_size) = state%output_data(1:state%out_size)
        call move_alloc(grow, state%output_data)
    end subroutine zlib_reserve_output

    subroutine zlib_append_byte(state, val)
        !! Append byte to output
        type(zlib_decompress_state_t), intent(inout) :: state
        integer, intent(in) :: val

        call zlib_reserve_output(state, 1)
        if (state%status /= 0) return
        state%out_size = state%out_size + 1
        state%output_data(state%out_size) = int(iand(val, 255), int8)
    end subroutine zlib_append_byte

    integer function zlib_decode_literal_symbol(state)
        !! Decode literal/Huffman symbol
        type(zlib_decompress_state_t), intent(inout) :: state
        integer :: codebits, used_len

        call zlib_ensure_bits(state, LITERAL_TABLE_BITS)
        if (state%status /= 0) then
            zlib_decode_literal_symbol = -1
            return
        end if
        codebits = iand(state%bit_buffer, state%mask_literal)
        used_len = state%literal_table_len(codebits)
        if (used_len == 0) then
            state%status = -13
            zlib_decode_literal_symbol = -1
            return
        end if
        zlib_decode_literal_symbol = state%literal_table(codebits)
        call zlib_drop_bits(state, used_len)
    end function zlib_decode_literal_symbol

    integer function zlib_decode_distance_symbol(state)
        !! Decode distance symbol
        type(zlib_decompress_state_t), intent(inout) :: state
        integer :: codebits, used_len

        call zlib_ensure_bits(state, DISTANCE_TABLE_BITS)
        if (state%status /= 0) then
            zlib_decode_distance_symbol = -1
            return
        end if
        codebits = iand(state%bit_buffer, state%mask_distance)
        used_len = state%distance_table_len(codebits)
        if (used_len == 0) then
            state%status = -13
            zlib_decode_distance_symbol = -1
            return
        end if
        zlib_decode_distance_symbol = state%distance_table(codebits)
        call zlib_drop_bits(state, used_len)
    end function zlib_decode_distance_symbol

    subroutine zlib_trim_output(state)
        !! Trim output to actual size
        type(zlib_decompress_state_t), intent(inout) :: state
        integer(int8), allocatable :: shrink(:)

        if (.not. allocated(state%output_data)) then
            allocate (state%output_data(0))
            state%out_size = 0
            return
        end if
        if (state%out_size <= 0) then
            state%out_size = 0
        else if (state%out_size < size(state%output_data)) then
            allocate (shrink(state%out_size))
            shrink = state%output_data(1:state%out_size)
            call move_alloc(shrink, state%output_data)
        end if
    end subroutine zlib_trim_output

end module fortplot_zlib_decompress

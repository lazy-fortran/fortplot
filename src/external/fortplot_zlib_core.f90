module fortplot_zlib_core
    !! Pure Fortran implementation of zlib compression, decompression, and CRC32
    !! Ported from STB image libraries for self-contained PNG support
    use, intrinsic :: iso_fortran_env, only: int8, int32
    use iso_c_binding, only: c_ptr, c_loc, c_f_pointer, c_associated
    implicit none
    
    private
    public :: zlib_compress, zlib_decompress, crc32_calculate
    
    private :: bit_reverse

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
        1, 2, 3, 4, 5, 7, 9, 13, &
        17, 25, 33, 49, 65, 97, 129, 193, &
        257, 385, 513, 769, 1025, 1537, 2049, 3073, &
        4097, 6145, 8193, 12289, 16385, 24577 ]
    integer, parameter :: distance_extra_bits(0:29) = [ &
        0, 0, 0, 0, 1, 1, 2, 2, &
        3, 3, 4, 4, 5, 5, 6, 6, &
        7, 7, 8, 8, 9, 9, 10, 10, &
        11, 11, 12, 12, 13, 13 ]

    ! CRC32 lookup table (standard polynomial 0xEDB88320)
    integer(int32), parameter :: crc_table(0:255) = [ &
        int(z'00000000',int32), int(z'77073096',int32), int(z'EE0E612C',int32), int(z'990951BA',int32), &
        int(z'076DC419',int32), int(z'706AF48F',int32), int(z'E963A535',int32), int(z'9E6495A3',int32), &
        int(z'0EDB8832',int32), int(z'79DCB8A4',int32), int(z'E0D5E91E',int32), int(z'97D2D988',int32), &
        int(z'09B64C2B',int32), int(z'7EB17CBD',int32), int(z'E7B82D07',int32), int(z'90BF1D91',int32), &
        int(z'1DB71064',int32), int(z'6AB020F2',int32), int(z'F3B97148',int32), int(z'84BE41DE',int32), &
        int(z'1ADAD47D',int32), int(z'6DDDE4EB',int32), int(z'F4D4B551',int32), int(z'83D385C7',int32), &
        int(z'136C9856',int32), int(z'646BA8C0',int32), int(z'FD62F97A',int32), int(z'8A65C9EC',int32), &
        int(z'14015C4F',int32), int(z'63066CD9',int32), int(z'FA0F3D63',int32), int(z'8D080DF5',int32), &
        int(z'3B6E20C8',int32), int(z'4C69105E',int32), int(z'D56041E4',int32), int(z'A2677172',int32), &
        int(z'3C03E4D1',int32), int(z'4B04D447',int32), int(z'D20D85FD',int32), int(z'A50AB56B',int32), &
        int(z'35B5A8FA',int32), int(z'42B2986C',int32), int(z'DBBBC9D6',int32), int(z'ACBCF940',int32), &
        int(z'32D86CE3',int32), int(z'45DF5C75',int32), int(z'DCD60DCF',int32), int(z'ABD13D59',int32), &
        int(z'26D930AC',int32), int(z'51DE003A',int32), int(z'C8D75180',int32), int(z'BFD06116',int32), &
        int(z'21B4F4B5',int32), int(z'56B3C423',int32), int(z'CFBA9599',int32), int(z'B8BDA50F',int32), &
        int(z'2802B89E',int32), int(z'5F058808',int32), int(z'C60CD9B2',int32), int(z'B10BE924',int32), &
        int(z'2F6F7C87',int32), int(z'58684C11',int32), int(z'C1611DAB',int32), int(z'B6662D3D',int32), &
        int(z'76DC4190',int32), int(z'01DB7106',int32), int(z'98D220BC',int32), int(z'EFD5102A',int32), &
        int(z'71B18589',int32), int(z'06B6B51F',int32), int(z'9FBFE4A5',int32), int(z'E8B8D433',int32), &
        int(z'7807C9A2',int32), int(z'0F00F934',int32), int(z'9609A88E',int32), int(z'E10E9818',int32), &
        int(z'7F6A0DBB',int32), int(z'086D3D2D',int32), int(z'91646C97',int32), int(z'E6635C01',int32), &
        int(z'6B6B51F4',int32), int(z'1C6C6162',int32), int(z'856530D8',int32), int(z'F262004E',int32), &
        int(z'6C0695ED',int32), int(z'1B01A57B',int32), int(z'8208F4C1',int32), int(z'F50FC457',int32), &
        int(z'65B0D9C6',int32), int(z'12B7E950',int32), int(z'8BBEB8EA',int32), int(z'FCB9887C',int32), &
        int(z'62DD1DDF',int32), int(z'15DA2D49',int32), int(z'8CD37CF3',int32), int(z'FBD44C65',int32), &
        int(z'4DB26158',int32), int(z'3AB551CE',int32), int(z'A3BC0074',int32), int(z'D4BB30E2',int32), &
        int(z'4ADFA541',int32), int(z'3DD895D7',int32), int(z'A4D1C46D',int32), int(z'D3D6F4FB',int32), &
        int(z'4369E96A',int32), int(z'346ED9FC',int32), int(z'AD678846',int32), int(z'DA60B8D0',int32), &
        int(z'44042D73',int32), int(z'33031DE5',int32), int(z'AA0A4C5F',int32), int(z'DD0D7CC9',int32), &
        int(z'5005713C',int32), int(z'270241AA',int32), int(z'BE0B1010',int32), int(z'C90C2086',int32), &
        int(z'5768B525',int32), int(z'206F85B3',int32), int(z'B966D409',int32), int(z'CE61E49F',int32), &
        int(z'5EDEF90E',int32), int(z'29D9C998',int32), int(z'B0D09822',int32), int(z'C7D7A8B4',int32), &
        int(z'59B33D17',int32), int(z'2EB40D81',int32), int(z'B7BD5C3B',int32), int(z'C0BA6CAD',int32), &
        int(z'EDB88320',int32), int(z'9ABFB3B6',int32), int(z'03B6E20C',int32), int(z'74B1D29A',int32), &
        int(z'EAD54739',int32), int(z'9DD277AF',int32), int(z'04DB2615',int32), int(z'73DC1683',int32), &
        int(z'E3630B12',int32), int(z'94643B84',int32), int(z'0D6D6A3E',int32), int(z'7A6A5AA8',int32), &
        int(z'E40ECF0B',int32), int(z'9309FF9D',int32), int(z'0A00AE27',int32), int(z'7D079EB1',int32), &
        int(z'F00F9344',int32), int(z'8708A3D2',int32), int(z'1E01F268',int32), int(z'6906C2FE',int32), &
        int(z'F762575D',int32), int(z'806567CB',int32), int(z'196C3671',int32), int(z'6E6B06E7',int32), &
        int(z'FED41B76',int32), int(z'89D32BE0',int32), int(z'10DA7A5A',int32), int(z'67DD4ACC',int32), &
        int(z'F9B9DF6F',int32), int(z'8EBEEFF9',int32), int(z'17B7BE43',int32), int(z'60B08ED5',int32), &
        int(z'D6D6A3E8',int32), int(z'A1D1937E',int32), int(z'38D8C2C4',int32), int(z'4FDFF252',int32), &
        int(z'D1BB67F1',int32), int(z'A6BC5767',int32), int(z'3FB506DD',int32), int(z'48B2364B',int32), &
        int(z'D80D2BDA',int32), int(z'AF0A1B4C',int32), int(z'36034AF6',int32), int(z'41047A60',int32), &
        int(z'DF60EFC3',int32), int(z'A867DF55',int32), int(z'316E8EEF',int32), int(z'4669BE79',int32), &
        int(z'CB61B38C',int32), int(z'BC66831A',int32), int(z'256FD2A0',int32), int(z'5268E236',int32), &
        int(z'CC0C7795',int32), int(z'BB0B4703',int32), int(z'220216B9',int32), int(z'5505262F',int32), &
        int(z'C5BA3BBE',int32), int(z'B2BD0B28',int32), int(z'2BB45A92',int32), int(z'5CB36A04',int32), &
        int(z'C2D7FFA7',int32), int(z'B5D0CF31',int32), int(z'2CD99E8B',int32), int(z'5BDEAE1D',int32), &
        int(z'9B64C2B0',int32), int(z'EC63F226',int32), int(z'756AA39C',int32), int(z'026D930A',int32), &
        int(z'9C0906A9',int32), int(z'EB0E363F',int32), int(z'72076785',int32), int(z'05005713',int32), &
        int(z'95BF4A82',int32), int(z'E2B87A14',int32), int(z'7BB12BAE',int32), int(z'0CB61B38',int32), &
        int(z'92D28E9B',int32), int(z'E5D5BE0D',int32), int(z'7CDCEFB7',int32), int(z'0BDBDF21',int32), &
        int(z'86D3D2D4',int32), int(z'F1D4E242',int32), int(z'68DDB3F8',int32), int(z'1FDA836E',int32), &
        int(z'81BE16CD',int32), int(z'F6B9265B',int32), int(z'6FB077E1',int32), int(z'18B74777',int32), &
        int(z'88085AE6',int32), int(z'FF0F6A70',int32), int(z'66063BCA',int32), int(z'11010B5C',int32), &
        int(z'8F659EFF',int32), int(z'F862AE69',int32), int(z'616BFFD3',int32), int(z'166CCF45',int32), &
        int(z'A00AE278',int32), int(z'D70DD2EE',int32), int(z'4E048354',int32), int(z'3903B3C2',int32), &
        int(z'A7672661',int32), int(z'D06016F7',int32), int(z'4969474D',int32), int(z'3E6E77DB',int32), &
        int(z'AED16A4A',int32), int(z'D9D65ADC',int32), int(z'40DF0B66',int32), int(z'37D83BF0',int32), &
        int(z'A9BCAE53',int32), int(z'DEBB9EC5',int32), int(z'47B2CF7F',int32), int(z'30B5FFE9',int32), &
        int(z'BDBDF21C',int32), int(z'CABAC28A',int32), int(z'53B39330',int32), int(z'24B4A3A6',int32), &
        int(z'BAD03605',int32), int(z'CDD70693',int32), int(z'54DE5729',int32), int(z'23D967BF',int32), &
        int(z'B3667A2E',int32), int(z'C4614AB8',int32), int(z'5D681B02',int32), int(z'2A6F2B94',int32), &
        int(z'B40BBE37',int32), int(z'C30C8EA1',int32), int(z'5A05DF1B',int32), int(z'2D02EF8D',int32) ]
    
contains

    function crc32_calculate(data, data_len) result(crc)
        !! Calculate CRC32 checksum using standard polynomial
        integer(int8), intent(in) :: data(*)
        integer, intent(in) :: data_len
        integer(int32) :: crc
        integer :: i
        integer(int8) :: byte_val
        
        crc = not(0_int32)  ! Initialize to 0xFFFFFFFF
        
        do i = 1, data_len
            byte_val = data(i)
            crc = ieor(ishft(crc, -8), crc_table(iand(ieor(crc, int(byte_val, int32)), 255)))
        end do
        
        crc = not(crc)  ! Final XOR with 0xFFFFFFFF
    end function crc32_calculate

    function zlib_compress(input_data, input_len, output_len) result(output_data)
        !! Full deflate compression with LZ77 and Huffman coding
        integer(int8), intent(in) :: input_data(*)
        integer, intent(in) :: input_len
        integer, intent(out) :: output_len
        integer(int8), allocatable :: output_data(:)
        
        integer(int8), allocatable :: compressed_block(:)
        integer :: compressed_block_len
        integer(int32) :: adler32_checksum
        integer :: pos
        
        ! Compress using deflate algorithm
        call deflate_compress(input_data, input_len, compressed_block, compressed_block_len)
        
        ! Calculate total output size: zlib header (2) + compressed data + adler32 (4)
        output_len = 2 + compressed_block_len + 4
        allocate(output_data(output_len))
        
        pos = 1
        
        ! Write zlib header
        output_data(pos) = int(z'78', int8)  ! CMF: 32K window, deflate
        pos = pos + 1
        output_data(pos) = int(z'5E', int8)  ! FLG: no preset dict, level 1 compression
        pos = pos + 1
        
        ! Copy compressed block
        output_data(pos:pos+compressed_block_len-1) = compressed_block(1:compressed_block_len)
        pos = pos + compressed_block_len
        
        ! Calculate and write Adler-32 checksum
        adler32_checksum = calculate_adler32(input_data, input_len)
        
        ! Write Adler-32 in big-endian format
        output_data(pos) = int(iand(ishft(adler32_checksum, -24), 255), int8)
        pos = pos + 1
        output_data(pos) = int(iand(ishft(adler32_checksum, -16), 255), int8)
        pos = pos + 1
        output_data(pos) = int(iand(ishft(adler32_checksum, -8), 255), int8)
        pos = pos + 1
        output_data(pos) = int(iand(adler32_checksum, 255), int8)
        
        deallocate(compressed_block)
    end function zlib_compress
       
    function zlib_decompress(input_data, input_len, status, verify_checksum) result(output_data)
        !! Decompress zlib (deflate) data using fixed Huffman tables
        integer(int8), intent(in) :: input_data(*)
        integer, intent(in) :: input_len
        integer, intent(out), optional :: status
        logical, intent(in), optional :: verify_checksum
        integer(int8), allocatable :: output_data(:)

        integer :: cmf, flg
        integer :: idx, deflate_end_idx
        integer(int32) :: bit_buffer
        integer :: bit_count
        integer :: literal_codes(0:285), literal_lengths(0:285)
        integer :: distance_codes(0:29), distance_lengths(0:29)
        integer :: literal_table(0:2**LITERAL_TABLE_BITS - 1)
        integer :: literal_table_len(0:2**LITERAL_TABLE_BITS - 1)
        integer :: distance_table(0:2**DISTANCE_TABLE_BITS - 1)
        integer :: distance_table_len(0:2**DISTANCE_TABLE_BITS - 1)
        integer :: mask_literal, mask_distance
        integer :: out_size
        integer :: status_local
        logical :: last_block
        integer :: btype
        integer :: length_code, distance_code
        integer :: length, distance, extra_bits, extra_val
        integer(int32) :: adler_expected, adler_calc
        integer :: len_low, len_high, nlen_low, nlen_high
        logical :: do_verify

        status_local = 0

        do_verify = .true.
        if (present(verify_checksum)) do_verify = verify_checksum

        if (input_len < 6) then
            status_local = -1
            goto 900
        end if

        cmf = iand(int(input_data(1)), 255)
        flg = iand(int(input_data(2)), 255)

        if (iand(cmf, 15) /= 8 .or. mod(cmf * 256 + flg, 31) /= 0) then
            status_local = -2
            goto 900
        end if
        if (iand(flg, 32) /= 0) then
            status_local = -3  ! preset dictionary not supported
            goto 900
        end if

        idx = 3
        deflate_end_idx = input_len - 4
        if (deflate_end_idx < idx) then
            status_local = -4
            goto 900
        end if

        bit_buffer = 0
        bit_count = 0

        call init_fixed_huffman_tables(literal_codes, literal_lengths, distance_codes, distance_lengths)
        call build_fixed_decode_table(literal_codes, literal_lengths, LITERAL_TABLE_BITS, literal_table, literal_table_len)
        call build_fixed_decode_table(distance_codes, distance_lengths, DISTANCE_TABLE_BITS, distance_table, distance_table_len)
        mask_literal = 2**LITERAL_TABLE_BITS - 1
        mask_distance = 2**DISTANCE_TABLE_BITS - 1

        allocate(output_data(max(256, input_len * 4)))
        out_size = 0

        last_block = .false.
        do while (.not. last_block .and. status_local == 0)
            last_block = (read_bits(1) == 1)
            btype = read_bits(2)
            if (status_local /= 0) exit

            select case (btype)
            case (0)
                call align_to_byte()
                if (status_local /= 0) exit
                if (idx + 3 > deflate_end_idx) then
                    status_local = -5
                    exit
                end if
                len_low = read_aligned_byte()
                len_high = read_aligned_byte()
                nlen_low = read_aligned_byte()
                nlen_high = read_aligned_byte()
                if (status_local /= 0) exit
                length = len_low + len_high * 256
                if (length /= iand(65535 - (nlen_low + nlen_high * 256), 65535)) then
                    status_local = -6
                    exit
                end if
                call reserve_output(length)
                do while (length > 0 .and. status_local == 0)
                    call append_byte(read_aligned_byte())
                    length = length - 1
                end do
            case (1)
                call decode_compressed_block()
            case default
                status_local = -7
            end select
        end do

        if (status_local == 0) call align_to_byte()

        if (status_local == 0) then
            if (idx + 3 > input_len) then
                status_local = -8
            else
                adler_expected = ishft(iand(int(input_data(idx)), 255), 24) + &
                                 ishft(iand(int(input_data(idx + 1)), 255), 16) + &
                                 ishft(iand(int(input_data(idx + 2)), 255), 8) + &
                                 iand(int(input_data(idx + 3)), 255)
                if (do_verify) then
                    adler_calc = calculate_adler32(output_data, out_size)
                    if (adler_expected /= adler_calc) status_local = -9
                end if
            end if
        end if

        if (status_local == 0) then
            call trim_output()
        else
            call reset_output()
        end if

900     continue
        if (.not. allocated(output_data)) allocate(output_data(0))
        if (present(status)) status = status_local
        return

    contains

        integer function read_bits(num_bits)
            integer, intent(in) :: num_bits
            integer :: mask
            if (status_local /= 0) then
                read_bits = 0
                return
            end if
            call ensure_bits(num_bits)
            if (status_local /= 0) then
                read_bits = 0
                return
            end if
            mask = 2**num_bits - 1
            read_bits = iand(bit_buffer, mask)
            call drop_bits(num_bits)
        end function read_bits

        subroutine ensure_bits(required)
            integer, intent(in) :: required
            integer :: byte_val
            do while (bit_count < required .and. status_local == 0)
                if (idx > deflate_end_idx) then
                    status_local = -5
                    return
                end if
                byte_val = iand(int(input_data(idx)), 255)
                bit_buffer = bit_buffer + ishft(byte_val, bit_count)
                bit_count = bit_count + 8
                idx = idx + 1
            end do
        end subroutine ensure_bits

        subroutine drop_bits(num_bits)
            integer, intent(in) :: num_bits
            bit_buffer = ishft(bit_buffer, -num_bits)
            bit_count = bit_count - num_bits
            if (bit_count < 0) bit_count = 0
        end subroutine drop_bits

        subroutine align_to_byte()
            integer :: remainder
            remainder = mod(bit_count, 8)
            if (remainder > 0) call drop_bits(remainder)
        end subroutine align_to_byte

        integer function read_aligned_byte()
            if (status_local /= 0) then
                read_aligned_byte = 0
                return
            end if
            if (bit_count /= 0) then
                status_local = -11
                read_aligned_byte = 0
                return
            end if
            if (idx > deflate_end_idx) then
                status_local = -5
                read_aligned_byte = 0
                return
            end if
            read_aligned_byte = iand(int(input_data(idx)), 255)
            idx = idx + 1
        end function read_aligned_byte

        subroutine reserve_output(extra)
            integer, intent(in) :: extra
            integer :: required, new_capacity
            integer(int8), allocatable :: grow(:)

            if (status_local /= 0) return
            required = out_size + extra
            if (required <= size(output_data)) return
            new_capacity = max(required, max(256, size(output_data) * 2))
            allocate(grow(new_capacity))
            if (out_size > 0) grow(1:out_size) = output_data(1:out_size)
            call move_alloc(grow, output_data)
        end subroutine reserve_output

        subroutine append_byte(val)
            integer, intent(in) :: val
            call reserve_output(1)
            if (status_local /= 0) return
            out_size = out_size + 1
            output_data(out_size) = int(iand(val, 255), int8)
        end subroutine append_byte

        subroutine decode_compressed_block()
            integer :: lit_symbol

            do
                lit_symbol = decode_literal_symbol()
                if (status_local /= 0) return
                if (lit_symbol < 256) then
                    call append_byte(lit_symbol)
                    if (status_local /= 0) return
                else if (lit_symbol == 256) then
                    exit
                else
                    length_code = lit_symbol
                    extra_bits = length_extra_bits(length_code)
                    if (extra_bits > 0) then
                        extra_val = read_bits(extra_bits)
                    else
                        extra_val = 0
                    end if
                    if (status_local /= 0) return
                    length = length_base(length_code) + extra_val

                    distance_code = decode_distance_symbol()
                    if (status_local /= 0) return
                    extra_bits = distance_extra_bits(distance_code)
                    if (extra_bits > 0) then
                        extra_val = read_bits(extra_bits)
                    else
                        extra_val = 0
                    end if
                    if (status_local /= 0) return
                    distance = distance_base(distance_code) + extra_val

                    if (distance <= 0 .or. distance > out_size) then
                        status_local = -12
                        return
                    end if

                    call reserve_output(length)
                    if (status_local /= 0) return
                    do extra_val = 1, length
                        call append_byte(iand(int(output_data(out_size - distance + 1)), 255))
                        if (status_local /= 0) return
                    end do
                end if
            end do
        end subroutine decode_compressed_block

        integer function decode_literal_symbol()
            integer :: codebits, used_len
            call ensure_bits(LITERAL_TABLE_BITS)
            if (status_local /= 0) then
                decode_literal_symbol = -1
                return
            end if
            codebits = iand(bit_buffer, mask_literal)
            used_len = literal_table_len(codebits)
            if (used_len == 0) then
                status_local = -13
                decode_literal_symbol = -1
                return
            end if
            decode_literal_symbol = literal_table(codebits)
            call drop_bits(used_len)
        end function decode_literal_symbol

        integer function decode_distance_symbol()
            integer :: codebits, used_len
            call ensure_bits(DISTANCE_TABLE_BITS)
            if (status_local /= 0) then
                decode_distance_symbol = -1
                return
            end if
            codebits = iand(bit_buffer, mask_distance)
            used_len = distance_table_len(codebits)
            if (used_len == 0) then
                status_local = -13
                decode_distance_symbol = -1
                return
            end if
            decode_distance_symbol = distance_table(codebits)
            call drop_bits(used_len)
        end function decode_distance_symbol

        subroutine trim_output()
            integer(int8), allocatable :: shrink(:)
            if (.not. allocated(output_data)) then
                allocate(output_data(0))
                out_size = 0
                return
            end if
            if (out_size <= 0) then
                call reset_output()
            else if (out_size < size(output_data)) then
                allocate(shrink(out_size))
                shrink = output_data(1:out_size)
                call move_alloc(shrink, output_data)
            end if
        end subroutine trim_output

        subroutine reset_output()
            if (allocated(output_data)) deallocate(output_data)
            allocate(output_data(0))
            out_size = 0
        end subroutine reset_output

        subroutine build_fixed_decode_table(codes, lengths, max_bits, table, table_len)
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
                    if (rev_code + base_idx * step <= limit) then
                        table(rev_code + base_idx * step) = symbol
                        table_len(rev_code + base_idx * step) = len
                    end if
                end do
            end do
        end subroutine build_fixed_decode_table

    end function zlib_decompress

    function calculate_adler32(data, data_len) result(adler32)
        !! Calculate Adler-32 checksum for zlib
        integer(int8), intent(in) :: data(*)
        integer, intent(in) :: data_len
        integer(int32) :: adler32
        
        integer(int32) :: s1, s2
        integer :: i
        
        s1 = 1_int32
        s2 = 0_int32
        
        do i = 1, data_len
            ! Handle signed bytes properly
            s1 = mod(s1 + iand(int(data(i), int32), 255_int32), 65521_int32)
            s2 = mod(s2 + s1, 65521_int32)
        end do
        
        adler32 = ior(ishft(s2, 16), s1)
    end function calculate_adler32
    
    subroutine deflate_compress(input_data, input_len, output_data, output_len)
        !! Full deflate compression implementation with LZ77 and Huffman coding
        integer(int8), intent(in) :: input_data(*)
        integer, intent(in) :: input_len
        integer(int8), allocatable, intent(out) :: output_data(:)
        integer, intent(out) :: output_len
        
        ! Hash table for LZ77
        integer :: hash_table(0:HASH_SIZE-1)
        integer :: hash_chain(WINDOW_SIZE)
        
        ! Huffman tables (fixed Huffman for simplicity)
        integer :: literal_codes(0:285)
        integer :: literal_lengths(0:285)
        integer :: distance_codes(0:29)
        integer :: distance_lengths(0:29)
        
        ! Output bit buffer
        integer(int8), allocatable :: bit_buffer(:)
        integer :: bit_pos, byte_pos
        integer :: i, pos, match_len, match_dist
        integer :: hash_val
        
        ! Initialize fixed Huffman tables
        call init_fixed_huffman_tables(literal_codes, literal_lengths, distance_codes, distance_lengths)
        
        ! Initialize hash table
        hash_table = -1
        hash_chain = -1
        
        ! Allocate bit buffer (worst case: no compression, plus some overhead)
        allocate(bit_buffer(max(64, input_len * 2)))
        bit_pos = 0
        byte_pos = 1
        
        ! Write deflate block header: BFINAL=1 (last block), BTYPE=01 (fixed Huffman)
        call write_bits(bit_buffer, bit_pos, byte_pos, 1, 1)
        call write_bits(bit_buffer, bit_pos, byte_pos, 1, 2)  ! BTYPE=01
        
        pos = 1
        do while (pos <= input_len)
            ! Find longest match
            call find_longest_match(input_data, pos, input_len, hash_table, hash_chain, match_len, match_dist)
            
            if (match_len >= MIN_MATCH) then
                ! Encode length-distance pair
                call encode_length_distance(bit_buffer, bit_pos, byte_pos, match_len, match_dist, &
                                            literal_codes, literal_lengths, distance_codes, distance_lengths)
                
                ! Update hash table for the matched string
                do i = 0, match_len - 1
                    if (pos + i <= input_len) then
                        hash_val = calculate_hash(input_data, pos + i, input_len)
                        call update_hash_table(hash_table, hash_chain, hash_val, pos + i)
                    end if
                end do
                pos = pos + match_len
            else
                ! Encode literal
                call encode_literal(bit_buffer, bit_pos, byte_pos, input_data(pos), literal_codes, literal_lengths)
                
                ! Update hash table
                if (pos <= input_len) then
                    hash_val = calculate_hash(input_data, pos, input_len)
                    call update_hash_table(hash_table, hash_chain, hash_val, pos)
                end if
                pos = pos + 1
            end if
        end do
        
        ! Encode end-of-block marker (code 256)
        call write_bits(bit_buffer, bit_pos, byte_pos, bit_reverse(literal_codes(256), literal_lengths(256)), literal_lengths(256))
        
        ! Align to byte boundary
        if (bit_pos > 0) then
            byte_pos = byte_pos + 1
        end if
        
        ! Copy result
        output_len = byte_pos - 1
        allocate(output_data(output_len))
        output_data(1:output_len) = bit_buffer(1:output_len)
        
        deallocate(bit_buffer)
    end subroutine deflate_compress
    
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

end module fortplot_zlib_core

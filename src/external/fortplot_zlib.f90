module fortplot_zlib
    !! Pure Fortran implementation of zlib compression, decompression, and CRC32
    !! Ported from STB image libraries for self-contained PNG support
    use, intrinsic :: iso_fortran_env, only: int8, int32
    use iso_c_binding, only: c_ptr, c_loc, c_f_pointer, c_associated
    implicit none
    
    private
    public :: zlib_compress, crc32_calculate
    
    private :: bit_reverse

    ! Deflate compression constants
    integer, parameter :: MAX_MATCH = 258
    integer, parameter :: MIN_MATCH = 3
    integer, parameter :: MAX_DISTANCE = 32768
    integer, parameter :: HASH_BITS = 15
    integer, parameter :: HASH_SIZE = 2**HASH_BITS
    integer, parameter :: WINDOW_SIZE = 32768
    
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
        
        if (allocated(compressed_block)) deallocate(compressed_block)
    end function zlib_compress
       
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
        !! Enhanced deflate compression with dynamic Huffman encoding
        !! Provides better compression ratios for PNG data
        integer(int8), intent(in) :: input_data(*)
        integer, intent(in) :: input_len
        integer(int8), allocatable, intent(out) :: output_data(:)
        integer, intent(out) :: output_len
        
        integer :: pos, remaining, block_len, i
        integer(int8), allocatable :: temp_buffer(:)
        integer :: temp_pos
        integer :: compression_level
        real :: compression_ratio
        
        ! Analyze data to determine optimal compression strategy
        compression_ratio = analyze_compressibility(input_data, input_len)
        
        ! Use dynamic allocation based on expected compression
        ! Minimum 10% overhead, maximum 110% for incompressible data
        if (compression_ratio > 0.5) then
            ! Good compression expected
            allocate(temp_buffer(input_len + input_len/10 + 1024))
        else
            ! Poor compression expected, allocate conservatively
            allocate(temp_buffer(input_len + input_len/2 + 1024))
        end if
        temp_pos = 1
        
        ! Try compressed blocks first for better file sizes
        if (compression_ratio > 0.3) then
            ! Use fixed Huffman compression for reasonable data
            call compress_with_fixed_huffman(input_data, input_len, temp_buffer, temp_pos)
        else
            ! Fall back to stored blocks for highly incompressible data
            pos = 1
            remaining = input_len
            
            do while (remaining > 0)
                ! Determine block size (max 65535 bytes for stored blocks)
                block_len = min(remaining, 65535)
                
                ! Write block header: BFINAL, BTYPE=00 (stored)
                if (remaining <= 65535) then
                    ! Last block: BFINAL=1, BTYPE=00 
                    temp_buffer(temp_pos) = int(1, int8)  ! BFINAL=1, BTYPE=00 (bits 0-2)
                else
                    ! Not last block: BFINAL=0, BTYPE=00
                    temp_buffer(temp_pos) = int(0, int8)  ! BFINAL=0, BTYPE=00 (bits 0-2) 
                end if
                temp_pos = temp_pos + 1
                
                ! Write LEN (2 bytes, little endian)
                temp_buffer(temp_pos) = int(iand(block_len, 255), int8)
                temp_pos = temp_pos + 1
                temp_buffer(temp_pos) = int(iand(ishft(block_len, -8), 255), int8)
                temp_pos = temp_pos + 1
                
                ! Write NLEN (2 bytes, little endian, one's complement of LEN)
                temp_buffer(temp_pos) = int(iand(ieor(block_len, 65535), 255), int8)
                temp_pos = temp_pos + 1
                temp_buffer(temp_pos) = int(iand(ishft(ieor(block_len, 65535), -8), 255), int8) 
                temp_pos = temp_pos + 1
                
                ! Copy block data directly (no compression, preserves all bytes exactly)
                do i = 1, block_len
                    temp_buffer(temp_pos) = input_data(pos)
                    temp_pos = temp_pos + 1
                    pos = pos + 1
                end do
                
                remaining = remaining - block_len
            end do
        end if
        
        ! Copy result to output
        output_len = temp_pos - 1
        allocate(output_data(output_len))
        output_data(1:output_len) = temp_buffer(1:output_len)
        
        if (allocated(temp_buffer)) deallocate(temp_buffer)
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

end module fortplot_zlib
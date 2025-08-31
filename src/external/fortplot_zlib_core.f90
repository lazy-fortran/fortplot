module fortplot_zlib_core
    !! Core zlib functionality: CRC32 calculation and main compression entry points
    !! Split from fortplot_zlib.f90 for file size compliance (Issue #884)
    use, intrinsic :: iso_fortran_env, only: int8, int32
    use fortplot_zlib_huffman, only: analyze_compressibility, compress_with_fixed_huffman
    implicit none
    
    private
    public :: zlib_compress, crc32_calculate
    
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
            crc = ieor(ishft(crc, 8), crc_table(iand(ieor(crc, int(byte_val, int32)), z'FF')))
        end do
        
        crc = not(crc)
    end function crc32_calculate

    function zlib_compress(input_data, input_len, output_len) result(output_data)
        !! Main zlib compression entry point with proper headers and checksums
        integer(int8), intent(in) :: input_data(*)
        integer, intent(in) :: input_len
        integer, intent(out) :: output_len
        integer(int8), allocatable :: output_data(:)
        integer(int8), allocatable :: compressed_data(:)
        integer :: compressed_len, pos
        integer(int32) :: adler32_checksum
        
        ! Emergency fix: Use massive buffer sizes to prevent CI crashes
        ! TODO: Fix fundamental compression algorithm buffer management
        ! The compression algorithm has serious buffer management issues
        allocate(output_data(max(input_len * 8 + 100000, 100000)))
        allocate(compressed_data(max(input_len * 8 + 50000, 100000)))
        
        pos = 1
        
        ! zlib header (2 bytes)
        output_data(pos) = int(z'78', int8)     ! CMF (Compression Method and flags)
        output_data(pos+1) = int(z'5E', int8)   ! FLG (preset dictionary flag)
        pos = pos + 2
        
        ! TEMPORARY FIX: Use uncompressed deflate blocks for reliability
        ! This ensures valid PNG files while the full Huffman implementation is debugged
        compressed_len = 1  ! Initialize starting position for compression
        call compress_with_uncompressed_blocks(input_data, input_len, compressed_data, compressed_len)
        
        ! Bounds check before copying compressed data
        if (compressed_len > size(compressed_data)) then
            print *, "ERROR: Compressed data size exceeds buffer:", compressed_len, "vs", size(compressed_data)
            output_len = 0
            deallocate(compressed_data)
            return
        end if
        
        if (pos + compressed_len - 1 > size(output_data)) then
            print *, "ERROR: Output buffer overflow:", pos + compressed_len - 1, "vs", size(output_data)
            output_len = 0
            deallocate(compressed_data)
            return
        end if
        
        ! Copy compressed data
        output_data(pos:pos+compressed_len-1) = compressed_data(1:compressed_len)
        pos = pos + compressed_len
        
        ! Calculate and append Adler-32 checksum (4 bytes, big-endian)
        adler32_checksum = calculate_adler32(input_data, input_len)
        output_data(pos) = int(iand(ishft(adler32_checksum, -24), z'FF'), int8)
        output_data(pos+1) = int(iand(ishft(adler32_checksum, -16), z'FF'), int8)
        output_data(pos+2) = int(iand(ishft(adler32_checksum, -8), z'FF'), int8)
        output_data(pos+3) = int(iand(adler32_checksum, z'FF'), int8)
        
        output_len = pos + 3
        
        deallocate(compressed_data)
    end function zlib_compress

    function calculate_adler32(data, data_len) result(adler32)
        !! Calculate Adler-32 checksum for zlib format
        integer(int8), intent(in) :: data(*)
        integer, intent(in) :: data_len
        integer(int32) :: adler32
        integer(int32) :: a, b
        integer :: i
        
        a = 1_int32
        b = 0_int32
        
        do i = 1, data_len
            a = mod(a + int(data(i), int32), 65521_int32)
            b = mod(b + a, 65521_int32)
        end do
        
        adler32 = ior(ishft(b, 16), a)
    end function calculate_adler32

    subroutine compress_with_uncompressed_blocks(input_data, input_len, output_buffer, output_pos)
        !! Create valid uncompressed deflate blocks for PNG compatibility
        !! This produces valid but larger PNG files until Huffman compression is fixed
        integer(int8), intent(in) :: input_data(*)
        integer, intent(in) :: input_len
        integer(int8), intent(inout) :: output_buffer(:)
        integer, intent(inout) :: output_pos
        
        integer :: pos, block_size, remaining
        integer :: len_field, nlen_field
        integer :: byte_pos
        
        pos = 1
        byte_pos = output_pos
        remaining = input_len
        
        do while (remaining > 0)
            ! Determine block size (use smaller blocks for better compatibility)
            block_size = min(remaining, 32768)  ! 32KB blocks for better compatibility
            
            ! Write block header (3 bits: BFINAL + BTYPE)
            if (remaining <= block_size) then
                ! BFINAL=1, BTYPE=00 (uncompressed, final block)
                output_buffer(byte_pos) = 1_int8
            else
                ! BFINAL=0, BTYPE=00 (uncompressed, not final)
                output_buffer(byte_pos) = 0_int8
            end if
            byte_pos = byte_pos + 1
            
            ! Write LEN (2 bytes, little endian)
            len_field = block_size
            output_buffer(byte_pos) = int(iand(len_field, z'FF'), int8)
            output_buffer(byte_pos + 1) = int(iand(ishft(len_field, -8), z'FF'), int8)
            byte_pos = byte_pos + 2
            
            ! Write NLEN (one's complement of LEN, 2 bytes, little endian)
            ! RFC 1951: NLEN is the one's complement of LEN
            nlen_field = iand(not(len_field), z'FFFF')
            output_buffer(byte_pos) = int(iand(nlen_field, z'FF'), int8)
            output_buffer(byte_pos + 1) = int(iand(ishft(nlen_field, -8), z'FF'), int8)
            byte_pos = byte_pos + 2
            
            ! Copy raw data
            output_buffer(byte_pos:byte_pos + block_size - 1) = input_data(pos:pos + block_size - 1)
            byte_pos = byte_pos + block_size
            pos = pos + block_size
            remaining = remaining - block_size
        end do
        
        output_pos = byte_pos
    end subroutine compress_with_uncompressed_blocks

end module fortplot_zlib_core
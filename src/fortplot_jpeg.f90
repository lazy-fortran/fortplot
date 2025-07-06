module fortplot_jpeg
    use fortplot_raster, only: raster_context, initialize_raster_backend, draw_axes_and_labels, draw_rotated_ylabel_raster
    use, intrinsic :: iso_fortran_env, only: wp => real64, int8, int32
    implicit none

    private
    public :: jpeg_context, create_jpeg_canvas, write_jpeg_file, get_jpeg_data
    public :: YDC_HT, UVDC_HT, YAC_HT, UVAC_HT  ! Export Huffman tables for testing
    public :: initialize_huffman_tables  ! Public initialization for testing
    
    ! STB validation data - exact values from stb_image_write.h
    ! These will be used to validate our Fortran implementation
    integer, parameter :: STB_YDC_CODES(12) = [0,2,3,4,5,6,14,30,62,126,254,510]
    integer, parameter :: STB_YDC_BITS(12) = [2,3,3,3,3,3,4,5,6,7,8,9]
    
    ! Bit accumulator for exact STB-style bit writing
    type :: bit_writer_t
        integer :: bit_buffer = 0
        integer :: bit_count = 0
        integer(1), allocatable :: output(:)
        integer :: output_pos = 1
    end type bit_writer_t
    
    ! Standard JPEG Huffman tables from STB implementation
    ! YDC_HT: Luminance DC Huffman table [code, bits]
    integer :: YDC_HT(256,2) = 0
    
    ! UVDC_HT: Chrominance DC Huffman table [code, bits]
    integer :: UVDC_HT(256,2) = 0
    
    ! YAC_HT: Luminance AC Huffman table [code, bits]
    integer :: YAC_HT(256,2) = 0
    
    ! UVAC_HT: Chrominance AC Huffman table [code, bits]
    integer :: UVAC_HT(256,2) = 0
    
    logical :: tables_initialized = .false.

    ! JPEG plotting context - extends raster context and adds JPEG-specific functionality
    type, extends(raster_context) :: jpeg_context
        integer :: quality = 85  ! JPEG quality (0-100)
    contains
        procedure :: save => jpeg_finalize
        procedure :: set_quality => jpeg_set_quality
    end type jpeg_context

contains

    subroutine initialize_huffman_tables()
        if (tables_initialized) return
        
        ! Initialize YDC_HT with STB values
        YDC_HT(1,:) = [0,2]     ! Category 0
        YDC_HT(2,:) = [2,3]     ! Category 1
        YDC_HT(3,:) = [3,3]     ! Category 2
        YDC_HT(4,:) = [4,3]     ! Category 3
        YDC_HT(5,:) = [5,3]     ! Category 4
        YDC_HT(6,:) = [6,3]     ! Category 5
        YDC_HT(7,:) = [14,4]    ! Category 6
        YDC_HT(8,:) = [30,5]    ! Category 7
        YDC_HT(9,:) = [62,6]    ! Category 8
        YDC_HT(10,:) = [126,7]  ! Category 9
        YDC_HT(11,:) = [254,8]  ! Category 10
        YDC_HT(12,:) = [510,9]  ! Category 11
        
        ! Initialize UVDC_HT with STB values
        UVDC_HT(1,:) = [0,2]      ! Category 0
        UVDC_HT(2,:) = [1,2]      ! Category 1
        UVDC_HT(3,:) = [2,2]      ! Category 2
        UVDC_HT(4,:) = [6,3]      ! Category 3
        UVDC_HT(5,:) = [14,4]     ! Category 4
        UVDC_HT(6,:) = [30,5]     ! Category 5
        UVDC_HT(7,:) = [62,6]     ! Category 6
        UVDC_HT(8,:) = [126,7]    ! Category 7
        UVDC_HT(9,:) = [254,8]    ! Category 8
        UVDC_HT(10,:) = [510,9]   ! Category 9
        UVDC_HT(11,:) = [1022,10] ! Category 10
        UVDC_HT(12,:) = [2046,11] ! Category 11
        
        ! Initialize YAC_HT with STB values - first row (0x00-0x0F)
        YAC_HT(1,:) = [10,4]      ! 0x00 EOB
        YAC_HT(2,:) = [0,2]       ! 0x01
        YAC_HT(3,:) = [1,2]       ! 0x02
        YAC_HT(4,:) = [4,3]       ! 0x03
        YAC_HT(5,:) = [11,4]      ! 0x04
        YAC_HT(6,:) = [26,5]      ! 0x05
        YAC_HT(7,:) = [120,7]     ! 0x06
        YAC_HT(8,:) = [248,8]     ! 0x07
        YAC_HT(9,:) = [1014,10]   ! 0x08
        YAC_HT(10,:) = [65410,16] ! 0x09
        YAC_HT(11,:) = [65411,16] ! 0x0A
        ! 0x0B-0x0F are unused (0,0)
        
        ! Second row (0x10-0x1F)
        YAC_HT(17,:) = [12,4]     ! 0x10
        YAC_HT(18,:) = [27,5]     ! 0x11
        YAC_HT(19,:) = [121,7]    ! 0x12
        YAC_HT(20,:) = [502,9]    ! 0x13
        YAC_HT(21,:) = [2038,11]  ! 0x14
        YAC_HT(22,:) = [65412,16] ! 0x15
        YAC_HT(23,:) = [65413,16] ! 0x16
        YAC_HT(24,:) = [65414,16] ! 0x17
        YAC_HT(25,:) = [65415,16] ! 0x18
        YAC_HT(26,:) = [65416,16] ! 0x19
        
        ! Add ZRL (Zero Run Length) at 0xF0 (241 in 1-indexed)
        YAC_HT(241,:) = [32707,15] ! 0xF0 ZRL
        
        ! Initialize UVAC_HT with STB values - first row (0x00-0x0F)
        UVAC_HT(1,:) = [0,2]      ! 0x00 EOB
        UVAC_HT(2,:) = [1,2]      ! 0x01
        UVAC_HT(3,:) = [4,3]      ! 0x02
        UVAC_HT(4,:) = [10,4]     ! 0x03
        UVAC_HT(5,:) = [24,5]     ! 0x04
        UVAC_HT(6,:) = [25,5]     ! 0x05
        UVAC_HT(7,:) = [56,6]     ! 0x06
        UVAC_HT(8,:) = [120,7]    ! 0x07
        UVAC_HT(9,:) = [500,9]    ! 0x08
        UVAC_HT(10,:) = [1014,10] ! 0x09
        UVAC_HT(11,:) = [4084,12] ! 0x0A
        ! 0x0B-0x0F are unused (0,0)
        
        ! Second row (0x10-0x1F)
        UVAC_HT(17,:) = [11,4]    ! 0x10
        UVAC_HT(18,:) = [57,6]    ! 0x11
        UVAC_HT(19,:) = [246,8]   ! 0x12
        UVAC_HT(20,:) = [501,9]   ! 0x13
        UVAC_HT(21,:) = [2038,11] ! 0x14
        UVAC_HT(22,:) = [4085,12] ! 0x15
        UVAC_HT(23,:) = [65416,16] ! 0x16
        UVAC_HT(24,:) = [65417,16] ! 0x17
        UVAC_HT(25,:) = [65418,16] ! 0x18
        UVAC_HT(26,:) = [65419,16] ! 0x19
        
        tables_initialized = .true.
    end subroutine initialize_huffman_tables

    function create_jpeg_canvas(width, height, quality) result(ctx)
        integer, intent(in) :: width, height
        integer, intent(in), optional :: quality
        type(jpeg_context) :: ctx

        ! Initialize Huffman tables if not already done
        call initialize_huffman_tables()

        ! Use common raster backend initialization
        call initialize_raster_backend(ctx, width, height)
        
        ! Set JPEG-specific quality
        if (present(quality)) then
            ctx%quality = quality
        else
            ctx%quality = 85  ! Default quality
        end if
    end function create_jpeg_canvas

    subroutine jpeg_set_quality(this, quality)
        class(jpeg_context), intent(inout) :: this
        integer, intent(in) :: quality
        
        ! Clamp quality to valid range
        this%quality = max(1, min(100, quality))
    end subroutine jpeg_set_quality

    ! All drawing methods are inherited from raster_context

    subroutine jpeg_finalize(this, filename)
        class(jpeg_context), intent(inout) :: this
        character(len=*), intent(in) :: filename

        call write_jpeg_file(filename, this%width, this%height, this%raster%image_data, this%quality)
    end subroutine jpeg_finalize

    ! Generate JPEG data from image data - pure Fortran implementation matching STB
    subroutine generate_jpeg_data(width, height, image_data, quality, jpeg_buffer)
        integer, intent(in) :: width, height, quality
        integer(1), intent(in) :: image_data(:)
        integer(1), allocatable, intent(out) :: jpeg_buffer(:)

        integer :: buffer_size, pos
        integer(1), allocatable :: compressed_data(:)
        
        ! Estimate buffer size for JPEG
        buffer_size = calculate_jpeg_buffer_size(width, height)
        allocate(jpeg_buffer(buffer_size))
        
        pos = 1
        
        ! Write JPEG file structure exactly like STB
        call write_jpeg_soi(jpeg_buffer, pos)
        call write_jpeg_app0(jpeg_buffer, pos)
        call write_jpeg_dqt(jpeg_buffer, pos, quality)
        ! Match STB: use subsampling when quality <= 90
        call write_jpeg_sof0(jpeg_buffer, pos, width, height, (quality <= 90))
        call write_jpeg_dht(jpeg_buffer, pos)
        call write_jpeg_sos(jpeg_buffer, pos)
        
        ! Encode image data using our STB-matching implementation
        call encode_jpeg_scan_data_stb_style(width, height, image_data, quality, compressed_data)
        call append_scan_data(jpeg_buffer, pos, compressed_data)
        
        ! Write end of image marker
        call write_jpeg_eoi(jpeg_buffer, pos)
        
        ! Resize buffer to actual size
        jpeg_buffer = jpeg_buffer(1:pos-1)
        
        if (allocated(compressed_data)) deallocate(compressed_data)
    end subroutine generate_jpeg_data

    ! Write JPEG data to file - pure Fortran implementation
    subroutine write_jpeg_file(filename, width, height, image_data, quality)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: width, height, quality
        integer(1), intent(in) :: image_data(:)
        
        integer(1), allocatable :: jpeg_buffer(:)
        integer :: jpeg_unit
        
        call generate_jpeg_data(width, height, image_data, quality, jpeg_buffer)
        
        open(newunit=jpeg_unit, file=filename, access='stream', form='unformatted', status='replace')
        write(jpeg_unit) jpeg_buffer
        close(jpeg_unit)
        
        deallocate(jpeg_buffer)
        print *, "JPEG file '", trim(filename), "' created successfully!"
    end subroutine write_jpeg_file

    ! Public wrapper for getting JPEG data
    subroutine get_jpeg_data(width, height, image_data, quality, jpeg_buffer)
        integer, intent(in) :: width, height, quality
        integer(1), intent(in) :: image_data(:)
        integer(1), allocatable, intent(out) :: jpeg_buffer(:)
        
        call generate_jpeg_data(width, height, image_data, quality, jpeg_buffer)
    end subroutine get_jpeg_data

    ! JPEG structure functions - each handles one responsibility

    function calculate_jpeg_buffer_size(width, height) result(size)
        integer, intent(in) :: width, height
        integer :: size
        
        ! Conservative estimate: headers + compressed data
        size = 1000 + width * height  ! Header overhead + uncompressed fallback
    end function calculate_jpeg_buffer_size

    subroutine write_jpeg_soi(buffer, pos)
        integer(1), intent(inout) :: buffer(:)
        integer, intent(inout) :: pos
        
        buffer(pos) = int(Z'FF', 1)     ! SOI marker
        buffer(pos+1) = int(Z'D8', 1)
        pos = pos + 2
    end subroutine write_jpeg_soi

    subroutine write_jpeg_app0(buffer, pos)
        integer(1), intent(inout) :: buffer(:)
        integer, intent(inout) :: pos
        
        ! APP0 JFIF header
        buffer(pos) = int(Z'FF', 1)     ! APP0 marker
        buffer(pos+1) = int(Z'E0', 1)
        buffer(pos+2) = int(0, 1)       ! Length high byte
        buffer(pos+3) = int(16, 1)      ! Length low byte (16 bytes)
        buffer(pos+4) = int(74, 1)      ! 'J'
        buffer(pos+5) = int(70, 1)      ! 'F'
        buffer(pos+6) = int(73, 1)      ! 'I'
        buffer(pos+7) = int(70, 1)      ! 'F'
        buffer(pos+8) = int(0, 1)       ! null terminator
        buffer(pos+9) = int(1, 1)       ! Major version
        buffer(pos+10) = int(1, 1)      ! Minor version
        buffer(pos+11) = int(0, 1)      ! Density units (0 = no units, aspect ratio only)
        buffer(pos+12) = int(0, 1)      ! X density high
        buffer(pos+13) = int(1, 1)      ! X density low (1)
        buffer(pos+14) = int(0, 1)      ! Y density high
        buffer(pos+15) = int(1, 1)      ! Y density low (1)
        buffer(pos+16) = int(0, 1)      ! Thumbnail width
        buffer(pos+17) = int(0, 1)      ! Thumbnail height
        pos = pos + 18
    end subroutine write_jpeg_app0

    subroutine write_jpeg_dqt(buffer, pos, quality)
        integer(1), intent(inout) :: buffer(:)
        integer, intent(inout) :: pos
        integer, intent(in) :: quality
        
        ! DQT (Define Quantization Tables) - STB writes both Y and UV tables
        buffer(pos) = int(Z'FF', 1)     ! DQT marker
        buffer(pos+1) = int(Z'DB', 1)
        buffer(pos+2) = int(0, 1)       ! Length high byte
        buffer(pos+3) = int(-124, 1)    ! Length low byte (132 bytes = 0x84)
        
        ! Y quantization table
        buffer(pos+4) = int(0, 1)       ! Table ID 0 and precision 0 (8-bit)
        call write_y_qtable(buffer(pos+5:), quality)
        
        ! UV quantization table
        buffer(pos+69) = int(1, 1)      ! Table ID 1 and precision 0 (8-bit)
        call write_uv_qtable(buffer(pos+70:), quality)
        
        pos = pos + 134
    end subroutine write_jpeg_dqt

    subroutine write_y_qtable(qtable, quality)
        integer(1), intent(out) :: qtable(64)
        integer, intent(in) :: quality
        
        integer :: i, quality_scale, yti
        ! Zigzag order indices
        integer, parameter :: zigzag(64) = [ &
             1,  2,  9, 17, 10,  3,  4, 11, &
            18, 25, 33, 26, 19, 12,  5,  6, &
            13, 20, 27, 34, 41, 49, 42, 35, &
            28, 21, 14,  7,  8, 15, 22, 29, &
            36, 43, 50, 57, 58, 51, 44, 37, &
            30, 23, 16, 24, 31, 38, 45, 52, &
            59, 60, 53, 46, 39, 32, 40, 47, &
            54, 61, 62, 55, 48, 56, 63, 64]
        
        ! STB Y quantization table in natural order (row-major)
        integer, parameter :: YQT(64) = [ &
            16, 11, 10, 16, 24, 40, 51, 61, &
            12, 12, 14, 19, 26, 58, 60, 55, &
            14, 13, 16, 24, 40, 57, 69, 56, &
            14, 17, 22, 29, 51, 87, 80, 62, &
            18, 22, 37, 56, 68,109,103, 77, &
            24, 35, 55, 64, 81,104,113, 92, &
            49, 64, 78, 87,103,121,120,101, &
            72, 92, 95, 98,112,100,103, 99]
        
        ! STB quality scaling (must match get_stb_quantization_tables)
        quality_scale = max(1, min(100, quality))
        if (quality_scale < 50) then
            quality_scale = 5000 / quality_scale
        else
            quality_scale = 200 - quality_scale * 2
        end if
        
        ! Scale and write in zigzag order like STB
        do i = 1, 64
            yti = (YQT(zigzag(i)) * quality_scale + 50) / 100
            qtable(i) = int(max(1, min(255, yti)), 1)
        end do
    end subroutine write_y_qtable
    
    subroutine write_uv_qtable(qtable, quality)
        integer(1), intent(out) :: qtable(64)
        integer, intent(in) :: quality
        
        integer :: i, quality_scale, uvti
        ! Zigzag order indices
        integer, parameter :: zigzag(64) = [ &
             1,  2,  9, 17, 10,  3,  4, 11, &
            18, 25, 33, 26, 19, 12,  5,  6, &
            13, 20, 27, 34, 41, 49, 42, 35, &
            28, 21, 14,  7,  8, 15, 22, 29, &
            36, 43, 50, 57, 58, 51, 44, 37, &
            30, 23, 16, 24, 31, 38, 45, 52, &
            59, 60, 53, 46, 39, 32, 40, 47, &
            54, 61, 62, 55, 48, 56, 63, 64]
        
        ! STB UV quantization table in natural order
        integer, parameter :: UVQT(64) = [ &
            17, 18, 24, 47, 99, 99, 99, 99, &
            18, 21, 26, 66, 99, 99, 99, 99, &
            24, 26, 56, 99, 99, 99, 99, 99, &
            47, 66, 99, 99, 99, 99, 99, 99, &
            99, 99, 99, 99, 99, 99, 99, 99, &
            99, 99, 99, 99, 99, 99, 99, 99, &
            99, 99, 99, 99, 99, 99, 99, 99, &
            99, 99, 99, 99, 99, 99, 99, 99]
        
        ! STB quality scaling (must match Y table)
        quality_scale = max(1, min(100, quality))
        if (quality_scale < 50) then
            quality_scale = 5000 / quality_scale
        else
            quality_scale = 200 - quality_scale * 2
        end if
        
        ! Scale and write in zigzag order like STB
        do i = 1, 64
            uvti = (UVQT(zigzag(i)) * quality_scale + 50) / 100
            qtable(i) = int(max(1, min(255, uvti)), 1)
        end do
    end subroutine write_uv_qtable

    subroutine write_jpeg_sof0(buffer, pos, width, height, subsample)
        integer(1), intent(inout) :: buffer(:)
        integer, intent(inout) :: pos
        integer, intent(in) :: width, height
        logical, intent(in) :: subsample
        
        ! SOF0 (Start of Frame) marker
        buffer(pos) = int(Z'FF', 1)     ! SOF0 marker
        buffer(pos+1) = int(Z'C0', 1)
        buffer(pos+2) = int(0, 1)       ! Length high byte
        buffer(pos+3) = int(17, 1)      ! Length low byte
        buffer(pos+4) = int(8, 1)       ! Sample precision (8 bits)
        
        ! Image dimensions
        call write_be16(buffer(pos+5:), height)
        call write_be16(buffer(pos+7:), width)
        
        buffer(pos+9) = int(3, 1)       ! Number of components (RGB)
        
        ! Component 1 (Y)
        buffer(pos+10) = int(1, 1)      ! Component ID
        buffer(pos+11) = merge(int(34, 1), int(17, 1), subsample)  ! 0x22 for 2x2, 0x11 for 1x1
        buffer(pos+12) = int(0, 1)      ! Quantization table ID
        
        ! Component 2 (Cb)
        buffer(pos+13) = int(2, 1)      ! Component ID
        buffer(pos+14) = int(17, 1)     ! Sampling factors (1x1)
        buffer(pos+15) = int(1, 1)      ! Quantization table ID (UV table)
        
        ! Component 3 (Cr)
        buffer(pos+16) = int(3, 1)      ! Component ID
        buffer(pos+17) = int(17, 1)     ! Sampling factors (1x1)
        buffer(pos+18) = int(1, 1)      ! Quantization table ID (UV table)
        
        pos = pos + 19
    end subroutine write_jpeg_sof0

    subroutine write_be16(buffer, value)
        integer(1), intent(out) :: buffer(2)
        integer, intent(in) :: value
        
        buffer(1) = int(ishft(value, -8), 1)    ! High byte
        buffer(2) = int(iand(value, 255), 1)    ! Low byte
    end subroutine write_be16

    subroutine write_jpeg_dht(buffer, pos)
        integer(1), intent(inout) :: buffer(:)
        integer, intent(inout) :: pos
        
        ! DHT (Define Huffman Table) - write all 4 tables in one segment like STB
        integer :: start_pos, length
        
        ! Standard Huffman tables from STB
        integer(1), parameter :: YDC_BITS(16) = [ &
            int(0,1),int(1,1),int(5,1),int(1,1),int(1,1),int(1,1),int(1,1),int(1,1), &
            int(1,1),int(0,1),int(0,1),int(0,1),int(0,1),int(0,1),int(0,1),int(0,1)]
        integer(1), parameter :: YDC_VALS(12) = [ &
            int(0,1),int(1,1),int(2,1),int(3,1),int(4,1),int(5,1), &
            int(6,1),int(7,1),int(8,1),int(9,1),int(10,1),int(11,1)]
            
        integer(1), parameter :: UVDC_BITS(16) = [ &
            int(0,1),int(3,1),int(1,1),int(1,1),int(1,1),int(1,1),int(1,1),int(1,1), &
            int(1,1),int(1,1),int(1,1),int(0,1),int(0,1),int(0,1),int(0,1),int(0,1)]
        integer(1), parameter :: UVDC_VALS(12) = [ &
            int(0,1),int(1,1),int(2,1),int(3,1),int(4,1),int(5,1), &
            int(6,1),int(7,1),int(8,1),int(9,1),int(10,1),int(11,1)]
        
        ! Write DHT marker
        buffer(pos) = int(Z'FF', 1)
        buffer(pos+1) = int(Z'C4', 1)  ! DHT marker
        start_pos = pos + 2
        pos = pos + 4  ! Skip length for now
        
        ! Y DC table (type=0, id=0)
        buffer(pos) = int(0, 1)  ! Type=DC(0), ID=0
        buffer(pos+1:pos+16) = YDC_BITS
        buffer(pos+17:pos+28) = YDC_VALS
        pos = pos + 29
        
        ! Y AC table (type=1, id=0) - use actual STB AC table data
        call write_stb_ac_table(buffer, pos, 0)  ! Y AC
        
        ! UV DC table (type=0, id=1)
        buffer(pos) = int(1, 1)  ! Type=DC(0), ID=1
        buffer(pos+1:pos+16) = UVDC_BITS
        buffer(pos+17:pos+28) = UVDC_VALS
        pos = pos + 29
        
        ! UV AC table (type=1, id=1)
        call write_stb_ac_table(buffer, pos, 1)  ! UV AC
        
        ! Write length (including the 2-byte length field itself)
        length = pos - start_pos
        buffer(start_pos) = int(ishft(length, -8), 1)
        buffer(start_pos+1) = int(iand(length, 255), 1)
        
    end subroutine write_jpeg_dht
    
    subroutine write_stb_ac_table(buffer, pos, table_id)
        integer(1), intent(inout) :: buffer(:)
        integer, intent(inout) :: pos
        integer, intent(in) :: table_id  ! 0=Y, 1=UV
        
        ! STB AC Huffman tables - exact values from stb_image_write.h
        integer(1), parameter :: YAC_BITS(16) = [ &
            int(0,1),int(2,1),int(1,1),int(3,1),int(3,1),int(2,1),int(4,1),int(3,1), &
            int(5,1),int(5,1),int(4,1),int(4,1),int(0,1),int(0,1),int(1,1),int(125,1)]
        integer(1), parameter :: YAC_VALS(162) = [ &
            int(1,1),int(2,1),int(3,1),int(0,1),int(4,1),int(17,1),int(5,1),int(18,1),int(33,1),int(49,1),int(65,1), &
            int(6,1),int(19,1),int(81,1),int(97,1),int(7,1),int(34,1),int(113,1),int(20,1),int(50,1),int(-127,1), &
            int(-111,1),int(-95,1),int(8,1),int(35,1),int(66,1),int(-79,1),int(-63,1),int(21,1),int(82,1),int(-47,1), &
            int(-16,1),int(36,1),int(51,1),int(98,1),int(114,1),int(-126,1),int(9,1),int(10,1),int(22,1),int(23,1), &
            int(24,1),int(25,1),int(26,1),int(37,1),int(38,1),int(39,1),int(40,1),int(41,1),int(42,1),int(52,1), &
            int(53,1),int(54,1),int(55,1),int(56,1),int(57,1),int(58,1),int(67,1),int(68,1),int(69,1),int(70,1), &
            int(71,1),int(72,1),int(73,1),int(74,1),int(83,1),int(84,1),int(85,1),int(86,1),int(87,1),int(88,1), &
            int(89,1),int(90,1),int(99,1),int(100,1),int(101,1),int(102,1),int(103,1),int(104,1),int(105,1),int(106,1), &
            int(115,1),int(116,1),int(117,1),int(118,1),int(119,1),int(120,1),int(121,1),int(122,1),int(-125,1), &
            int(-124,1),int(-123,1),int(-122,1),int(-121,1),int(-120,1),int(-119,1),int(-118,1),int(-110,1), &
            int(-109,1),int(-108,1),int(-107,1),int(-106,1),int(-105,1),int(-104,1),int(-103,1),int(-102,1), &
            int(-94,1),int(-93,1),int(-92,1),int(-91,1),int(-90,1),int(-89,1),int(-88,1),int(-87,1),int(-86,1), &
            int(-78,1),int(-77,1),int(-76,1),int(-75,1),int(-74,1),int(-73,1),int(-72,1),int(-71,1),int(-70,1), &
            int(-62,1),int(-61,1),int(-60,1),int(-59,1),int(-58,1),int(-57,1),int(-56,1),int(-55,1),int(-54,1), &
            int(-46,1),int(-45,1),int(-44,1),int(-43,1),int(-42,1),int(-41,1),int(-40,1),int(-39,1),int(-38,1), &
            int(-31,1),int(-30,1),int(-29,1),int(-28,1),int(-27,1),int(-26,1),int(-25,1),int(-24,1),int(-23,1), &
            int(-22,1),int(-15,1),int(-14,1),int(-13,1),int(-12,1),int(-11,1),int(-10,1),int(-9,1),int(-8,1), &
            int(-7,1),int(-6,1)]
            
        integer(1), parameter :: UVAC_BITS(16) = [ &
            int(0,1),int(2,1),int(1,1),int(2,1),int(4,1),int(4,1),int(3,1),int(4,1), &
            int(7,1),int(5,1),int(4,1),int(4,1),int(0,1),int(1,1),int(2,1),int(119,1)]
        integer(1), parameter :: UVAC_VALS(162) = [ &
            int(0,1),int(1,1),int(2,1),int(3,1),int(17,1),int(4,1),int(5,1),int(33,1),int(49,1),int(6,1),int(18,1), &
            int(65,1),int(81,1),int(7,1),int(97,1),int(113,1),int(19,1),int(34,1),int(50,1),int(-127,1),int(8,1), &
            int(20,1),int(66,1),int(-111,1),int(-95,1),int(-79,1),int(-63,1),int(9,1),int(35,1),int(51,1),int(82,1), &
            int(-16,1),int(21,1),int(98,1),int(114,1),int(-47,1),int(10,1),int(22,1),int(36,1),int(52,1),int(-31,1), &
            int(37,1),int(-15,1),int(23,1),int(24,1),int(25,1),int(26,1),int(38,1),int(39,1),int(40,1),int(41,1), &
            int(42,1),int(53,1),int(54,1),int(55,1),int(56,1),int(57,1),int(58,1),int(67,1),int(68,1),int(69,1), &
            int(70,1),int(71,1),int(72,1),int(73,1),int(74,1),int(83,1),int(84,1),int(85,1),int(86,1),int(87,1), &
            int(88,1),int(89,1),int(90,1),int(99,1),int(100,1),int(101,1),int(102,1),int(103,1),int(104,1),int(105,1), &
            int(106,1),int(115,1),int(116,1),int(117,1),int(118,1),int(119,1),int(120,1),int(121,1),int(122,1), &
            int(-126,1),int(-125,1),int(-124,1),int(-123,1),int(-122,1),int(-121,1),int(-120,1),int(-119,1), &
            int(-118,1),int(-110,1),int(-109,1),int(-108,1),int(-107,1),int(-106,1),int(-105,1),int(-104,1), &
            int(-103,1),int(-102,1),int(-94,1),int(-93,1),int(-92,1),int(-91,1),int(-90,1),int(-89,1),int(-88,1), &
            int(-87,1),int(-86,1),int(-78,1),int(-77,1),int(-76,1),int(-75,1),int(-74,1),int(-73,1),int(-72,1), &
            int(-71,1),int(-70,1),int(-62,1),int(-61,1),int(-60,1),int(-59,1),int(-58,1),int(-57,1),int(-56,1), &
            int(-55,1),int(-54,1),int(-46,1),int(-45,1),int(-44,1),int(-43,1),int(-42,1),int(-41,1),int(-40,1), &
            int(-39,1),int(-38,1),int(-30,1),int(-29,1),int(-28,1),int(-27,1),int(-26,1),int(-25,1),int(-24,1), &
            int(-23,1),int(-22,1),int(-14,1),int(-13,1),int(-12,1),int(-11,1),int(-10,1),int(-9,1),int(-8,1), &
            int(-7,1),int(-6,1)]
        
        if (table_id == 0) then
            ! Y AC table
            buffer(pos) = int(16, 1)  ! Type=AC(1), ID=0 = 0x10
            buffer(pos+1:pos+16) = YAC_BITS
            buffer(pos+17:pos+178) = YAC_VALS
            pos = pos + 179
        else
            ! UV AC table
            buffer(pos) = int(17, 1)  ! Type=AC(1), ID=1 = 0x11
            buffer(pos+1:pos+16) = UVAC_BITS
            buffer(pos+17:pos+178) = UVAC_VALS
            pos = pos + 179
        end if
    end subroutine write_stb_ac_table

    subroutine write_dc_huffman_table(buffer, pos, table_id)
        integer(1), intent(inout) :: buffer(:)
        integer, intent(inout) :: pos
        integer, intent(in) :: table_id  ! 0=luminance, 1=chrominance
        
        ! Standard DC Huffman bit counts and values
        integer(1), parameter :: lum_dc_bits(16) = [ &
            int(0,1),int(1,1),int(5,1),int(1,1),int(1,1),int(1,1),int(1,1),int(1,1), &
            int(1,1),int(0,1),int(0,1),int(0,1),int(0,1),int(0,1),int(0,1),int(0,1)]
        integer(1), parameter :: lum_dc_vals(12) = [ &
            int(0,1),int(1,1),int(2,1),int(3,1),int(4,1),int(5,1), &
            int(6,1),int(7,1),int(8,1),int(9,1),int(10,1),int(11,1)]
        integer(1), parameter :: chr_dc_bits(16) = [ &
            int(0,1),int(3,1),int(1,1),int(1,1),int(1,1),int(1,1),int(1,1),int(1,1), &
            int(1,1),int(1,1),int(1,1),int(0,1),int(0,1),int(0,1),int(0,1),int(0,1)]
        integer(1), parameter :: chr_dc_vals(12) = [ &
            int(0,1),int(1,1),int(2,1),int(3,1),int(4,1),int(5,1), &
            int(6,1),int(7,1),int(8,1),int(9,1),int(10,1),int(11,1)]
        
        integer :: start_pos, total_vals
        
        start_pos = pos
        buffer(pos) = int(Z'FF', 1)     ! DHT marker
        buffer(pos+1) = int(Z'C4', 1)
        
        if (table_id == 0) then
            total_vals = 12  ! Luminance DC values
            buffer(pos+2) = int(0, 1)       ! Length high
            buffer(pos+3) = int(19 + total_vals, 1)  ! Length low
            buffer(pos+4) = int(0, 1)       ! Table class (0=DC) + ID (0)
            buffer(pos+5:pos+20) = lum_dc_bits
            buffer(pos+21:pos+32) = lum_dc_vals
            pos = pos + 33
        else
            total_vals = 12  ! Chrominance DC values
            buffer(pos+2) = int(0, 1)       ! Length high
            buffer(pos+3) = int(19 + total_vals, 1)  ! Length low
            buffer(pos+4) = int(1, 1)       ! Table class (0=DC) + ID (1)
            buffer(pos+5:pos+20) = chr_dc_bits
            buffer(pos+21:pos+32) = chr_dc_vals
            pos = pos + 33
        end if
    end subroutine write_dc_huffman_table
    
    subroutine write_ac_huffman_table(buffer, pos, table_id)
        integer(1), intent(inout) :: buffer(:)
        integer, intent(inout) :: pos
        integer, intent(in) :: table_id  ! 0=luminance, 1=chrominance
        
        ! Standard AC Huffman bit counts
        integer(1), parameter :: lum_ac_bits(16) = [ &
            int(0,1),int(2,1),int(1,1),int(3,1),int(3,1),int(2,1),int(4,1),int(3,1), &
            int(5,1),int(5,1),int(4,1),int(4,1),int(0,1),int(0,1),int(1,1),int(125,1)]
        integer(1), parameter :: chr_ac_bits(16) = [ &
            int(0,1),int(2,1),int(1,1),int(2,1),int(4,1),int(4,1),int(3,1),int(4,1), &
            int(7,1),int(5,1),int(4,1),int(4,1),int(0,1),int(1,1),int(2,1),int(119,1)]
        
        ! Complete AC values for luminance (162 values)
        integer(1), parameter :: lum_ac_vals(162) = [ &
            int(Z'01',1), int(Z'02',1), int(Z'03',1), int(Z'00',1), int(Z'04',1), int(Z'11',1), &
            int(Z'05',1), int(Z'12',1), int(Z'21',1), int(Z'31',1), int(Z'41',1), int(Z'06',1), &
            int(Z'13',1), int(Z'51',1), int(Z'61',1), int(Z'07',1), int(Z'22',1), int(Z'71',1), &
            int(Z'14',1), int(Z'32',1), int(Z'81',1), int(Z'91',1), int(Z'A1',1), int(Z'08',1), &
            int(Z'23',1), int(Z'42',1), int(Z'B1',1), int(Z'C1',1), int(Z'15',1), int(Z'52',1), &
            int(Z'D1',1), int(Z'F0',1), int(Z'24',1), int(Z'33',1), int(Z'62',1), int(Z'72',1), &
            int(Z'82',1), int(Z'09',1), int(Z'0A',1), int(Z'16',1), int(Z'17',1), int(Z'18',1), &
            int(Z'19',1), int(Z'1A',1), int(Z'25',1), int(Z'26',1), int(Z'27',1), int(Z'28',1), &
            int(Z'29',1), int(Z'2A',1), int(Z'34',1), int(Z'35',1), int(Z'36',1), int(Z'37',1), &
            int(Z'38',1), int(Z'39',1), int(Z'3A',1), int(Z'43',1), int(Z'44',1), int(Z'45',1), &
            int(Z'46',1), int(Z'47',1), int(Z'48',1), int(Z'49',1), int(Z'4A',1), int(Z'53',1), &
            int(Z'54',1), int(Z'55',1), int(Z'56',1), int(Z'57',1), int(Z'58',1), int(Z'59',1), &
            int(Z'5A',1), int(Z'63',1), int(Z'64',1), int(Z'65',1), int(Z'66',1), int(Z'67',1), &
            int(Z'68',1), int(Z'69',1), int(Z'6A',1), int(Z'73',1), int(Z'74',1), int(Z'75',1), &
            int(Z'76',1), int(Z'77',1), int(Z'78',1), int(Z'79',1), int(Z'7A',1), int(Z'83',1), &
            int(Z'84',1), int(Z'85',1), int(Z'86',1), int(Z'87',1), int(Z'88',1), int(Z'89',1), &
            int(Z'8A',1), int(Z'92',1), int(Z'93',1), int(Z'94',1), int(Z'95',1), int(Z'96',1), &
            int(Z'97',1), int(Z'98',1), int(Z'99',1), int(Z'9A',1), int(Z'A2',1), int(Z'A3',1), &
            int(Z'A4',1), int(Z'A5',1), int(Z'A6',1), int(Z'A7',1), int(Z'A8',1), int(Z'A9',1), &
            int(Z'AA',1), int(Z'B2',1), int(Z'B3',1), int(Z'B4',1), int(Z'B5',1), int(Z'B6',1), &
            int(Z'B7',1), int(Z'B8',1), int(Z'B9',1), int(Z'BA',1), int(Z'C2',1), int(Z'C3',1), &
            int(Z'C4',1), int(Z'C5',1), int(Z'C6',1), int(Z'C7',1), int(Z'C8',1), int(Z'C9',1), &
            int(Z'CA',1), int(Z'D2',1), int(Z'D3',1), int(Z'D4',1), int(Z'D5',1), int(Z'D6',1), &
            int(Z'D7',1), int(Z'D8',1), int(Z'D9',1), int(Z'DA',1), int(Z'E1',1), int(Z'E2',1), &
            int(Z'E3',1), int(Z'E4',1), int(Z'E5',1), int(Z'E6',1), int(Z'E7',1), int(Z'E8',1), &
            int(Z'E9',1), int(Z'EA',1), int(Z'F1',1), int(Z'F2',1), int(Z'F3',1), int(Z'F4',1), &
            int(Z'F5',1), int(Z'F6',1), int(Z'F7',1), int(Z'F8',1), int(Z'F9',1), int(Z'FA',1) ]
        
        ! Complete AC values for chrominance (162 values)
        integer(1), parameter :: chr_ac_vals(162) = [ &
            int(Z'00',1), int(Z'01',1), int(Z'02',1), int(Z'03',1), int(Z'11',1), int(Z'04',1), &
            int(Z'05',1), int(Z'21',1), int(Z'31',1), int(Z'06',1), int(Z'12',1), int(Z'41',1), &
            int(Z'51',1), int(Z'07',1), int(Z'61',1), int(Z'71',1), int(Z'13',1), int(Z'22',1), &
            int(Z'32',1), int(Z'81',1), int(Z'08',1), int(Z'14',1), int(Z'42',1), int(Z'91',1), &
            int(Z'A1',1), int(Z'B1',1), int(Z'C1',1), int(Z'09',1), int(Z'23',1), int(Z'33',1), &
            int(Z'52',1), int(Z'F0',1), int(Z'15',1), int(Z'62',1), int(Z'72',1), int(Z'D1',1), &
            int(Z'0A',1), int(Z'16',1), int(Z'24',1), int(Z'34',1), int(Z'E1',1), int(Z'25',1), &
            int(Z'F1',1), int(Z'17',1), int(Z'18',1), int(Z'19',1), int(Z'1A',1), int(Z'26',1), &
            int(Z'27',1), int(Z'28',1), int(Z'29',1), int(Z'2A',1), int(Z'35',1), int(Z'36',1), &
            int(Z'37',1), int(Z'38',1), int(Z'39',1), int(Z'3A',1), int(Z'43',1), int(Z'44',1), &
            int(Z'45',1), int(Z'46',1), int(Z'47',1), int(Z'48',1), int(Z'49',1), int(Z'4A',1), &
            int(Z'53',1), int(Z'54',1), int(Z'55',1), int(Z'56',1), int(Z'57',1), int(Z'58',1), &
            int(Z'59',1), int(Z'5A',1), int(Z'63',1), int(Z'64',1), int(Z'65',1), int(Z'66',1), &
            int(Z'67',1), int(Z'68',1), int(Z'69',1), int(Z'6A',1), int(Z'73',1), int(Z'74',1), &
            int(Z'75',1), int(Z'76',1), int(Z'77',1), int(Z'78',1), int(Z'79',1), int(Z'7A',1), &
            int(Z'82',1), int(Z'83',1), int(Z'84',1), int(Z'85',1), int(Z'86',1), int(Z'87',1), &
            int(Z'88',1), int(Z'89',1), int(Z'8A',1), int(Z'92',1), int(Z'93',1), int(Z'94',1), &
            int(Z'95',1), int(Z'96',1), int(Z'97',1), int(Z'98',1), int(Z'99',1), int(Z'9A',1), &
            int(Z'A2',1), int(Z'A3',1), int(Z'A4',1), int(Z'A5',1), int(Z'A6',1), int(Z'A7',1), &
            int(Z'A8',1), int(Z'A9',1), int(Z'AA',1), int(Z'B2',1), int(Z'B3',1), int(Z'B4',1), &
            int(Z'B5',1), int(Z'B6',1), int(Z'B7',1), int(Z'B8',1), int(Z'B9',1), int(Z'BA',1), &
            int(Z'C2',1), int(Z'C3',1), int(Z'C4',1), int(Z'C5',1), int(Z'C6',1), int(Z'C7',1), &
            int(Z'C8',1), int(Z'C9',1), int(Z'CA',1), int(Z'D2',1), int(Z'D3',1), int(Z'D4',1), &
            int(Z'D5',1), int(Z'D6',1), int(Z'D7',1), int(Z'D8',1), int(Z'D9',1), int(Z'DA',1), &
            int(Z'E2',1), int(Z'E3',1), int(Z'E4',1), int(Z'E5',1), int(Z'E6',1), int(Z'E7',1), &
            int(Z'E8',1), int(Z'E9',1), int(Z'EA',1), int(Z'F2',1), int(Z'F3',1), int(Z'F4',1), &
            int(Z'F5',1), int(Z'F6',1), int(Z'F7',1), int(Z'F8',1), int(Z'F9',1), int(Z'FA',1) ]
        
        buffer(pos) = int(Z'FF', 1)     ! DHT marker
        buffer(pos+1) = int(Z'C4', 1)
        
        if (table_id == 0) then
            buffer(pos+2) = int(0, 1)       ! Length high
            buffer(pos+3) = int(-77, 1)     ! Length low (16 + 162 + 1) = 179 = 0xB3
            buffer(pos+4) = int(16, 1)      ! Table class (1=AC) + ID (0)
            buffer(pos+5:pos+20) = lum_ac_bits
            buffer(pos+21:pos+182) = lum_ac_vals
            pos = pos + 183
        else
            buffer(pos+2) = int(0, 1)       ! Length high
            buffer(pos+3) = int(-77, 1)     ! Length low (16 + 162 + 1) = 179 = 0xB3
            buffer(pos+4) = int(17, 1)      ! Table class (1=AC) + ID (1)
            buffer(pos+5:pos+20) = chr_ac_bits
            buffer(pos+21:pos+182) = chr_ac_vals
            pos = pos + 183
        end if
    end subroutine write_ac_huffman_table

    subroutine write_jpeg_sos(buffer, pos)
        integer(1), intent(inout) :: buffer(:)
        integer, intent(inout) :: pos
        
        ! SOS (Start of Scan) marker
        buffer(pos) = int(Z'FF', 1)     ! SOS marker
        buffer(pos+1) = int(Z'DA', 1)
        buffer(pos+2) = int(0, 1)       ! Length high byte
        buffer(pos+3) = int(12, 1)      ! Length low byte
        buffer(pos+4) = int(3, 1)       ! Number of components
        
        ! Component scan parameters
        buffer(pos+5) = int(1, 1)       ! Component ID (Y)
        buffer(pos+6) = int(0, 1)       ! DC=0, AC=0 (Y tables)
        buffer(pos+7) = int(2, 1)       ! Component ID (Cb)
        buffer(pos+8) = int(17, 1)      ! DC=1, AC=1 (UV tables) = 0x11
        buffer(pos+9) = int(3, 1)       ! Component ID (Cr)
        buffer(pos+10) = int(17, 1)     ! DC=1, AC=1 (UV tables) = 0x11
        
        buffer(pos+11) = int(0, 1)      ! Start of spectral selection
        buffer(pos+12) = int(63, 1)     ! End of spectral selection
        buffer(pos+13) = int(0, 1)      ! Successive approximation
        
        pos = pos + 14
    end subroutine write_jpeg_sos

    ! STB-style scan data encoding
    subroutine encode_jpeg_scan_data_stb_style(width, height, image_data, quality, compressed_data)
        integer, intent(in) :: width, height, quality
        integer(1), intent(in) :: image_data(:)
        integer(1), allocatable, intent(out) :: compressed_data(:)
        
        type(bit_writer_t) :: writer
        real, allocatable :: ycbcr_data(:,:,:)
        integer :: DCY, DCU, DCV, c
        
        ! Ensure Huffman tables are initialized
        call initialize_huffman_tables()
        
        ! Initialize bit writer with STB-style buffer
        allocate(writer%output(1024))  ! Initial size
        writer%bit_buffer = 0
        writer%bit_count = 0
        writer%output_pos = 1
        
        ! Convert RGB to YCbCr exactly like STB
        call rgb_to_ycbcr(width, height, image_data, ycbcr_data)
        
        ! Initialize DC values
        DCY = 0
        DCU = 0  
        DCV = 0
        
        ! Process 8x8 blocks exactly like STB processDU
        call encode_blocks_stb_style(writer, width, height, ycbcr_data, quality, DCY, DCU, DCV)
        
        ! STB-style fillBits - flush remaining bits
        if (writer%bit_count > 0) then
            call stb_write_bits(writer, int(Z'7F'), 7)
        end if
        
        ! STB may handle remaining bits differently
        ! Remove final flush to match STB behavior
        
        ! Copy output to result
        allocate(compressed_data(writer%output_pos - 1))
        compressed_data = writer%output(1:writer%output_pos - 1)
        
        deallocate(writer%output)
        deallocate(ycbcr_data)
    end subroutine encode_jpeg_scan_data_stb_style

    subroutine append_scan_data(buffer, pos, scan_data)
        integer(1), intent(inout) :: buffer(:)
        integer, intent(inout) :: pos
        integer(1), intent(in) :: scan_data(:)
        
        integer :: i
        
        do i = 1, size(scan_data)
            buffer(pos + i - 1) = scan_data(i)
        end do
        pos = pos + size(scan_data)
    end subroutine append_scan_data

    subroutine write_jpeg_eoi(buffer, pos)
        integer(1), intent(inout) :: buffer(:)
        integer, intent(inout) :: pos
        
        buffer(pos) = int(Z'FF', 1)     ! EOI marker
        buffer(pos+1) = int(Z'D9', 1)
        pos = pos + 2
    end subroutine write_jpeg_eoi

    ! Minimal JPEG encoding functions - focused on working output

    function calculate_average_luminance(width, height, rgb_data) result(y_avg)
        integer, intent(in) :: width, height
        integer(1), intent(in) :: rgb_data(:)
        integer :: y_avg
        
        real :: total_y, r, g, b
        integer :: x, y, pixel_count
        
        total_y = 0.0
        pixel_count = 0
        
        do y = 1, min(height, 10)  ! Sample first 10 rows for efficiency
            do x = 1, min(width, 10)   ! Sample first 10 columns
                call extract_rgb_pixel(rgb_data, width, x, y, r, g, b)
                total_y = total_y + (0.299*r + 0.587*g + 0.114*b)
                pixel_count = pixel_count + 1
            end do
        end do
        
        y_avg = nint(total_y / real(pixel_count))
    end function calculate_average_luminance

    subroutine create_minimal_valid_scan_data(scan_data, data_size, y_avg)
        integer(1), intent(out) :: scan_data(:)
        integer, intent(in) :: data_size, y_avg
        
        integer :: i, dc_value
        
        ! Create a valid minimal JPEG scan with uniform DC values
        ! This creates a solid color image but is actually decodable
        
        ! Map luminance to DC coefficient range
        dc_value = max(-127, min(127, y_avg - 128))
        
        ! Fill with minimal entropy-coded data representing uniform blocks
        do i = 1, data_size
            select case (mod(i-1, 8))
            case (0)
                ! DC coefficient (simplified encoding)
                scan_data(i) = int(128 + dc_value/2, 1)  ! Scaled DC value
            case (1)
                ! AC coefficient terminator
                scan_data(i) = int(0, 1)  ! End of block
            case default
                ! Padding/AC coefficients (mostly zeros)
                scan_data(i) = int(0, 1)
            end select
        end do
        
        ! Ensure we don't have forbidden byte sequences
        call fix_forbidden_sequences(scan_data, data_size)
    end subroutine create_minimal_valid_scan_data

    subroutine fix_forbidden_sequences(scan_data, data_size)
        integer(1), intent(inout) :: scan_data(:)
        integer, intent(in) :: data_size
        
        integer :: i
        
        ! Fix any 0xFF sequences that could be mistaken for markers
        do i = 1, data_size - 1
            if (scan_data(i) == int(Z'FF', 1)) then
                ! Must be followed by 0x00 in scan data, not another marker
                if (scan_data(i+1) /= int(0, 1) .and. scan_data(i+1) < int(Z'D0', 1)) then
                    scan_data(i+1) = int(0, 1)
                end if
            end if
        end do
    end subroutine fix_forbidden_sequences

    subroutine rgb_to_ycbcr(width, height, rgb_data, ycbcr_data)
        integer, intent(in) :: width, height
        integer(1), intent(in) :: rgb_data(:)
        real, allocatable, intent(out) :: ycbcr_data(:,:,:)
        
        integer :: x, y, idx
        real :: r, g, b
        
        allocate(ycbcr_data(width, height, 3))
        
        do y = 1, height
            do x = 1, width
                ! Extract RGB values from packed data
                call extract_rgb_pixel(rgb_data, width, x, y, r, g, b)
                
                ! Convert to YCbCr using STB coefficients (exact match)
                ! STB subtracts 128 from Y but not from U/V
                ycbcr_data(x, y, 1) = 0.299*r + 0.587*g + 0.114*b - 128.0    ! Y (STB style)
                ycbcr_data(x, y, 2) = -0.16874*r - 0.33126*g + 0.5*b         ! Cb (STB style)
                ycbcr_data(x, y, 3) = 0.5*r - 0.41869*g - 0.08131*b          ! Cr (STB style)
            end do
        end do
    end subroutine rgb_to_ycbcr

    subroutine extract_rgb_pixel(rgb_data, width, x, y, r, g, b)
        integer(1), intent(in) :: rgb_data(:)
        integer, intent(in) :: width, x, y
        real, intent(out) :: r, g, b
        
        integer :: base_idx
        
        ! Calculate base index for flat RGB data (no PNG filter bytes)
        base_idx = ((y - 1) * width + (x - 1)) * 3 + 1
        
        r = real(iand(int(rgb_data(base_idx)), 255))
        g = real(iand(int(rgb_data(base_idx + 1)), 255))  
        b = real(iand(int(rgb_data(base_idx + 2)), 255))
    end subroutine extract_rgb_pixel

    ! Legacy function - replaced by encode_blocks_stb_style
    ! subroutine encode_ycbcr_blocks - see encode_blocks_stb_style

    subroutine extract_8x8_block(component_data, width, height, start_x, start_y, block)
        real, intent(in) :: component_data(:,:)
        integer, intent(in) :: width, height, start_x, start_y
        real, intent(out) :: block(8, 8)
        
        integer :: x, y, src_x, src_y
        
        do y = 1, 8
            do x = 1, 8
                src_x = min(start_x + x - 1, width)
                src_y = min(start_y + y - 1, height)
                ! STB already subtracts 128 from Y in RGB->YCbCr conversion
                ! U and V are already centered around 0
                block(x, y) = component_data(src_x, src_y)
            end do
        end do
    end subroutine extract_8x8_block

    subroutine apply_dct_8x8(block, coeffs)
        real, intent(in) :: block(8, 8)
        integer, intent(out) :: coeffs(64)
        
        real :: dct_block(8, 8)
        integer :: u, v, x, y, idx
        real :: sum_val, cu, cv
        real, parameter :: PI = 3.14159265358979323846
        
        ! 2D DCT transformation
        do v = 0, 7
            do u = 0, 7
                sum_val = 0.0
                
                do y = 0, 7
                    do x = 0, 7
                        sum_val = sum_val + block(x+1, y+1) * &
                                 cos((2*x+1)*u*PI/16.0) * cos((2*y+1)*v*PI/16.0)
                    end do
                end do
                
                ! Apply normalization factors
                cu = merge(1.0/sqrt(2.0), 1.0, u == 0)
                cv = merge(1.0/sqrt(2.0), 1.0, v == 0)
                
                dct_block(u+1, v+1) = 0.25 * cu * cv * sum_val
            end do
        end do
        
        ! Convert to zigzag order
        call zigzag_reorder(dct_block, coeffs)
    end subroutine apply_dct_8x8

    subroutine zigzag_reorder(dct_block, coeffs)
        real, intent(in) :: dct_block(8, 8)
        integer, intent(out) :: coeffs(64)
        
        ! STB zigzag order table (converted from 0-based to 1-based)
        integer, parameter :: zigzag_table(64) = [ &
            1,  2,  6,  7, 15, 16, 28, 29, &
            3,  5,  8, 14, 17, 27, 30, 43, &
            4,  9, 13, 18, 26, 31, 42, 44, &
           10, 12, 19, 25, 32, 41, 45, 54, &
           11, 20, 24, 33, 40, 46, 53, 55, &
           21, 23, 34, 39, 47, 52, 56, 61, &
           22, 35, 38, 48, 51, 57, 60, 62, &
           36, 37, 49, 50, 58, 59, 63, 64]
        
        integer :: i, row, col
        
        do i = 1, 64
            row = (zigzag_table(i) - 1) / 8 + 1
            col = mod(zigzag_table(i) - 1, 8) + 1
            coeffs(i) = nint(dct_block(col, row))
        end do
    end subroutine zigzag_reorder

    subroutine quantize_block(coeffs, quality)
        integer, intent(inout) :: coeffs(64)
        integer, intent(in) :: quality
        
        integer :: qtable(64), i, scale_factor
        
        ! Get standard quantization table
        call get_quantization_table(qtable, quality)
        
        ! Apply quantization
        do i = 1, 64
            coeffs(i) = coeffs(i) / max(1, qtable(i))
        end do
    end subroutine quantize_block

    subroutine get_quantization_table(qtable, quality)
        integer, intent(out) :: qtable(64)
        integer, intent(in) :: quality
        
        integer :: base_table(64), i, scale_factor
        
        ! Standard JPEG quantization table
        base_table = [16, 11, 10, 16, 24, 40, 51, 61, &
                      12, 12, 14, 19, 26, 58, 60, 55, &
                      14, 13, 16, 24, 40, 57, 69, 56, &
                      14, 17, 22, 29, 51, 87, 80, 62, &
                      18, 22, 37, 56, 68, 109, 103, 77, &
                      24, 35, 55, 64, 81, 104, 113, 92, &
                      49, 64, 78, 87, 103, 121, 120, 101, &
                      72, 92, 95, 98, 112, 100, 103, 99]
        
        scale_factor = max(1, min(100, quality))
        do i = 1, 64
            qtable(i) = max(1, (base_table(i) * 100) / scale_factor)
        end do
    end subroutine get_quantization_table

    ! Legacy function - replaced by process_du_stb_style
    ! subroutine encode_block_huffman - see process_du_stb_style

    ! STB-style block encoding matching processDU function exactly
    subroutine encode_blocks_stb_style(writer, width, height, ycbcr_data, quality, DCY, DCU, DCV)
        type(bit_writer_t), intent(inout) :: writer
        integer, intent(in) :: width, height, quality
        real, intent(in) :: ycbcr_data(:,:,:)
        integer, intent(inout) :: DCY, DCU, DCV
        
        integer :: x, y, component
        real :: block_8x8(8, 8)
        real :: y_16x16(256)  ! 16x16 block for subsampling
        integer :: DU(64)
        real :: fdtbl_Y(64), fdtbl_UV(64)
        logical :: subsample
        integer :: block_count, mcu_count
        
        ! Get quantization tables exactly like STB
        call get_stb_quantization_tables(fdtbl_Y, fdtbl_UV, quality)
        
        ! Match STB logic: use subsampling when quality <= 90
        subsample = (quality <= 90)
        
        block_count = 0
        mcu_count = 0
        
        if (subsample) then
            ! Process each MCU (16x16 for 2x2 subsampling)
            do y = 1, height, 16
                do x = 1, width, 16
                mcu_count = mcu_count + 1
                ! Process Y component
                ! For 2x2 subsampling, we need to process 4 Y blocks per MCU
                ! Even for 8x8 image, we need all 4 blocks
                
                ! Process Y component exactly like STB: extract 16x16 then process 4 8x8 blocks
                call process_y_blocks_stb_exact(writer, ycbcr_data(:,:,1), width, height, x, y, fdtbl_Y, DCY)
                block_count = block_count + 4
                
                ! Process U and V components (1 block each, subsampled)
                ! For each MCU, we always process U and V blocks
                ! print *, "Processing U/V at", x, y
                call extract_and_subsample_stb_style(ycbcr_data(:,:,2), width, height, x, y, block_8x8)
                call process_du_stb_style(writer, block_8x8, 8, fdtbl_UV, DCU, .false.)
                block_count = block_count + 1
                
                call extract_and_subsample_stb_style(ycbcr_data(:,:,3), width, height, x, y, block_8x8)
                call process_du_stb_style(writer, block_8x8, 8, fdtbl_UV, DCV, .false.)
                block_count = block_count + 1
            end do
        end do
        else
            ! No subsampling - process 8x8 blocks like STB default
            do y = 1, height, 8
                do x = 1, width, 8
                    ! Process Y block
                    call extract_8x8_block(ycbcr_data(:,:,1), width, height, x, y, block_8x8)
                    call process_du_stb_style(writer, block_8x8, 8, fdtbl_Y, DCY, .true.)
                    block_count = block_count + 1
                    
                    ! Process U block
                    call extract_8x8_block(ycbcr_data(:,:,2), width, height, x, y, block_8x8)
                    call process_du_stb_style(writer, block_8x8, 8, fdtbl_UV, DCU, .false.)
                    block_count = block_count + 1
                    
                    ! Process V block
                    call extract_8x8_block(ycbcr_data(:,:,3), width, height, x, y, block_8x8)
                    call process_du_stb_style(writer, block_8x8, 8, fdtbl_UV, DCV, .false.)
                    block_count = block_count + 1
                end do
            end do
        end if
    end subroutine encode_blocks_stb_style
    
    ! Exact STB processDU function implementation
    subroutine process_du_stb_style(writer, CDU, du_stride, fdtbl, DC, is_luma)
        type(bit_writer_t), intent(inout) :: writer
        real, intent(in) :: CDU(8, 8)
        integer, intent(in) :: du_stride
        real, intent(in) :: fdtbl(64)
        integer, intent(inout) :: DC
        logical, intent(in) :: is_luma
        
        
        integer :: DU(64)
        integer :: diff, end0pos, i, startpos, nrzeroes
        integer :: code, bits, category
        ! EOB codes depend on luma/chroma - removed hardcoded values
        integer, parameter :: M16_CODE = 65472, M16_BITS = 16  ! From STB YAC_HT[0xF0]
        
        ! Apply DCT and quantization exactly like STB
        call apply_dct_and_quantize_stb_style(CDU, DU, fdtbl)
        
        ! Encode DC coefficient exactly like STB
        diff = DU(1) - DC  ! STB: diff = DU[0] - DC
        ! print *, "DC encoding: DU(1)=", DU(1), "DC=", DC, "diff=", diff, "is_luma=", is_luma
        if (diff == 0) then
            ! STB: stbiw__jpg_writeBits(s, bitBuf, bitCnt, HTDC[0])
            if (is_luma) then
                call stb_write_bits(writer, STB_YDC_CODES(1), STB_YDC_BITS(1))  ! Category 0
            else
                call stb_write_bits(writer, 0, 2)  ! UVDC category 0: {0,2}
            end if
        else
            category = get_dc_category(diff)
            if (category <= 11) then
                if (is_luma) then
                    call stb_write_bits(writer, STB_YDC_CODES(category + 1), STB_YDC_BITS(category + 1))
                else
                    ! Use UVDC table
                    call stb_write_bits(writer, get_uvdc_code(category), get_uvdc_bits(category))
                end if
                call write_dc_value_bits_stb_style(writer, diff, category)
            end if
        end if
        
        ! Update DC for next block
        DC = DU(1)
        
        ! Encode AC coefficients exactly like STB
        end0pos = 64
        do while (end0pos > 1 .and. DU(end0pos) == 0)
            end0pos = end0pos - 1
        end do
        
        if (end0pos == 1) then
            ! STB: stbiw__jpg_writeBits(s, bitBuf, bitCnt, EOB)
            ! Write EOB using correct table
            if (is_luma) then
                call stb_write_bits(writer, YAC_HT(1, 1), YAC_HT(1, 2))  ! Y EOB
            else
                call stb_write_bits(writer, UVAC_HT(1, 1), UVAC_HT(1, 2))  ! UV EOB
            end if
            return
        end if
        
        ! Encode AC coefficients with run-length encoding
        i = 2
        do while (i <= end0pos)
            startpos = i
            do while (DU(i) == 0 .and. i <= end0pos)
                i = i + 1
            end do
            nrzeroes = i - startpos
            
            ! Handle runs of 16 or more zeros
            do while (nrzeroes >= 16)
                call stb_write_bits(writer, M16_CODE, M16_BITS)  ! STB M16zeroes
                nrzeroes = nrzeroes - 16
            end do
            
            if (i <= end0pos) then
                category = get_ac_category(DU(i))
                ! Symbol is (nrzeroes << 4) + category
                call encode_ac_symbol_stb_style(writer, ishft(nrzeroes, 4) + category, is_luma)
                call write_ac_value_bits_stb_style(writer, DU(i), category)
                i = i + 1
            end if
        end do
        
        if (end0pos /= 64) then
            ! Write EOB using correct table
            if (is_luma) then
                call stb_write_bits(writer, YAC_HT(1, 1), YAC_HT(1, 2))  ! Y EOB
            else
                call stb_write_bits(writer, UVAC_HT(1, 1), UVAC_HT(1, 2))  ! UV EOB
            end if
        end if
        
    end subroutine process_du_stb_style

    ! Legacy functions - replaced by STB-style implementation
    ! encode_ac_coefficient and encode_eob_marker are now in process_du_stb_style
    
    ! Helper functions for Huffman encoding
    
    function get_dc_category(dc_val) result(category)
        integer, intent(in) :: dc_val
        integer :: category
        
        integer :: abs_val
        
        abs_val = abs(dc_val)
        
        if (abs_val == 0) then
            category = 0
        else if (abs_val == 1) then
            category = 1
        else if (abs_val <= 3) then
            category = 2
        else if (abs_val <= 7) then
            category = 3
        else if (abs_val <= 15) then
            category = 4
        else if (abs_val <= 31) then
            category = 5
        else if (abs_val <= 63) then
            category = 6
        else if (abs_val <= 127) then
            category = 7
        else if (abs_val <= 255) then
            category = 8
        else if (abs_val <= 511) then
            category = 9
        else if (abs_val <= 1023) then
            category = 10
        else
            category = 11
        end if
    end function get_dc_category
    
    function get_ac_category(ac_val) result(category)
        integer, intent(in) :: ac_val
        integer :: category
        
        ! Same as DC category calculation
        category = get_dc_category(ac_val)
    end function get_ac_category
    
    ! Exact STB bit writing algorithm ported to Fortran
    subroutine stb_write_bits(writer, code, bits)
        type(bit_writer_t), intent(inout) :: writer
        integer, intent(in) :: code, bits
        
        integer :: c
        logical, save :: debug_trace = .false.
        
        if (debug_trace .and. bits > 0) then
            write(*, '("Writing bits: value=", I0, " (0x", Z0, ") bits=", I0, " binary=", B0)') &
                  code, code, bits, code
        end if
        
        ! STB algorithm: bitCnt += bs[1]
        writer%bit_count = writer%bit_count + bits
        
        ! STB algorithm: bitBuf |= bs[0] << (24 - bitCnt)
        writer%bit_buffer = ior(writer%bit_buffer, ishft(code, 24 - writer%bit_count))
        
        ! STB algorithm: while(bitCnt >= 8)
        do while (writer%bit_count >= 8)
            ! STB algorithm: unsigned char c = (bitBuf >> 16) & 255
            c = iand(ishft(writer%bit_buffer, -16), 255)
            
            ! Write byte to output
            call write_byte_to_output(writer, int(c, 1))
            
            ! STB algorithm: if(c == 255) stbiw__putc(s, 0)
            if (c == 255) then
                call write_byte_to_output(writer, 0_1)  ! Byte stuffing
            end if
            
            ! STB algorithm: bitBuf <<= 8; bitCnt -= 8
            writer%bit_buffer = ishft(writer%bit_buffer, 8)
            writer%bit_count = writer%bit_count - 8
        end do
    end subroutine stb_write_bits
    
    subroutine write_byte_to_output(writer, byte)
        type(bit_writer_t), intent(inout) :: writer
        integer(1), intent(in) :: byte
        
        ! Expand output buffer if needed
        if (writer%output_pos > size(writer%output)) then
            call expand_output_buffer(writer)
        end if
        
        writer%output(writer%output_pos) = byte
        writer%output_pos = writer%output_pos + 1
    end subroutine write_byte_to_output
    
    subroutine expand_output_buffer(writer)
        type(bit_writer_t), intent(inout) :: writer
        integer(1), allocatable :: temp(:)
        integer :: new_size
        
        new_size = size(writer%output) * 2
        allocate(temp(new_size))
        temp(1:size(writer%output)) = writer%output
        temp(size(writer%output)+1:) = 0_1
        
        deallocate(writer%output)
        allocate(writer%output(new_size))
        writer%output = temp
        deallocate(temp)
    end subroutine expand_output_buffer
    
    ! STB-style helper functions
    subroutine get_stb_quantization_tables(fdtbl_Y, fdtbl_UV, quality)
        real, intent(out) :: fdtbl_Y(64), fdtbl_UV(64)
        integer, intent(in) :: quality
        
        ! STB quantization tables in natural order (row-major)
        integer, parameter :: YQT(64) = [ &
            16, 11, 10, 16, 24, 40, 51, 61, &
            12, 12, 14, 19, 26, 58, 60, 55, &
            14, 13, 16, 24, 40, 57, 69, 56, &
            14, 17, 22, 29, 51, 87, 80, 62, &
            18, 22, 37, 56, 68,109,103, 77, &
            24, 35, 55, 64, 81,104,113, 92, &
            49, 64, 78, 87,103,121,120,101, &
            72, 92, 95, 98,112,100,103, 99]
        integer, parameter :: UVQT(64) = [ &
            17, 18, 24, 47, 99, 99, 99, 99, &
            18, 21, 26, 66, 99, 99, 99, 99, &
            24, 26, 56, 99, 99, 99, 99, 99, &
            47, 66, 99, 99, 99, 99, 99, 99, &
            99, 99, 99, 99, 99, 99, 99, 99, &
            99, 99, 99, 99, 99, 99, 99, 99, &
            99, 99, 99, 99, 99, 99, 99, 99, &
            99, 99, 99, 99, 99, 99, 99, 99]
        real, parameter :: aasf(8) = [1.0*2.828427125, 1.387039845*2.828427125, 1.306562965*2.828427125, 1.175875602*2.828427125, &
                                     1.0*2.828427125, 0.785694958*2.828427125, 0.541196100*2.828427125, 0.275899379*2.828427125]
        
        integer :: YTable(64), UVTable(64)
        integer :: i, row, col, k, yti, uvti, quality_scale
        
        ! STB quality scaling
        quality_scale = max(1, min(100, quality))
        if (quality_scale < 50) then
            quality_scale = 5000 / quality_scale
        else
            quality_scale = 200 - quality_scale * 2
        end if
        
        ! Scale quantization tables
        do i = 1, 64
            yti = (YQT(i) * quality_scale + 50) / 100
            YTable(i) = max(1, min(255, yti))
            uvti = (UVQT(i) * quality_scale + 50) / 100
            UVTable(i) = max(1, min(255, uvti))
        end do
        
        ! Apply STB zigzag and AAF scaling
        do row = 1, 8
            do col = 1, 8
                k = (row - 1) * 8 + col
                fdtbl_Y(k) = 1.0 / (YTable(k) * aasf(row) * aasf(col))
                fdtbl_UV(k) = 1.0 / (UVTable(k) * aasf(row) * aasf(col))
            end do
        end do
    end subroutine get_stb_quantization_tables
    
    subroutine apply_dct_and_quantize_stb_style(CDU, DU, fdtbl)
        real, intent(in) :: CDU(8, 8)
        integer, intent(out) :: DU(64)
        real, intent(in) :: fdtbl(64)
        
        real :: temp_cdu(8, 8)
        integer :: i, j, k
        ! STB zigzag order table (converted from 0-based to 1-based)
        integer, parameter :: zigzag(64) = [ &
            1,  2,  6,  7, 15, 16, 28, 29, &
            3,  5,  8, 14, 17, 27, 30, 43, &
            4,  9, 13, 18, 26, 31, 42, 44, &
           10, 12, 19, 25, 32, 41, 45, 54, &
           11, 20, 24, 33, 40, 46, 53, 55, &
           21, 23, 34, 39, 47, 52, 56, 61, &
           22, 35, 38, 48, 51, 57, 60, 62, &
           36, 37, 49, 50, 58, 59, 63, 64]
        
        ! Copy input and apply DCT
        temp_cdu = CDU
        call apply_stb_dct_8x8(temp_cdu)
        
        ! Quantize in natural order, then apply zigzag (like STB)
        ! Note: STB processes in row-major order (y*8+x), we need column-major for Fortran arrays
        do i = 1, 8
            do j = 1, 8
                k = (i-1)*8 + j  ! Natural order index (1-64)
                ! Quantize and store in zigzag position
                DU(zigzag(k)) = nint(temp_cdu(j, i) * fdtbl(k))
            end do
        end do
    end subroutine apply_dct_and_quantize_stb_style
    
    subroutine apply_stb_dct_8x8(block)
        real, intent(inout) :: block(8, 8)
        
        integer :: i
        
        ! Apply STB DCT to each row
        do i = 1, 8
            call stb_dct_exact(block(1,i), block(2,i), block(3,i), block(4,i), &
                              block(5,i), block(6,i), block(7,i), block(8,i))
        end do
        
        ! Apply STB DCT to each column  
        do i = 1, 8
            call stb_dct_exact(block(i,1), block(i,2), block(i,3), block(i,4), &
                              block(i,5), block(i,6), block(i,7), block(i,8))
        end do
    end subroutine apply_stb_dct_8x8
    
    ! Exact STB DCT implementation from stb_image_write.h
    subroutine stb_dct_exact(d0p, d1p, d2p, d3p, d4p, d5p, d6p, d7p)
        real, intent(inout) :: d0p, d1p, d2p, d3p, d4p, d5p, d6p, d7p
        
        real :: d0, d1, d2, d3, d4, d5, d6, d7
        real :: z1, z2, z3, z4, z5, z11, z13
        real :: tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7
        real :: tmp10, tmp11, tmp12, tmp13
        
        ! Copy inputs
        d0 = d0p; d1 = d1p; d2 = d2p; d3 = d3p
        d4 = d4p; d5 = d5p; d6 = d6p; d7 = d7p
        
        ! Butterfly additions/subtractions
        tmp0 = d0 + d7
        tmp7 = d0 - d7
        tmp1 = d1 + d6
        tmp6 = d1 - d6
        tmp2 = d2 + d5
        tmp5 = d2 - d5
        tmp3 = d3 + d4
        tmp4 = d3 - d4
        
        ! Even part
        tmp10 = tmp0 + tmp3   ! phase 2
        tmp13 = tmp0 - tmp3
        tmp11 = tmp1 + tmp2
        tmp12 = tmp1 - tmp2
        
        d0 = tmp10 + tmp11    ! phase 3
        d4 = tmp10 - tmp11
        
        z1 = (tmp12 + tmp13) * 0.707106781  ! c4
        d2 = tmp13 + z1      ! phase 5
        d6 = tmp13 - z1
        
        ! Odd part
        tmp10 = tmp4 + tmp5   ! phase 2
        tmp11 = tmp5 + tmp6
        tmp12 = tmp6 + tmp7
        
        ! The rotator is modified from fig 4-8 to avoid extra negations.
        z5 = (tmp10 - tmp12) * 0.382683433  ! c6
        z2 = tmp10 * 0.541196100 + z5       ! c2-c6
        z4 = tmp12 * 1.306562965 + z5       ! c2+c6
        z3 = tmp11 * 0.707106781            ! c4
        
        z11 = tmp7 + z3       ! phase 5
        z13 = tmp7 - z3
        
        d5p = z13 + z2        ! phase 6
        d3p = z13 - z2
        d1p = z11 + z4
        d7p = z11 - z4
        
        d0p = d0; d2p = d2; d4p = d4; d6p = d6
    end subroutine stb_dct_exact
    
    function get_uvdc_code(category) result(code)
        integer, intent(in) :: category
        integer :: code
        
        integer, parameter :: UVDC_CODES(12) = [0,1,2,6,14,30,62,126,254,510,1022,2046]
        
        if (category >= 1 .and. category <= 12) then
            code = UVDC_CODES(category)
        else
            code = 0
        end if
    end function get_uvdc_code
    
    function get_uvdc_bits(category) result(bits)
        integer, intent(in) :: category
        integer :: bits
        
        integer, parameter :: UVDC_BITS(12) = [2,2,2,3,4,5,6,7,8,9,10,11]
        
        if (category >= 1 .and. category <= 12) then
            bits = UVDC_BITS(category)
        else
            bits = 0
        end if
    end function get_uvdc_bits
    
    subroutine encode_ac_symbol_stb_style(writer, symbol, is_luma)
        type(bit_writer_t), intent(inout) :: writer
        integer, intent(in) :: symbol
        logical, intent(in) :: is_luma
        
        ! Use exact STB AC Huffman tables
        if (is_luma) then
            call encode_yac_symbol(writer, symbol)
        else
            call encode_uvac_symbol(writer, symbol)
        end if
    end subroutine encode_ac_symbol_stb_style
    
    subroutine encode_yac_symbol(writer, symbol)
        type(bit_writer_t), intent(inout) :: writer
        integer, intent(in) :: symbol
        
        ! Use the exported YAC_HT table that's already correct
        if (symbol >= 1 .and. symbol <= 256) then
            call stb_write_bits(writer, YAC_HT(symbol, 1), YAC_HT(symbol, 2))
        else
            ! Invalid symbol - default to EOB
            call stb_write_bits(writer, YAC_HT(1, 1), YAC_HT(1, 2))  ! EOB
        end if
    end subroutine encode_yac_symbol
    
    subroutine encode_uvac_symbol(writer, symbol)
        type(bit_writer_t), intent(inout) :: writer
        integer, intent(in) :: symbol
        
        ! Use the exported UVAC_HT table that's already correct
        if (symbol >= 1 .and. symbol <= 256) then
            call stb_write_bits(writer, UVAC_HT(symbol, 1), UVAC_HT(symbol, 2))
        else
            ! Invalid symbol - default to EOB
            call stb_write_bits(writer, UVAC_HT(1, 1), UVAC_HT(1, 2))  ! EOB
        end if
    end subroutine encode_uvac_symbol
    
    subroutine write_dc_value_bits_stb_style(writer, dc_val, category)
        type(bit_writer_t), intent(inout) :: writer
        integer, intent(in) :: dc_val, category
        
        integer :: value_bits
        
        if (dc_val >= 0) then
            value_bits = dc_val
        else
            value_bits = dc_val + (2**category - 1)
        end if
        
        call stb_write_bits(writer, value_bits, category)
    end subroutine write_dc_value_bits_stb_style
    
    subroutine write_ac_value_bits_stb_style(writer, ac_val, category)
        type(bit_writer_t), intent(inout) :: writer
        integer, intent(in) :: ac_val, category
        
        call write_dc_value_bits_stb_style(writer, ac_val, category)
    end subroutine write_ac_value_bits_stb_style
    
    subroutine extract_subsampled_8x8_block(component_data, width, height, start_x, start_y, block)
        real, intent(in) :: component_data(:,:)
        integer, intent(in) :: width, height, start_x, start_y
        real, intent(out) :: block(8, 8)
        
        integer :: x, y, src_x, src_y
        real :: p1, p2, p3, p4
        
        ! STB-style 4-pixel averaging for subsampling
        do y = 1, 8
            do x = 1, 8
                ! Get 2x2 block of pixels and average them like STB
                src_x = start_x + (x - 1) * 2
                src_y = start_y + (y - 1) * 2
                
                ! Get 4 pixels with boundary clamping
                p1 = component_data(min(src_x, width), min(src_y, height))
                p2 = component_data(min(src_x + 1, width), min(src_y, height))
                p3 = component_data(min(src_x, width), min(src_y + 1, height))
                p4 = component_data(min(src_x + 1, width), min(src_y + 1, height))
                
                ! STB: subU[pos] = (U[j+0] + U[j+1] + U[j+16] + U[j+17]) * 0.25f
                block(x, y) = (p1 + p2 + p3 + p4) * 0.25
            end do
        end do
    end subroutine extract_subsampled_8x8_block
    
    subroutine extract_and_subsample_stb_style(component_data, width, height, start_x, start_y, subsampled_block)
        real, intent(in) :: component_data(:,:)
        integer, intent(in) :: width, height, start_x, start_y
        real, intent(out) :: subsampled_block(8, 8)
        
        real :: full_16x16(256)  ! STB uses linear array
        integer :: x, y, pos, src_x, src_y
        integer :: yy, xx, j
        
        ! First extract 16x16 block exactly like STB
        pos = 1
        do y = 0, 15
            do x = 0, 15
                src_y = min(start_y + y, height)
                src_x = min(start_x + x, width)
                full_16x16(pos) = component_data(src_x, src_y)
                pos = pos + 1
            end do
        end do
        
        ! Then subsample exactly like STB: subU[pos] = (U[j+0] + U[j+1] + U[j+16] + U[j+17]) * 0.25f
        pos = 1
        do yy = 0, 7
            do xx = 0, 7
                j = yy*32 + xx*2 + 1  ! +1 for Fortran 1-based indexing
                subsampled_block(xx+1, yy+1) = (full_16x16(j) + full_16x16(j+1) + full_16x16(j+16) + full_16x16(j+17)) * 0.25
                pos = pos + 1
            end do
        end do
    end subroutine extract_and_subsample_stb_style
    
    subroutine process_y_blocks_stb_exact(writer, y_data, width, height, start_x, start_y, fdtbl, DCY)
        type(bit_writer_t), intent(inout) :: writer
        real, intent(in) :: y_data(:,:)
        integer, intent(in) :: width, height, start_x, start_y
        real, intent(in) :: fdtbl(64)
        integer, intent(inout) :: DCY
        
        real :: Y_16x16(256)
        real :: block_8x8(8, 8)
        integer :: row, col, pos, src_x, src_y
        integer :: i, j
        
        ! Extract 16x16 Y block exactly like STB
        pos = 1
        do row = 0, 15
            src_y = start_y + row
            if (src_y > height) src_y = height  ! Clamp like STB
            do col = 0, 15
                src_x = start_x + col
                if (src_x > width) src_x = width  ! Clamp like STB
                Y_16x16(pos) = y_data(src_x, src_y)
                pos = pos + 1
            end do
        end do
        
        ! Process 4 8x8 blocks exactly like STB: Y+0, Y+8, Y+128, Y+136
        ! Top-left (Y+0)
        do i = 1, 8
            do j = 1, 8
                pos = (i-1)*16 + j
                block_8x8(j, i) = Y_16x16(pos)
            end do
        end do
        call process_du_stb_style(writer, block_8x8, 8, fdtbl, DCY, .true.)
        
        ! Top-right (Y+8)
        do i = 1, 8
            do j = 1, 8
                pos = (i-1)*16 + j + 8
                block_8x8(j, i) = Y_16x16(pos)
            end do
        end do
        call process_du_stb_style(writer, block_8x8, 8, fdtbl, DCY, .true.)
        
        ! Bottom-left (Y+128)
        do i = 1, 8
            do j = 1, 8
                pos = (i+7)*16 + j
                block_8x8(j, i) = Y_16x16(pos)
            end do
        end do
        call process_du_stb_style(writer, block_8x8, 8, fdtbl, DCY, .true.)
        
        ! Bottom-right (Y+136)
        do i = 1, 8
            do j = 1, 8
                pos = (i+7)*16 + j + 8
                block_8x8(j, i) = Y_16x16(pos)
            end do
        end do
        call process_du_stb_style(writer, block_8x8, 8, fdtbl, DCY, .true.)
    end subroutine process_y_blocks_stb_exact
    
    subroutine extract_16x16_y_block(y_data, width, height, start_x, start_y, y_block)
        real, intent(in) :: y_data(:,:)
        integer, intent(in) :: width, height, start_x, start_y
        real, intent(out) :: y_block(256)  ! 16x16 = 256
        
        integer :: x, y, pos, src_x, src_y
        
        ! Extract 16x16 block like STB does
        pos = 1
        do y = 0, 15
            do x = 0, 15
                ! Clamp coordinates like STB
                src_y = min(start_y + y, height)
                src_x = min(start_x + x, width)
                y_block(pos) = y_data(src_x, src_y)
                pos = pos + 1
            end do
        end do
    end subroutine extract_16x16_y_block
    
    subroutine process_du_stb_style_16(writer, y_data, offset, stride, fdtbl, DC, is_luma)
        type(bit_writer_t), intent(inout) :: writer
        real, intent(in) :: y_data(:)
        integer, intent(in) :: offset, stride
        real, intent(in) :: fdtbl(64)
        integer, intent(inout) :: DC
        logical, intent(in) :: is_luma
        
        real :: cdu_8x8(8, 8)
        integer :: x, y, src_pos
        
        ! Extract 8x8 block from 16x16 data with given offset and stride
        do y = 1, 8
            do x = 1, 8
                src_pos = offset + (y-1)*stride + (x-1)
                cdu_8x8(x, y) = y_data(src_pos)
            end do
        end do
        
        ! Process this 8x8 block
        call process_du_stb_style(writer, cdu_8x8, 8, fdtbl, DC, is_luma)
    end subroutine process_du_stb_style_16
    
    subroutine write_dc_value_bits(buffer, pos, dc_val, category)
        integer, intent(inout) :: buffer(:)
        integer, intent(inout) :: pos
        integer, intent(in) :: dc_val, category
        
        integer :: value_bits
        
        ! Convert DC value to JPEG representation
        if (dc_val >= 0) then
            value_bits = dc_val
        else
            value_bits = dc_val + (2**category - 1)
        end if
        
        ! Store value bits (simplified)
        buffer(pos) = iand(value_bits, 255)
        pos = pos + 1
    end subroutine write_dc_value_bits
    
    subroutine write_ac_value_bits(buffer, pos, ac_val, category)
        integer, intent(inout) :: buffer(:)
        integer, intent(inout) :: pos
        integer, intent(in) :: ac_val, category
        
        ! Same as DC value bits
        call write_dc_value_bits(buffer, pos, ac_val, category)
    end subroutine write_ac_value_bits

    subroutine pack_scan_data(scan_buffer, size, compressed_data)
        integer, intent(in) :: scan_buffer(:)
        integer, intent(in) :: size
        integer(1), intent(out) :: compressed_data(:)
        
        integer :: i
        
        do i = 1, size
            compressed_data(i) = int(iand(scan_buffer(i), 255), 1)
        end do
    end subroutine pack_scan_data

end module fortplot_jpeg
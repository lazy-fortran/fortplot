program debug_scan_start
    use iso_c_binding
    use fortplot_jpeg
    implicit none
    
    interface
        function stbi_write_jpg(filename, w, h, comp, data, quality) &
                bind(C, name="stbi_write_jpg")
            import :: c_char, c_int, c_ptr
            character(kind=c_char), intent(in) :: filename(*)
            integer(c_int), value :: w, h, comp, quality
            type(c_ptr), value :: data
            integer(c_int) :: stbi_write_jpg
        end function
    end interface
    
    ! Test with minimal 8x8 solid color to isolate encoding differences
    integer, parameter :: width = 8, height = 8
    integer(1), target :: image_data(width * height * 3)
    integer(1), allocatable :: our_data(:), stb_data(:)
    integer :: i, result, our_size, stb_size, sos_pos
    character(len=*), parameter :: our_file = 'scan_our.jpg'
    character(len=*), parameter :: stb_file = 'scan_stb.jpg'
    
    print *, "Scan Data Start Comparison (8x8 solid color)"
    print *, "============================================="
    
    ! Create solid red image to minimize encoding complexity
    do i = 1, size(image_data), 3
        image_data(i) = int(127, 1)   ! Red = 127 (safe for signed int8)
        image_data(i+1) = int(0, 1)   ! Green = 0
        image_data(i+2) = int(0, 1)   ! Blue = 0
    end do
    
    ! Generate both JPEGs
    call write_jpeg_file(our_file, width, height, image_data, 95)
    result = stbi_write_jpg(stb_file//c_null_char, width, height, 3, c_loc(image_data), 95)
    
    if (result == 0) then
        print *, "STB write failed"
        stop 1
    end if
    
    ! Read both files
    call read_file(our_file, our_data, our_size)
    call read_file(stb_file, stb_data, stb_size)
    
    print *, "File sizes: Our=", our_size, ", STB=", stb_size
    
    ! Find SOS marker in STB file to locate scan data start
    sos_pos = find_sos_marker(stb_data)
    if (sos_pos > 0) then
        print *, "SOS marker found at byte", sos_pos
        print *, "Scan data starts around byte", sos_pos + 12
        
        ! Compare first 50 bytes of scan data
        call compare_scan_data(our_data, stb_data, sos_pos + 12, 50)
    else
        print *, "Could not find SOS marker"
    end if
    
contains

    subroutine read_file(filename, data, size)
        character(len=*), intent(in) :: filename
        integer(1), allocatable, intent(out) :: data(:)
        integer, intent(out) :: size
        integer :: unit
        
        open(newunit=unit, file=filename, access='stream', form='unformatted', status='old')
        inquire(unit=unit, size=size)
        allocate(data(size))
        read(unit) data
        close(unit)
    end subroutine read_file
    
    function find_sos_marker(data) result(pos)
        integer(1), intent(in) :: data(:)
        integer :: pos, i
        
        pos = 0
        do i = 1, size(data)-1
            if (data(i) == int(z'FF', 1) .and. data(i+1) == int(z'DA', 1)) then
                pos = i
                return
            end if
        end do
    end function find_sos_marker
    
    subroutine compare_scan_data(our_data, stb_data, start_pos, num_bytes)
        integer(1), intent(in) :: our_data(:), stb_data(:)
        integer, intent(in) :: start_pos, num_bytes
        integer :: i, our_pos, stb_pos
        logical :: match
        
        print *, "Comparing first", num_bytes, "bytes of scan data:"
        
        ! Find corresponding position in our data
        our_pos = find_sos_marker(our_data)
        if (our_pos == 0) then
            print *, "Could not find SOS in our file"
            return
        end if
        our_pos = our_pos + 12
        
        match = .true.
        do i = 0, num_bytes-1
            if (our_pos + i <= size(our_data) .and. start_pos + i <= size(stb_data)) then
                if (our_data(our_pos + i) /= stb_data(start_pos + i)) then
                    print *, "First difference at scan offset", i
                    print *, "Our:", int(our_data(our_pos + i)), "(0x" // byte_to_hex(our_data(our_pos + i)) // ")"
                    print *, "STB:", int(stb_data(start_pos + i)), "(0x" // byte_to_hex(stb_data(start_pos + i)) // ")"
                    match = .false.
                    exit
                end if
            end if
        end do
        
        if (match) then
            print *, "First", num_bytes, "bytes match!"
        end if
    end subroutine compare_scan_data
    
    function byte_to_hex(b) result(hex_str)
        integer(1), intent(in) :: b
        character(len=2) :: hex_str
        write(hex_str, '(Z2.2)') b
    end function byte_to_hex

end program debug_scan_start
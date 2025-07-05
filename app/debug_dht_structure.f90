program debug_dht_structure
    use, intrinsic :: iso_fortran_env, only: int8
    implicit none
    
    integer(1), allocatable :: stb_data(:), our_data(:)
    integer :: i, pos
    integer :: table_count, bits_sum, table_id
    
    call read_binary_file("stb_minimal.jpg", stb_data)
    call read_binary_file("our_minimal.jpg", our_data)
    
    print *, "Analyzing DHT structure..."
    print *
    
    ! Find DHT marker in STB
    do i = 1, size(stb_data)-1
        if (stb_data(i) == int(-1, int8) .and. stb_data(i+1) == int(-60, int8)) then  ! 0xFF 0xC4
            print '(A,I0,A,Z4.4)', "STB DHT at position ", i, ", length = 0x", &
                ishft(iand(int(stb_data(i+2)), 255), 8) + iand(int(stb_data(i+3)), 255)
            pos = i + 4
            call analyze_dht_tables(stb_data, pos, "STB")
            exit
        end if
    end do
    
    print *
    
    ! Find DHT marker in our file
    do i = 1, size(our_data)-1
        if (our_data(i) == int(-1, int8) .and. our_data(i+1) == int(-60, int8)) then  ! 0xFF 0xC4
            print '(A,I0,A,Z4.4)', "Our DHT at position ", i, ", length = 0x", &
                ishft(iand(int(our_data(i+2)), 255), 8) + iand(int(our_data(i+3)), 255)
            pos = i + 4
            call analyze_dht_tables(our_data, pos, "Our")
            exit
        end if
    end do
    
contains

    subroutine analyze_dht_tables(data, start_pos, label)
        integer(1), intent(in) :: data(:)
        integer, intent(inout) :: start_pos
        character(*), intent(in) :: label
        
        integer :: pos, i, table_info, table_type, table_id
        integer :: bits_count, total_bytes
        integer(1) :: bits(16)
        
        pos = start_pos
        total_bytes = 0
        
        do while (pos < size(data)-20)
            ! Check if we hit another marker
            if (data(pos) == int(-1, int8)) exit
            
            ! Read table info
            table_info = iand(int(data(pos)), 255)
            table_type = ishft(table_info, -4)  ! Upper 4 bits
            table_id = iand(table_info, 15)     ! Lower 4 bits
            
            print '(A,A,I0,A,I0)', label, " Table: Type=", table_type, " ID=", table_id
            
            ! Read 16 bytes of bit counts
            bits = data(pos+1:pos+16)
            bits_count = 0
            do i = 1, 16
                bits_count = bits_count + iand(int(bits(i)), 255)
            end do
            
            print '(A,I0,A)', "  Bit counts sum: ", bits_count, " values"
            print '(A,I0,A)', "  Table size: ", 1 + 16 + bits_count, " bytes"
            
            total_bytes = total_bytes + 1 + 16 + bits_count
            pos = pos + 1 + 16 + bits_count
        end do
        
        print '(A,A,I0,A)', label, " total DHT bytes: ", total_bytes, " (excluding length field)"
        start_pos = pos
        
    end subroutine
    
    subroutine read_binary_file(filename, data)
        character(*), intent(in) :: filename
        integer(1), allocatable, intent(out) :: data(:)
        integer :: unit, filesize, iostat
        
        open(newunit=unit, file=filename, form='unformatted', access='stream', &
             status='old', action='read')
        inquire(unit, size=filesize)
        allocate(data(filesize))
        read(unit, iostat=iostat) data
        close(unit)
    end subroutine
    
end program debug_dht_structure
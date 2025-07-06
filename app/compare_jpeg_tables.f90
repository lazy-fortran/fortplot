program compare_jpeg_tables
    implicit none
    
    integer :: stb_unit, our_unit, ios
    integer :: pos
    integer(1) :: b1, b2
    integer(2) :: marker_val, length
    integer(1), allocatable :: stb_data(:), our_data(:)
    logical :: found_dqt = .false.
    logical :: found_dht = .false.
    
    ! Open files
    open(newunit=stb_unit, file='stb_large_complex.jpg', &
         form='unformatted', access='stream', status='old', iostat=ios)
    if (ios /= 0) stop "Error opening STB file"
    
    open(newunit=our_unit, file='our_large_complex.jpg', &
         form='unformatted', access='stream', status='old', iostat=ios)
    if (ios /= 0) stop "Error opening our file"
    
    print *, "Comparing JPEG quantization and Huffman tables..."
    print *, ""
    
    ! Scan for markers
    pos = 1
    do
        read(stb_unit, iostat=ios) b1
        if (ios /= 0) exit
        
        if (b1 == int(-1, 1)) then  ! 0xFF
            read(stb_unit, iostat=ios) b2
            if (ios /= 0) exit
            
            marker_val = ior(ishft(int(b1, 2), 8), int(b2, 2))
            
            select case (int(b2))
            case (int(z'DB'))  ! DQT marker
                print '(A,I0,A,Z4.4)', "DQT marker at position ", pos, ": 0x", marker_val
                call compare_dqt_tables(stb_unit, our_unit, pos)
                found_dqt = .true.
                
            case (int(z'C4'))  ! DHT marker
                print '(A,I0,A,Z4.4)', "DHT marker at position ", pos, ": 0x", marker_val
                call compare_dht_tables(stb_unit, our_unit, pos)
                found_dht = .true.
                
            case (int(z'DA'))  ! SOS marker
                print '(A,I0)', "SOS marker at position ", pos
                exit  ! Stop at SOS
            end select
            
            pos = pos + 1
        end if
        pos = pos + 1
    end do
    
    close(stb_unit)
    close(our_unit)
    
contains
    
    subroutine compare_dqt_tables(stb_u, our_u, marker_pos)
        integer, intent(in) :: stb_u, our_u, marker_pos
        integer(1) :: stb_len_bytes(2), our_len_bytes(2)
        integer :: table_length, i
        integer(1), allocatable :: stb_table(:), our_table(:)
        
        ! Read length
        read(stb_u) stb_len_bytes
        read(our_u, pos=marker_pos+2) our_len_bytes
        
        table_length = ior(ishft(int(stb_len_bytes(1)), 8), int(stb_len_bytes(2))) - 2
        
        allocate(stb_table(table_length), our_table(table_length))
        
        read(stb_u) stb_table
        read(our_u, pos=marker_pos+4) our_table
        
        ! Compare tables
        print '(A,I0)', "  Table length: ", table_length
        
        do i = 1, min(table_length, 10)
            if (stb_table(i) /= our_table(i)) then
                print '(A,I3,A,I3,A,I3)', "  Byte ", i, ": STB=", stb_table(i), " OUR=", our_table(i)
            end if
        end do
        
        if (all(stb_table == our_table)) then
            print *, "  Tables are IDENTICAL"
        else
            print *, "  Tables DIFFER"
        end if
        print *, ""
        
        deallocate(stb_table, our_table)
    end subroutine compare_dqt_tables
    
    subroutine compare_dht_tables(stb_u, our_u, marker_pos)
        integer, intent(in) :: stb_u, our_u, marker_pos
        integer(1) :: stb_len_bytes(2), our_len_bytes(2)
        integer :: table_length, i
        integer(1), allocatable :: stb_table(:), our_table(:)
        integer(1) :: table_info
        
        ! Read length
        read(stb_u) stb_len_bytes
        read(our_u, pos=marker_pos+2) our_len_bytes
        
        table_length = ior(ishft(int(stb_len_bytes(1)), 8), int(stb_len_bytes(2))) - 2
        
        allocate(stb_table(table_length), our_table(table_length))
        
        read(stb_u) stb_table
        read(our_u, pos=marker_pos+4) our_table
        
        ! Parse table info
        table_info = stb_table(1)
        print '(A,Z2.2)', "  Table class/ID: 0x", table_info
        print '(A,I0,A,I0)', "  Table class: ", ishft(int(table_info), -4), &
              " (0=DC, 1=AC), Table ID: ", iand(int(table_info), 15)
        
        ! Compare first few bytes
        print '(A,I0)', "  Table length: ", table_length
        
        do i = 1, min(table_length, 20)
            if (stb_table(i) /= our_table(i)) then
                print '(A,I3,A,Z2.2,A,Z2.2)', "  Byte ", i, ": STB=0x", stb_table(i), " OUR=0x", our_table(i)
            end if
        end do
        
        if (all(stb_table == our_table)) then
            print *, "  Tables are IDENTICAL"
        else
            print *, "  Tables DIFFER"
            ! Count differences
            print '(A,I0,A,I0)', "  Total differences: ", &
                  count(stb_table /= our_table), " out of ", table_length
        end if
        print *, ""
        
        deallocate(stb_table, our_table)
    end subroutine compare_dht_tables
    
end program compare_jpeg_tables
program analyze_jpeg_scan_data
    implicit none
    
    integer :: stb_unit, our_unit, ios
    integer :: i, pos
    integer(1) :: stb_byte, our_byte
    logical :: found_sos = .false.
    integer :: sos_pos = 0
    integer :: diff_count = 0
    integer(1), dimension(2) :: marker
    
    ! Open files
    open(newunit=stb_unit, file='stb_large_complex.jpg', &
         form='unformatted', access='stream', status='old', iostat=ios)
    if (ios /= 0) then
        print *, "Error opening STB file"
        stop
    end if
    
    open(newunit=our_unit, file='our_large_complex.jpg', &
         form='unformatted', access='stream', status='old', iostat=ios)
    if (ios /= 0) then
        print *, "Error opening our file"
        stop
    end if
    
    ! Find SOS marker (0xFF 0xDA)
    pos = 1
    do
        read(stb_unit, iostat=ios) marker(1)
        if (ios /= 0) exit
        
        if (marker(1) == int(-1, 1)) then  ! 0xFF
            read(stb_unit, iostat=ios) marker(2)
            if (ios /= 0) exit
            
            if (marker(2) == int(-38, 1)) then  ! 0xDA
                found_sos = .true.
                sos_pos = pos
                print '(A,I0)', "Found SOS marker at position: ", pos
                exit
            end if
            pos = pos + 1
        end if
        pos = pos + 1
    end do
    
    if (.not. found_sos) then
        print *, "SOS marker not found!"
        stop
    end if
    
    ! Read SOS header (12 bytes after marker)
    print *, "SOS Header:"
    do i = 1, 12
        read(stb_unit, iostat=ios) stb_byte
        read(our_unit, pos=sos_pos+1+i, iostat=ios) our_byte
        if (stb_byte /= our_byte) then
            print '(A,I3,A,Z2.2,A,Z2.2)', "  Byte ", i, ": STB=0x", stb_byte, " OUR=0x", our_byte
        end if
    end do
    
    ! Now analyze scan data starting after SOS header
    print *, ""
    print *, "Scan data analysis (first 100 bytes):"
    print *, "Position  STB   OUR   Binary(STB)      Binary(OUR)      Interpretation"
    print *, "--------  ----  ----  --------------   --------------   --------------"
    
    do i = 1, 100
        read(stb_unit, iostat=ios) stb_byte
        if (ios /= 0) exit
        
        read(our_unit, pos=sos_pos+13+i, iostat=ios) our_byte
        if (ios /= 0) exit
        
        if (stb_byte /= our_byte) then
            diff_count = diff_count + 1
            if (diff_count <= 20) then  ! Show first 20 differences
                print '(I8,2X,Z2.2,4X,Z2.2,4X,B8.8,3X,B8.8,3X,A)', &
                    sos_pos+13+i, stb_byte, our_byte, stb_byte, our_byte, &
                    analyze_byte_context(i, stb_byte, our_byte)
            end if
        end if
    end do
    
    print *, ""
    print '(A,I0)', "Total differences in first 100 scan bytes: ", diff_count
    
    ! Close files
    close(stb_unit)
    close(our_unit)
    
contains
    
    function analyze_byte_context(pos, stb_val, our_val) result(interpretation)
        integer, intent(in) :: pos
        integer(1), intent(in) :: stb_val, our_val
        character(len=100) :: interpretation
        
        if (pos == 1) then
            ! First scan byte after SOS
            write(interpretation, '(A,I0,A,I0)') "First scan byte: STB=", stb_val, " OUR=", our_val
        else if (stb_val == int(-1, 1) .and. our_val /= int(-1, 1)) then
            interpretation = "STB has 0xFF marker, OUR doesn't"
        else if (stb_val /= int(-1, 1) .and. our_val == int(-1, 1)) then
            interpretation = "OUR has 0xFF marker, STB doesn't"
        else if (abs(int(stb_val) - int(our_val)) <= 2) then
            interpretation = "Small difference (rounding?)"
        else
            interpretation = "Significant difference"
        end if
        
    end function analyze_byte_context
    
end program analyze_jpeg_scan_data
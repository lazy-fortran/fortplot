program analyze_jpeg_structure
    implicit none
    
    integer(1), allocatable :: data(:)
    integer :: unit, size, i
    character(len=*), parameter :: filename = 'debug_stb_16x16.jpg'
    
    ! Read STB file to analyze structure
    open(newunit=unit, file=filename, access='stream', form='unformatted', status='old')
    inquire(unit=unit, size=size)
    allocate(data(size))
    read(unit) data
    close(unit)
    
    print *, "JPEG Structure Analysis for ", filename
    print *, "File size:", size, "bytes"
    print *, "================================"
    
    ! Find all JPEG markers
    i = 1
    do while (i < size)
        if (data(i) == int(z'FF', 1)) then
            if (i < size) then
                select case (data(i+1))
                case (int(z'D8', 1))
                    print *, "SOI (Start of Image) at byte", i
                case (int(z'E0', 1))
                    print *, "APP0 marker at byte", i
                case (int(z'DB', 1))
                    print *, "DQT (Quantization Table) at byte", i
                case (int(z'C0', 1))
                    print *, "SOF0 (Start of Frame) at byte", i
                case (int(z'C4', 1))
                    print *, "DHT (Huffman Table) at byte", i
                case (int(z'DA', 1))
                    print *, "SOS (Start of Scan) at byte", i
                    print *, "Scan data begins around byte", i+12, "(approximate)"
                    exit  ! Scan data follows, no more markers until EOI
                case (int(z'D9', 1))
                    print *, "EOI (End of Image) at byte", i
                case default
                    ! Skip unknown markers
                end select
            end if
        end if
        i = i + 1
    end do
    
    print *, "Divergence occurs at byte 620, which is in the scan data section"
    
end program analyze_jpeg_structure
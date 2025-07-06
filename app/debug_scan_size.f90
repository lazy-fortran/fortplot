program debug_scan_size
    use fortplot_jpeg
    implicit none
    
    integer :: header_size, scan_start, scan_end
    integer :: stb_size, our_size
    integer(1), allocatable :: stb_data(:), our_data(:)
    integer :: unit, i
    
    ! Read STB file
    open(newunit=unit, file="stb_large_complex.jpg", access='stream', form='unformatted', status='old')
    inquire(unit=unit, size=stb_size)
    allocate(stb_data(stb_size))
    read(unit) stb_data
    close(unit)
    
    ! Read our file
    open(newunit=unit, file="our_large_complex.jpg", access='stream', form='unformatted', status='old')
    inquire(unit=unit, size=our_size)
    allocate(our_data(our_size))
    read(unit) our_data
    close(unit)
    
    print *, "File sizes: STB =", stb_size, "Our =", our_size
    print *, "Difference:", stb_size - our_size, "bytes"
    
    ! Find SOS marker (FF DA)
    do i = 1, min(stb_size, our_size) - 1
        if (stb_data(i) == int(Z'FF', 1) .and. stb_data(i+1) == int(Z'DA', 1)) then
            scan_start = i
            print *, "SOS marker at byte", i
            exit
        end if
    end do
    
    ! Find EOI marker (FF D9)
    do i = stb_size - 1, 1, -1
        if (stb_data(i) == int(Z'FF', 1) .and. stb_data(i+1) == int(Z'D9', 1)) then
            scan_end = i
            print *, "STB EOI marker at byte", i
            exit
        end if
    end do
    
    print *, "STB scan data size:", scan_end - scan_start - 14  ! 14 bytes for SOS header
    
    ! Find our EOI
    do i = our_size - 1, 1, -1
        if (our_data(i) == int(Z'FF', 1) .and. our_data(i+1) == int(Z'D9', 1)) then
            scan_end = i
            print *, "Our EOI marker at byte", i
            exit
        end if
    end do
    
    print *, "Our scan data size:", scan_end - scan_start - 14
    
    ! Check headers are identical
    print *, ""
    print *, "Header comparison (first", scan_start, "bytes):"
    do i = 1, scan_start
        if (stb_data(i) /= our_data(i)) then
            print '("  Difference at byte ", I0, ": STB=", Z2.2, " Our=", Z2.2)', &
                  i, stb_data(i), our_data(i)
        end if
    end do
    
end program
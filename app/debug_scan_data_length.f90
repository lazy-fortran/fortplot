program debug_scan_data_length
    use fortplot_jpeg
    use iso_fortran_env, only: int8
    implicit none
    
    integer(1) :: image_data(64 * 64 * 3)
    integer(1), allocatable :: jpeg_data(:)
    integer :: i, sos_pos, eoi_pos
    
    ! Create simple test pattern
    image_data = 127_int8
    
    ! Generate JPEG
    call get_jpeg_data(64, 64, image_data, 85, jpeg_data)
    
    print *, "Total JPEG size:", size(jpeg_data), "bytes"
    
    ! Find SOS marker
    sos_pos = 0
    do i = 1, size(jpeg_data) - 1
        if (jpeg_data(i) == int(Z'FF', 1) .and. jpeg_data(i+1) == int(Z'DA', 1)) then
            sos_pos = i
            exit
        end if
    end do
    
    ! Find EOI marker
    eoi_pos = 0
    do i = 1, size(jpeg_data) - 1
        if (jpeg_data(i) == int(Z'FF', 1) .and. jpeg_data(i+1) == int(Z'D9', 1)) then
            eoi_pos = i
            exit
        end if
    end do
    
    if (sos_pos > 0 .and. eoi_pos > 0) then
        print *, "SOS marker at:", sos_pos
        print *, "EOI marker at:", eoi_pos
        print *, "Scan data length:", eoi_pos - (sos_pos + 14), "bytes"
        print *, "(SOS header is 14 bytes)"
    else
        print *, "Could not find markers"
    end if
    
    ! Show last few bytes before EOI
    if (eoi_pos > 10) then
        print *, "Last 10 bytes before EOI:"
        print '(10(Z2.2, " "))', jpeg_data(eoi_pos-10:eoi_pos-1)
    end if
    
end program debug_scan_data_length
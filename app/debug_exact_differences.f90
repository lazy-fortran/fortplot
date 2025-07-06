program debug_exact_differences
    use fortplot_jpeg
    use, intrinsic :: iso_fortran_env, only: int8
    implicit none
    
    integer(1) :: pixels(3, 16, 16)  ! RGB image data
    integer :: x, y
    integer(1), parameter :: white = -1_int8  ! 255 as signed int8
    integer(1), parameter :: black = 0_int8
    integer(1), parameter :: gray = 127_int8
    
    ! Create identical test pattern
    do y = 1, 16
        do x = 1, 16
            if (y <= 8) then
                if (x <= 8) then
                    pixels(:, x, y) = white
                else
                    pixels(:, x, y) = black
                end if
            else
                if (x <= 8) then
                    pixels(1, x, y) = white
                    pixels(2:3, x, y) = black
                else
                    pixels(:, x, y) = gray
                end if
            end if
        end do
    end do
    
    ! Generate our JPEG
    call write_jpeg_file("debug_ours.jpg", 16, 16, reshape(pixels, [3*16*16]), 95)
    
    ! Compare with STB
    call compare_scan_data("debug_ours.jpg", "stb_pattern_q95.jpg")
    
contains
    
    subroutine compare_scan_data(file1, file2)
        character(*), intent(in) :: file1, file2
        integer, parameter :: max_size = 10000
        integer(1) :: data1(max_size), data2(max_size)
        integer :: size1, size2, i, diff_count
        integer :: scan_start1, scan_start2
        
        call read_file(file1, data1, size1)
        call read_file(file2, data2, size2)
        
        print *, "File sizes:", size1, size2
        
        ! Find scan data start (after SOS marker 0xFF 0xDA)
        scan_start1 = find_scan_start(data1, size1)
        scan_start2 = find_scan_start(data2, size2)
        
        print *, "Scan starts at:", scan_start1, scan_start2
        
        ! Compare scan data byte by byte
        diff_count = 0
        do i = 1, min(size1 - scan_start1, size2 - scan_start2)
            if (data1(scan_start1 + i) /= data2(scan_start2 + i)) then
                diff_count = diff_count + 1
                if (diff_count <= 10) then
                    print '("Diff at offset ", I0, ": ours=0x", Z2.2, " stb=0x", Z2.2)', &
                          i, data1(scan_start1 + i), data2(scan_start2 + i)
                end if
            end if
        end do
        
        print *, "Total differences in scan data:", diff_count
    end subroutine
    
    function find_scan_start(data, size) result(pos)
        integer(1), intent(in) :: data(:)
        integer, intent(in) :: size
        integer :: pos, i
        
        pos = 0
        do i = 1, size - 1
            if (data(i) == -1 .and. data(i+1) == -38) then  ! 0xFF 0xDA
                pos = i + 12  ! Skip SOS header (12 bytes)
                exit
            end if
        end do
    end function
    
    subroutine read_file(filename, data, size)
        character(*), intent(in) :: filename
        integer(1), intent(out) :: data(:)
        integer, intent(out) :: size
        integer :: unit, iostat
        
        open(newunit=unit, file=filename, access='stream', form='unformatted', status='old')
        size = 0
        do
            read(unit, iostat=iostat) data(size + 1)
            if (iostat /= 0) exit
            size = size + 1
        end do
        close(unit)
    end subroutine
    
end program
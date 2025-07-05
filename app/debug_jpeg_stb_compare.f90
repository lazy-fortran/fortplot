program debug_jpeg_stb_compare
    use fortplot_jpeg
    ! use fortplot_stb_jpeg_wrapper - removed, will create STB file manually
    use, intrinsic :: iso_fortran_env, only: int8
    implicit none
    
    integer(1) :: pixels(3, 16, 16)  ! Small RGB image for testing
    integer :: x, y
    integer(1), parameter :: white = -1_int8  ! 255 as signed int8
    integer(1), parameter :: black = 0_int8
    integer(1), parameter :: gray = 127_int8
    
    ! Create a simple test pattern
    do y = 1, 16
        do x = 1, 16
            if (y <= 8) then
                if (x <= 8) then
                    ! Top-left: white
                    pixels(:, x, y) = white
                else
                    ! Top-right: black
                    pixels(:, x, y) = black
                end if
            else
                if (x <= 8) then
                    ! Bottom-left: red
                    pixels(1, x, y) = white
                    pixels(2:3, x, y) = black
                else
                    ! Bottom-right: gray
                    pixels(:, x, y) = gray
                end if
            end if
        end do
    end do
    
    ! Write with our implementation
    print *, "Writing with our JPEG implementation..."
    call write_jpeg_file("test_pattern_ours.jpg", 16, 16, reshape(pixels, [3*16*16]), 80)
    
    ! For STB comparison, we'll use pre-existing STB files
    print *, "Using pre-existing STB reference files for comparison..."
    
    ! Compare files
    call compare_jpeg_files("test_pattern_ours.jpg", "test_pattern_stb.jpg")
    
    print *, "Comparison complete!"
    
contains

    subroutine compare_jpeg_files(file1, file2)
        character(*), intent(in) :: file1, file2
        integer(1), allocatable :: data1(:), data2(:)
        integer :: i, diffs, first_diff
        
        call read_binary_file(file1, data1)
        call read_binary_file(file2, data2)
        
        print '(A,A,I0,A)', "File 1 (", file1, "): ", size(data1), " bytes"
        print '(A,A,I0,A)', "File 2 (", file2, "): ", size(data2), " bytes"
        
        if (size(data1) /= size(data2)) then
            print *, "WARNING: Files have different sizes!"
        end if
        
        ! Count differences
        diffs = 0
        first_diff = 0
        do i = 1, min(size(data1), size(data2))
            if (data1(i) /= data2(i)) then
                diffs = diffs + 1
                if (first_diff == 0) first_diff = i
            end if
        end do
        
        if (diffs == 0) then
            print *, "Files are IDENTICAL!"
        else
            print '(A,I0,A,I0)', "Files differ at ", diffs, " bytes, first at byte ", first_diff
            ! Show first few differences
            print *, "First differences (offset: ours vs stb):"
            diffs = 0
            do i = 1, min(size(data1), size(data2))
                if (data1(i) /= data2(i)) then
                    print '(I6,A,Z2.2,A,Z2.2)', i, ": 0x", data1(i), " vs 0x", data2(i)
                    diffs = diffs + 1
                    if (diffs >= 10) exit
                end if
            end do
        end if
        
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
    
end program debug_jpeg_stb_compare
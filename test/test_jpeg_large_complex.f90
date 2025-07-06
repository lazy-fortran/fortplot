program test_jpeg_large_complex
    use iso_c_binding
    use fortplot_jpeg
    use iso_fortran_env, only: int8
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
    
    integer, parameter :: width = 64, height = 64
    integer(1), target :: image_data(width * height * 3)
    integer(1), allocatable :: our_jpeg(:), stb_jpeg(:)
    integer :: x, y, idx, result, unit, file_size, i
    real :: r, g, b, t
    character(len=100) :: filename
    logical :: files_match
    
    print *, "JPEG Large Complex Image Test"
    print *, "============================="
    
    ! Create a complex test pattern
    do y = 0, height - 1
        do x = 0, width - 1
            idx = (y * width + x) * 3 + 1
            
            ! Create complex gradients and patterns
            t = real(x + y) / real(width + height - 2)
            
            ! Red channel: horizontal gradient with sine wave
            r = 128.0 + 127.0 * sin(real(x) * 6.28 / real(width))
            
            ! Green channel: vertical gradient  
            g = real(y) * 255.0 / real(height - 1)
            
            ! Blue channel: diagonal gradient with blocks
            b = t * 255.0
            
            ! Add some solid color blocks
            if (mod(x/8, 2) == mod(y/8, 2)) then
                b = 255.0 - b
            end if
            
            image_data(idx) = int(min(255.0, max(0.0, r)), 1)
            image_data(idx + 1) = int(min(255.0, max(0.0, g)), 1) 
            image_data(idx + 2) = int(min(255.0, max(0.0, b)), 1)
        end do
    end do
    
    ! Write with STB
    filename = "stb_large_complex.jpg" // c_null_char
    result = stbi_write_jpg(filename, width, height, 3, c_loc(image_data), 85)
    if (result == 0) error stop "STB write failed"
    
    ! Write with our implementation
    call write_jpeg_file("our_large_complex.jpg", width, height, image_data, 85)
    
    ! Compare files
    files_match = compare_files_byte_by_byte("stb_large_complex.jpg", "our_large_complex.jpg")
    
    if (files_match) then
        print *, "SUCCESS: Files match exactly!"
    else
        print *, "FAILURE: Files differ"
    end if
    
contains

    function compare_files_byte_by_byte(file1, file2) result(match)
        character(len=*), intent(in) :: file1, file2
        logical :: match
        integer(1), allocatable :: data1(:), data2(:)
        integer :: size1, size2, unit1, unit2, i
        integer :: first_diff_pos
        
        ! Read first file
        open(newunit=unit1, file=file1, access='stream', form='unformatted', status='old')
        inquire(unit=unit1, size=size1)
        allocate(data1(size1))
        read(unit1) data1
        close(unit1)
        
        ! Read second file
        open(newunit=unit2, file=file2, access='stream', form='unformatted', status='old')
        inquire(unit=unit2, size=size2)
        allocate(data2(size2))
        read(unit2) data2
        close(unit2)
        
        print '("File sizes: STB=", I0, " bytes, Ours=", I0, " bytes")', size1, size2
        
        match = .true.
        first_diff_pos = 0
        
        if (size1 /= size2) then
            match = .false.
            print *, "Files have different sizes"
        end if
        
        ! Find differences
        do i = 1, min(size1, size2)
            if (data1(i) /= data2(i)) then
                if (first_diff_pos == 0) first_diff_pos = i
                match = .false.
            end if
        end do
        
        if (.not. match .and. first_diff_pos > 0) then
            print '("First difference at byte ", I0, " (0x", Z0, ")")', first_diff_pos, first_diff_pos
            print '("STB: 0x", Z2.2, " (", I0, ")")', data1(first_diff_pos), data1(first_diff_pos)
            print '("Our: 0x", Z2.2, " (", I0, ")")', data2(first_diff_pos), data2(first_diff_pos)
            
            ! Show context
            print *, "Context (10 bytes before and after):"
            print *, "STB:"
            print '(21(Z2.2, " "))', data1(max(1,first_diff_pos-10):min(size1,first_diff_pos+10))
            print *, "Our:"
            print '(21(Z2.2, " "))', data2(max(1,first_diff_pos-10):min(size2,first_diff_pos+10))
            
            ! Try to identify which JPEG marker we're in
            call identify_jpeg_context(data1, first_diff_pos)
        end if
        
        deallocate(data1, data2)
    end function
    
    subroutine identify_jpeg_context(data, pos)
        integer(1), intent(in) :: data(:)
        integer, intent(in) :: pos
        integer :: i, marker_start
        
        ! Search backwards for JPEG marker
        marker_start = 0
        do i = pos, 1, -1
            if (data(i) == int(Z'FF', 1) .and. i < size(data)) then
                if (data(i+1) >= int(Z'C0', 1)) then
                    marker_start = i
                    exit
                end if
            end if
        end do
        
        if (marker_start > 0) then
            print '("In JPEG marker: 0xFF", Z2.2, " at offset ", I0, " (", I0, " bytes from marker)")', &
                  data(marker_start+1), marker_start, pos - marker_start
        else
            print *, "Could not identify JPEG marker context"
        end if
    end subroutine

end program
program debug_exact_divergence
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
    
    ! Create a MINIMAL test case to isolate the issue
    integer, parameter :: width = 16, height = 16  ! Smaller for easier debugging
    integer(1), target :: image_data(width * height * 3)
    integer(1), allocatable :: our_jpeg(:), stb_jpeg(:)
    integer :: x, y, idx, result, i
    real :: t, r
    character(len=*), parameter :: our_file = 'debug_our_16x16.jpg'
    character(len=*), parameter :: stb_file = 'debug_stb_16x16.jpg'
    
    print *, "JPEG Exact Divergence Debug (16x16)"
    print *, "==================================="
    
    ! Create the same complex pattern but smaller
    idx = 1
    do y = 0, height - 1
        do x = 0, width - 1
            t = real(x) / real(width-1) * 6.28
            r = real(y) / real(height-1)
            
            image_data(idx) = int(127.5 * (1.0 + sin(t * 3) * cos(r * 4)), kind=1)   ! Red
            image_data(idx+1) = int(127.5 * (1.0 + cos(t * 2) * sin(r * 3)), kind=1) ! Green
            image_data(idx+2) = int(127.5 * (1.0 + sin(t * 4) * cos(r * 2)), kind=1) ! Blue
            idx = idx + 3
        end do
    end do
    
    ! Generate our JPEG
    call write_jpeg_file(our_file, width, height, image_data, 95)
    print *, "Our JPEG written to ", our_file
    
    ! Generate STB JPEG
    result = stbi_write_jpg(stb_file//c_null_char, width, height, 3, c_loc(image_data), 95)
    if (result == 0) then
        print *, "ERROR: STB JPEG generation failed"
        stop 1
    end if
    print *, "STB JPEG written to ", stb_file
    
    ! Compare files byte by byte
    call compare_jpeg_files(our_file, stb_file)
    
contains

    subroutine compare_jpeg_files(file1, file2)
        character(len=*), intent(in) :: file1, file2
        integer(1), allocatable :: data1(:), data2(:)
        integer :: size1, size2, unit1, unit2, i
        logical :: match
        
        ! Read our file
        open(newunit=unit1, file=file1, access='stream', form='unformatted', status='old')
        inquire(unit=unit1, size=size1)
        allocate(data1(size1))
        read(unit1) data1
        close(unit1)
        
        ! Read STB file  
        open(newunit=unit2, file=file2, access='stream', form='unformatted', status='old')
        inquire(unit=unit2, size=size2)
        allocate(data2(size2))
        read(unit2) data2
        close(unit2)
        
        print *, "File sizes: Our=", size1, " bytes, STB=", size2, " bytes"
        
        if (size1 /= size2) then
            print *, "Files have different sizes!"
        end if
        
        ! Compare byte by byte
        match = .true.
        do i = 1, min(size1, size2)
            if (data1(i) /= data2(i)) then
                print *, "First difference at byte", i, "(0x" // trim(int_to_hex(i-1)) // ")"
                print *, "Our: 0x" // trim(byte_to_hex(data1(i))) // " (", data1(i), ")"
                print *, "STB: 0x" // trim(byte_to_hex(data2(i))) // " (", data2(i), ")"
                
                ! Show context
                print *, "Context (10 bytes before and after):"
                call show_context(data1, data2, i, min(size1, size2))
                
                match = .false.
                exit
            end if
        end do
        
        if (match .and. size1 == size2) then
            print *, "SUCCESS: Files match exactly!"
        else if (match) then
            print *, "Files match up to the shorter file length"
        end if
        
        deallocate(data1, data2)
    end subroutine compare_jpeg_files
    
    subroutine show_context(data1, data2, pos, maxlen)
        integer(1), intent(in) :: data1(:), data2(:)
        integer, intent(in) :: pos, maxlen
        integer :: start_pos, end_pos, i
        
        start_pos = max(1, pos - 10)
        end_pos = min(maxlen, pos + 10)
        
        write(*, '(A)', advance='no') 'Our: '
        do i = start_pos, end_pos
            if (i == pos) write(*, '(A)', advance='no') '['
            write(*, '(Z2.2)', advance='no') data1(i)
            if (i == pos) write(*, '(A)', advance='no') ']'
            write(*, '(A)', advance='no') ' '
        end do
        print *
        
        write(*, '(A)', advance='no') 'STB: '
        do i = start_pos, end_pos
            if (i == pos) write(*, '(A)', advance='no') '['
            write(*, '(Z2.2)', advance='no') data2(i)
            if (i == pos) write(*, '(A)', advance='no') ']'
            write(*, '(A)', advance='no') ' '
        end do
        print *
    end subroutine show_context
    
    function byte_to_hex(b) result(hex_str)
        integer(1), intent(in) :: b
        character(len=2) :: hex_str
        write(hex_str, '(Z2.2)') b
    end function byte_to_hex
    
    function int_to_hex(i) result(hex_str)
        integer, intent(in) :: i
        character(len=8) :: hex_str
        write(hex_str, '(Z0)') i
        hex_str = adjustl(hex_str)
    end function int_to_hex

end program debug_exact_divergence
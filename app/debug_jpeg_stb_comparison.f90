program debug_jpeg_stb_comparison
    ! Simple program to compare our JPEG output with STB's output
    use stb_image_write_wrapper
    use fortplot_jpeg
    use iso_c_binding
    use, intrinsic :: iso_fortran_env, only: int8, int32, real64
    implicit none
    
    call test_simple_image()
    
contains

    subroutine test_simple_image()
        integer(int8), target :: rgb_data(3, 8, 8)
        character(len=100) :: filename
        integer :: i, j, result
        
        print *, "=== Creating simple 8x8 test image ==="
        
        ! Create gradient pattern
        do j = 1, 8
            do i = 1, 8
                rgb_data(1, i, j) = int(min(255, (i-1) * 36), int8)  ! Red gradient
                rgb_data(2, i, j) = int(min(255, (j-1) * 36), int8)  ! Green gradient  
                rgb_data(3, i, j) = 0_int8                            ! No blue
            end do
        end do
        
        ! Write using STB
        filename = trim("stb_test_8x8.jpg")//char(0)
        result = stb_write_jpeg_wrapper(filename, 8, 8, 3, c_loc(rgb_data), 90)
        
        if (result == 1) then
            print *, "STB JPEG written successfully: stb_test_8x8.jpg"
        else
            print *, "STB JPEG write failed!"
        end if
        
        ! Now we need to write using our implementation
        call write_our_jpeg_simple()
        
        ! Compare file sizes
        call compare_file_sizes()
        
        ! Hexdump both files
        call hexdump_files()
    end subroutine test_simple_image
    
    subroutine write_our_jpeg_simple()
        type(jpeg_context) :: ctx
        integer :: width = 8, height = 8
        integer :: i, j
        
        print *, ""
        print *, "Creating our JPEG..."
        
        ! Create context
        ctx = create_jpeg_canvas(width, height, 90)
        
        ! Set pixels (need to check how this works)
        do j = 1, height
            do i = 1, width
                ! Set pixel - this may need adjustment based on API
                ! ctx%pixels(i,j,:) = [red, green, blue]
            end do
        end do
        
        ! Save
        call ctx%save("our_test_8x8.jpg")
        
        print *, "Our JPEG written: our_test_8x8.jpg"
    end subroutine write_our_jpeg_simple
    
    subroutine compare_file_sizes()
        integer :: stb_size, our_size
        logical :: exists
        
        print *, ""
        print *, "File size comparison:"
        
        inquire(file="stb_test_8x8.jpg", exist=exists, size=stb_size)
        if (exists) then
            print *, "STB file size:", stb_size, "bytes"
        end if
        
        inquire(file="our_test_8x8.jpg", exist=exists, size=our_size)
        if (exists) then
            print *, "Our file size:", our_size, "bytes"
        end if
    end subroutine compare_file_sizes
    
    subroutine hexdump_files()
        integer(int8) :: buffer(64)
        integer :: unit, ios, i, bytes_read
        
        print *, ""
        print *, "First 64 bytes of STB JPEG:"
        
        open(newunit=unit, file="stb_test_8x8.jpg", access='stream', &
             status='old', iostat=ios)
        if (ios == 0) then
            read(unit, iostat=ios) buffer
            bytes_read = 64
            if (ios < 0) bytes_read = 64 + ios
            
            do i = 1, bytes_read
                write(*, '(Z2.2,1X)', advance='no') iand(255, int(buffer(i)))
                if (mod(i, 16) == 0) write(*,*)
            end do
            if (mod(bytes_read, 16) /= 0) write(*,*)
            
            close(unit)
        end if
        
        print *, ""
        print *, "First 64 bytes of our JPEG:"
        
        open(newunit=unit, file="our_test_8x8.jpg", access='stream', &
             status='old', iostat=ios)
        if (ios == 0) then
            read(unit, iostat=ios) buffer
            bytes_read = 64
            if (ios < 0) bytes_read = 64 + ios
            
            do i = 1, bytes_read
                write(*, '(Z2.2,1X)', advance='no') iand(255, int(buffer(i)))
                if (mod(i, 16) == 0) write(*,*)
            end do
            if (mod(bytes_read, 16) /= 0) write(*,*)
            
            close(unit)
        end if
    end subroutine hexdump_files

end program debug_jpeg_stb_comparison
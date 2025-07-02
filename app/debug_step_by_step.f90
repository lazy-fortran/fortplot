program debug_step_by_step
    use fortplot_mpeg_stream
    use iso_c_binding
    implicit none
    
    character(len=*), parameter :: test_file = "debug_step.dat"
    
    print *, "=== Step-by-step debug ==="
    
    ! Open file
    call stream_open_write(test_file)
    print *, "Initial: position =", stream_tell_write()
    
    ! Write 3 bits: 1, 0, 1
    call stream_put_bit(1)
    print *, "After bit 1: position =", stream_tell_write()
    call stream_put_bit(0) 
    print *, "After bit 0: position =", stream_tell_write()
    call stream_put_bit(1)
    print *, "After bit 1: position =", stream_tell_write()
    
    ! At this point we should have written 3 bits: 101
    ! Let's see what happens if we close and read
    call stream_close_write()
    
    print *, "=== File content after 3 bits ==="
    call show_file_content(test_file)
    
    ! Now test seek
    call stream_open_write(test_file)
    print *, "Reopened file, position =", stream_tell_write()
    
    call stream_seek_write(1_c_long)  ! Seek to bit position 1
    print *, "After seek to 1: position =", stream_tell_write()
    
    ! Write 5 more bits: 1, 1, 0, 1, 1  
    call stream_put_bit(1)
    print *, "After bit 1: position =", stream_tell_write()
    call stream_put_bit(1)
    print *, "After bit 1: position =", stream_tell_write()
    call stream_put_bit(0)
    print *, "After bit 0: position =", stream_tell_write()
    call stream_put_bit(1)
    print *, "After bit 1: position =", stream_tell_write()
    call stream_put_bit(1)
    print *, "After bit 1: position =", stream_tell_write()
    
    call stream_close_write()
    
    print *, "=== Final file content ==="
    call show_file_content(test_file)
    
contains

    subroutine show_file_content(filename)
        character(len=*), intent(in) :: filename
        character(len=1) :: byte_val
        integer :: iostat, unit_num, file_size, i
        
        open(newunit=unit_num, file=filename, access='stream', form='unformatted', status='old')
        inquire(unit=unit_num, size=file_size)
        
        print *, "File size:", file_size, "bytes"
        do i = 1, file_size
            read(unit_num, iostat=iostat) byte_val
            if (iostat == 0) then
                print *, "Byte", i, ":", to_binary(iachar(byte_val)), "(", iachar(byte_val), ")"
            end if
        end do
        close(unit_num)
    end subroutine

    function to_binary(val) result(binary_str)
        integer, intent(in) :: val
        character(len=8) :: binary_str
        integer :: i
        
        do i = 8, 1, -1
            if (btest(val, i-1)) then
                binary_str(9-i:9-i) = '1'
            else
                binary_str(9-i:9-i) = '0'
            end if
        end do
    end function to_binary

end program debug_step_by_step
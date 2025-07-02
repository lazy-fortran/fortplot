program debug_seek_simple
    use fortplot_mpeg_stream
    use iso_c_binding
    implicit none
    
    character(len=*), parameter :: test_file = "debug_seek.dat"
    character(len=1) :: byte_val
    
    print *, "Testing simple seek operation..."
    
    ! Write 3 bits: 101
    call stream_open_write(test_file)
    print *, "Writing bit 1 at position", stream_tell_write()
    call stream_put_bit(1)
    print *, "Writing bit 0 at position", stream_tell_write()
    call stream_put_bit(0)
    print *, "Writing bit 1 at position", stream_tell_write()
    call stream_put_bit(1)
    print *, "Current position after 3 bits:", stream_tell_write()
    
    ! Seek back to position 1
    print *, "Seeking to position 1..."
    call stream_seek_write(1_c_long)
    print *, "Position after seek:", stream_tell_write()
    
    ! Write 5 more bits: 11011
    print *, "Writing bit 1 at position", stream_tell_write()
    call stream_put_bit(1)
    print *, "Writing bit 1 at position", stream_tell_write()
    call stream_put_bit(1)
    print *, "Writing bit 0 at position", stream_tell_write()
    call stream_put_bit(0)
    print *, "Writing bit 1 at position", stream_tell_write()
    call stream_put_bit(1)
    print *, "Writing bit 1 at position", stream_tell_write()
    call stream_put_bit(1)
    print *, "Final position:", stream_tell_write()
    
    call stream_close_write()
    
    ! Expected: first bit 1, then overwritten from position 1 with 11011
    ! So: 1 + 11011 = 111011 (but we need 8 bits)
    ! Expected pattern should be: 1 (pos 0) + 11011 (pos 1-5) + 11 (padding) = 11101111
    print *, "Expected: 11101111 (binary)"
    
    ! Read back and show result
    open(unit=10, file=test_file, access='stream', form='unformatted')
    read(10) byte_val
    close(10)
    
    print *, "Actual:  ", to_binary(iachar(byte_val))
    print *, "Decimal: ", iachar(byte_val)
    
contains

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

end program debug_seek_simple
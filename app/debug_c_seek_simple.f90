program debug_c_seek_simple
    use iso_c_binding
    implicit none
    
    interface
        subroutine mwopen(filename) bind(c, name='mwopen')
            import :: c_char
            character(kind=c_char), intent(in) :: filename(*)
        end subroutine
        
        subroutine mwclose() bind(c, name='mwclose')
        end subroutine
        
        subroutine mputb(bit) bind(c, name='mputb')
            import :: c_int
            integer(c_int), value :: bit
        end subroutine
        
        subroutine mwseek(position) bind(c, name='mwseek')
            import :: c_long
            integer(c_long), value :: position
        end subroutine
        
        function mwtell() bind(c, name='mwtell') result(position)
            import :: c_long
            integer(c_long) :: position
        end function
    end interface
    
    character(len=20, kind=c_char) :: c_file = "debug_c_seek.dat" // c_null_char
    character(len=1) :: byte_val
    
    print *, "Testing C seek operation..."
    
    ! Write 3 bits: 101
    call mwopen(c_file)
    print *, "Writing bit 1 at position", mwtell()
    call mputb(1_c_int)
    print *, "Writing bit 0 at position", mwtell()
    call mputb(0_c_int)
    print *, "Writing bit 1 at position", mwtell()
    call mputb(1_c_int)
    print *, "Current position after 3 bits:", mwtell()
    
    ! Seek back to position 1
    print *, "Seeking to position 1..."
    call mwseek(1_c_long)
    print *, "Position after seek:", mwtell()
    
    ! Write 5 more bits: 11011
    print *, "Writing bit 1 at position", mwtell()
    call mputb(1_c_int)
    print *, "Writing bit 1 at position", mwtell()
    call mputb(1_c_int)
    print *, "Writing bit 0 at position", mwtell()
    call mputb(0_c_int)
    print *, "Writing bit 1 at position", mwtell()
    call mputb(1_c_int)
    print *, "Writing bit 1 at position", mwtell()
    call mputb(1_c_int)
    print *, "Final position:", mwtell()
    
    call mwclose()
    
    ! Read back and show result
    open(unit=10, file="debug_c_seek.dat", access='stream', form='unformatted')
    read(10) byte_val
    close(10)
    
    print *, "C result: ", to_binary(iachar(byte_val))
    print *, "Decimal:  ", iachar(byte_val)
    
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

end program debug_c_seek_simple
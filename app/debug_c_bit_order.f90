program debug_c_bit_order
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
    end interface
    
    character(len=20, kind=c_char) :: c_file = "debug_c_bits.dat" // c_null_char
    character(len=1) :: byte_val
    integer :: i
    
    print *, "Testing C bit order by writing 8 individual bits..."
    
    ! Write exactly 8 bits: 10110010
    call mwopen(c_file)
    call mputb(1_c_int)  ! bit 0
    call mputb(0_c_int)  ! bit 1
    call mputb(1_c_int)  ! bit 2
    call mputb(1_c_int)  ! bit 3
    call mputb(0_c_int)  ! bit 4
    call mputb(0_c_int)  ! bit 5
    call mputb(1_c_int)  ! bit 6
    call mputb(0_c_int)  ! bit 7
    call mwclose()
    
    ! Read back and show result
    open(unit=10, file="debug_c_bits.dat", access='stream', form='unformatted')
    read(10) byte_val
    close(10)
    
    print *, "Input bits:  10110010"
    print *, "C result:   ", to_binary(iachar(byte_val))
    print *, "Decimal:    ", iachar(byte_val)
    
    ! This will tell us the actual bit ordering used by C
    
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

end program debug_c_bit_order
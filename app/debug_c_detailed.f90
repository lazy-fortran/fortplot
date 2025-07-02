program debug_c_detailed
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
    end interface
    
    character(len=25, kind=c_char) :: c_file = "debug_c_detailed.dat" // c_null_char
    character(len=1) :: byte_val
    
    print *, "=== C detailed debug ==="
    
    ! Test 1: Write some bits, close, check file
    call mwopen(c_file)
    call mputb(1_c_int)  ! bit 0 = 1
    call mputb(0_c_int)  ! bit 1 = 0  
    call mputb(1_c_int)  ! bit 2 = 1
    call mwclose()       ! This will flush with 1s
    
    print *, "=== After writing 101 and closing ==="
    call show_file_content("debug_c_detailed.dat")
    
    ! Test 2: Reopen, seek to position 1, write different pattern
    call mwopen(c_file)
    call mwseek(1_c_long)  ! Seek to bit position 1
    call mputb(1_c_int)    ! bit 1 = 1
    call mputb(1_c_int)    ! bit 2 = 1
    call mputb(0_c_int)    ! bit 3 = 0
    call mputb(1_c_int)    ! bit 4 = 1
    call mputb(1_c_int)    ! bit 5 = 1
    call mwclose()
    
    print *, "=== After seeking to 1 and writing 11011 ==="
    call show_file_content("debug_c_detailed.dat")
    
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

end program debug_c_detailed
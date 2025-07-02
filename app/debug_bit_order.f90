program debug_bit_order
    use iso_c_binding
    use fortplot_mpeg_stream
    implicit none
    
    interface
        subroutine mwopen(filename) bind(c, name='mwopen')
            import :: c_char
            character(kind=c_char) :: filename(*)
        end subroutine
        
        subroutine mwclose() bind(c, name='mwclose')
        end subroutine
        
        subroutine mputb(bit) bind(c, name='mputb')
            import :: c_int
            integer(c_int), value :: bit
        end subroutine
    end interface
    
    character(len=20, kind=c_char) :: c_file = "debug_bit_order.dat" // c_null_char
    
    ! Write a simple bit pattern using low-level functions
    print *, "Writing bit pattern 00101010 (42 decimal) using C functions"
    
    call mwopen(c_file)
    
    ! Write 00101010
    call mputb(0_c_int)
    call mputb(0_c_int)
    call mputb(1_c_int)
    call mputb(0_c_int)
    call mputb(1_c_int)
    call mputb(0_c_int)
    call mputb(1_c_int)
    call mputb(0_c_int)
    
    call mwclose()
    
    print *, "File content:"
    call execute_command_line("od -t x1 debug_bit_order.dat")
    
end program debug_bit_order
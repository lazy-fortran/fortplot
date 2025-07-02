program debug_c_vs_fortran_vars
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
        
        subroutine mropen(filename) bind(c, name='mropen')
            import :: c_char
            character(kind=c_char) :: filename(*)
        end subroutine
        
        subroutine mrclose() bind(c, name='mrclose')
        end subroutine
        
        subroutine mputv(value, bits) bind(c, name='mputv')
            import :: c_int
            integer(c_int), value :: value
            integer(c_int), value :: bits
        end subroutine
        
        function mgetv(bits) bind(c, name='mgetv') result(value)
            import :: c_int
            integer(c_int), value :: bits
            integer(c_int) :: value
        end function
    end interface
    
    character(len=20, kind=c_char) :: c_file = "debug_c_vars.dat" // c_null_char
    character(len=*), parameter :: fortran_file = "debug_fortran_vars.dat"
    integer :: test_value, test_bits
    integer :: c_read_value, fortran_read_value
    integer :: i
    
    ! Test single value: 42 with 8 bits
    test_value = 42
    test_bits = 8
    
    print *, "Writing value", test_value, "with", test_bits, "bits"
    
    ! Write with C implementation
    call mwopen(c_file)
    call mputv(int(test_value, c_int), int(test_bits, c_int))
    call mwclose()
    
    ! Write with Fortran implementation
    call stream_open_write(fortran_file)
    call stream_put_variable(test_value, test_bits)
    call stream_close_write()
    
    ! Check file sizes
    call execute_command_line("ls -la debug_*.dat")
    
    ! Show binary content
    print *, "C file content:"
    call execute_command_line("od -t x1 debug_c_vars.dat")
    print *, "Fortran file content:"
    call execute_command_line("od -t x1 debug_fortran_vars.dat")
    
    ! Read back with C implementation
    call mropen(c_file)
    c_read_value = int(mgetv(int(test_bits, c_int)))
    call mrclose()
    
    ! Read back with Fortran implementation
    call stream_open_read(fortran_file)
    fortran_read_value = stream_get_variable(test_bits)
    call stream_close_read()
    
    print *, "C read back:", c_read_value, "expected:", test_value
    print *, "Fortran read back:", fortran_read_value, "expected:", test_value
    
    ! Test bit pattern for value 42 (00101010)
    print *, "Binary representation of 42: 00101010"
    print *, "Testing individual bits..."
    
    ! Write individual bits with Fortran
    call stream_open_write("debug_bits.dat")
    ! Write 00101010 (42)
    call stream_put_bit(0)
    call stream_put_bit(0)
    call stream_put_bit(1)
    call stream_put_bit(0)
    call stream_put_bit(1)
    call stream_put_bit(0)
    call stream_put_bit(1)
    call stream_put_bit(0)
    call stream_close_write()
    
    print *, "Individual bits file:"
    call execute_command_line("od -t x1 debug_bits.dat")
    
end program debug_c_vs_fortran_vars
program debug_c_variable_ops
    use iso_c_binding
    implicit none
    
    interface
        subroutine mwopen(filename) bind(c, name='mwopen')
            import :: c_char
            character(kind=c_char), intent(in) :: filename(*)
        end subroutine
        
        subroutine mwclose() bind(c, name='mwclose')
        end subroutine
        
        subroutine mropen(filename) bind(c, name='mropen')
            import :: c_char
            character(kind=c_char), intent(in) :: filename(*)
        end subroutine
        
        subroutine mrclose() bind(c, name='mrclose')
        end subroutine
        
        subroutine mputv(bits, value) bind(c, name='mputv')
            import :: c_int
            integer(c_int), value :: bits, value
        end subroutine
        
        function mgetv(bits) bind(c, name='mgetv') result(value)
            import :: c_int
            integer(c_int), value :: bits
            integer(c_int) :: value
        end function
    end interface
    
    character(len=20, kind=c_char) :: c_file = "debug_c_vars.dat" // c_null_char
    integer :: test_value = 42
    integer :: test_bits = 8
    integer :: read_value
    
    print *, "Writing value", test_value, "with", test_bits, "bits"
    
    ! Write with C implementation
    call mwopen(c_file)
    call mputv(int(test_bits, c_int), int(test_value, c_int))
    call mwclose()
    
    print *, "File content after write:"
    call execute_command_line("od -t x1 debug_c_vars.dat")
    
    ! Read back with C implementation  
    call mropen(c_file)
    read_value = int(mgetv(int(test_bits, c_int)))
    call mrclose()
    
    print *, "Read back value:", read_value
    
    if (read_value /= test_value) then
        print *, "ERROR: Expected", test_value, "but got", read_value
    else
        print *, "SUCCESS: Values match!"
    end if
    
end program debug_c_variable_ops
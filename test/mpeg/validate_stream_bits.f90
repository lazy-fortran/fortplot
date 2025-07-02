program validate_stream_bits
    use iso_c_binding
    implicit none
    
    ! Test bit-level stream operations
    call test_bit_operations()
    call test_variable_length_operations()
    call test_alignment_operations()
    
    print *, "PASS: stream bit operations validation"
    
contains

    subroutine test_bit_operations()
        interface
            function mgetb() bind(c, name='mgetb') result(bit)
                import :: c_int
                integer(c_int) :: bit
            end function
            
            subroutine mputb(bit) bind(c, name='mputb')
                import :: c_int
                integer(c_int), value :: bit
            end subroutine
        end interface
        
        integer(c_int) :: test_bit, read_bit
        character(len=20, kind=c_char) :: test_file = "test_bits.dat" // c_null_char
        
        ! Test single bit write/read
        call setup_test_file(test_file)
        
        test_bit = 1_c_int
        call mputb(test_bit)
        call cleanup_write()
        
        call setup_read_file(test_file)
        read_bit = mgetb()
        call cleanup_read()
        
        if (read_bit /= test_bit) then
            error stop "Bit read/write mismatch"
        end if
        
        print *, "PASS: single bit operations"
    end subroutine

    subroutine test_variable_length_operations()
        interface
            subroutine mputv(value, bits) bind(c, name='mputv')
                import :: c_int
                integer(c_int), value :: value, bits
            end subroutine
            
            function mgetv(bits) bind(c, name='mgetv') result(value)
                import :: c_int
                integer(c_int), value :: bits
                integer(c_int) :: value
            end function
        end interface
        
        integer(c_int) :: test_value, read_value, num_bits
        character(len=20, kind=c_char) :: test_file = "test_varlen.dat" // c_null_char
        
        ! Test variable length operations
        call setup_test_file(test_file)
        
        test_value = 42_c_int
        num_bits = 8_c_int
        call mputv(test_value, num_bits)
        call cleanup_write()
        
        call setup_read_file(test_file)
        read_value = mgetv(num_bits)
        call cleanup_read()
        
        if (read_value /= test_value) then
            error stop "Variable length read/write mismatch"
        end if
        
        print *, "PASS: variable length operations"
    end subroutine

    subroutine test_alignment_operations()
        interface
            subroutine readalign() bind(c, name='readalign')
            end subroutine
            
            subroutine zeroflush() bind(c, name='zeroflush')
            end subroutine
        end interface
        
        ! Test alignment functions
        call readalign()
        call zeroflush()
        
        print *, "PASS: alignment operations"
    end subroutine

    subroutine setup_test_file(filename)
        character(len=*, kind=c_char), intent(in) :: filename
        interface
            subroutine mwopen(filename) bind(c, name='mwopen')
                import :: c_char
                character(kind=c_char), intent(in) :: filename(*)
            end subroutine
        end interface
        call mwopen(filename)
    end subroutine

    subroutine cleanup_write()
        interface
            subroutine mwclose() bind(c, name='mwclose')
            end subroutine
        end interface
        call mwclose()
    end subroutine

    subroutine setup_read_file(filename)
        character(len=*, kind=c_char), intent(in) :: filename
        interface
            subroutine mropen(filename) bind(c, name='mropen')
                import :: c_char
                character(kind=c_char), intent(in) :: filename(*)
            end subroutine
        end interface
        call mropen(filename)
    end subroutine

    subroutine cleanup_read()
        interface
            subroutine mrclose() bind(c, name='mrclose')
            end subroutine
        end interface
        call mrclose()
    end subroutine

end program validate_stream_bits
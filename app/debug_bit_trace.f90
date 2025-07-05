program debug_bit_trace
    use fortplot_jpeg
    use iso_fortran_env, only: int8
    implicit none
    
    type :: bit_writer_t
        integer :: bit_buffer = 0
        integer :: bit_count = 0
        integer(1), allocatable :: output(:)
        integer :: output_pos = 1
    end type bit_writer_t
    
    type(bit_writer_t) :: writer
    integer :: i
    
    ! Initialize writer
    allocate(writer%output(100))
    writer%bit_buffer = 0
    writer%bit_count = 0 
    writer%output_pos = 1
    
    print *, "Testing bit writing sequence..."
    
    ! Write some test bits
    call test_write_bits(writer, 101, 8)  ! First byte 0x65
    call test_write_bits(writer, 20, 8)   ! Second byte 0x14
    
    ! Flush with proper padding
    if (writer%bit_count > 0) then
        print *, "Flushing with bit_count =", writer%bit_count
        call test_write_bits(writer, ishft(1, 8 - writer%bit_count) - 1, 8 - writer%bit_count)
    end if
    
    ! Show output
    print *, "Output bytes:"
    do i = 1, writer%output_pos - 1
        write(*, '(Z2.2,A)', advance='no') writer%output(i), " "
    end do
    print *
    
contains

    subroutine test_write_bits(writer, code, bits)
        type(bit_writer_t), intent(inout) :: writer
        integer, intent(in) :: code, bits
        integer :: c
        
        print '(A,I0,A,I0,A,B32.32)', "Writing code=", code, " bits=", bits, " binary=", code
        
        writer%bit_count = writer%bit_count + bits
        writer%bit_buffer = ior(writer%bit_buffer, ishft(code, 24 - writer%bit_count))
        
        print '(A,B32.32,A,I0)', "  Buffer after OR: ", writer%bit_buffer, " count=", writer%bit_count
        
        do while (writer%bit_count >= 8)
            c = iand(ishft(writer%bit_buffer, -16), 255)
            print '(A,Z2.2)', "  Emitting byte: ", c
            
            writer%output(writer%output_pos) = int(c, 1)
            writer%output_pos = writer%output_pos + 1
            
            if (c == 255) then
                print *, "  Byte stuffing: 00"
                writer%output(writer%output_pos) = 0_int8
                writer%output_pos = writer%output_pos + 1
            end if
            
            writer%bit_buffer = ishft(writer%bit_buffer, 8)
            writer%bit_count = writer%bit_count - 8
        end do
    end subroutine

end program debug_bit_trace
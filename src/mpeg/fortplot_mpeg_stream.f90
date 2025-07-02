module fortplot_mpeg_stream
    use iso_c_binding
    implicit none
    private
    
    ! Stream I/O constants
    integer, parameter :: STREAM_READ = 0
    integer, parameter :: STREAM_WRITE = 1
    
    ! Stream state type
    type :: stream_state_t
        integer :: mode
        integer(c_long) :: position
        logical :: is_open
        character(len=:), allocatable :: filename
        logical :: eof_reached
        integer :: unit_number
        integer :: bit_buffer
        integer :: bits_in_buffer
    end type stream_state_t
    
    ! Global stream states (matching C implementation pattern)
    type(stream_state_t) :: read_stream_state
    type(stream_state_t) :: write_stream_state
    
    public :: stream_open_read, stream_close_read
    public :: stream_open_write, stream_close_write
    public :: stream_get_bit, stream_put_bit
    public :: stream_get_variable, stream_put_variable
    public :: stream_tell_read, stream_tell_write
    public :: stream_seek_read, stream_seek_write
    public :: stream_eof, stream_align_read, stream_flush_write, stream_flush_write_zeros
    
contains

    subroutine stream_open_read(filename)
        character(len=*), intent(in) :: filename
        integer :: ios
        
        read_stream_state%filename = filename
        read_stream_state%mode = STREAM_READ
        read_stream_state%position = 0_c_long
        read_stream_state%eof_reached = .false.
        read_stream_state%bit_buffer = 0
        read_stream_state%bits_in_buffer = 0
        
        ! Open file for binary read
        open(newunit=read_stream_state%unit_number, file=filename, &
             access='stream', form='unformatted', status='old', &
             action='read', iostat=ios)
        
        read_stream_state%is_open = (ios == 0)
        if (.not. read_stream_state%is_open) then
            print *, "Warning: Failed to open file for reading: ", filename
        end if
    end subroutine stream_open_read

    subroutine stream_close_read()
        if (read_stream_state%is_open) then
            close(read_stream_state%unit_number)
            read_stream_state%is_open = .false.
            if (allocated(read_stream_state%filename)) then
                deallocate(read_stream_state%filename)
            end if
        end if
    end subroutine stream_close_read

    subroutine stream_open_write(filename)
        character(len=*), intent(in) :: filename
        integer :: ios
        
        write_stream_state%filename = filename
        write_stream_state%mode = STREAM_WRITE
        write_stream_state%position = 0_c_long
        write_stream_state%eof_reached = .false.
        write_stream_state%bit_buffer = 0
        write_stream_state%bits_in_buffer = 0
        
        ! Open file for binary write
        open(newunit=write_stream_state%unit_number, file=filename, &
             access='stream', form='unformatted', status='replace', &
             action='write', iostat=ios)
        
        write_stream_state%is_open = (ios == 0)
        if (.not. write_stream_state%is_open) then
            print *, "Warning: Failed to open file for writing: ", filename
        end if
    end subroutine stream_open_write

    subroutine stream_close_write()
        if (write_stream_state%is_open) then
            ! Flush any remaining bits
            call stream_flush_write()
            close(write_stream_state%unit_number)
            write_stream_state%is_open = .false.
            if (allocated(write_stream_state%filename)) then
                deallocate(write_stream_state%filename)
            end if
        end if
    end subroutine stream_close_write

    function stream_get_bit() result(bit_value)
        integer :: bit_value
        integer(c_int8_t) :: byte_val
        integer :: ios
        
        if (.not. read_stream_state%is_open) then
            bit_value = 0
            return
        end if
        
        ! If no bits in buffer, read next byte
        if (read_stream_state%bits_in_buffer == 0) then
            read(read_stream_state%unit_number, iostat=ios) byte_val
            if (ios /= 0) then
                read_stream_state%eof_reached = .true.
                bit_value = 0
                return
            end if
            read_stream_state%bit_buffer = int(byte_val)
            read_stream_state%bits_in_buffer = 8
        end if
        
        ! Extract highest bit
        bit_value = ishft(read_stream_state%bit_buffer, -7) 
        bit_value = iand(bit_value, 1)
        
        ! Shift buffer and decrement count
        read_stream_state%bit_buffer = ishft(read_stream_state%bit_buffer, 1)
        read_stream_state%bit_buffer = iand(read_stream_state%bit_buffer, 255)
        read_stream_state%bits_in_buffer = read_stream_state%bits_in_buffer - 1
        read_stream_state%position = read_stream_state%position + 1
    end function stream_get_bit

    subroutine stream_put_bit(bit_value)
        integer, intent(in) :: bit_value
        integer(c_int8_t) :: byte_val
        
        if (.not. write_stream_state%is_open) return
        
        ! Add bit to buffer
        write_stream_state%bit_buffer = ishft(write_stream_state%bit_buffer, 1)
        write_stream_state%bit_buffer = ior(write_stream_state%bit_buffer, iand(bit_value, 1))
        write_stream_state%bits_in_buffer = write_stream_state%bits_in_buffer + 1
        
        ! If buffer is full, write byte
        if (write_stream_state%bits_in_buffer == 8) then
            byte_val = int(write_stream_state%bit_buffer, c_int8_t)
            write(write_stream_state%unit_number) byte_val
            write_stream_state%bit_buffer = 0
            write_stream_state%bits_in_buffer = 0
        end if
        
        write_stream_state%position = write_stream_state%position + 1
    end subroutine stream_put_bit

    function stream_get_variable(num_bits) result(value)
        integer, intent(in) :: num_bits
        integer :: value
        integer :: i, bit_val
        
        if (.not. read_stream_state%is_open) then
            value = 0
            return
        end if
        
        ! Read bits from most significant to least significant
        value = 0
        do i = num_bits-1, 0, -1
            bit_val = stream_get_bit()
            if (bit_val /= 0) then
                value = ior(value, ishft(1, i))
            end if
        end do
    end function stream_get_variable

    subroutine stream_put_variable(value, num_bits)
        integer, intent(in) :: value, num_bits
        integer :: i, bit_value
        
        if (.not. write_stream_state%is_open) return
        
        ! Write bits from most significant to least significant  
        do i = num_bits-1, 0, -1
            bit_value = ishft(value, -i)
            bit_value = iand(bit_value, 1)
            call stream_put_bit(bit_value)
        end do
    end subroutine stream_put_variable

    function stream_tell_read() result(position)
        integer(c_long) :: position
        position = read_stream_state%position
    end function stream_tell_read

    function stream_tell_write() result(position)
        integer(c_long) :: position
        position = write_stream_state%position
    end function stream_tell_write

    subroutine stream_seek_read(position)
        integer(c_long), intent(in) :: position
        read_stream_state%position = position
    end subroutine stream_seek_read

    subroutine stream_seek_write(position)
        integer(c_long), intent(in) :: position
        write_stream_state%position = position
    end subroutine stream_seek_write

    function stream_eof() result(is_eof)
        logical :: is_eof
        is_eof = read_stream_state%eof_reached
    end function stream_eof

    subroutine stream_align_read()
        ! Align to byte boundary for marker reading
        integer(c_long) :: remainder
        remainder = mod(read_stream_state%position, 8_c_long)
        if (remainder /= 0) then
            read_stream_state%position = read_stream_state%position + (8_c_long - remainder)
        end if
    end subroutine stream_align_read

    subroutine stream_flush_write()
        ! Flush any buffered bits with zeros
        integer(c_int8_t) :: byte_val
        integer :: bits_to_pad
        
        if (.not. write_stream_state%is_open) return
        
        if (write_stream_state%bits_in_buffer > 0) then
            ! Calculate how many bits to pad
            bits_to_pad = 8 - write_stream_state%bits_in_buffer
            
            ! Pad remaining bits with ones (to match C mwclose behavior)
            do while (write_stream_state%bits_in_buffer < 8)
                write_stream_state%bit_buffer = ishft(write_stream_state%bit_buffer, 1) + 1
                write_stream_state%bits_in_buffer = write_stream_state%bits_in_buffer + 1
                write_stream_state%position = write_stream_state%position + 1
            end do
            
            byte_val = int(write_stream_state%bit_buffer, c_int8_t)
            write(write_stream_state%unit_number) byte_val
            write_stream_state%bit_buffer = 0
            write_stream_state%bits_in_buffer = 0
        end if
    end subroutine stream_flush_write

    subroutine stream_flush_write_zeros()
        use iso_c_binding, only: c_int8_t
        implicit none
        
        integer(c_int8_t) :: byte_val
        integer :: bits_to_pad
        
        if (.not. write_stream_state%is_open) return
        
        if (write_stream_state%bits_in_buffer > 0) then
            ! Calculate how many bits to pad
            bits_to_pad = 8 - write_stream_state%bits_in_buffer
            
            ! Pad remaining bits with zeros (to match C zeroflush behavior)
            do while (write_stream_state%bits_in_buffer < 8)
                write_stream_state%bit_buffer = ishft(write_stream_state%bit_buffer, 1)
                write_stream_state%bits_in_buffer = write_stream_state%bits_in_buffer + 1
                write_stream_state%position = write_stream_state%position + 1
            end do
            
            byte_val = int(write_stream_state%bit_buffer, c_int8_t)
            write(write_stream_state%unit_number) byte_val
            write_stream_state%bit_buffer = 0
            write_stream_state%bits_in_buffer = 0
        end if
    end subroutine stream_flush_write_zeros

end module fortplot_mpeg_stream
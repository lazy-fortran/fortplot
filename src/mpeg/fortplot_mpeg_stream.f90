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
        integer :: bit_position  ! Position within current byte (0-7)
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
        read_stream_state%bit_position = 0
        
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
        write_stream_state%bit_position = 7
        
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
        
        ! If no bits in buffer or we need a new byte, read next byte
        if (read_stream_state%bits_in_buffer == 0 .or. read_stream_state%bit_position >= 8) then
            read(read_stream_state%unit_number, iostat=ios) byte_val
            if (ios /= 0) then
                read_stream_state%eof_reached = .true.
                bit_value = 0
                return
            end if
            read_stream_state%bit_buffer = int(byte_val)
            read_stream_state%bits_in_buffer = 8
            read_stream_state%bit_position = 0
        end if
        
        ! Extract bit from buffer (MSB first, like C implementation)
        bit_value = iand(ishft(read_stream_state%bit_buffer, -(7 - read_stream_state%bit_position)), 1)
        read_stream_state%bit_position = read_stream_state%bit_position + 1
        read_stream_state%position = read_stream_state%position + 1
        
        ! Update bits_in_buffer to track remaining bits
        read_stream_state%bits_in_buffer = 8 - read_stream_state%bit_position
    end function stream_get_bit

    subroutine stream_put_bit(bit_value)
        integer, intent(in) :: bit_value
        integer(c_int8_t) :: byte_val
        integer, parameter :: bit_set_mask(0:7) = [1, 2, 4, 8, 16, 32, 64, 128]
        integer(c_long) :: target_byte_pos
        
        if (.not. write_stream_state%is_open) return
        
        ! Match C implementation exactly:
        ! For mput1: current_write_byte|=bit_set_mask[write_position--];
        ! For mput0: write_position--;  
        if (iand(bit_value, 1) == 1) then
            write_stream_state%bit_buffer = ior(write_stream_state%bit_buffer, &
                                             bit_set_mask(write_stream_state%bit_position))
        end if
        
        ! Decrement bit position (like C: write_position--)
        write_stream_state%bit_position = write_stream_state%bit_position - 1
        
        ! If position goes negative, write byte and reset (like C)
        if (write_stream_state%bit_position < 0) then
            byte_val = int(write_stream_state%bit_buffer, c_int8_t)
            
            ! Calculate the correct file position for this byte
            ! The byte should be written at the position where it started (position - 7)
            target_byte_pos = ishft(write_stream_state%position - 7, -3) + 1
            write(write_stream_state%unit_number, pos=target_byte_pos) byte_val
            
            write_stream_state%bit_position = 7
            write_stream_state%bit_buffer = 0
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
        use iso_c_binding, only: c_int8_t
        integer(c_long), intent(in) :: position
        
        integer(c_long) :: target_byte_pos
        integer :: target_bit_pos
        integer(c_int8_t) :: byte_val
        integer :: iostat
        
        if (.not. read_stream_state%is_open) return
        
        ! Calculate target byte and bit positions
        target_byte_pos = position / 8_c_long
        target_bit_pos = int(mod(position, 8_c_long))
        
        ! Seek to target byte position and read byte
        read(read_stream_state%unit_number, pos=target_byte_pos + 1, iostat=iostat) byte_val
        if (iostat == 0) then
            read_stream_state%bit_buffer = int(byte_val)
            read_stream_state%bits_in_buffer = 8
            read_stream_state%bit_position = target_bit_pos
        else
            ! Handle end of file or read error
            read_stream_state%eof_reached = .true.
            read_stream_state%bit_buffer = 0
            read_stream_state%bits_in_buffer = 0
        read_stream_state%bit_position = 0
            read_stream_state%bit_position = 0
        end if
        
        read_stream_state%position = position
    end subroutine stream_seek_read

    subroutine stream_seek_write(position)
        use iso_c_binding, only: c_int8_t
        integer(c_long), intent(in) :: position
        
        integer(c_long) :: target_byte_pos, file_length_bytes
        integer :: target_bit_pos, mask
        integer(c_int8_t) :: byte_val
        integer :: iostat
        
        if (.not. write_stream_state%is_open) return
        
        ! Step 1: if (write_position != 7) {putc(current_write_byte,swout);}
        if (write_stream_state%bit_position /= 7) then
            byte_val = int(write_stream_state%bit_buffer, c_int8_t)
            write(write_stream_state%unit_number) byte_val
        end if
        
        ! Step 2 & 3: fseek(swout,0,2L); Length = ftell(swout);
        inquire(unit=write_stream_state%unit_number, size=file_length_bytes, iostat=iostat)
        if (iostat /= 0) file_length_bytes = 0
        
        ! Step 4: fseek(swout,(distance+7)>>3,0L);
        target_byte_pos = ishft(position + 7, -3)
        
        ! Step 5 & 6: Check if seeking beyond file length in bits
        if (ishft(file_length_bytes, 3) <= position) then
            ! Beyond file end - start clean
            write_stream_state%bit_buffer = 0
            write_stream_state%bit_position = 7 - int(iand(position, 7_c_long))
        else
            ! Within existing file - read existing byte at target position
            read(write_stream_state%unit_number, pos=target_byte_pos + 1, iostat=iostat) byte_val
            if (iostat == 0) then
                write_stream_state%bit_buffer = int(byte_val)
            else
                write_stream_state%bit_buffer = 0
            end if
            write_stream_state%bit_position = 7 - int(iand(position, 7_c_long))
        end if
        
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
        
        ! Match C mwclose behavior: while(write_position!=7) {mput1();}
        do while (write_stream_state%bit_position /= 7)
            call stream_put_bit(1)
        end do
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
        write_stream_state%bit_position = 7
        end if
    end subroutine stream_flush_write_zeros

end module fortplot_mpeg_stream
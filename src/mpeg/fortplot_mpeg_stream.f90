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
    public :: stream_eof, stream_align_read, stream_flush_write
    
contains

    subroutine stream_open_read(filename)
        character(len=*), intent(in) :: filename
        
        read_stream_state%filename = filename
        read_stream_state%mode = STREAM_READ
        read_stream_state%position = 0_c_long
        read_stream_state%is_open = .true.
        read_stream_state%eof_reached = .false.
    end subroutine stream_open_read

    subroutine stream_close_read()
        if (read_stream_state%is_open) then
            read_stream_state%is_open = .false.
            if (allocated(read_stream_state%filename)) then
                deallocate(read_stream_state%filename)
            end if
        end if
    end subroutine stream_close_read

    subroutine stream_open_write(filename)
        character(len=*), intent(in) :: filename
        
        write_stream_state%filename = filename
        write_stream_state%mode = STREAM_WRITE
        write_stream_state%position = 0_c_long
        write_stream_state%is_open = .true.
        write_stream_state%eof_reached = .false.
    end subroutine stream_open_write

    subroutine stream_close_write()
        if (write_stream_state%is_open) then
            write_stream_state%is_open = .false.
            if (allocated(write_stream_state%filename)) then
                deallocate(write_stream_state%filename)
            end if
        end if
    end subroutine stream_close_write

    function stream_get_bit() result(bit_value)
        integer :: bit_value
        
        if (.not. read_stream_state%is_open) then
            bit_value = 0
            return
        end if
        
        ! Placeholder implementation - would read actual bit from file
        bit_value = 0
        read_stream_state%position = read_stream_state%position + 1
    end function stream_get_bit

    subroutine stream_put_bit(bit_value)
        integer, intent(in) :: bit_value
        
        if (.not. write_stream_state%is_open) return
        
        ! Placeholder implementation - would write actual bit to file
        write_stream_state%position = write_stream_state%position + 1
    end subroutine stream_put_bit

    function stream_get_variable(num_bits) result(value)
        integer, intent(in) :: num_bits
        integer :: value
        integer :: i
        
        if (.not. read_stream_state%is_open) then
            value = 0
            return
        end if
        
        ! Placeholder implementation - would read variable length value
        value = 0
        do i = 1, num_bits
            value = value + stream_get_bit()
        end do
    end function stream_get_variable

    subroutine stream_put_variable(value, num_bits)
        integer, intent(in) :: value, num_bits
        integer :: i, bit_value
        
        if (.not. write_stream_state%is_open) return
        
        ! Placeholder implementation - would write variable length value
        do i = 1, num_bits
            bit_value = ibits(value, i-1, 1)
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
        integer(c_long) :: remainder
        remainder = mod(write_stream_state%position, 8_c_long)
        if (remainder /= 0) then
            write_stream_state%position = write_stream_state%position + (8_c_long - remainder)
        end if
    end subroutine stream_flush_write

end module fortplot_mpeg_stream
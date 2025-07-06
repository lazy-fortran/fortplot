program debug_checker_difference
    implicit none
    
    integer(1), allocatable :: stb_data(:), our_data(:)
    integer :: stb_size, our_size, i, first_diff
    character(len=*), parameter :: stb_file = "debug_stb_checker.jpg"
    character(len=*), parameter :: our_file = "debug_our_checker.jpg"
    
    print *, "Analyzing checker pattern differences"
    print *, "====================================="
    
    ! Read both files
    call read_binary_file(stb_file, stb_data, stb_size)
    call read_binary_file(our_file, our_data, our_size)
    
    print *, "STB size:", stb_size
    print *, "Our size:", our_size
    print *, "Difference:", abs(stb_size - our_size), "bytes"
    print *, ""
    
    ! Find first difference
    first_diff = 0
    do i = 1, min(stb_size, our_size)
        if (stb_data(i) /= our_data(i)) then
            first_diff = i
            exit
        end if
    end do
    
    if (first_diff > 0) then
        print *, "First difference at byte:", first_diff
        print *, "STB byte:", int(stb_data(first_diff)), "(0x" // byte_to_hex(stb_data(first_diff)) // ")"
        print *, "Our byte:", int(our_data(first_diff)), "(0x" // byte_to_hex(our_data(first_diff)) // ")"
        print *, ""
        
        ! Show context around difference
        print *, "Context (10 bytes before and after):"
        call show_context(stb_data, our_data, first_diff, stb_size, our_size)
        
        ! Check if this is in scan data
        call find_scan_position(stb_data, first_diff)
    else
        if (stb_size /= our_size) then
            print *, "Files are identical up to", min(stb_size, our_size), "bytes"
            print *, "But sizes differ - one file is truncated"
        else
            print *, "Files are identical!"
        end if
    end if
    
contains

    subroutine read_binary_file(filename, data, size)
        character(len=*), intent(in) :: filename
        integer(1), allocatable, intent(out) :: data(:)
        integer, intent(out) :: size
        integer :: unit
        
        open(newunit=unit, file=filename, access='stream', form='unformatted', status='old')
        inquire(unit=unit, size=size)
        allocate(data(size))
        read(unit) data
        close(unit)
    end subroutine read_binary_file
    
    function byte_to_hex(b) result(hex_str)
        integer(1), intent(in) :: b
        character(len=2) :: hex_str
        write(hex_str, '(Z2.2)') b
    end function byte_to_hex
    
    subroutine show_context(stb_data, our_data, pos, stb_size, our_size)
        integer(1), intent(in) :: stb_data(:), our_data(:)
        integer, intent(in) :: pos, stb_size, our_size
        integer :: start_pos, end_pos, i
        
        start_pos = max(1, pos - 10)
        end_pos = min(min(stb_size, our_size), pos + 10)
        
        write(*, '(A)', advance='no') "STB: "
        do i = start_pos, end_pos
            if (i == pos) then
                write(*, '("[", Z2.2, "]", 1X)', advance='no') stb_data(i)
            else
                write(*, '(Z2.2, 1X)', advance='no') stb_data(i)
            end if
        end do
        print *
        
        write(*, '(A)', advance='no') "Our: "
        do i = start_pos, end_pos
            if (i == pos) then
                write(*, '("[", Z2.2, "]", 1X)', advance='no') our_data(i)
            else
                write(*, '(Z2.2, 1X)', advance='no') our_data(i)
            end if
        end do
        print *
    end subroutine show_context
    
    subroutine find_scan_position(data, pos)
        integer(1), intent(in) :: data(:)
        integer, intent(in) :: pos
        integer :: i, sos_pos
        
        ! Find SOS marker (0xFF 0xDA)
        sos_pos = 0
        do i = 1, size(data) - 1
            if (data(i) == int(Z'FF', 1) .and. data(i+1) == int(Z'DA', 1)) then
                sos_pos = i
                exit
            end if
        end do
        
        if (sos_pos > 0) then
            print *, "SOS marker at byte:", sos_pos
            if (pos > sos_pos + 12) then
                print *, "Difference is at scan data offset:", pos - (sos_pos + 12)
            else
                print *, "Difference is in SOS header"
            end if
        else
            print *, "Could not find SOS marker"
        end if
    end subroutine find_scan_position

end program debug_checker_difference
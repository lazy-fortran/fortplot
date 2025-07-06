program test_exact_stb_match
    use fortplot_jpeg, only: get_jpeg_data
    use, intrinsic :: iso_fortran_env, only: int8
    implicit none
    
    integer(int8), allocatable :: our_data(:), stb_data(:)
    integer(int8), allocatable :: rgb_data(:)
    integer :: i, our_size, stb_size
    logical :: match
    
    ! Create 8x8 uniform gray test data
    allocate(rgb_data(192))  ! 8*8*3 = 192
    do i = 1, 192
        rgb_data(i) = int(127, int8)
    end do
    
    ! Generate our JPEG
    call get_jpeg_data(8, 8, rgb_data, 90, our_data)
    our_size = size(our_data)
    
    ! Read STB reference (we'll create it with stb_debug program)
    call read_stb_reference(stb_data, stb_size)
    
    print *, "=== STB vs Our comparison ==="
    print *, "STB size:", stb_size
    print *, "Our size:", our_size
    print *, ""
    
    ! Compare sizes
    if (our_size /= stb_size) then
        print *, "FAIL: Size mismatch!"
        print *, "Difference:", our_size - stb_size, "bytes"
        return
    end if
    
    ! Compare byte by byte
    match = .true.
    do i = 1, min(our_size, stb_size)
        if (our_data(i) /= stb_data(i)) then
            if (match) then
                print *, "FAIL: First difference at byte", i
                print *, "Hex offset:", i-1
                write(*,'(A,I4,A,Z2.2,A,Z2.2)') "Byte ", i, ": STB=", stb_data(i), " Our=", our_data(i)
                match = .false.
            end if
            
            ! Show context around difference
            if (i >= max(1, i-5) .and. i <= min(our_size, i+5)) then
                write(*,'(A,I4,A,Z2.2,A,Z2.2)') "Byte ", i, ": STB=", stb_data(i), " Our=", our_data(i)
            end if
        end if
    end do
    
    if (match) then
        print *, "SUCCESS: Perfect match with STB!"
    else
        print *, ""
        print *, "Focus on scan data (after FF DA):"
        call show_scan_data_diff(stb_data, our_data, our_size)
    end if
    
contains
    
    subroutine read_stb_reference(stb_data, stb_size)
        integer(int8), allocatable, intent(out) :: stb_data(:)
        integer, intent(out) :: stb_size
        integer :: unit, iostat
        
        ! Try to read stb_debug.jpg
        open(newunit=unit, file='stb_debug.jpg', form='unformatted', access='stream', iostat=iostat)
        if (iostat /= 0) then
            print *, "ERROR: Cannot read stb_debug.jpg"
            print *, "Run: make debug ARGS=debug_scan_bits first"
            stb_size = 0
            return
        end if
        
        ! Get file size
        inquire(unit=unit, size=stb_size)
        allocate(stb_data(stb_size))
        
        ! Read all data
        read(unit) stb_data
        close(unit)
    end subroutine read_stb_reference
    
    subroutine show_scan_data_diff(stb_data, our_data, data_size)
        integer(int8), intent(in) :: stb_data(:), our_data(:)
        integer, intent(in) :: data_size
        integer :: i, sos_pos
        
        ! Find SOS marker (FF DA)
        sos_pos = 0
        do i = 1, data_size-1
            if (stb_data(i) == int(z'FF', 1) .and. stb_data(i+1) == int(z'DA', 1)) then
                sos_pos = i + 14  ! Skip SOS header (12 bytes + 2 for marker)
                exit
            end if
        end do
        
        if (sos_pos > 0) then
            print *, "Scan data starts at byte", sos_pos
            print *, "Last 10 bytes of scan data:"
            do i = max(sos_pos, data_size-15), data_size-2  ! Exclude EOI
                write(*,'(A,I4,A,Z2.2,A,Z2.2)') "Byte ", i, ": STB=", stb_data(i), " Our=", our_data(i)
            end do
        end if
    end subroutine show_scan_data_diff
    
end program test_exact_stb_match
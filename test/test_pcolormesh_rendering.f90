program test_pcolormesh_rendering
    !! Test pcolormesh rendering functionality
    !! Verifies that pcolormesh produces visible output in all backends
    
    use iso_fortran_env, only: wp => real64
    use fortplot
    implicit none
    
    logical :: test_passed
    character(len=256) :: error_msg
    
    test_passed = .true.
    error_msg = ""
    
    ! Test basic pcolormesh rendering
    call test_basic_pcolormesh()
    
    ! Test ASCII backend rendering  
    call test_ascii_pcolormesh()
    
    ! Test PNG backend rendering
    call test_png_pcolormesh()
    
    ! Test PDF backend rendering
    call test_pdf_pcolormesh()
    
    if (test_passed) then
        print *, "PASS: All pcolormesh rendering tests passed"
        stop 0
    else
        print *, "FAIL: ", trim(error_msg)
        stop 1
    end if
    
contains

    subroutine test_basic_pcolormesh()
        !! Test basic pcolormesh with simple gradient
        real(wp) :: x(6), y(5), c(4, 5)
        integer :: i, j
        
        print *, "Testing basic pcolormesh..."
        
        ! Create coordinate arrays
        do i = 1, 6
            x(i) = real(i-1, wp) * 0.4_wp
        end do
        do i = 1, 5
            y(i) = real(i-1, wp) * 0.3_wp
        end do
        
        ! Create test data - simple gradient
        do i = 1, 4
            do j = 1, 5
                c(i, j) = real(i, wp) + real(j, wp) * 0.5_wp
            end do
        end do
        
        ! Create pcolormesh plot
        call figure(figsize=[6.4_wp, 4.8_wp])
        call title('Test Pcolormesh')
        call xlabel('X')
        call ylabel('Y')
        call pcolormesh(x, y, c, colormap='viridis')
        call savefig('test_pcolormesh_basic.png')
        
        ! Verify file was created
        if (.not. file_exists('test_pcolormesh_basic.png')) then
            test_passed = .false.
            error_msg = "Basic pcolormesh PNG not created"
        end if
    end subroutine test_basic_pcolormesh
    
    subroutine test_ascii_pcolormesh()
        !! Test ASCII backend produces visible output
        real(wp) :: x(4), y(4), c(3, 3)
        integer :: i, j
        character(len=10000) :: ascii_content
        character(len=256) :: line
        integer :: unit, iostat
        
        print *, "Testing ASCII pcolormesh..."
        
        ! Create smaller grid for ASCII
        do i = 1, 4
            x(i) = real(i-1, wp)
        end do
        do i = 1, 4
            y(i) = real(i-1, wp)
        end do
        
        ! Create test data
        do i = 1, 3
            do j = 1, 3
                c(i, j) = real(i * j, wp)
            end do
        end do
        
        ! Create pcolormesh plot
        call figure()
        call pcolormesh(x, y, c)
        call savefig('test_pcolormesh_ascii.txt')
        
        ! Read and verify ASCII output contains density characters
        open(newunit=unit, file='test_pcolormesh_ascii.txt', status='old', iostat=iostat)
        if (iostat /= 0) then
            test_passed = .false.
            error_msg = "ASCII pcolormesh file not created"
            return
        end if
        
        ! Read entire file content
        ascii_content = ""
        do
            read(unit, '(A)', iostat=iostat) line
            if (iostat /= 0) exit
            ascii_content = trim(ascii_content) // line
        end do
        close(unit)
        
        ! Check for presence of density characters from ASCII_CHARS
        if (index(ascii_content, '#') == 0 .and. &
            index(ascii_content, '*') == 0 .and. &
            index(ascii_content, '+') == 0 .and. &
            index(ascii_content, '=') == 0) then
            test_passed = .false.
            error_msg = "ASCII pcolormesh contains no visible data"
        end if
    end subroutine test_ascii_pcolormesh
    
    subroutine test_png_pcolormesh()
        !! Test PNG backend produces non-empty file
        real(wp) :: x(4), y(4), c(3, 3)
        integer :: i, j, file_size
        
        print *, "Testing PNG pcolormesh..."
        
        ! Create grid
        do i = 1, 4
            x(i) = real(i-1, wp)
        end do
        do i = 1, 4
            y(i) = real(i-1, wp)
        end do
        
        ! Create test data
        do i = 1, 3
            do j = 1, 3
                c(i, j) = real(i + j, wp)
            end do
        end do
        
        ! Create pcolormesh plot
        call figure(figsize=[4.0_wp, 3.0_wp])
        call pcolormesh(x, y, c, colormap='plasma')
        call savefig('test_pcolormesh.png')
        
        ! Check file size is reasonable (not empty)
        file_size = get_file_size('test_pcolormesh.png')
        if (file_size < 1000) then  ! PNG header alone is ~100 bytes
            test_passed = .false.
            error_msg = "PNG pcolormesh file too small or empty"
        end if
    end subroutine test_png_pcolormesh
    
    subroutine test_pdf_pcolormesh()
        !! Test PDF backend produces valid output
        real(wp) :: x(3), y(3), c(2, 2)
        integer :: i, j, file_size
        
        print *, "Testing PDF pcolormesh..."
        
        ! Create minimal grid
        do i = 1, 3
            x(i) = real(i-1, wp) * 2.0_wp
        end do
        do i = 1, 3
            y(i) = real(i-1, wp) * 2.0_wp
        end do
        
        ! Create test data
        c(1, 1) = 0.0_wp
        c(1, 2) = 0.5_wp
        c(2, 1) = 0.5_wp
        c(2, 2) = 1.0_wp
        
        ! Create pcolormesh plot
        call figure()
        call pcolormesh(x, y, c, colormap='coolwarm')
        call savefig('test_pcolormesh.pdf')
        
        ! Check file exists and has content
        file_size = get_file_size('test_pcolormesh.pdf')
        if (file_size < 500) then  ! PDF header is ~200 bytes minimum
            test_passed = .false.
            error_msg = "PDF pcolormesh file too small or empty"
        end if
    end subroutine test_pdf_pcolormesh
    
    logical function file_exists(filename)
        character(len=*), intent(in) :: filename
        inquire(file=filename, exist=file_exists)
    end function file_exists
    
    integer function get_file_size(filename)
        character(len=*), intent(in) :: filename
        integer :: unit, iostat
        integer(8) :: size
        
        inquire(file=filename, size=size)
        get_file_size = int(size)
    end function get_file_size

end program test_pcolormesh_rendering
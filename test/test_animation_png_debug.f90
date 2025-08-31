program test_animation_png_debug
    use fortplot
    use fortplot_animation
    implicit none

    type(figure_t) :: fig
    type(animation_t) :: anim
    real(wp), dimension(10) :: x, y
    integer :: i
    integer(1), allocatable :: png_data(:)
    integer :: status
    
    print *, "=== ANIMATION PNG CORRUPTION DEBUG TEST ==="
    
    ! Create simple test data
    do i = 1, 10
        x(i) = real(i-1, wp) * 0.1_wp
        y(i) = sin(x(i))
    end do
    
    ! Create figure and plot
    call figure(figsize=[4.0_wp, 3.0_wp])
    call add_plot(x, y, label='test wave')
    call title('PNG Debug Test')
    call xlabel('x')
    call ylabel('y')
    
    print *, "1. Testing direct PNG save from figure..."
    call savefig('test_direct_png.png')
    
    ! Check if direct PNG works
    call check_png_file_validity('test_direct_png.png', 'Direct PNG save')
    
    print *, "2. Testing PNG extraction for animation..."
    
    ! Setup PNG backend for animation
    call fig%setup_png_backend_for_animation()
    
    ! Extract PNG data 
    call fig%extract_png_data_for_animation(png_data, status)
    
    print '(A,I0)', "PNG extraction status: ", status
    if (allocated(png_data)) then
        print '(A,I0,A)', "PNG data size: ", size(png_data), " bytes"
        
        ! Write PNG data to file
        call write_png_data_to_file(png_data, 'test_animation_png.png')
        
        ! Check validity
        call check_png_file_validity('test_animation_png.png', 'Animation PNG extraction')
        
        ! Examine PNG header
        call examine_png_header(png_data)
    else
        print *, "ERROR: PNG data not allocated!"
    end if
    
    print *, "3. Testing animation frame generation..."
    
    ! Create simple animation
    anim = FuncAnimation(update_simple, frames=3, interval=100, fig=fig)
    
    ! Try to save PNG sequence (which should work if PNG extraction works)
    call anim%save_frame_sequence("test_frame_")
    
    ! Check generated frames
    call check_png_file_validity('test_frame_0.png', 'Animation frame 0')
    call check_png_file_validity('test_frame_1.png', 'Animation frame 1')
    call check_png_file_validity('test_frame_2.png', 'Animation frame 2')
    
    print *, "=== DEBUG TEST COMPLETE ==="

contains
    
    subroutine update_simple(frame)
        integer, intent(in) :: frame
        real(wp) :: phase
        
        phase = real(frame, wp) * 0.5_wp
        y = sin(x + phase)
        call set_ydata(y)
    end subroutine update_simple
    
    subroutine write_png_data_to_file(png_data, filename)
        integer(1), intent(in) :: png_data(:)
        character(len=*), intent(in) :: filename
        integer :: unit, i
        
        open(newunit=unit, file=filename, form='unformatted', access='stream')
        do i = 1, size(png_data)
            write(unit) png_data(i)
        end do
        close(unit)
    end subroutine write_png_data_to_file
    
    subroutine check_png_file_validity(filename, test_name)
        character(len=*), intent(in) :: filename, test_name
        logical :: file_exists
        integer :: file_size
        
        inquire(file=filename, exist=file_exists, size=file_size)
        
        print '(A,A,A)', "Checking ", trim(test_name), ":"
        if (file_exists) then
            print '(A,I0,A)', "  File exists, size: ", file_size, " bytes"
            if (file_size == 0) then
                print *, "  ERROR: File is empty (0 bytes)!"
            else if (file_size < 100) then
                print *, "  WARNING: File too small to be valid PNG!"
            else
                print *, "  File size looks reasonable"
            end if
        else
            print *, "  ERROR: File does not exist!"
        end if
    end subroutine check_png_file_validity
    
    subroutine examine_png_header(png_data)
        integer(1), intent(in) :: png_data(:)
        integer :: i
        
        print *, "Examining PNG header (first 20 bytes):"
        if (size(png_data) >= 20) then
            write(*, '(A)', advance='no') "  Hex: "
            do i = 1, 20
                write(*, '(Z2.2,1X)', advance='no') png_data(i)
            end do
            print *
            
            ! Check PNG signature
            if (size(png_data) >= 8) then
                if (png_data(1) == -119 .and. png_data(2) == 80 .and. &
                    png_data(3) == 78 .and. png_data(4) == 71) then
                    print *, "  PNG signature: VALID"
                else
                    print *, "  PNG signature: INVALID!"
                    print '(A,4(Z2.2,1X))', "  Expected: 89 50 4E 47, Got: ", &
                        png_data(1), png_data(2), png_data(3), png_data(4)
                end if
            end if
        else
            print *, "  PNG data too short for header analysis"
        end if
    end subroutine examine_png_header
    
end program test_animation_png_debug
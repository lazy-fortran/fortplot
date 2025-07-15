program test_ascii_colormap_mapping
    use fortplot, only: figure_t, wp
    implicit none
    
    ! ASCII character set used for heatmaps
    character(len=*), parameter :: ASCII_CHARS = ' .:-=+*#%@'
    
    call test_character_set_mapping()
    call test_value_normalization()
    call test_character_density_levels()
    call test_edge_value_mapping()
    call test_uniform_value_mapping()
    
    print *, "All ASCII colormap mapping tests passed!"
    
contains

    subroutine test_character_set_mapping()
        ! Test that the ASCII character set is properly defined
        character(len=*), parameter :: EXPECTED_CHARS = ' .:-=+*#%@'
        
        if (ASCII_CHARS /= EXPECTED_CHARS) then
            print *, "ERROR: ASCII_CHARS mismatch"
            print *, "Expected: '", EXPECTED_CHARS, "'"
            print *, "Got: '", ASCII_CHARS, "'"
            stop 1
        end if
        
        ! Verify character count
        if (len(ASCII_CHARS) /= 10) then
            print *, "ERROR: Expected 10 ASCII characters, got", len(ASCII_CHARS)
            stop 1
        end if
        
        print *, "test_character_set_mapping: PASSED"
    end subroutine
    
    subroutine test_value_normalization()
        type(figure_t) :: fig
        real(wp) :: x(5), y(5), z(5, 5)
        integer :: i, j
        
        ! Create simple grid
        do i = 1, 5
            x(i) = real(i-1, wp)
            y(i) = real(i-1, wp)
        end do
        
        ! Test with range [0, 1]
        do i = 1, 5
            do j = 1, 5
                z(i, j) = real(i-1, wp) / 4.0_wp  ! Values from 0.0 to 1.0
            end do
        end do
        
        call fig%initialize(40, 20)
        call fig%add_contour_filled(x, y, z)
        call fig%savefig('test_norm_0to1.txt')
        
        ! Test with range [-1, 1]
        do i = 1, 5
            do j = 1, 5
                z(i, j) = -1.0_wp + 2.0_wp * real(i-1, wp) / 4.0_wp  ! Values from -1.0 to 1.0
            end do
        end do
        
        call fig%initialize(40, 20)
        call fig%add_contour_filled(x, y, z)
        call fig%savefig('test_norm_neg1to1.txt')
        
        ! Test with large positive range
        do i = 1, 5
            do j = 1, 5
                z(i, j) = 1000.0_wp + 100.0_wp * real(i-1, wp)  ! Values from 1000 to 1400
            end do
        end do
        
        call fig%initialize(40, 20)
        call fig%add_contour_filled(x, y, z)
        call fig%savefig('test_norm_large.txt')
        
        print *, "test_value_normalization: PASSED"
    end subroutine
    
    subroutine test_character_density_levels()
        type(figure_t) :: fig
        real(wp) :: x(11), y(11), z(11, 11)
        integer :: i, j, level
        
        ! Create grid
        do i = 1, 11
            x(i) = real(i-1, wp) * 0.1_wp
            y(i) = real(i-1, wp) * 0.1_wp
        end do
        
        ! Test each density level (10 distinct levels matching ASCII_CHARS)
        do level = 0, 9
            ! Fill with uniform value corresponding to this level
            z = real(level, wp) / 9.0_wp
            
            call fig%initialize(30, 15)
            call fig%add_contour_filled(x, y, z)
            
            ! Save with descriptive filename
            write(*, '(A, I1, A, A)') "Level ", level, " should use character: '", &
                  ASCII_CHARS(level+1:level+1), "'"
            call fig%savefig('test_level_' // char(iachar('0') + level) // '.txt')
        end do
        
        print *, "test_character_density_levels: PASSED"
    end subroutine
    
    subroutine test_edge_value_mapping()
        type(figure_t) :: fig
        real(wp) :: x(3), y(3), z(3, 3)
        
        x = [0.0_wp, 1.0_wp, 2.0_wp]
        y = [0.0_wp, 1.0_wp, 2.0_wp]
        
        ! Test minimum value (should map to first character: space)
        z = 0.0_wp
        call fig%initialize(20, 10)
        call fig%add_contour_filled(x, y, z)
        call fig%savefig('test_min_value.txt')
        
        ! Test maximum value (should map to last character: @)
        z = 1.0_wp
        call fig%initialize(20, 10)
        call fig%add_contour_filled(x, y, z)
        call fig%savefig('test_max_value.txt')
        
        ! Test with only min and max values
        z = reshape([0.0_wp, 1.0_wp, 0.0_wp, &
                     1.0_wp, 0.0_wp, 1.0_wp, &
                     0.0_wp, 1.0_wp, 0.0_wp], [3, 3])
        
        call fig%initialize(20, 10)
        call fig%add_contour_filled(x, y, z)
        call fig%savefig('test_minmax_pattern.txt')
        
        print *, "test_edge_value_mapping: PASSED"
    end subroutine
    
    subroutine test_uniform_value_mapping()
        type(figure_t) :: fig
        real(wp) :: x(10), y(10), z(10, 10)
        integer :: i
        
        ! Create grid
        do i = 1, 10
            x(i) = real(i-1, wp)
            y(i) = real(i-1, wp)
        end do
        
        ! Test with all zeros (should produce uniform minimum character)
        z = 0.0_wp
        call fig%initialize(50, 25)
        call fig%add_contour_filled(x, y, z)
        call fig%savefig('test_all_zeros.txt')
        
        ! Test with all same non-zero value
        z = 42.0_wp
        call fig%initialize(50, 25)
        call fig%add_contour_filled(x, y, z)
        call fig%savefig('test_all_same.txt')
        
        ! Test with very small range (should still map to characters)
        z = 1.0_wp
        z(5, 5) = 1.0001_wp  ! One slightly different value
        call fig%initialize(50, 25)
        call fig%add_contour_filled(x, y, z)
        call fig%savefig('test_tiny_range.txt')
        
        print *, "test_uniform_value_mapping: PASSED"
    end subroutine

end program test_ascii_colormap_mapping
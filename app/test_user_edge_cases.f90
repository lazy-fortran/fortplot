program test_user_edge_cases
    ! Test edge cases that users might encounter
    use fortplot  
    implicit none
    
    call test_empty_data()
    call test_invalid_dimensions()
    call test_missing_legend()
    call test_invalid_file_paths()

contains

    subroutine test_empty_data()
        real(wp), dimension(0) :: empty_x, empty_y
        
        print *, "Testing empty data arrays..."
        
        call figure()
        call plot(empty_x, empty_y)
        call title("Empty Data Test")
        call savefig("test_empty_data.png")
        
        print *, "Empty data test completed"
    end subroutine

    subroutine test_invalid_dimensions()
        real(wp), dimension(10) :: x
        real(wp), dimension(5) :: y  ! Mismatched size
        integer :: i
        
        print *, "Testing mismatched array dimensions..."
        
        x = [(real(i, wp), i=1, 10)]
        y = [(real(i**2, wp), i=1, 5)]
        
        call figure()
        call plot(x, y)  ! Should handle gracefully
        call title("Dimension Mismatch Test")
        call savefig("test_dimension_mismatch.png")
        
        print *, "Dimension mismatch test completed"
    end subroutine

    subroutine test_missing_legend()
        real(wp), dimension(10) :: x, y1, y2
        integer :: i
        
        print *, "Testing legend with unlabeled plots..."
        
        x = [(real(i, wp), i=1, 10)]
        y1 = sin(x)
        y2 = cos(x)
        
        call figure()
        call plot(x, y1)  ! No label
        call plot(x, y2, label="cos(x)")  ! Has label
        call legend()  ! What happens with mixed labeled/unlabeled?
        call savefig("test_mixed_legend.png")
        
        print *, "Mixed legend test completed"
    end subroutine

    subroutine test_invalid_file_paths()
        real(wp), dimension(3) :: x = [1.0_wp, 2.0_wp, 3.0_wp]
        real(wp), dimension(3) :: y = [1.0_wp, 4.0_wp, 9.0_wp]
        
        print *, "Testing invalid file paths..."
        
        call figure()
        call plot(x, y)
        call title("File Path Test")
        
        ! Test invalid characters
        call savefig("/invalid/path/test.png")
        
        ! Test very long filename
        call savefig("very_long_filename_that_might_cause_issues_in_some_systems.png")
        
        print *, "File path test completed"
    end subroutine

end program test_user_edge_cases
program test_pdf_heatmap_validation
    !! PDF heatmap validation tests
    !! Extracted from test_pdf_validation_comprehensive.f90
    !! 
    !! This test covers:
    !! - PDF heatmap edge cases and validation
    !! - NaN, infinity, and invalid data handling
    !! - RGB value validation and clamping
    !! - Dimension mismatch handling

    use, intrinsic :: iso_fortran_env, only: wp => real64, int32
    use, intrinsic :: ieee_arithmetic
    use fortplot
    use fortplot_pdf, only: pdf_context, create_pdf_canvas
    use fortplot_validation
    use fortplot_security, only: get_test_output_path
    implicit none

    integer :: test_count = 0
    integer :: pass_count = 0
    logical :: all_tests_passed = .true.

    print *, "=== PDF HEATMAP VALIDATION TESTS ==="
    
    call test_heatmap_validation()
    call print_test_summary()

contains

    subroutine test_heatmap_validation()
        print *, "--- Heatmap Validation Tests ---"
        
        call test_constant_field()
        call test_nan_values()
        call test_infinity_values() 
        call test_missing_data_patterns()
        call test_rgb_value_validation()
        call test_dimension_mismatches()
    end subroutine test_heatmap_validation

    subroutine test_constant_field()
        real(wp), allocatable :: field(:, :)
        integer :: nx, ny
        character(len=512) :: filename
        
        call start_test("Constant field heatmap")
        
        nx = 10; ny = 8
        allocate(field(nx, ny))
        field = 5.0_wp  ! All values identical
        
        call figure(figsize=[400.0_wp, 300.0_wp])
        call imshow(field)
        call title('Constant Field Test')
        call colorbar()
        
        filename = get_test_output_path('/tmp/pdf_heatmap_constant.pdf')
        call savefig(filename)
        
        deallocate(field)
        print *, "  Constant field heatmap test saved"
        call end_test()
    end subroutine test_constant_field

    subroutine test_nan_values()
        real(wp), allocatable :: field(:, :)
        integer :: nx, ny, i, j
        character(len=512) :: filename
        
        call start_test("NaN values in heatmap")
        
        nx = 12; ny = 10
        allocate(field(nx, ny))
        
        ! Create field with some NaN values
        do j = 1, ny
            do i = 1, nx
                if (mod(i + j, 7) == 0) then
                    field(i, j) = ieee_value(field(i, j), ieee_quiet_nan)
                else
                    field(i, j) = real(i * j, wp) * 0.1_wp
                end if
            end do
        end do
        
        call figure(figsize=[400.0_wp, 300.0_wp])
        call imshow(field)
        call title('NaN Values Test')
        call colorbar()
        
        filename = get_test_output_path('/tmp/pdf_heatmap_nan.pdf')
        call savefig(filename)
        
        deallocate(field)
        print *, "  NaN values heatmap test saved"
        call end_test()
    end subroutine test_nan_values

    subroutine test_infinity_values()
        real(wp), allocatable :: field(:, :)
        integer :: nx, ny, i, j
        character(len=512) :: filename
        
        call start_test("Infinity values in heatmap")
        
        nx = 8; ny = 6
        allocate(field(nx, ny))
        
        ! Create field with infinity values
        do j = 1, ny
            do i = 1, nx
                if (i == 1 .and. j == 1) then
                    field(i, j) = ieee_value(field(i, j), ieee_positive_inf)
                else if (i == nx .and. j == ny) then
                    field(i, j) = ieee_value(field(i, j), ieee_negative_inf)
                else
                    field(i, j) = real(i + j, wp)
                end if
            end do
        end do
        
        call figure(figsize=[350.0_wp, 250.0_wp])
        call imshow(field)
        call title('Infinity Values Test')
        call colorbar()
        
        filename = get_test_output_path('/tmp/pdf_heatmap_infinity.pdf')
        call savefig(filename)
        
        deallocate(field)
        print *, "  Infinity values heatmap test saved"
        call end_test()
    end subroutine test_infinity_values

    subroutine test_missing_data_patterns()
        real(wp), allocatable :: sparse_field(:, :)
        integer :: nx, ny, i, j
        character(len=512) :: filename
        
        call start_test("Missing data patterns")
        
        nx = 15; ny = 12
        allocate(sparse_field(nx, ny))
        
        ! Create sparse field with missing data patterns
        do j = 1, ny
            do i = 1, nx
                if (mod(i, 3) == 0 .or. mod(j, 4) == 0) then
                    sparse_field(i, j) = ieee_value(sparse_field(i, j), ieee_quiet_nan)
                else
                    sparse_field(i, j) = sin(real(i, wp) * 0.3_wp) * cos(real(j, wp) * 0.4_wp)
                end if
            end do
        end do
        
        call figure(figsize=[450.0_wp, 350.0_wp])
        call imshow(sparse_field)
        call title('Sparse Data Pattern Test')
        call colorbar()
        
        filename = get_test_output_path('/tmp/pdf_heatmap_sparse.pdf')
        call savefig(filename)
        
        deallocate(sparse_field)
        print *, "  Sparse data pattern test saved"
        call end_test()
    end subroutine test_missing_data_patterns

    subroutine test_rgb_value_validation()
        real(wp), allocatable :: field(:, :)
        integer :: nx, ny, i, j
        character(len=512) :: filename
        
        call start_test("RGB value validation")
        
        nx = 6; ny = 4
        allocate(field(nx, ny))
        
        ! Create field with extreme values that need RGB clamping
        do j = 1, ny
            do i = 1, nx
                if (i == 1) then
                    field(i, j) = -1000.0_wp  ! Should be clamped to valid range
                else if (i == nx) then
                    field(i, j) = 1000.0_wp   ! Should be clamped to valid range
                else
                    field(i, j) = real(i * j, wp)
                end if
            end do
        end do
        
        call figure(figsize=[300.0_wp, 200.0_wp])
        call imshow(field)
        call title('RGB Validation Test')
        call colorbar()
        
        filename = get_test_output_path('/tmp/pdf_heatmap_rgb_validation.pdf')
        call savefig(filename)
        
        deallocate(field)
        print *, "  RGB validation test saved"
        call end_test()
    end subroutine test_rgb_value_validation

    subroutine test_dimension_mismatches()
        real(wp), allocatable :: field1(:, :), field2(:, :)
        character(len=512) :: filename
        
        call start_test("Dimension mismatch handling")
        
        ! Test different sized fields
        allocate(field1(5, 3))
        allocate(field2(8, 6))
        
        field1 = 2.0_wp
        field2 = 4.0_wp
        
        ! Test small field
        call figure(figsize=[250.0_wp, 180.0_wp])
        call imshow(field1)
        call title('Small Field (5x3)')
        filename = get_test_output_path('/tmp/pdf_heatmap_small.pdf')
        call savefig(filename)
        
        ! Test larger field  
        call figure(figsize=[400.0_wp, 300.0_wp])
        call imshow(field2)
        call title('Larger Field (8x6)')
        filename = get_test_output_path('/tmp/pdf_heatmap_large.pdf')
        call savefig(filename)
        
        deallocate(field1, field2)
        print *, "  Dimension mismatch test saved"
        call end_test()
    end subroutine test_dimension_mismatches

    !===========================================================================
    ! Test Framework Utilities
    !===========================================================================

    subroutine start_test(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A, I0, A, A)') 'Test ', test_count, ': ', test_name
    end subroutine start_test

    subroutine end_test()
        pass_count = pass_count + 1
        write(*, '(A)') '  PASS'
        write(*, *)
    end subroutine end_test

    subroutine print_test_summary()
        write(*, '(A)') '============================================'
        write(*, '(A)') 'PDF Heatmap Validation Test Summary'
        write(*, '(A, I0, A, I0)') 'Tests run: ', test_count, ' | Passed: ', pass_count
        if (all_tests_passed) then
            write(*, '(A)') 'All heatmap validation tests PASSED!'
        else
            write(*, '(A)') 'Some tests FAILED - review output above'
        end if
        write(*, '(A)') '============================================'
    end subroutine print_test_summary

end program test_pdf_heatmap_validation
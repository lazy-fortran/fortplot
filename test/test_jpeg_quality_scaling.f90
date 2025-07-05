program test_jpeg_quality_scaling
    use iso_fortran_env, only: int32
    implicit none
    
    call test_quality_factor_formula()
    call test_quantization_table_scaling()
    
    print *, "All quality scaling tests passed!"
    
contains
    
    subroutine test_quality_factor_formula()
        integer(int32) :: expected_factor, actual_factor
        
        ! Test quality < 50: factor = 5000/quality
        call check_quality_factor(1, 5000)
        call check_quality_factor(10, 500)
        call check_quality_factor(25, 200)
        call check_quality_factor(49, 102)
        
        ! Test quality >= 50: factor = 200 - quality*2
        call check_quality_factor(50, 100)
        call check_quality_factor(75, 50)
        call check_quality_factor(85, 30)
        call check_quality_factor(90, 20)
        call check_quality_factor(100, 0)
        
        print *, "Quality factor formula test passed"
    end subroutine
    
    subroutine check_quality_factor(quality, expected)
        integer(int32), intent(in) :: quality, expected
        integer(int32) :: factor
        
        if (quality < 50) then
            factor = 5000 / quality
        else
            factor = 200 - quality * 2
        end if
        
        if (factor /= expected) then
            print *, "FAIL: Quality", quality, "expected factor", expected, "got", factor
            error stop
        end if
    end subroutine
    
    subroutine test_quantization_table_scaling()
        integer(int32), parameter :: std_luma(64) = [ &
            16, 11, 10, 16, 24, 40, 51, 61, &
            12, 12, 14, 19, 26, 58, 60, 55, &
            14, 13, 16, 24, 40, 57, 69, 56, &
            14, 17, 22, 29, 51, 87, 80, 62, &
            18, 22, 37, 56, 68, 109, 103, 77, &
            24, 35, 55, 64, 81, 104, 113, 92, &
            49, 64, 78, 87, 103, 121, 120, 101, &
            72, 92, 95, 98, 112, 100, 103, 99 &
        ]
        
        integer(int32) :: scaled_table(64)
        integer(int32) :: i, expected_val, actual_val
        
        ! Test quality = 50 (factor = 100)
        call scale_quantization_table(std_luma, 50, scaled_table)
        
        ! At quality 50, values should be clamped between 1 and 255
        do i = 1, 64
            if (scaled_table(i) < 1 .or. scaled_table(i) > 255) then
                print *, "FAIL: Scaled value", scaled_table(i), "out of range [1,255]"
                error stop
            end if
        end do
        
        ! Test quality = 100 (factor = 0, should use 1)
        call scale_quantization_table(std_luma, 100, scaled_table)
        
        ! At quality 100, all values should be 1
        do i = 1, 64
            if (scaled_table(i) /= 1) then
                print *, "FAIL: Quality 100 should give all 1s, got", scaled_table(i)
                error stop
            end if
        end do
        
        print *, "Quantization table scaling test passed"
    end subroutine
    
    subroutine scale_quantization_table(base_table, quality, scaled_table)
        integer(int32), intent(in) :: base_table(64), quality
        integer(int32), intent(out) :: scaled_table(64)
        
        integer(int32) :: factor, i, temp
        
        if (quality < 50) then
            factor = 5000 / quality
        else
            factor = 200 - quality * 2
        end if
        
        ! Ensure factor is at least 1
        if (factor < 1) factor = 1
        
        do i = 1, 64
            temp = (base_table(i) * factor + 50) / 100
            if (temp < 1) temp = 1
            if (temp > 255) temp = 255
            scaled_table(i) = temp
        end do
    end subroutine
    
end program
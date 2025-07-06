program verify_stb_bits
    implicit none
    
    ! Test the STB bit calculation exactly
    call test_negative_dc_values()
    
contains

    subroutine test_negative_dc_values()
        integer :: test_values(10) = [-50, -12, -3, -1, -6, -24, -9, -20, -17, -16]
        integer :: i, val, category, tmp1, value_bits
        
        print *, "Testing STB bit calculation for negative DC values:"
        print *, ""
        
        do i = 1, size(test_values)
            val = test_values(i)
            
            ! STB calculation: stbiw__jpg_calcBits
            tmp1 = merge(-val, val, val < 0)  ! tmp1 = val < 0 ? -val : val;
            val = merge(val-1, val, val < 0)  ! val = val < 0 ? val-1 : val;
            
            ! Calculate category (bits[1])
            category = 1
            do while (ishft(tmp1, -category) > 0)
                category = category + 1
            end do
            
            ! Calculate value bits
            value_bits = iand(val, ishft(1, category) - 1)  ! bits[0] = val & ((1<<bits[1])-1);
            
            print '(A,I4,A,I2,A,B0,A,I2,A)', "DC value ", test_values(i), &
                  " -> category ", category, ", value_bits ", value_bits, &
                  " (", category, " bits)"
        end do
        
        print *, ""
        print *, "Checking specific case: DC = -50"
        val = -50
        tmp1 = -val  ! tmp1 = 50
        val = val - 1  ! val = -51
        
        ! Category calculation
        category = 1
        do while (ishft(tmp1, -category) > 0)
            category = category + 1
        end do
        
        print '(A,I0)', "tmp1 = ", tmp1
        print '(A,I0)', "val after val-1 = ", val
        print '(A,I0)', "category = ", category
        print '(A,I0)', "mask = 2^category - 1 = ", ishft(1, category) - 1
        print '(A,B0)', "val in binary = ", val
        print '(A,B0)', "mask in binary = ", ishft(1, category) - 1
        print '(A,B0)', "value_bits = val & mask = ", iand(val, ishft(1, category) - 1)
        
    end subroutine test_negative_dc_values

end program verify_stb_bits
program test_jpeg_ac_encoding
    use iso_fortran_env, only: int32
    implicit none
    
    call test_run_length_encoding()
    call test_m16zeroes_marker()
    call test_eob_detection()
    
    print *, "All AC encoding tests passed!"
    
contains
    
    subroutine test_run_length_encoding()
        integer(int32) :: block(64), encoded_pairs(128, 2), pair_count
        integer(int32) :: i
        
        ! Test block with mixed zeros and non-zeros
        ! Zigzag order: 0,1,8,16,9,2,3,10,17,24,32,25,18,11,4,5...
        block = 0
        block(1) = 42      ! DC coefficient (zigzag index 0)
        block(2) = 5       ! AC coefficient at zigzag index 1 (block position 1)
        block(3) = -3      ! AC coefficient at zigzag index 5 (block position 2)
        block(12) = 7      ! AC coefficient at zigzag index 7 (block position 11)
        
        call encode_ac_coefficients(block, encoded_pairs, pair_count)
        
        
        ! Expected: (0,5), (3,-3), (7,7), EOB
        if (pair_count /= 4) then
            print *, "FAIL: Expected 4 pairs, got", pair_count
            error stop
        end if
        
        ! Check first pair: 0 zeros, value 5
        if (encoded_pairs(1, 1) /= 0 .or. encoded_pairs(1, 2) /= 5) then
            print *, "FAIL: First pair should be (0,5), got", encoded_pairs(1, 1), encoded_pairs(1, 2)
            error stop
        end if
        
        ! Check second pair: 3 zeros, value -3
        if (encoded_pairs(2, 1) /= 3 .or. encoded_pairs(2, 2) /= -3) then
            print *, "FAIL: Second pair should be (3,-3), got", encoded_pairs(2, 1), encoded_pairs(2, 2)
            error stop
        end if
        
        ! Check third pair: 7 zeros, value 7
        if (encoded_pairs(3, 1) /= 7 .or. encoded_pairs(3, 2) /= 7) then
            print *, "FAIL: Third pair should be (7,7), got", encoded_pairs(3, 1), encoded_pairs(3, 2)
            error stop
        end if
        
        print *, "Run-length encoding test passed"
    end subroutine
    
    subroutine test_m16zeroes_marker()
        integer(int32) :: block(64), encoded_pairs(128, 2), pair_count
        integer(int32) :: i
        
        ! Test block with > 15 consecutive zeros
        block = 0
        block(1) = 42    ! DC coefficient
        block(2) = 5     ! AC coefficient at position 1
        block(20) = 3    ! AC coefficient at position 19 (17 zeros before)
        
        call encode_ac_coefficients(block, encoded_pairs, pair_count)
        
        
        ! Expected: (0,5), (15,3), EOB
        if (pair_count /= 3) then
            print *, "FAIL: Expected 3 pairs for M16zeroes test, got", pair_count
            error stop
        end if
        
        ! Check M16zeroes with value: (15,3)
        if (encoded_pairs(2, 1) /= 15 .or. encoded_pairs(2, 2) /= 3) then
            print *, "FAIL: M16zeroes marker should be (15,3), got", encoded_pairs(2, 1), encoded_pairs(2, 2)
            error stop
        end if
        
        print *, "M16zeroes marker test passed"
    end subroutine
    
    subroutine test_eob_detection()
        integer(int32) :: block(64), encoded_pairs(128, 2), pair_count
        integer(int32) :: i
        
        ! Test block with all zeros after first few coefficients
        block = 0
        block(1) = 42    ! DC coefficient
        block(2) = 5     ! AC coefficient
        block(3) = -2    ! AC coefficient
        ! Rest are zeros
        
        call encode_ac_coefficients(block, encoded_pairs, pair_count)
        
        ! Expected: (0,5), (0,-2), EOB
        if (pair_count /= 3) then
            print *, "FAIL: Expected 3 pairs for EOB test, got", pair_count
            error stop
        end if
        
        ! Check EOB marker: (0,0)
        if (encoded_pairs(3, 1) /= 0 .or. encoded_pairs(3, 2) /= 0) then
            print *, "FAIL: EOB marker should be (0,0), got", encoded_pairs(3, 1), encoded_pairs(3, 2)
            error stop
        end if
        
        print *, "EOB detection test passed"
    end subroutine
    
    subroutine encode_ac_coefficients(block, encoded_pairs, pair_count)
        integer(int32), intent(in) :: block(64)
        integer(int32), intent(out) :: encoded_pairs(128, 2)
        integer(int32), intent(out) :: pair_count
        
        integer(int32) :: zigzag_order(64) = [ &
            0, 1, 8, 16, 9, 2, 3, 10, 17, 24, 32, 25, 18, 11, 4, 5, &
            12, 19, 26, 33, 40, 48, 41, 34, 27, 20, 13, 6, 7, 14, 21, 28, &
            35, 42, 49, 56, 57, 50, 43, 36, 29, 22, 15, 23, 30, 37, 44, 51, &
            58, 59, 52, 45, 38, 31, 39, 46, 53, 60, 61, 54, 47, 55, 62, 63 &
        ]
        
        integer(int32) :: i, zero_count, last_nonzero, coeff_idx
        
        pair_count = 0
        
        ! Find last non-zero coefficient
        last_nonzero = 0
        do i = 64, 2, -1  ! Skip DC coefficient
            coeff_idx = zigzag_order(i) + 1  ! Convert to 1-based indexing
            if (block(coeff_idx) /= 0) then
                last_nonzero = i
                exit
            end if
        end do
        
        ! Encode AC coefficients
        zero_count = 0
        do i = 2, last_nonzero  ! Skip DC coefficient
            coeff_idx = zigzag_order(i) + 1  ! Convert to 1-based indexing
            
            if (block(coeff_idx) == 0) then
                zero_count = zero_count + 1
                
                ! Handle more than 15 zeros
                if (zero_count == 16) then
                    pair_count = pair_count + 1
                    encoded_pairs(pair_count, 1) = 15  ! M16zeroes marker
                    encoded_pairs(pair_count, 2) = 0
                    zero_count = 0
                end if
            else
                pair_count = pair_count + 1
                encoded_pairs(pair_count, 1) = zero_count
                encoded_pairs(pair_count, 2) = block(coeff_idx)
                zero_count = 0
            end if
        end do
        
        ! Add EOB marker if we have any AC coefficients
        if (last_nonzero > 1) then
            pair_count = pair_count + 1
            encoded_pairs(pair_count, 1) = 0  ! EOB marker
            encoded_pairs(pair_count, 2) = 0
        end if
    end subroutine
    
end program
program debug_ac_loop_logic
    implicit none
    
    ! Test the AC coefficient loop logic with a simple case
    integer, parameter :: DC_VAL = 10
    integer :: DU(64)
    integer :: end0pos, i, startpos, nrzeroes
    
    ! Create a test case: DC=10, some zeros, then AC60=7, rest zeros
    DU = 0
    DU(1) = DC_VAL  ! DC coefficient
    DU(61) = 7      ! AC coefficient at position 60 (61 in 1-based)
    ! Rest are zeros
    
    print *, 'Test case: DC=', DC_VAL, ', AC60=7, rest=0'
    print *, 'DU array (first 10 elements):', DU(1:10)
    
    ! STB's end0pos finding logic (converted to 1-based)
    ! STB: end0pos = 63; for(; (end0pos>0)&&(DU[end0pos]==0); --end0pos) {}
    end0pos = 64  ! 1-based equivalent of STB's 63
    do while (end0pos > 1 .and. DU(end0pos) == 0)
        end0pos = end0pos - 1
    end do
    print *, 'Found end0pos =', end0pos
    
    ! STB's AC coefficient loop (converted to 1-based)
    ! STB: for(i = 1; i <= end0pos; ++i)
    print *, '--- Starting AC coefficient loop ---'
    i = 2  ! Start at AC coefficient 1 (1-based array, so index 2)
    do while (i <= end0pos)
        print *, 'Outer loop: i =', i, ', end0pos =', end0pos
        startpos = i
        print *, '  startpos =', startpos, ', DU(i) =', DU(i)
        
        ! STB: for (; DU[i]==0 && i<=end0pos; ++i) {}
        do while (DU(i) == 0 .and. i <= end0pos)
            print *, '    Inner loop: skipping zero at i =', i
            i = i + 1
        end do
        
        nrzeroes = i - startpos
        print *, '  After inner loop: i =', i, ', nrzeroes =', nrzeroes
        
        if (i <= end0pos) then
            print *, '  Processing coefficient at i =', i, ', value =', DU(i)
        else
            print *, '  i > end0pos, would access DU(', i, ') - out of range!'
        end if
        
        ! STB doesn't check i <= end0pos here, it directly accesses DU[i]
        print *, '  STB would access DU(', i, ') =', DU(i), '(no bounds check)'
        
        i = i + 1
        print *, '  Incremented i to', i
    end do
    
    print *, '--- End of AC coefficient loop ---'
    
end program debug_ac_loop_logic
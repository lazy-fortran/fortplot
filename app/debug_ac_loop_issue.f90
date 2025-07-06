program debug_ac_loop_issue
    implicit none
    
    integer :: DU(64), i, startpos, nrzeroes, end0pos
    
    ! Create a pattern that should expose the issue
    ! Last non-zero at position 20, followed by zeros
    DU = 0
    DU(1) = 100    ! DC
    DU(5) = 50     ! AC at position 5
    DU(10) = 30    ! AC at position 10
    DU(20) = 10    ! Last AC at position 20
    ! Positions 21-64 are zero
    
    print *, "Testing AC loop with DU ending at position 20"
    print *, "Non-zero coefficients at: 1(DC), 5, 10, 20"
    print *, ""
    
    ! Find end0pos like our implementation
    end0pos = 64
    do while (end0pos > 1 .and. DU(end0pos) == 0)
        end0pos = end0pos - 1
    end do
    print *, "end0pos =", end0pos
    
    ! Test our AC loop logic
    print *, ""
    print *, "Our AC loop execution:"
    i = 2  ! Start at AC coefficient 1
    do while (i <= end0pos)
        startpos = i
        ! Skip zeros
        do while (DU(i) == 0 .and. i <= end0pos)
            i = i + 1
        end do
        nrzeroes = i - startpos
        
        print *, "  Position", i, ": value =", DU(i), ", run =", nrzeroes
        
        ! Check critical issue: what if i > end0pos after zero skip?
        if (i > end0pos) then
            print *, "  ISSUE: i > end0pos after zero skip!"
            exit
        end if
        
        ! Encode the coefficient (simplified)
        i = i + 1
    end do
    
    print *, ""
    print *, "STB equivalent behavior:"
    print *, "STB would stop at position", end0pos, "and write EOB because end0pos /= 63"
    
end program debug_ac_loop_issue
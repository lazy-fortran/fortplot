program user_test_evidence
    ! Evidence of README vs actual API mismatch for figure() function
    use fortplot
    implicit none
    
    ! README shows: call figure(800, 600)
    ! Actual API requires: call figure(figsize=[800.0_wp, 600.0_wp])
    
    ! This would fail based on README documentation:
    ! call figure(800, 600)  ! Type mismatch error
    
    ! This is what actually works:
    call figure(figsize=[800.0_wp, 600.0_wp])
    
    print *, "Evidence test completed"
end program user_test_evidence
program debug_scan_decode
    implicit none
    
    ! Our scan data: 65 14 51 45 15
    ! STB scan data: 65 14 51 40 1f
    
    print *, "Scan data comparison:"
    print *, "Ours: 65 14 51 45 15"
    print *, "STB:  65 14 51 40 1f"
    print *, ""
    print *, "In binary:"
    print '(A,5(B8.8,1X))', "Ours: ", 101_1, 20_1, 81_1, 69_1, 21_1
    print '(A,5(B8.8,1X))', "STB:  ", 101_1, 20_1, 81_1, 64_1, 31_1
    print *, ""
    print *, "Differences at bytes 4-5:"
    print *, "Ours: 45 15 = 01000101 00010101"
    print *, "STB:  40 1f = 01000000 00011111"
    
end program debug_scan_decode
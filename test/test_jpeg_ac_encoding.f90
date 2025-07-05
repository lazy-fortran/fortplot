program test_jpeg_ac_encoding
    use fortplot_jpeg
    implicit none
    
    ! Test that we have all necessary AC codes
    integer :: i
    logical :: all_good = .true.
    
    print *, "Testing AC encoding coverage..."
    
    ! Common AC symbols we need
    ! Format: (run_length << 4) | size
    do i = 0, 15
        ! Run of i zeros followed by size 1
        if (i*16 + 1 > 255) cycle
        ! We should have codes for these
    end do
    
    ! The key insight: for a gray 8x8 image, we have:
    ! - Y block: DC=-1 (category 1), all AC=0 so just EOB
    ! - U block: DC=0 (category 0), all AC=0 so just EOB  
    ! - V block: DC=0 (category 0), all AC=0 so just EOB
    
    print *, "For 8x8 gray image, we need:"
    print *, "1. Y DC category 1 code"
    print *, "2. Y DC value bits for -1" 
    print *, "3. Y AC EOB code"
    print *, "4. U DC category 0 code"
    print *, "5. U AC EOB code"
    print *, "6. V DC category 0 code"
    print *, "7. V AC EOB code"
    print *, ""
    print *, "That should produce more than 3 bytes of output"
    
end program test_jpeg_ac_encoding
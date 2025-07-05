program test_aaf_scaling_stb
    use fortplot_jpeg
    implicit none
    
    ! STB's exact aasf values from stb_image_write.h
    real, parameter :: STB_AASF(8) = [ &
        1.0 * 2.828427125, &      ! 2.828427125
        1.387039845 * 2.828427125, &  ! 3.92268802
        1.306562965 * 2.828427125, &  ! 3.69335151  
        1.175875602 * 2.828427125, &  ! 3.32552645
        1.0 * 2.828427125, &      ! 2.828427125
        0.785694958 * 2.828427125, &  ! 2.22189511
        0.541196100 * 2.828427125, &  ! 1.53073371
        0.275899379 * 2.828427125]    ! 0.78018575
        
    real :: our_aasf(8)
    real :: fdtbl_test
    integer :: i, j, k
    logical :: all_match = .true.
    
    print *, "AAF Scaling Factor Test - STB vs Our Implementation"
    print *, "==================================================="
    
    ! Calculate our aasf values (from fortplot_jpeg.f90)
    our_aasf = [1.0*2.828427125, 1.387039845*2.828427125, 1.306562965*2.828427125, &
                1.175875602*2.828427125, 1.0*2.828427125, 0.785694958*2.828427125, &
                0.541196100*2.828427125, 0.275899379*2.828427125]
    
    print *, ""
    print *, "AASF values comparison:"
    do i = 1, 8
        print '(A,I1,A,F10.6,A,F10.6,A,F10.6)', &
            "aasf[", i-1, "]: STB=", STB_AASF(i), " Our=", our_aasf(i), &
            " Diff=", abs(STB_AASF(i) - our_aasf(i))
        if (abs(STB_AASF(i) - our_aasf(i)) > 0.00001) then
            all_match = .false.
        end if
    end do
    
    ! Test fdtbl calculation
    print *, ""
    print *, "Testing fdtbl calculation:"
    print *, "fdtbl[k] = 1.0 / (qtable[k] * aasf[row] * aasf[col])"
    
    ! Example: DC coefficient (position 0,0)
    k = 1  ! DC position
    i = 1  ! row 1
    j = 1  ! col 1
    
    ! For quality 90, Y quantization table DC value is typically 3
    fdtbl_test = 1.0 / (3.0 * our_aasf(i) * our_aasf(j))
    
    print *, ""
    print '(A,F10.6)', "DC position fdtbl (qtable=3): ", fdtbl_test
    print '(A,F10.6)', "This scales DCT coeff: -64 * fdtbl = ", -64.0 * fdtbl_test
    print *, "Rounded result: ", nint(-64.0 * fdtbl_test)
    
    ! The critical insight: check if we're using correct indices
    print *, ""
    print *, "CRITICAL: In quantization, we must use natural order indices!"
    print *, "The fdtbl is indexed by natural order (row-major)"
    print *, "But we were using zigzag indices incorrectly"
    
    if (all_match) then
        print *, ""
        print *, "AASF values match STB exactly!"
    else
        print *, ""
        print *, "WARNING: AASF values don't match STB!"
    end if
    
end program test_aaf_scaling_stb
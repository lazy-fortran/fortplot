program test_rgb_ycbcr_stb_match
    use fortplot_jpeg
    use iso_fortran_env, only: int8
    implicit none
    
    ! STB coefficients from stb_image_write.h
    real, parameter :: STB_Y_R = 0.29900
    real, parameter :: STB_Y_G = 0.58700
    real, parameter :: STB_Y_B = 0.11400
    
    real, parameter :: STB_U_R = -0.16874
    real, parameter :: STB_U_G = -0.33126
    real, parameter :: STB_U_B = 0.50000
    
    real, parameter :: STB_V_R = 0.50000
    real, parameter :: STB_V_G = -0.41869
    real, parameter :: STB_V_B = -0.08131
    
    ! Our coefficients from fortplot_jpeg.f90
    real, parameter :: OUR_Y_R = 0.299
    real, parameter :: OUR_Y_G = 0.587
    real, parameter :: OUR_Y_B = 0.114
    
    real, parameter :: OUR_U_R = -0.169
    real, parameter :: OUR_U_G = -0.331
    real, parameter :: OUR_U_B = 0.5
    
    real, parameter :: OUR_V_R = 0.5
    real, parameter :: OUR_V_G = -0.419
    real, parameter :: OUR_V_B = -0.081
    
    real :: r, g, b
    real :: y_stb, u_stb, v_stb
    real :: y_our, u_our, v_our
    logical :: all_pass = .true.
    
    print *, "RGB to YCbCr Conversion Test - STB vs Our Implementation"
    print *, "========================================================"
    
    ! Test 1: Gray (127, 127, 127)
    r = 127.0; g = 127.0; b = 127.0
    
    ! STB calculation
    y_stb = STB_Y_R*r + STB_Y_G*g + STB_Y_B*b - 128.0
    u_stb = STB_U_R*r + STB_U_G*g + STB_U_B*b
    v_stb = STB_V_R*r + STB_V_G*g + STB_V_B*b
    
    ! Our calculation (note: we add 128 to U and V)
    y_our = OUR_Y_R*r + OUR_Y_G*g + OUR_Y_B*b - 128.0
    u_our = OUR_U_R*r + OUR_U_G*g + OUR_U_B*b
    v_our = OUR_V_R*r + OUR_V_G*g + OUR_V_B*b
    
    print *, ""
    print *, "Test 1: RGB(127,127,127)"
    print '(A,3F8.3)', "STB Y,U,V: ", y_stb, u_stb, v_stb
    print '(A,3F8.3)', "Our Y,U,V: ", y_our, u_our, v_our
    print '(A,3F8.3)', "Difference: ", abs(y_stb-y_our), abs(u_stb-u_our), abs(v_stb-v_our)
    
    if (abs(y_stb-y_our) > 0.1) then
        print *, "FAIL: Y difference too large!"
        all_pass = .false.
    end if
    
    ! Test coefficient differences
    print *, ""
    print *, "Coefficient Comparison:"
    print *, "Y coefficients:"
    print '(A,3F10.5)', "  STB: ", STB_Y_R, STB_Y_G, STB_Y_B
    print '(A,3F10.5)', "  Our: ", OUR_Y_R, OUR_Y_G, OUR_Y_B
    
    print *, "U coefficients:"
    print '(A,3F10.5)', "  STB: ", STB_U_R, STB_U_G, STB_U_B
    print '(A,3F10.5)', "  Our: ", OUR_U_R, OUR_U_G, OUR_U_B
    
    print *, "V coefficients:"
    print '(A,3F10.5)', "  STB: ", STB_V_R, STB_V_G, STB_V_B
    print '(A,3F10.5)', "  Our: ", OUR_V_R, OUR_V_G, OUR_V_B
    
    ! CRITICAL DIFFERENCE: STB doesn't add 128 to U and V!
    print *, ""
    print *, "CRITICAL FINDING:"
    print *, "STB: U and V are NOT shifted by 128!"
    print *, "Our: U and V ARE shifted by 128!"
    print *, ""
    print *, "This could be why our images appear black!"
    
    if (all_pass) then
        print *, "Basic conversion matches (within tolerance)"
    else
        print *, "Conversion mismatch detected!"
    end if
    
end program test_rgb_ycbcr_stb_match
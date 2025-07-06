program debug_uv_values
    implicit none
    
    real :: r, g, b, y_val, u_val, v_val
    
    ! Test RGB(127, 0, 0)
    r = 127.0
    g = 0.0
    b = 0.0
    
    ! Our conversion (should match STB)
    y_val = 0.299*r + 0.587*g + 0.114*b - 128.0
    u_val = -0.16874*r - 0.33126*g + 0.5*b
    v_val = 0.5*r - 0.41869*g - 0.08131*b
    
    ! STB conversion for comparison
    print *, "RGB input: (", int(r), ",", int(g), ",", int(b), ")"
    print *, "Our Y:", y_val
    print *, "Our U:", u_val  
    print *, "Our V:", v_val
    print *, ""
    
    ! Manual STB calculation
    print *, "STB formulas:"
    print *, "Y = +0.29900*r + 0.58700*g + 0.11400*b - 128"
    print *, "U = -0.16874*r - 0.33126*g + 0.50000*b"
    print *, "V = +0.50000*r - 0.41869*g - 0.08131*b"
    print *, ""
    
    print *, "STB Y:", +0.29900*r + 0.58700*g + 0.11400*b - 128
    print *, "STB U:", -0.16874*r - 0.33126*g + 0.50000*b  
    print *, "STB V:", +0.50000*r - 0.41869*g - 0.08131*b
    print *, ""
    
    ! DC coefficients after DCT (solid color)
    print *, "DC coefficients (solid color * 8):"
    print *, "Y DC:", y_val * 8.0
    print *, "U DC:", u_val * 8.0
    print *, "V DC:", v_val * 8.0
    
end program debug_uv_values
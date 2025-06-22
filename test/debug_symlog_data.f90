program debug_symlog_data
    !! Debug the symlog data to understand the axis limit issue
    use fortplot
    implicit none
    
    real(wp), dimension(50) :: x_exp, y_symlog
    integer :: i
    real(wp) :: y_min, y_max
    
    ! Generate the same data as the symlog example
    x_exp = [(real(i, wp), i=1, 50)]
    y_symlog = x_exp**3 - 50.0_wp * x_exp
    
    y_min = minval(y_symlog)
    y_max = maxval(y_symlog)
    
    print *, "=== Symlog Data Analysis ==="
    print *, "Data range: [", y_min, ",", y_max, "]"
    print *, "First few values:", y_symlog(1:5)
    print *, "Last few values:", y_symlog(46:50)
    print *, "Minimum occurs at x =", x_exp(minloc(y_symlog, 1))
    print *, "Maximum occurs at x =", x_exp(maxloc(y_symlog, 1))
    
end program debug_symlog_data
program scientific_subplot_analysis
    !! Comprehensive example demonstrating subplot functionality for scientific analysis
    !! Shows professional multi-plot figures for comparative visualization
    use iso_fortran_env, only: wp => real64
    use fortplot, only: figure, subplot, plot, xlabel, ylabel, title, savefig, legend, &
                        scatter, xlim, ylim
    implicit none
    
    integer, parameter :: n = 100
    real(wp) :: time(n), signal_a(n), signal_b(n), comparison(n)
    real(wp) :: x_data(20), y_linear(20), y_quadratic(20), y_exponential(20)
    integer :: i
    
    ! Generate time series data
    do i = 1, n
        time(i) = real(i-1, wp) * 0.1_wp
        signal_a(i) = sin(time(i) * 2.0_wp) + 0.1_wp * cos(time(i) * 10.0_wp)
        signal_b(i) = cos(time(i) * 1.5_wp) * exp(-time(i) * 0.05_wp)
        comparison(i) = signal_a(i) - signal_b(i)
    end do
    
    ! Generate mathematical function data
    do i = 1, 20
        x_data(i) = real(i-1, wp) * 0.5_wp
        y_linear(i) = x_data(i)
        y_quadratic(i) = x_data(i)**2 * 0.1_wp
        y_exponential(i) = exp(-x_data(i) * 0.2_wp)
    end do
    
    ! Example 1: Scientific time series analysis (3x1 layout)
    call figure(800, 900)
    
    ! Signal A analysis
    call subplot(3, 1, 1)
    call plot(time, signal_a, label="Signal A")
    call title("Signal A: Modulated Sine Wave")
    call ylabel("Amplitude")
    call xlim(0.0_wp, 10.0_wp)
    call legend()
    
    ! Signal B analysis
    call subplot(3, 1, 2)
    call plot(time, signal_b, label="Signal B")
    call title("Signal B: Damped Cosine")
    call ylabel("Amplitude")
    call xlim(0.0_wp, 10.0_wp)
    call legend()
    
    ! Comparison analysis
    call subplot(3, 1, 3)
    call plot(time, signal_a, label="Signal A")
    call plot(time, signal_b, label="Signal B")
    call plot(time, comparison, label="Difference")
    call title("Comparative Analysis")
    call xlabel("Time")
    call ylabel("Amplitude")
    call xlim(0.0_wp, 10.0_wp)
    call legend()
    
    call savefig("scientific_time_series_analysis.png")
    
    ! Example 2: Mathematical function comparison (2x2 layout)
    call figure(800, 600)
    
    ! Linear function
    call subplot(2, 2, 1)
    call plot(x_data, y_linear, label="f(x) = x")
    call title("Linear Function")
    call xlabel("X")
    call ylabel("f(X)")
    call legend()
    
    ! Quadratic function
    call subplot(2, 2, 2)
    call plot(x_data, y_quadratic, label="f(x) = x²/10")
    call title("Quadratic Function")
    call xlabel("X")
    call ylabel("f(X)")
    call legend()
    
    ! Exponential function
    call subplot(2, 2, 3)
    call plot(x_data, y_exponential, label="f(x) = e^(-x/5)")
    call title("Exponential Decay")
    call xlabel("X")
    call ylabel("f(X)")
    call legend()
    
    ! Comparison of all functions
    call subplot(2, 2, 4)
    call plot(x_data, y_linear, label="Linear")
    call plot(x_data, y_quadratic, label="Quadratic")
    call plot(x_data, y_exponential, label="Exponential")
    call title("Function Comparison")
    call xlabel("X")
    call ylabel("f(X)")
    call legend()
    
    call savefig("mathematical_function_comparison.png")
    
    ! Example 3: Before/after analysis (1x2 layout)
    call figure(1000, 400)
    
    ! Before treatment
    call subplot(1, 2, 1)
    call scatter(x_data(1:15), y_linear(1:15) + 0.2_wp * sin(x_data(1:15) * 5.0_wp), &
                 label="Raw Data")
    call title("Before Treatment")
    call xlabel("X Variable")
    call ylabel("Response")
    call legend()
    
    ! After treatment (filtered/processed)
    call subplot(1, 2, 2)
    call plot(x_data(1:15), y_linear(1:15), label="Processed Data")
    call title("After Treatment")
    call xlabel("X Variable")
    call ylabel("Response")
    call legend()
    
    call savefig("before_after_analysis.png")
    
    print *, "Scientific subplot examples created:"
    print *, "- scientific_time_series_analysis.png (3x1 layout)"
    print *, "- mathematical_function_comparison.png (2x2 layout)" 
    print *, "- before_after_analysis.png (1x2 layout)"
    print *, ""
    print *, "These demonstrate professional multi-subplot figures for:"
    print *, "- Comparative visualization"
    print *, "- Time series analysis"
    print *, "- Mathematical function studies"
    print *, "- Before/after comparisons"
    
end program scientific_subplot_analysis
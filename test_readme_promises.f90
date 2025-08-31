program test_readme_promises
    ! Test EVERY promise made in README.md
    use fortplot
    implicit none
    
    real(wp), dimension(50) :: x, y, t, damped_sine, damped_cosine
    integer :: i
    
    ! README Promise #1: Basic stateful API should work
    print *, "Testing README basic stateful API promise..."
    x = [(real(i-1, wp) * 0.2_wp, i=1, 50)]
    y = sin(x)
    
    call figure()
    call plot(x, y)
    call title("Function Plot")
    call xlabel("x")
    call ylabel("y")
    call xlim(0.0_wp, 10.0_wp)
    call ylim(-1.0_wp, 1.0_wp)
    call savefig("test_readme_basic.png")
    
    ! README Promise #2: 3D plotting should work
    print *, "Testing README 3D plotting promise..."
    call figure(figsize=[8.0_wp, 6.0_wp])
    call add_3d_plot(x(1:30), y(1:30), sin(x(1:30)*2), label="3D curve")
    call title("3D Line Plot")
    call savefig("test_3d_plot.png")
    
    ! README Promise #3: Legend with multiple plots
    print *, "Testing README legend promise..."
    call figure(figsize=[8.0_wp, 6.0_wp])
    call plot(x, sin(x), label="sin(x)", linestyle="b-")
    call plot(x, cos(x), label="cos(x)", linestyle="r--")
    call plot(x, sin(2*x), label="sin(2x)", linestyle="g:")
    call legend()
    call savefig("test_trig_functions.pdf")
    
    ! README Promise #4: Unicode and Greek letters
    print *, "Testing README Unicode/Greek letters promise..."
    t = x
    damped_sine = exp(-0.1_wp * t) * sin(t)
    damped_cosine = exp(-0.1_wp * t) * cos(t)
    call figure(figsize=[8.0_wp, 6.0_wp])
    call title("Wave Functions: \psi(\omega t) = A e^{-\lambda t} sin(\omega t)")
    call xlabel("Time \tau (normalized)")
    call ylabel("Amplitude \Psi (V)")
    call plot(t, damped_sine, label="\alpha decay")
    call plot(t, damped_cosine, label="\beta oscillation")
    call legend()
    call savefig("test_unicode_demo.png")
    
    ! README Promise #5: Scientific errorbar plots
    print *, "Testing README scientific errorbar promise..."
    real(wp), dimension(20) :: x_sci, y_sci, yerr, y_theory
    x_sci = [(real(i-1, wp) * 0.5_wp, i=1, 20)]
    y_sci = sin(x_sci) + 0.1_wp * ([(real(i, wp), i=1, 20)] - 10.0_wp) / 10.0_wp
    yerr = 0.1_wp
    y_theory = sin(x_sci)
    
    call figure(figsize=[8.0_wp, 6.0_wp])
    call errorbar(x_sci, y_sci, yerr=yerr, marker='o', label='Experimental data')
    call plot(x_sci, y_theory, label='Theory', linestyle='-')
    call legend()
    call savefig("test_scientific_plot.png")
    
    print *, "All README promises tested successfully!"
    
end program test_readme_promises
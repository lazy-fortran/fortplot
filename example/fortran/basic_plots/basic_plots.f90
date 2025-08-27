program basic_plots
    !! Basic plotting examples using both simple and OO APIs
    use fortplot
    use fortplot_errors, only: SUCCESS
    implicit none

    call simple_plots()
    call multi_line_plot()

contains

    subroutine simple_plots()
        real(wp), dimension(50) :: x, y
        integer :: i, save_status
        logical :: all_success
        
        print *, "=== Basic Plots ==="
        
        ! Generate simple sine data - show 2 complete periods (0 to 4π)
        x = [(real(i-1, wp) * 4.0_wp * 3.141592653589793_wp / 49.0_wp, i=1, 50)]
        y = sin(x)
        
        ! Simple plot using functional API
        call figure()
        call plot(x, y, label='sin(x)')
        call title('Simple Sine Wave')
        call xlabel('x')
        call ylabel('sin(x)')
        all_success = .true.
        
        call savefig_with_status('output/example/fortran/basic_plots/simple_plot.png', save_status)
        if (save_status /= SUCCESS) all_success = .false.
        
        call savefig_with_status('output/example/fortran/basic_plots/simple_plot.pdf', save_status)
        if (save_status /= SUCCESS) all_success = .false.
        
        call savefig_with_status('output/example/fortran/basic_plots/simple_plot.txt', save_status)
        if (save_status /= SUCCESS) all_success = .false.
        
        if (all_success) then
            print *, "Created: simple_plot.png/pdf/txt"
        else
            print *, "ERROR: Failed to create some simple_plot files"
        end if
        
    end subroutine simple_plots

    subroutine multi_line_plot()
        real(wp), dimension(100) :: x, sx, cx
        type(figure_t) :: fig
        integer :: i, save_status
        logical :: all_success

        x = [(real(i, wp), i=0, size(x) - 1)]/5.0_wp
        sx = sin(x)
        cx = cos(x)
        
        ! Multi-line plot using OO interface
        call figure(figsize=[8.0_wp, 6.0_wp])
        call xlabel("x")
        call ylabel("y")
        call title("Sine and Cosine Functions")
        call add_plot(x, sx, label="sin(x)")
        call add_plot(x, cx, label="cos(x)")
        call legend()  ! Add legend for labeled plots
        all_success = .true.
        
        call savefig_with_status('output/example/fortran/basic_plots/multi_line.png', save_status)
        if (save_status /= SUCCESS) all_success = .false.
        
        call savefig_with_status('output/example/fortran/basic_plots/multi_line.pdf', save_status)
        if (save_status /= SUCCESS) all_success = .false.
        
        call savefig_with_status('output/example/fortran/basic_plots/multi_line.txt', save_status)
        if (save_status /= SUCCESS) all_success = .false.
        
        if (all_success) then
            print *, "Created: multi_line.png/pdf/txt"
        else
            print *, "ERROR: Failed to create some multi_line files"
        end if
        
    end subroutine multi_line_plot

end program basic_plots
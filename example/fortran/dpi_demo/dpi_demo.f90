program dpi_demo
    !! Demonstrate DPI support in FortPlot's OO interface
    !! Shows how to use DPI parameter with figure initialization and property access
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure, only: figure_t
    implicit none

    type(figure_t) :: fig1, fig2, fig3
    real(wp), parameter :: pi = 3.14159265358979323846_wp
    real(wp) :: x(100), y(100)
    integer :: i

    print *, "FortPlot DPI Demo"
    print *, "================="

    ! Generate sample data
    do i = 1, 100
        x(i) = 2.0_wp * pi * real(i-1, wp) / 99.0_wp
        y(i) = sin(x(i)) * exp(-x(i) / (2.0_wp * pi))
    end do

    ! Example 1: Default DPI (100)
    print *, "Creating figure with default DPI (100)..."
    call fig1%initialize(width=800, height=600)
    call fig1%plot(x, y)
    call fig1%set_title("Default DPI (100)")
    call fig1%set_xlabel("x")
    call fig1%set_ylabel("sin(x) * exp(-x/(2π))")
    print *, "DPI:", fig1%get_dpi()
    call fig1%savefig("dpi_demo_default.png")

    ! Example 2: High DPI (300)
    print *, "Creating figure with high DPI (300)..."
    call fig2%initialize(width=800, height=600, dpi=300.0_wp)
    call fig2%plot(x, y)
    call fig2%set_title("High DPI (300)")
    call fig2%set_xlabel("x")
    call fig2%set_ylabel("sin(x) * exp(-x/(2π))")
    print *, "DPI:", fig2%get_dpi()
    call fig2%savefig("dpi_demo_high.png")

    ! Example 3: Low DPI (72)
    print *, "Creating figure with low DPI (72)..."
    call fig3%initialize(width=800, height=600, dpi=72.0_wp)
    call fig3%plot(x, y)
    call fig3%set_title("Low DPI (72)")
    call fig3%set_xlabel("x")
    call fig3%set_ylabel("sin(x) * exp(-x/(2π))")
    print *, "DPI:", fig3%get_dpi()
    call fig3%savefig("dpi_demo_low.png")

    ! Example 4: DPI property modification
    print *, "Demonstrating DPI property modification..."
    call fig1%set_dpi(150.0_wp)
    print *, "Modified fig1 DPI to:", fig1%get_dpi()
    call fig1%savefig("dpi_demo_modified.png")

    print *, "DPI demo completed!"
    print *, "Generated files:"
    print *, "  - dpi_demo_default.png  (DPI: 100)"
    print *, "  - dpi_demo_high.png     (DPI: 300)"
    print *, "  - dpi_demo_low.png      (DPI: 72)"
    print *, "  - dpi_demo_modified.png (DPI: 150)"

end program dpi_demo
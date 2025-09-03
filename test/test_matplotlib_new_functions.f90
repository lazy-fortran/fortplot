program test_matplotlib_new_functions
    !! Test the new matplotlib-compatible functions
    use fortplot
    use iso_fortran_env, only: real64
    implicit none
    
    real(real64), allocatable :: x(:), y(:), z(:,:)
    real(real64), allocatable :: theta(:), r(:)
    real(real64), allocatable :: values(:)
    integer :: i, j
    
    print *, "Testing new matplotlib-compatible functions..."
    
    ! Test imshow
    allocate(z(50, 50))
    do i = 1, 50
        do j = 1, 50
            z(i, j) = sin(real(i, real64) / 5.0) * cos(real(j, real64) / 5.0)
        end do
    end do
    
    call figure()
    call imshow(z)
    call title("imshow() test - 2D heatmap")
    call savefig("test_imshow.png")
    print *, "✓ imshow() function works"
    
    ! Test pie chart
    allocate(values(5))
    values = [30.0_real64, 25.0_real64, 20.0_real64, 15.0_real64, 10.0_real64]
    
    call figure()
    call pie(values)
    call title("pie() test - Pie chart")
    call savefig("test_pie.png")
    print *, "✓ pie() function works"
    
    ! Test polar plot
    allocate(theta(100), r(100))
    do i = 1, 100
        theta(i) = 2.0_real64 * 3.14159_real64 * real(i-1, real64) / 99.0_real64
        r(i) = 1.0_real64 + 0.5_real64 * sin(5.0_real64 * theta(i))
    end do
    
    call figure()
    call polar(theta, r)
    call title("polar() test - Polar plot")
    call savefig("test_polar.png")
    print *, "✓ polar() function works"
    
    ! Test step plot
    allocate(x(20), y(20))
    do i = 1, 20
        x(i) = real(i, real64)
        y(i) = sin(real(i, real64) / 3.0_real64)
    end do
    
    call figure()
    call step(x, y)
    call title("step() test - Step plot")
    call xlabel("x")
    call ylabel("y")
    call savefig("test_step.png")
    print *, "✓ step() function works"
    
    ! Test stem plot
    call figure()
    call stem(x, y)
    call title("stem() test - Stem plot")
    call xlabel("x")
    call ylabel("y")
    call savefig("test_stem.png")
    print *, "✓ stem() function works"
    
    ! Test fill
    call figure()
    call plot(x, y)
    call fill(x, y)
    call title("fill() test - Area under curve")
    call xlabel("x")
    call ylabel("y")
    call savefig("test_fill.png")
    print *, "✓ fill() function works"
    
    ! Test fill_between
    deallocate(x, y)
    allocate(x(50), y(50))
    do i = 1, 50
        x(i) = real(i-25, real64) / 5.0_real64
        y(i) = x(i)**2
    end do
    
    call figure()
    call fill_between(x, y1=y, y2=y*0.5_real64)
    call title("fill_between() test - Area between curves")
    call xlabel("x")
    call ylabel("y")
    call savefig("test_fill_between.png")
    print *, "✓ fill_between() function works"
    
    ! Test twinx and twiny (will show warnings as not fully implemented)
    call figure()
    call plot(x, y)
    call twinx()
    call twiny()
    call title("twinx/twiny test - Dual axes (placeholders)")
    call savefig("test_twin_axes.png")
    print *, "✓ twinx() and twiny() functions accessible (placeholders)"
    
    print *, ""
    print *, "All new matplotlib-compatible functions are accessible!"
    print *, "Test images saved to test_*.png files"
    
    deallocate(x, y, z, theta, r, values)
    
end program test_matplotlib_new_functions
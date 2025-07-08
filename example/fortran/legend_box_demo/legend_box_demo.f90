program legend_box_demo
    !! Demonstration of fixed legend box spacing
    use fortplot
    implicit none
    
    type(figure_t) :: fig
    real(8), allocatable :: x(:), y(:)
    integer :: i, n
    
    n = 50
    allocate(x(n), y(n))
    
    ! Create simple data
    do i = 1, n
        x(i) = real(i-1, 8) / real(n-1, 8) * 2.0d0 * 3.14159d0
        y(i) = sin(x(i))
    end do
    
    ! Create figure with multiple legend entries
    call fig%initialize(800, 600)
    
    ! Add multiple plots to test legend spacing
    call fig%add_plot(x, y, label='sin(x)')
    call fig%add_plot(x, y*0.5d0, label='0.5 sin(x)')
    call fig%add_plot(x, cos(x)*0.7d0, label='0.7 cos(x)')
    call fig%add_plot(x, -y*0.3d0, label='-0.3 sin(x)')
    
    call fig%set_title('Legend Box Spacing Demo')
    call fig%set_xlabel('x')
    call fig%set_ylabel('y')
    
    ! Save with default legend position
    call fig%legend()
    call fig%savefig('output/example/fortran/legend_box_demo/legend_box_demo_default.png')
    call fig%savefig('output/example/fortran/legend_box_demo/legend_box_demo_default.pdf')
    call fig%savefig('output/example/fortran/legend_box_demo/legend_box_demo_default.txt')
    print *, 'Created legend_box_demo_default.png/pdf/txt'
    
    ! Test different legend positions
    call fig%legend('upper left')
    call fig%savefig('output/example/fortran/legend_box_demo/legend_box_demo_upper_left.png')
    call fig%savefig('output/example/fortran/legend_box_demo/legend_box_demo_upper_left.pdf')
    call fig%savefig('output/example/fortran/legend_box_demo/legend_box_demo_upper_left.txt')
    print *, 'Created legend_box_demo_upper_left.png/pdf/txt'
    
    call fig%legend('lower right')
    call fig%savefig('output/example/fortran/legend_box_demo/legend_box_demo_lower_right.png')
    call fig%savefig('output/example/fortran/legend_box_demo/legend_box_demo_lower_right.pdf')
    call fig%savefig('output/example/fortran/legend_box_demo/legend_box_demo_lower_right.txt')
    print *, 'Created legend_box_demo_lower_right.png/pdf/txt'
    
    deallocate(x, y)
    
    print *, 'Legend box demo completed!'
end program legend_box_demo
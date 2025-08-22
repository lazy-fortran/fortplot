program test_legend_box_exact_height
    !! Test to verify legend box height matches matplotlib behavior
    use fortplot
    use fortplot_security, only: get_test_output_path
    implicit none
    
    type(figure_t) :: fig
    real(8), allocatable :: x(:), y(:)
    integer :: i, n
    
    n = 10
    allocate(x(n), y(n))
    
    ! Simple data
    x = [(real(i, 8), i = 1, n)]
    y = x
    
    ! Test with single entry
    call fig%initialize(400, 300)
    call fig%add_plot(x, y, label='Single entry')
    call fig%legend()
    call fig%savefig(get_test_output_path('output/test/test_legend_box_exact_height/test_legend_single.png'))
    call fig%savefig(get_test_output_path('/tmp/test_legend_single.png'))
    print *, 'Created test_legend_single.png'
    
    ! Test with two entries
    call fig%initialize(400, 300)
    call fig%add_plot(x, y, label='Entry 1')
    call fig%add_plot(x, y*2, label='Entry 2')
    call fig%legend()
    call fig%savefig(get_test_output_path('output/test/test_legend_box_exact_height/test_legend_double.png'))
    call fig%savefig(get_test_output_path('/tmp/test_legend_double.png'))
    print *, 'Created test_legend_double.png'
    
    ! Test with four entries
    call fig%initialize(400, 300)
    call fig%add_plot(x, y, label='Entry 1')
    call fig%add_plot(x, y*2, label='Entry 2')
    call fig%add_plot(x, y*3, label='Entry 3')
    call fig%add_plot(x, y*4, label='Entry 4')
    call fig%legend()
    call fig%savefig(get_test_output_path('output/test/test_legend_box_exact_height/test_legend_quad.png'))
    call fig%savefig(get_test_output_path('/tmp/test_legend_quad.png'))
    print *, 'Created test_legend_quad.png'
    
    deallocate(x, y)
    print *, 'Test completed - check PNG files for box height'
end program test_legend_box_exact_height
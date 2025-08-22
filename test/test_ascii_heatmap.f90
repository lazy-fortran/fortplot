program test_ascii_heatmap
    use fortplot, only: figure_t, wp
    use fortplot_security, only: get_test_output_path
    implicit none
    integer :: i, j
    real(wp), allocatable :: x(:), y(:), z(:,:)
    type(figure_t) :: fig
    
    ! Test data - 2D Gaussian
    allocate(x(20), y(20), z(20,20))
    do i = 1, 20
        x(i) = -2.0_wp + 4.0_wp * (i - 1) / 19.0_wp
        y(i) = -2.0_wp + 4.0_wp * (i - 1) / 19.0_wp
    end do
    
    do i = 1, 20
        do j = 1, 20
            z(i,j) = exp(-(x(i)**2 + y(j)**2))
        end do
    end do
    
    ! Test contour_filled ASCII output
    call fig%initialize(80, 25)
    call figure_add_contour_filled(fig, x, y, z)
    call figure_savefig(fig, get_test_output_path('/tmp/test_contour_filled.txt'))
    
    ! Test pcolormesh ASCII output - needs edge coordinates
    block
        real(wp), allocatable :: x_edges(:), y_edges(:)
        integer :: i
        
        allocate(x_edges(21), y_edges(21))
        
        ! Create edge coordinates (one more than centers)
        do i = 1, 21
            x_edges(i) = -2.0_wp - 0.1_wp + 4.2_wp * (i - 1) / 20.0_wp
            y_edges(i) = -2.0_wp - 0.1_wp + 4.2_wp * (i - 1) / 20.0_wp
        end do
        
        call fig%initialize(80, 25)
        call figure_add_pcolormesh(fig, x_edges, y_edges, z)
        call figure_savefig(fig, get_test_output_path('/tmp/test_pcolormesh.txt'))
    end block
    
    print *, "ASCII heatmap tests completed"
    
end program test_ascii_heatmap
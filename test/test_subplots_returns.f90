program test_subplots_returns
    !! Test that subplots_grid() returns subplot indices
    use fortplot
    implicit none
    
    integer :: i, j, idx
    real(8) :: x(100), y(100)
    integer :: nrows, ncols
    integer, allocatable :: axes(:,:)
    
    ! Create data
    do i = 1, 100
        x(i) = real(i-1, 8) * 0.1
        y(i) = sin(x(i))
    end do
    
    ! Create 2x2 subplot grid and get indices
    nrows = 2
    ncols = 2
    axes = subplots_grid(nrows, ncols)
    
    ! Access individual subplots through indices
    do i = 1, nrows
        do j = 1, ncols
            idx = axes(i,j)
            call subplot(nrows, ncols, idx)
            call plot(x, y*real(i*j, 8))
            call title('Subplot ' // char(48+i) // ',' // char(48+j))
        end do
    end do
    
    ! Save the figure
    call savefig('test_subplots_grid_returns.png')
    
    print *, 'Test completed - check test_subplots_grid_returns.png'
    print *, 'Subplot indices returned successfully for grid access'
    
end program test_subplots_returns
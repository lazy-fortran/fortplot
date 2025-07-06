program debug_legend_spacing
    use fortplot
    use fortplot_legend_layout, only: legend_box_t, calculate_legend_box
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    type(figure_t) :: fig
    real(wp), allocatable :: x(:), y(:)
    integer :: i
    character(len=50) :: labels(3)
    type(legend_box_t) :: box
    real(wp) :: data_width, data_height
    
    ! Simple test data
    allocate(x(10), y(10))
    x = [(real(i, wp), i = 1, 10)]
    y = x
    
    ! Test with 3 entries
    call fig%initialize(600, 400)
    call fig%add_plot(x, y, label='First entry')
    call fig%add_plot(x, y*2, label='Second entry')
    call fig%add_plot(x, y*3, label='Third entry')
    
    ! Calculate box dimensions
    labels = ['First entry ', 'Second entry', 'Third entry ']
    data_width = 10.0_wp
    data_height = 30.0_wp
    box = calculate_legend_box(labels, data_width, data_height, 3, 2) ! upper right
    
    print *, "Legend box debug info:"
    print *, "  padding:", box%padding
    print *, "  entry_height:", box%entry_height
    print *, "  entry_spacing:", box%entry_spacing
    print *, "  total height:", box%height
    print *, "  Expected content height:", 3 * box%entry_height + 2 * box%entry_spacing
    print *, "  With padding:", 2 * box%padding + 3 * box%entry_height + 2 * box%entry_spacing
    
    call fig%legend()
    call fig%savefig('debug_legend_spacing.png')
    
    deallocate(x, y)
end program debug_legend_spacing
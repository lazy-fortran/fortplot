program test_legend_box_intersection
    !! Test for issue #38: Legend boxes often intersect with legend text
    use fortplot
    implicit none
    
    type(figure_t) :: fig
    real(8), allocatable :: x(:), y1(:), y2(:), y3(:)
    integer :: i, n
    
    n = 100
    allocate(x(n), y1(n), y2(n), y3(n))
    
    ! Create test data
    do i = 1, n
        x(i) = real(i-1) / real(n-1) * 10.0
        y1(i) = sin(x(i))
        y2(i) = cos(x(i)) * 0.5
        y3(i) = sin(x(i) * 2.0) * 0.3
    end do
    
    print *, "Testing legend box intersection issue..."
    
    ! Test with different legend text lengths
    call fig%initialize(800, 600)
    
    ! Add plots with various label lengths to test box sizing
    call fig%add_plot(x, y1, label='Short')
    call fig%add_plot(x, y2, label='Medium length label')
    call fig%add_plot(x, y3, label='This is a very long legend label to test box sizing')
    
    call fig%set_title('Legend Box Intersection Test')
    call fig%set_xlabel('X axis')
    call fig%set_ylabel('Y axis')
    call fig%set_xlim(0.0d0, 10.0d0)
    call fig%set_ylim(-1.5d0, 1.5d0)
    
    ! Save to different formats to test rendering
    call fig%savefig('output/test/test_legend_box_intersection/test_legend_box_png.png')
    call fig%savefig('output/test/test_legend_box_intersection/test_legend_box_pdf.pdf')
    call fig%savefig('/tmp/test_legend_box_png.png')
    call fig%savefig('/tmp/test_legend_box_pdf.pdf')
    
    print *, "Saved test_legend_box_png.png and test_legend_box_pdf.pdf"
    print *, "Check if legend box intersects with text"
    
    deallocate(x, y1, y2, y3)
    
    print *, "Test completed. Check generated files for legend box issues."
end program test_legend_box_intersection
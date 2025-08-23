program bar_chart_demo
    !! Example demonstrating bar chart plotting capabilities
    !! Shows vertical bars, horizontal bars, and grouped bars
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot
    implicit none
    
    type(figure_t) :: fig
    real(wp) :: categories(5), sales_q1(5), sales_q2(5)
    real(wp) :: products(4), revenues(4)
    integer :: i
    
    ! Create category data for vertical bars
    do i = 1, 5
        categories(i) = real(i, wp)
        sales_q1(i) = 20.0_wp + real(i * 5, wp) + sin(real(i, wp)) * 10.0_wp
        sales_q2(i) = 15.0_wp + real(i * 4, wp) + cos(real(i, wp)) * 8.0_wp
    end do
    
    ! Create product revenue data for horizontal bars
    do i = 1, 4
        products(i) = real(i, wp)
        revenues(i) = 50.0_wp + real(i * 15, wp)
    end do
    
    ! Basic vertical bar chart
    call figure(figsize=[8.0_wp, 6.0_wp])
    call bar(categories, sales_q1)
    call title('Basic Vertical Bar Chart')
    call xlabel('Categories')
    call ylabel('Sales ($k)')
    call savefig('output/example/fortran/bar_chart_demo/bar_chart_basic.png')
    write(*,*) 'Created bar_chart_basic.png'
    
    ! Horizontal bar chart
    call figure(figsize=[8.0_wp, 6.0_wp])
    call barh(products, revenues)
    call title('Horizontal Bar Chart')
    call xlabel('Revenue ($k)')
    call ylabel('Products')
    call savefig('output/example/fortran/bar_chart_demo/bar_chart_horizontal.png')
    write(*,*) 'Created bar_chart_horizontal.png'
    
    ! Custom width bar chart
    call figure(figsize=[8.0_wp, 6.0_wp])
    call bar(categories, sales_q1, width=0.5_wp)
    call title('Bar Chart with Custom Width (0.5)')
    call xlabel('Categories')
    call ylabel('Sales ($k)')
    call savefig('output/example/fortran/bar_chart_demo/bar_chart_custom_width.png')
    write(*,*) 'Created bar_chart_custom_width.png'
    
    ! Grouped bar charts
    call figure(figsize=[8.0_wp, 6.0_wp])
    call bar(categories - 0.2_wp, sales_q1, width=0.35_wp, &
                label='Q1 Sales', color=[0.0_wp, 0.447_wp, 0.698_wp])
    call bar(categories + 0.2_wp, sales_q2, width=0.35_wp, &
                label='Q2 Sales', color=[0.835_wp, 0.369_wp, 0.0_wp])
    call legend()
    call title('Grouped Bar Chart - Quarterly Sales')
    call xlabel('Categories')
    call ylabel('Sales ($k)')
    call savefig('output/example/fortran/bar_chart_demo/bar_chart_grouped.png')
    write(*,*) 'Created bar_chart_grouped.png'
    
    ! Mixed positive and negative values
    call figure(figsize=[8.0_wp, 6.0_wp])
    sales_q1(1:5) = [15.0_wp, -8.0_wp, 22.0_wp, -5.0_wp, 18.0_wp]
    call bar(categories, sales_q1, label='Profit/Loss')
    call title('Bar Chart with Positive and Negative Values')
    call xlabel('Categories')
    call ylabel('Profit/Loss ($k)')
    call savefig('output/example/fortran/bar_chart_demo/bar_chart_mixed_values.png')
    write(*,*) 'Created bar_chart_mixed_values.png'
    
    write(*,*) 'Bar chart demonstration completed!'
    
end program bar_chart_demo
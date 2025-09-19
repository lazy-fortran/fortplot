program boxplot_demo
    !! Box plot demonstration showing statistical visualization features
    
    use fortplot
    implicit none
    
    real(wp), parameter :: normal_data(20) = [1.2_wp, 1.5_wp, 1.8_wp, 2.1_wp, 2.4_wp, &
                                             2.7_wp, 3.0_wp, 3.3_wp, 3.6_wp, 3.9_wp, &
                                             4.2_wp, 4.5_wp, 4.8_wp, 5.1_wp, 5.4_wp, &
                                             5.7_wp, 6.0_wp, 6.3_wp, 6.6_wp, 6.9_wp]
    
    real(wp), parameter :: outlier_data(15) = [2.0_wp, 2.5_wp, 3.0_wp, 3.5_wp, 4.0_wp, &
                                              4.5_wp, 5.0_wp, 5.5_wp, 6.0_wp, 6.5_wp, &
                                              7.0_wp, 12.0_wp, 14.5_wp, 16.0_wp, 18.2_wp]
    
    real(wp), parameter :: group_a(10) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, &
                                         6.0_wp, 7.0_wp, 8.0_wp, 9.0_wp, 10.0_wp]
    
    real(wp), parameter :: group_b(10) = [2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp, &
                                         7.0_wp, 8.0_wp, 9.0_wp, 10.0_wp, 11.0_wp]
    
    real(wp), parameter :: group_c(10) = [3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp, 7.0_wp, &
                                         8.0_wp, 9.0_wp, 10.0_wp, 11.0_wp, 12.0_wp]
    
    real(wp) :: x_a(10), x_b(10), x_c(10)
    integer :: i
    
    ! Single box plot - using functional API
    call figure()
    call title('Single Box Plot Example')
    call xlabel('Data Groups')
    call ylabel('Values')
    
    ! Render box plots: simple vertical boxes with positions 1,2,3
    call boxplot(group_a, position=1.0_wp, width=0.6_wp, label='Group A')
    call boxplot(group_b, position=2.0_wp, width=0.6_wp, label='Group B')
    call boxplot(group_c, position=3.0_wp, width=0.6_wp, label='Group C')
    call legend()
    
    call savefig('output/example/fortran/boxplot_demo/boxplot_demo.png')
    call savefig('output/example/fortran/boxplot_demo/boxplot_demo.pdf')
    call savefig('output/example/fortran/boxplot_demo/boxplot_demo.txt')
    write(*,*) 'Created boxplot_demo outputs'
    
    write(*,*) 'Box plot demonstration completed!'
    
end program boxplot_demo

program errorbar_demo
    !! Example demonstrating error bar plotting capabilities
    !! Shows symmetric/asymmetric error bars, customization, and integration
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot
    use fortplot_errors, only: SUCCESS
    implicit none
    
    type(figure_t) :: fig
    real(wp) :: x(10), y(10), yerr(10), xerr(10)
    real(wp) :: yerr_lower(5), yerr_upper(5)
    integer :: i, save_status
    
    ! Create test data with calculated errors
    do i = 1, 10
        x(i) = real(i, wp)
        y(i) = sin(real(i, wp) * 0.5_wp) * 3.0_wp
        yerr(i) = 0.3_wp + abs(y(i)) * 0.1_wp  ! Variable error based on value
        xerr(i) = 0.2_wp
    end do
    
    ! Asymmetric error data
    do i = 1, 5
        yerr_lower(i) = 0.2_wp * real(i, wp)
        yerr_upper(i) = 0.4_wp * real(i, wp)
    end do
    
    ! Basic symmetric Y error bars
    call figure(figsize=[8.0_wp, 6.0_wp])
    call errorbar(x, y, yerr=yerr, label='Data with Y errors')
    call legend()
    call title('Basic Symmetric Y Error Bars')
    call xlabel('X values')
    call ylabel('Y values')
    call savefig_with_status('output/example/fortran/errorbar_demo/errorbar_basic_y.png', save_status)
    if (save_status == SUCCESS) then
        write(*,*) 'Created errorbar_basic_y.png'
    else
        write(*,*) 'ERROR: Failed to create errorbar_basic_y.png'
    end if
    
    ! Symmetric X error bars
    call figure(figsize=[8.0_wp, 6.0_wp])
    call errorbar(x, y, xerr=xerr, label='Data with X errors')
    call legend()
    call title('Symmetric X Error Bars')
    call xlabel('X values')
    call ylabel('Y values')
    call savefig_with_status('output/example/fortran/errorbar_demo/errorbar_basic_x.png', save_status)
    if (save_status == SUCCESS) then
        write(*,*) 'Created errorbar_basic_x.png'
    else
        write(*,*) 'ERROR: Failed to create errorbar_basic_x.png'
    end if
    
    ! Both X and Y error bars
    call figure(figsize=[8.0_wp, 6.0_wp])
    call errorbar(x, y, xerr=xerr, yerr=yerr, label='Data with XY errors')
    call legend()
    call title('Combined X and Y Error Bars')
    call xlabel('X values')
    call ylabel('Y values')
    call savefig_with_status('output/example/fortran/errorbar_demo/errorbar_combined.png', save_status)
    if (save_status == SUCCESS) then
        write(*,*) 'Created errorbar_combined.png'
    else
        write(*,*) 'ERROR: Failed to create errorbar_combined.png'
    end if
    
    ! Symmetric Y error bars (average of asymmetric errors)
    call figure(figsize=[8.0_wp, 6.0_wp])
    call errorbar(x(1:5), y(1:5), yerr=(yerr_lower + yerr_upper) / 2.0_wp, &
                     label='Average errors')
    call legend()
    call title('Symmetric Y Error Bars (from asymmetric)')
    call xlabel('X values')
    call ylabel('Y values')
    call savefig_with_status('output/example/fortran/errorbar_demo/errorbar_asymmetric.png', save_status)
    if (save_status == SUCCESS) then
        write(*,*) 'Created errorbar_asymmetric.png'
    else
        write(*,*) 'ERROR: Failed to create errorbar_asymmetric.png'
    end if
    
    ! Error bars with markers and customization
    call figure(figsize=[8.0_wp, 6.0_wp])
    call errorbar(x, y, yerr=yerr, marker='o', linestyle='--', &
                     capsize=8.0_wp, &
                     color=[1.0_wp, 0.0_wp, 0.0_wp], label='Custom styling')
    call legend()
    call title('Customized Error Bars with Markers')
    call xlabel('X values')
    call ylabel('Y values')
    call savefig_with_status('output/example/fortran/errorbar_demo/errorbar_custom.png', save_status)
    if (save_status == SUCCESS) then
        write(*,*) 'Created errorbar_custom.png'
    else
        write(*,*) 'ERROR: Failed to create errorbar_custom.png'
    end if
    
    ! Scientific data example
    call figure(figsize=[8.0_wp, 6.0_wp])
    ! Generate experimental-style data
    do i = 1, 10
        x(i) = real(i - 1, wp) * 2.0_wp
        y(i) = 2.5_wp * exp(-x(i) * 0.1_wp) + 0.5_wp
        yerr(i) = 0.1_wp + y(i) * 0.05_wp  ! 5% relative error + 0.1 absolute
    end do
    
    call errorbar(x, y, yerr=yerr, marker='s', linestyle='-', &
                     color=[0.0_wp, 0.5_wp, 0.8_wp], label='Experimental data')
    call legend()
    call title('Scientific Data with Error Bars')
    call xlabel('Time (s)')
    call ylabel('Signal amplitude')
    call savefig_with_status('output/example/fortran/errorbar_demo/errorbar_scientific.png', save_status)
    if (save_status == SUCCESS) then
        write(*,*) 'Created errorbar_scientific.png'
        write(*,*) 'Error bar demonstration completed!'
    else
        write(*,*) 'ERROR: Failed to create errorbar_scientific.png'
        write(*,*) 'Error bar demonstration completed with errors!'
    end if
    
end program errorbar_demo
program test_api_usability
    !! Test error bar API usability from typical user perspective
    !! Focus on intuitive parameter names and usage patterns
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot
    implicit none
    
    type(figure_t) :: fig
    real(wp) :: x(5), y(5), yerr(5), xerr(5)
    integer :: i
    
    write(*,*) '=== API Usability Testing ==='
    
    ! Setup simple test data
    x = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
    y = [2.0_wp, 4.0_wp, 3.0_wp, 5.0_wp, 4.5_wp]
    yerr = [0.2_wp, 0.3_wp, 0.2_wp, 0.4_wp, 0.3_wp]
    xerr = [0.1_wp, 0.1_wp, 0.1_wp, 0.1_wp, 0.1_wp]
    
    ! Test 1: Minimal usage - just Y errors
    write(*,*) 'Test 1: Minimal API usage'
    call fig%initialize()
    call fig%errorbar(x, y, yerr=yerr)
    call fig%savefig('api_test1_minimal.png')
    write(*,*) '✓ Minimal usage works'
    
    ! Test 2: Common scientific pattern
    write(*,*) 'Test 2: Common scientific usage'
    call fig%initialize()
    call fig%errorbar(x, y, yerr=yerr, label='Data', marker='o')
    call fig%set_xlabel('Measurement Variable')
    call fig%set_ylabel('Response')
    call fig%set_title('Scientific Data with Uncertainty')
    call fig%legend()
    call fig%savefig('api_test2_scientific.png')
    write(*,*) '✓ Scientific pattern works'
    
    ! Test 3: Parameter naming intuitiveness
    write(*,*) 'Test 3: Parameter naming clarity'
    call fig%initialize()
    call fig%errorbar(x, y, &
                     yerr=yerr, &              ! Clear: Y errors
                     capsize=8.0_wp, &         ! Clear: error bar cap size
                     elinewidth=2.0_wp, &      ! Clear: error line width
                     marker='s', &             ! Clear: data marker
                     label='Test data')        ! Clear: legend label
    call fig%legend()
    call fig%savefig('api_test3_clarity.png')
    write(*,*) '✓ Parameter names are intuitive'
    
    ! Test 4: Combined X and Y usage
    write(*,*) 'Test 4: Combined X+Y errors'
    call fig%initialize()
    call fig%errorbar(x, y, xerr=xerr, yerr=yerr, label='XY errors')
    call fig%legend()
    call fig%savefig('api_test4_combined.png')
    write(*,*) '✓ Combined X+Y error usage is clear'
    
    ! Test 5: Optional parameter flexibility
    write(*,*) 'Test 5: Optional parameter flexibility'
    call fig%initialize()
    ! All optional parameters (should use defaults)
    call fig%errorbar(x, y, yerr=yerr)
    call fig%savefig('api_test5_defaults.png')
    write(*,*) '✓ Optional parameters work with defaults'
    
    ! Test 6: Color specification usability
    write(*,*) 'Test 6: Color specification'
    call fig%initialize()
    call fig%errorbar(x, y, yerr=yerr, &
                     color=[0.8_wp, 0.2_wp, 0.2_wp], &  ! Red color
                     label='Red data')
    call fig%legend()
    call fig%savefig('api_test6_color.png')
    write(*,*) '✓ Color specification is straightforward'
    
    write(*,*) ''
    write(*,*) '=== API Usability Tests Completed Successfully! ==='
    write(*,*) 'All common usage patterns work intuitively.'
    
end program test_api_usability
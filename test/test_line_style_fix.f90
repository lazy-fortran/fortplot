program test_line_style_fix
    !! Test that line style patterns are properly rendered in PNG backend
    !! Fixes issue #1159 where PNG line styles were garbled
    use fortplot
    implicit none
    
    real(wp), dimension(20) :: x, y
    integer :: i
    
    print *, "Testing fix for issue #1159: PNG line styles garbled"
    
    ! Create shorter lines to better test pattern continuity
    do i = 1, 20
        x(i) = real(i-1, wp) * 0.5_wp
    end do
    
    call figure(figsize=[8.0_wp, 4.0_wp])
    
    ! Test patterns at different y levels
    y = 3.5_wp
    call plot(x, y, linestyle='-', label='Solid')
    
    y = 2.5_wp
    call plot(x, y, linestyle='--', label='Dashed (6px-3px)')
    
    y = 1.5_wp
    call plot(x, y, linestyle=':', label='Dotted (1px-3px)')
    
    y = 0.5_wp
    call plot(x, y, linestyle='-.', label='DashDot')
    
    call xlim(0.0_wp, 9.5_wp)
    call ylim(0.0_wp, 4.0_wp)
    call title('Issue #1159 Fix: PNG Line Styles')
    call legend()
    
    call savefig('test_issue_1159_fix.png')
    call savefig('test_issue_1159_fix.pdf')
    
    print *, "Test complete. Generated test_issue_1159_fix.png and .pdf"
    print *, "Patterns should now be regular and match between PNG and PDF"
    
end program test_line_style_fix
program test_legend_empty_label_fix
    !! Test that empty labels don't create legend entries (Issue #328)
    use fortplot
    implicit none
    
    real(wp), dimension(10) :: x, y1, y2, y3
    integer :: i
    
    print *, "Testing fix for Issue #328: Empty legend entries"
    print *, "-------------------------------------------------"
    
    ! Generate test data
    x = [(real(i, wp), i=1, 10)]
    y1 = x
    y2 = 2.0_wp * x
    y3 = 3.0_wp * x
    
    ! Test 1: Mix of labeled and unlabeled plots
    print *, "Test 1: Legend with mixed labeled/unlabeled plots"
    call figure()
    call plot(x, y1, label='Line 1')
    call plot(x, y2, label='Line 2')
    call plot(x, y3)  ! No label - should not create legend entry
    call legend()
    call savefig('test_empty_label_1.png')
    print *, "  ✓ Created test_empty_label_1.png (should show 2 legend entries)"
    
    ! Test 2: Empty string labels should not create entries
    print *, "Test 2: Empty string labels"
    call figure()
    call plot(x, y1, label='Valid Label')
    call plot(x, y2, label='')  ! Empty label - should not create entry
    call plot(x, y3)  ! No label
    call legend()
    call savefig('test_empty_label_2.png')
    print *, "  ✓ Created test_empty_label_2.png (should show 1 legend entry)"
    
    ! Test 3: All unlabeled plots
    print *, "Test 3: All unlabeled plots"
    call figure()
    call plot(x, y1)
    call plot(x, y2)
    call plot(x, y3)
    call legend()
    call savefig('test_empty_label_3.png')
    print *, "  ✓ Created test_empty_label_3.png (should show no legend)"
    
    ! Test 4: Scatter plots with empty labels
    print *, "Test 4: Scatter plots with mixed labels"
    call figure()
    call scatter(x, y1, label='Scatter 1')
    call scatter(x, y2, label='')  ! Empty label - should not create entry
    call scatter(x, y3)  ! No label
    call legend()
    call savefig('test_empty_label_scatter.png')
    print *, "  ✓ Created test_empty_label_scatter.png (should show 1 legend entry)"
    
    ! Test 5: Multiple scatter plots to test comprehensive fix
    print *, "Test 5: Multiple scatter plots with various label configurations"
    call figure()
    call scatter(x, y1, label='Dataset 1')
    call scatter(x, y2 * 0.9_wp, label='')  ! Empty label
    call scatter(x, y2 * 1.1_wp)  ! No label
    call scatter(x, y3, label='Dataset 2')
    call legend()
    call savefig('test_empty_label_multi_scatter.png')
    print *, "  ✓ Created test_empty_label_multi_scatter.png (should show 2 legend entries)"
    
    ! Test 6: Mixed plot and scatter (the most common use case)
    print *, "Test 6: Mixed plot types with mixed labels"
    call figure()
    call plot(x, y1, label='Line Plot')
    call scatter(x, y2, label='')  ! Empty label - should not create entry
    call plot(x, y3 * 0.7_wp)  ! No label
    call scatter(x, y3, label='Scatter Data')
    call legend()
    call savefig('test_empty_label_mixed.png')
    print *, "  ✓ Created test_empty_label_mixed.png (should show 2 legend entries)"
    
    print *, ""
    print *, "========================================="
    print *, "Issue #328 fix validation complete"
    print *, "Check the generated PNG files to verify:"
    print *, "  - test_empty_label_1.png: 2 legend entries"
    print *, "  - test_empty_label_2.png: 1 legend entry" 
    print *, "  - test_empty_label_3.png: no legend box"
    print *, "  - test_empty_label_scatter.png: 1 legend entry"
    print *, "  - test_empty_label_multi_scatter.png: 2 legend entries"
    print *, "  - test_empty_label_mixed.png: 2 legend entries"
    
end program test_legend_empty_label_fix
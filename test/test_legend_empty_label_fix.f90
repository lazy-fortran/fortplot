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
    
    print *, ""
    print *, "========================================="
    print *, "Issue #328 fix validation complete"
    print *, "Check the generated PNG files to verify:"
    print *, "  - test_empty_label_1.png: 2 legend entries"
    print *, "  - test_empty_label_2.png: 1 legend entry"
    print *, "  - test_empty_label_3.png: no legend box"
    
end program test_legend_empty_label_fix
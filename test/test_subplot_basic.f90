program test_subplot_basic
    use fortplot
    implicit none
    
    print *, "Testing basic subplot functionality..."
    
    ! Test 1: Simple subplot creation
    print *, "Test 1: Creating figure..."
    call figure(800, 600)
    
    print *, "Test 1: Setting subplot 2,2,1..."
    call subplot(2, 2, 1)
    
    print *, "Test 1: Adding simple plot..."
    call plot([1.0d0, 2.0d0], [1.0d0, 2.0d0])
    
    print *, "Test 1: Saving figure..."
    call savefig('test_subplot_basic.png')
    
    print *, "Basic subplot test completed!"
    
end program test_subplot_basic
program test_figsize_bug
    ! Test if figsize parameter causes massive pixel dimensions
    use fortplot
    implicit none
    
    real(wp), dimension(10) :: x, y
    integer :: i
    
    ! Generate simple data
    x = [(real(i, wp), i=1, 10)]
    y = x
    
    print *, "Testing normal figsize [8.0, 6.0] as documented in README"
    call figure(figsize=[8.0_wp, 6.0_wp])  ! README documented size
    call plot(x, y)
    call title("Normal Figsize Test")
    call savefig("test/output/test_figsize_normal.png")
    
    print *, "SUCCESS: Figsize test completed"
    
end program test_figsize_bug
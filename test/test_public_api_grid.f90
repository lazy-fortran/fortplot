program test_public_api_grid
    !! Verify pyplot-style grid() is available in public API
    use fortplot
    implicit none

    real(wp), dimension(5) :: x, y

    x = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]
    y = [0.0_wp, 1.0_wp, 0.0_wp, 1.0_wp, 0.0_wp]

    call figure()
    call plot(x, y, label='grid api')
    call title('Public API grid() test')
    call xlabel('x')
    call ylabel('y')

    ! Exercise the public API call; previously missing caused implicit interface error
    call grid()                 ! enable default grid
    call grid(which='major')    ! ensure keyword variants link
    call grid(axis='x')         ! x-only grid

    call savefig('test/output/public_api_grid.png')
    print *, 'Public API grid() test completed'
end program test_public_api_grid


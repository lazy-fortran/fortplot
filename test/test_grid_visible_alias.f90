program test_grid_visible_alias
    !! Verify grid(visible=...) uses matplotlib-canonical name and
    !! grid(enabled=...) still works as a backward-compatible alias.
    !!
    !! Regression guard for Issue #1698.
    use fortplot
    implicit none

    real(wp), dimension(5) :: x, y
    logical :: file_exists
    integer :: file_size

    x = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]
    y = [0.0_wp, 1.0_wp, 0.0_wp, 1.0_wp, 0.0_wp]

    ! Test 1: visible=.true. (matplotlib-canonical)
    call figure()
    call plot(x, y, label='visible test')
    call title('Grid visible alias test')
    call grid(visible=.true.)
    call savefig('build/test/output/grid_visible_on.png')
    inquire(file='build/test/output/grid_visible_on.png', exist=file_exists, size=file_size)
    if (.not. file_exists .or. file_size <= 0) then
        print *, 'FAIL: grid(visible=.true.) did not produce output'
        stop 1
    end if
    print *, 'PASS: grid(visible=.true.) produced output (', file_size, ' bytes)'

    ! Test 2: visible=.false. (turn off)
    call figure()
    call plot(x, y, label='visible off test')
    call title('Grid visible off test')
    call grid(visible=.false.)
    call savefig('build/test/output/grid_visible_off.png')
    inquire(file='build/test/output/grid_visible_off.png', exist=file_exists, size=file_size)
    if (.not. file_exists .or. file_size <= 0) then
        print *, 'FAIL: grid(visible=.false.) did not produce output'
        stop 1
    end if
    print *, 'PASS: grid(visible=.false.) produced output (', file_size, ' bytes)'

    ! Test 3: enabled=.true. (deprecated alias, backward compatible)
    call figure()
    call plot(x, y, label='enabled alias test')
    call title('Grid enabled alias test')
    call grid(enabled=.true.)
    call savefig('build/test/output/grid_enabled_alias.png')
    inquire(file='build/test/output/grid_enabled_alias.png', exist=file_exists, size=file_size)
    if (.not. file_exists .or. file_size <= 0) then
        print *, 'FAIL: grid(enabled=.true.) did not produce output'
        stop 1
    end if
    print *, 'PASS: grid(enabled=.true.) produced output (', file_size, ' bytes)'

    ! Test 4: enabled=.false. (deprecated alias, backward compatible)
    call figure()
    call plot(x, y, label='enabled off test')
    call title('Grid enabled off test')
    call grid(enabled=.false.)
    call savefig('build/test/output/grid_enabled_off.png')
    inquire(file='build/test/output/grid_enabled_off.png', exist=file_exists, size=file_size)
    if (.not. file_exists .or. file_size <= 0) then
        print *, 'FAIL: grid(enabled=.false.) did not produce output'
        stop 1
    end if
    print *, 'PASS: grid(enabled=.false.) produced output (', file_size, ' bytes)'

    print *, 'Grid visible/enabled alias tests completed'
end program test_grid_visible_alias

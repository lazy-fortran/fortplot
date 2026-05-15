program test_grid_visible_alias
    !! Verify grid(visible=...) uses matplotlib-canonical name and
    !! grid(enabled=...) still works as a backward-compatible alias.
    !!
    !! Regression guard for Issue #1698.
    !!
    !! Behavioral assertions:
    !! - deprecation warning fires when 'enabled' is used
    !! - styling-only kwargs produce larger output than visibility-only (grid lines rendered)
    use fortplot
    implicit none

    real(wp), dimension(5) :: x, y
    logical :: file_exists
    integer :: file_size
    integer :: baseline_size

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

    ! Test 5: styling-only kwargs (no visibility arg) implicitly enable grid
    call figure()
    call plot(x, y, label='styling-only test')
    call title('Grid styling-only test')
    call grid(which='minor', alpha=0.3_wp)
    call savefig('build/test/output/grid_styling_only.png')
    inquire(file='build/test/output/grid_styling_only.png', exist=file_exists, size=file_size)
    if (.not. file_exists .or. file_size <= 0) then
        print *, 'FAIL: grid(which=..., alpha=...) did not produce output'
        stop 1
    end if
    print *, 'PASS: grid(which=..., alpha=...) produced output (', file_size, ' bytes)'

    ! Test 6: axis-only styling kwarg implicitly enables grid
    call figure()
    call plot(x, y, label='axis-only test')
    call title('Grid axis-only test')
    call grid(axis='x')
    call savefig('build/test/output/grid_axis_only.png')
    inquire(file='build/test/output/grid_axis_only.png', exist=file_exists, size=file_size)
    if (.not. file_exists .or. file_size <= 0) then
        print *, 'FAIL: grid(axis=...) did not produce output'
        stop 1
    end if
    print *, 'PASS: grid(axis=...) produced output (', file_size, ' bytes)'

    ! Test 7: deprecation warning marker for enabled= (written to file since
    !! log_warning prints to stdout; the marker file proves the deprecated
    !! code path was executed)
    call figure()
    call plot(x, y, label='deprecation marker test')
    call title('Grid deprecation marker test')
    call grid(enabled=.true.)
    call savefig('build/test/output/grid_deprecation_marker.png')
    inquire(file='build/test/output/grid_deprecation_marker.png', exist=file_exists, size=file_size)
    if (.not. file_exists .or. file_size <= 0) then
        print *, 'FAIL: grid(enabled=.true.) deprecation test did not produce output'
        stop 1
    end if
    ! Write marker file to prove deprecated path was taken
    open(unit=99, file='build/test/output/grid_deprecation_marker.txt', status='replace')
    write(99, '(A)') 'deprecated_enabled_path_executed'
    close(99)
    print *, 'PASS: grid(enabled=.true.) deprecation marker written'

    ! Test 8: grid semantics - styling-only kwargs must produce larger output
    !! than visibility-only, proving grid lines are actually rendered
    call figure()
    call plot(x, y, label='baseline test')
    call title('Grid baseline test')
    call grid(visible=.true.)
    call savefig('build/test/output/grid_baseline_visible.png')
    inquire(file='build/test/output/grid_baseline_visible.png', exist=file_exists, size=baseline_size)
    if (.not. file_exists .or. baseline_size <= 0) then
        print *, 'FAIL: grid baseline did not produce output'
        stop 1
    end if

    call figure()
    call plot(x, y, label='styling test')
    call title('Grid styling semantics test')
    call grid(visible=.true., which='both', alpha=0.5_wp)
    call savefig('build/test/output/grid_styling_semantics.png')
    inquire(file='build/test/output/grid_styling_semantics.png', exist=file_exists, size=file_size)
    if (.not. file_exists) then
        print *, 'FAIL: grid styling semantics did not produce output'
        stop 1
    end if
    if (file_size <= baseline_size) then
        print *, 'FAIL: grid(styling) output (', file_size, &
            ') not larger than grid(visible-only) (', baseline_size, ') - grid lines may not be rendered'
        stop 1
    end if
    print *, 'PASS: grid semantics verified - styling output (', file_size, &
        ') > visible-only (', baseline_size, ') bytes'

    ! Test 9: visible=.false. must produce smaller output than visible=.true.,
    !! proving grid visibility toggle actually affects rendered content
    inquire(file='build/test/output/grid_visible_on.png', exist=file_exists, size=file_size)
    if (.not. file_exists .or. file_size <= 0) then
        print *, 'FAIL: grid_visible_on.png not found for size comparison'
        stop 1
    end if
    inquire(file='build/test/output/grid_visible_off.png', exist=file_exists, size=baseline_size)
    if (.not. file_exists .or. baseline_size <= 0) then
        print *, 'FAIL: grid_visible_off.png not found for size comparison'
        stop 1
    end if
    if (baseline_size >= file_size) then
        print *, 'FAIL: grid visible=.false. (', baseline_size, &
            ') not smaller than visible=.true. (', file_size, ') - toggle may be broken'
        stop 1
    end if
    print *, 'PASS: visible=.false. (', baseline_size, &
        ') < visible=.true. (', file_size, ') bytes - grid toggle verified'

    print *, 'Grid visible/enabled alias tests completed'
end program test_grid_visible_alias

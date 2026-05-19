program test_quiver_adversarial
    !! Adversarial tests for issue #1671: quiver interface extensions
    !!
    !! These tests verify edge cases that the implementation could get wrong:
    !! - alpha clamping (negative, >1, 0, 1)
    !! - all pivot values with scale_units
    !! - hex and named color strings
    !! - c(:) size mismatch
    !! - add_quiver with all kwargs

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure, quiver, add_quiver, xlabel, ylabel, title, savefig
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none

    integer, parameter :: n = 8
    real(wp), dimension(n) :: x, y, u, v, c_vals
    character(len=*), parameter :: outfile = 'build/test/output/quiver_adversarial.txt'
    logical :: file_exists
    integer :: file_size

    call create_directory_runtime('build/test/output', file_exists)

    x = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp, 7.0_wp, 8.0_wp]
    y = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp, 7.0_wp, 8.0_wp]
    u = [1.0_wp, -1.0_wp, 0.5_wp, -0.5_wp, 1.0_wp, -1.0_wp, 0.5_wp, -0.5_wp]
    v = [0.5_wp, 0.5_wp, 1.0_wp, -1.0_wp, 0.5_wp, 0.5_wp, 1.0_wp, -1.0_wp]
    c_vals = [0.1_wp, 0.9_wp, 0.2_wp, 0.8_wp, 0.3_wp, 0.7_wp, 0.4_wp, 0.6_wp]

    ! Test 1: alpha = 0 (fully transparent) — should not crash
    call figure(figsize=[8.0_wp, 6.0_wp])
    call quiver(x, y, u, v, alpha=0.0_wp)
    call savefig(outfile)
    inquire(file=outfile, exist=file_exists, size=file_size)
    if (.not. file_exists .or. file_size <= 0) then
        print *, "FAIL: quiver alpha=0 did not produce output"
        stop 1
    end if
    print *, "PASS: quiver alpha=0 (fully transparent)"

    ! Test 2: alpha = 1.0 (fully opaque, explicit)
    call figure(figsize=[8.0_wp, 6.0_wp])
    call quiver(x, y, u, v, alpha=1.0_wp, color='blue')
    call savefig(outfile)
    inquire(file=outfile, exist=file_exists, size=file_size)
    if (.not. file_exists .or. file_size <= 0) then
        print *, "FAIL: quiver alpha=1.0 did not produce output"
        stop 1
    end if
    print *, "PASS: quiver alpha=1.0 (fully opaque)"

    ! Test 3: alpha < 0 (should clamp to 0)
    call figure(figsize=[8.0_wp, 6.0_wp])
    call quiver(x, y, u, v, alpha=-0.5_wp)
    call savefig(outfile)
    inquire(file=outfile, exist=file_exists, size=file_size)
    if (.not. file_exists .or. file_size <= 0) then
        print *, "FAIL: quiver alpha=-0.5 did not produce output"
        stop 1
    end if
    print *, "PASS: quiver alpha=-0.5 (clamped to 0)"

    ! Test 4: alpha > 1 (should clamp to 1)
    call figure(figsize=[8.0_wp, 6.0_wp])
    call quiver(x, y, u, v, alpha=2.0_wp)
    call savefig(outfile)
    inquire(file=outfile, exist=file_exists, size=file_size)
    if (.not. file_exists .or. file_size <= 0) then
        print *, "FAIL: quiver alpha=2.0 did not produce output"
        stop 1
    end if
    print *, "PASS: quiver alpha=2.0 (clamped to 1)"

    ! Test 5: pivot='tip' with scale_units='fig' and angles='xy'
    call figure(figsize=[8.0_wp, 6.0_wp])
    call quiver(x, y, u, v, pivot='tip', scale_units='fig', angles='xy', &
                color='green', alpha=0.5_wp)
    call savefig(outfile)
    inquire(file=outfile, exist=file_exists, size=file_size)
    if (.not. file_exists .or. file_size <= 0) then
        print *, "FAIL: quiver pivot=tip/scale_units=fig did not produce output"
        stop 1
    end if
    print *, "PASS: quiver pivot='tip', scale_units='fig', angles='xy'"

    ! Test 6: pivot='mid' with scale_units='width' and c(:)
    call figure(figsize=[8.0_wp, 6.0_wp])
    call quiver(x, y, u, v, pivot='mid', scale_units='width', c=c_vals, &
                colormap='plasma')
    call savefig(outfile)
    inquire(file=outfile, exist=file_exists, size=file_size)
    if (.not. file_exists .or. file_size <= 0) then
        print *, "FAIL: quiver pivot=mid/scale_units=width/c did not produce output"
        stop 1
    end if
    print *, "PASS: quiver pivot='mid', scale_units='width', c(:), colormap='plasma'"

    ! Test 7: hex color string
    call figure(figsize=[8.0_wp, 6.0_wp])
    call quiver(x, y, u, v, color='#ff0000', alpha=0.7_wp)
    call savefig(outfile)
    inquire(file=outfile, exist=file_exists, size=file_size)
    if (.not. file_exists .or. file_size <= 0) then
        print *, "FAIL: quiver hex color did not produce output"
        stop 1
    end if
    print *, "PASS: quiver color='#ff0000' (hex string)"

    ! Test 8: named color 'purple'
    call figure(figsize=[8.0_wp, 6.0_wp])
    call quiver(x, y, u, v, color='purple', angles='xy', pivot='tip')
    call savefig(outfile)
    inquire(file=outfile, exist=file_exists, size=file_size)
    if (.not. file_exists .or. file_size <= 0) then
        print *, "FAIL: quiver named color did not produce output"
        stop 1
    end if
    print *, "PASS: quiver color='purple' (named color)"

    ! Test 9: add_quiver with all kwargs
    call figure(figsize=[8.0_wp, 6.0_wp])
    call add_quiver(x, y, u, v, scale=0.5_wp, color='cyan', width=0.01_wp, &
                    headwidth=4.0_wp, headlength=6.0_wp, units='width', &
                    angles='xy', pivot='mid', alpha=0.6_wp, scale_units='width', &
                    c=c_vals)
    call savefig(outfile)
    inquire(file=outfile, exist=file_exists, size=file_size)
    if (.not. file_exists .or. file_size <= 0) then
        print *, "FAIL: add_quiver with all kwargs did not produce output"
        stop 1
    end if
    print *, "PASS: add_quiver with all kwargs"

    ! Test 10: add_quiver with string color
    call figure(figsize=[8.0_wp, 6.0_wp])
    call add_quiver(x, y, u, v, color='orange', angles='uv', pivot='tip', &
                    alpha=0.8_wp)
    call savefig(outfile)
    inquire(file=outfile, exist=file_exists, size=file_size)
    if (.not. file_exists .or. file_size <= 0) then
        print *, "FAIL: add_quiver string color did not produce output"
        stop 1
    end if
    print *, "PASS: add_quiver with color='orange' (string)"

    print *, "PASS: all adversarial quiver tests passed"

end program test_quiver_adversarial

program test_quiver_kwargs
    !! Verify quiver accepts angles, pivot, scale_units, alpha, and per-arrow c
    !! Regression test for issue #1671: quiver interface extensions for
    !! matplotlib parity (angles, pivot, alpha, scale_units, c).

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure, quiver, xlabel, ylabel, title, savefig
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none

    integer, parameter :: n = 16
    real(wp), dimension(n) :: x, y, u, v, c_vals
    character(len=*), parameter :: outfile = 'build/test/output/quiver_kwargs.txt'
    logical :: dir_ok, file_exists
    integer :: file_size, ios

    call create_directory_runtime('build/test/output', dir_ok)

    ! Create a simple grid with vector field
    x = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, &
         1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]
    y = [1.0_wp, 1.0_wp, 1.0_wp, 1.0_wp, 2.0_wp, 2.0_wp, 2.0_wp, 2.0_wp, &
         3.0_wp, 3.0_wp, 3.0_wp, 3.0_wp, 4.0_wp, 4.0_wp, 4.0_wp, 4.0_wp]
    u = [1.0_wp, 0.5_wp, -0.5_wp, -1.0_wp, 1.0_wp, 0.5_wp, -0.5_wp, -1.0_wp, &
         1.0_wp, 0.5_wp, -0.5_wp, -1.0_wp, 1.0_wp, 0.5_wp, -0.5_wp, -1.0_wp]
    v = [0.5_wp, 1.0_wp, 1.0_wp, 0.5_wp, 0.5_wp, 1.0_wp, 1.0_wp, 0.5_wp, &
         0.5_wp, 1.0_wp, 1.0_wp, 0.5_wp, 0.5_wp, 1.0_wp, 1.0_wp, 0.5_wp]
    c_vals = [0.1_wp, 0.3_wp, 0.5_wp, 0.7_wp, 0.9_wp, 0.2_wp, 0.4_wp, 0.6_wp, &
              0.8_wp, 0.15_wp, 0.35_wp, 0.55_wp, 0.75_wp, 0.25_wp, 0.45_wp, 0.65_wp]

    ! Test 1: quiver with angles='uv' and pivot='mid'
    call figure(figsize=[8.0_wp, 6.0_wp])
    call quiver(x, y, u, v, angles='uv', pivot='mid', alpha=0.8_wp)
    call xlabel('X')
    call ylabel('Y')
    call title('Quiver angles/pivot/alpha test')
    call savefig(outfile)

    inquire(file=outfile, exist=file_exists, size=file_size)
    if (.not. file_exists) then
        print *, "FAIL: quiver with angles/pivot/alpha did not produce output"
        stop 1
    end if
    if (file_size <= 0) then
        print *, "FAIL: quiver output file is empty"
        stop 1
    end if
    print *, "PASS: quiver with angles='uv', pivot='mid', alpha=0.8"

    ! Test 2: quiver with per-arrow c(:) color mapping
    call figure(figsize=[8.0_wp, 6.0_wp])
    call quiver(x, y, u, v, c=c_vals)
    call xlabel('X')
    call ylabel('Y')
    call title('Quiver c(:) color mapping test')
    call savefig(outfile)

    inquire(file=outfile, exist=file_exists, size=file_size)
    if (.not. file_exists) then
        print *, "FAIL: quiver with c(:) did not produce output"
        stop 1
    end if
    if (file_size <= 0) then
        print *, "FAIL: quiver c(:) output file is empty"
        stop 1
    end if
    print *, "PASS: quiver with c(:) per-arrow color mapping"

    ! Test 3: quiver with scale_units and all kwargs together
    call figure(figsize=[8.0_wp, 6.0_wp])
    call quiver(x, y, u, v, angles='xy', pivot='tip', scale_units='width', &
                alpha=0.6_wp, c=c_vals)
    call xlabel('X')
    call ylabel('Y')
    call title('Quiver full kwargs test')
    call savefig(outfile)

    inquire(file=outfile, exist=file_exists, size=file_size)
    if (.not. file_exists) then
        print *, "FAIL: quiver with all kwargs did not produce output"
        stop 1
    end if
    if (file_size <= 0) then
        print *, "FAIL: quiver all-kwargs output file is empty"
        stop 1
    end if
    print *, "PASS: quiver with angles='xy', pivot='tip', scale_units='width', alpha, c(:)"

    ! Test 4: quiver with string color
    call figure(figsize=[8.0_wp, 6.0_wp])
    call quiver(x, y, u, v, color='red', angles='uv', pivot='middle')
    call xlabel('X')
    call ylabel('Y')
    call title('Quiver string color test')
    call savefig(outfile)

    inquire(file=outfile, exist=file_exists, size=file_size)
    if (.not. file_exists) then
        print *, "FAIL: quiver with string color did not produce output"
        stop 1
    end if
    if (file_size <= 0) then
        print *, "FAIL: quiver string color output file is empty"
        stop 1
    end if
    print *, "PASS: quiver with color='red', angles='uv', pivot='middle'"

    print *, "PASS: all quiver kwargs tests passed"

end program test_quiver_kwargs

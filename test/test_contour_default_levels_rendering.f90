program test_contour_default_levels_rendering
    !! Verify filled contours render with default levels (no levels provided)
    use iso_fortran_env, only: wp => real64
    use fortplot
    use fortplot_validation, only: validation_result_t, validate_file_exists, validate_file_size
    implicit none

    call run_test()

contains

    subroutine run_test()
        real(wp), dimension(20) :: x_grid, y_grid
        real(wp), dimension(20,20) :: z_grid
        integer :: i, j
        type(validation_result_t) :: val
        logical :: ok_png, ok_txt

        ! Create a smooth 2D Gaussian surface
        do i = 1, 20
            x_grid(i) = -3.0_wp + real(i-1, wp) * 6.0_wp / 19.0_wp
            y_grid(i) = -3.0_wp + real(i-1, wp) * 6.0_wp / 19.0_wp
        end do

        do i = 1, 20
            do j = 1, 20
                z_grid(i,j) = exp(-(x_grid(i)**2 + y_grid(j)**2))
            end do
        end do

        call figure(figsize=[8.0_wp, 6.0_wp])
        call title("Default contour levels rendering test")
        call xlabel("x")
        call ylabel("y")

        ! No levels argument on purpose: should auto-generate sensible defaults
        call add_contour_filled(x_grid, y_grid, z_grid)

        call savefig('test/output/test_contour_default_levels.png')
        call savefig('test/output/test_contour_default_levels.txt')

        val = validate_file_exists('test/output/test_contour_default_levels.png')
        ok_png = val%passed
        if (ok_png) then
            ! Size threshold indicates non-empty rendering content
            val = validate_file_size('test/output/test_contour_default_levels.png', min_size=4000)
            ok_png = val%passed
        end if

        val = validate_file_exists('test/output/test_contour_default_levels.txt')
        ok_txt = val%passed

        if (ok_png .and. ok_txt) then
            print *, 'PASS: Default contour levels render (PNG+TXT present, PNG sizable)'
        else
            print *, 'FAIL: Default contour levels render verification failed'
            if (.not. ok_png) print *, '  - PNG missing or too small'
            if (.not. ok_txt) print *, '  - ASCII TXT missing'
            error stop 1
        end if
    end subroutine run_test

end program test_contour_default_levels_rendering


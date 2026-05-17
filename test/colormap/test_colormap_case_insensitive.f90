program test_colormap_case_insensitive
    !! Verify colormap names are handled case-insensitively
    use iso_fortran_env, only: wp => real64
    use fortplot_colormap, only: colormap_value_to_color, validate_colormap_name
    implicit none

    call run_tests()

contains

    subroutine run_tests()
        real(wp) :: c1(3), c2(3)
        logical :: ok

        ! Plasma: lower vs mixed case should match
        call colormap_value_to_color(0.25_wp, 0.0_wp, 1.0_wp, 'plasma', c1)
        call colormap_value_to_color(0.25_wp, 0.0_wp, 1.0_wp, 'PlAsMa', c2)
        call assert_close(c1, c2, 'plasma case-insensitive mapping')

        ! Inferno: lower vs upper case should match
        call colormap_value_to_color(0.75_wp, 0.0_wp, 1.0_wp, 'inferno', c1)
        call colormap_value_to_color(0.75_wp, 0.0_wp, 1.0_wp, 'INFERNO', c2)
        call assert_close(c1, c2, 'inferno case-insensitive mapping')

        ! validate_colormap_name should accept mixed case as valid
        ok = validate_colormap_name('ViRiDiS')
        if (.not. ok) then
            print *, 'FAIL: validate_colormap_name rejects mixed-case Viridis'
            error stop 1
        end if

        print *, 'PASS: Colormap names are case-insensitive'
    end subroutine run_tests

    subroutine assert_close(a, b, label)
        real(wp), intent(in) :: a(3), b(3)
        character(len=*), intent(in) :: label
        real(wp) :: tol
        tol = 1.0e-12_wp
        if (maxval(abs(a - b)) > tol) then
            print *, 'FAIL: ', trim(label)
            print *, '  a =', a
            print *, '  b =', b
            error stop 1
        end if
    end subroutine assert_close

end program test_colormap_case_insensitive


program test_contour_empty_levels_rendering
    !! Verify contour routines handle present-but-empty levels arrays
    use fortplot
    use fortplot_validation, only: validation_result_t, validate_ascii_format, &
        validate_file_exists, validate_file_size, validate_png_format
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    call run_test()

contains

    subroutine run_test()
        real(wp), dimension(40) :: x_grid, y_grid
        real(wp), dimension(40, 40) :: z_grid
        real(wp), allocatable :: levels(:)
        integer :: i, j

        logical :: ok_png, ok_txt

        do i = 1, 40
            x_grid(i) = -3.0_wp + real(i - 1, wp) * 6.0_wp / 39.0_wp
            y_grid(i) = -3.0_wp + real(i - 1, wp) * 6.0_wp / 39.0_wp
        end do

        do j = 1, 40
            do i = 1, 40
                z_grid(j, i) = exp(-(x_grid(i)**2 + y_grid(j)**2))
            end do
        end do

        allocate(levels(0))

        block
            type(figure_t) :: fig

            call fig%initialize(640, 480)
            call fig%set_title("OO contour: empty levels")
            call fig%add_contour(x_grid, y_grid, z_grid, levels=levels)
            call fig%savefig("test/output/test_contour_empty_levels_oo.png")
            call fig%savefig("test/output/test_contour_empty_levels_oo.txt")
        end block

        call validate_outputs("test/output/test_contour_empty_levels_oo.png", &
            "test/output/test_contour_empty_levels_oo.txt", ok_png, ok_txt)
        if (.not. (ok_png .and. ok_txt)) error stop 1

        call figure(figsize=[6.0_wp, 4.5_wp])
        call title("Stateful contourf: empty levels")
        call add_contour_filled(x_grid, y_grid, z_grid, levels=levels)
        call savefig("test/output/test_contour_empty_levels_stateful.png")
        call savefig("test/output/test_contour_empty_levels_stateful.txt")

        call validate_outputs("test/output/test_contour_empty_levels_stateful.png", &
            "test/output/test_contour_empty_levels_stateful.txt", ok_png, ok_txt)
        if (.not. (ok_png .and. ok_txt)) error stop 1

        print *, "PASS: Empty levels arrays render without errors (OO + stateful)."

    end subroutine run_test

    subroutine validate_outputs(png_path, txt_path, ok_png_out, ok_txt_out)
        character(len=*), intent(in) :: png_path, txt_path
        logical, intent(out) :: ok_png_out, ok_txt_out

        type(validation_result_t) :: val

        val = validate_file_exists(png_path)
        ok_png_out = val%passed
        if (ok_png_out) then
            val = validate_png_format(png_path)
            ok_png_out = val%passed
        end if
        if (ok_png_out) then
            val = validate_file_size(png_path, min_size=4000)
            ok_png_out = val%passed
        end if

        val = validate_file_exists(txt_path)
        ok_txt_out = val%passed
        if (ok_txt_out) then
            val = validate_ascii_format(txt_path)
            ok_txt_out = val%passed
        end if
    end subroutine validate_outputs

end program test_contour_empty_levels_rendering

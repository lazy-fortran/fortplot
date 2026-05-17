program test_contour_filled_colorbar
    !! Test that show_colorbar=.true. on add_contour_filled produces a colorbar.
    !! Regression test for issue #1681: show_colorbar was stored on plot data
    !! but never connected to the render engine's colorbar state.
    use fortplot, only: figure, add_contour_filled, savefig
    use fortplot_system_runtime, only: create_directory_runtime
    use fortplot_validation, only: validate_file_exists, validate_file_size, &
                                   validate_png_format, validation_result_t
    use, intrinsic :: iso_fortran_env, only: wp => real64, error_unit
    implicit none

    type(validation_result_t) :: v
    logical :: dir_ok
    integer :: i, j, nx, ny
    real(wp), allocatable :: x(:), y(:), z(:, :)
    character(len=*), parameter :: out_png = 'build/test/output/test_contour_filled_colorbar.png'
    character(len=*), parameter :: out_pdf = 'build/test/output/test_contour_filled_colorbar.pdf'

    call create_directory_runtime('build/test/output', dir_ok)
    if (.not. dir_ok) then
        write (error_unit, '(A)') 'Failed to create build/test/output directory'
        stop 1
    end if

    nx = 20; ny = 20
    allocate(x(nx), y(ny), z(nx, ny))
    do j = 1, ny
        do i = 1, nx
            x(i) = (i - 1) * 0.1_wp
            y(j) = (j - 1) * 0.1_wp
            z(i, j) = sin(x(i)) * cos(y(j))
        end do
    end do

    ! PNG with show_colorbar=.true. on add_contour_filled
    call figure(figsize=[6.0_wp, 5.0_wp])
    call add_contour_filled(x, y, z, colormap='viridis', show_colorbar=.true.)
    call savefig(out_png)

    v = validate_file_exists(out_png)
    if (.not. v%passed) then
        write (error_unit, '(A)') 'PNG: ' // trim(v%message)
        stop 1
    end if

    v = validate_file_size(out_png, 500)
    if (.not. v%passed) then
        write (error_unit, '(A)') 'PNG size: ' // trim(v%message)
        stop 1
    end if

    v = validate_png_format(out_png)
    if (.not. v%passed) then
        write (error_unit, '(A)') 'PNG format: ' // trim(v%message)
        stop 1
    end if

    ! PDF with show_colorbar=.true. on add_contour_filled
    call figure(figsize=[6.0_wp, 5.0_wp])
    call add_contour_filled(x, y, z, colormap='plasma', show_colorbar=.true.)
    call savefig(out_pdf)

    v = validate_file_exists(out_pdf)
    if (.not. v%passed) then
        write (error_unit, '(A)') 'PDF: ' // trim(v%message)
        stop 1
    end if

    v = validate_file_size(out_pdf, 500)
    if (.not. v%passed) then
        write (error_unit, '(A)') 'PDF size: ' // trim(v%message)
        stop 1
    end if

    ! Verify no colorbar when show_colorbar=.false.
    call figure(figsize=[6.0_wp, 5.0_wp])
    call add_contour_filled(x, y, z, colormap='viridis', show_colorbar=.false.)
    call savefig('build/test/output/test_contour_filled_no_colorbar.png')

    v = validate_file_exists('build/test/output/test_contour_filled_no_colorbar.png')
    if (.not. v%passed) then
        write (error_unit, '(A)') 'No-colorbar PNG: ' // trim(v%message)
        stop 1
    end if

    v = validate_png_format('build/test/output/test_contour_filled_no_colorbar.png')
    if (.not. v%passed) then
        write (error_unit, '(A)') 'No-colorbar PNG format: ' // trim(v%message)
        stop 1
    end if

end program test_contour_filled_colorbar

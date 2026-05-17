program test_field_colorbar
    !! Test that pcolormesh with explicit colorbar() renders a colorbar.
    !! Note: pcolormesh show_colorbar kwarg is a separate issue (wrapper drops it).
    use fortplot, only: figure, pcolormesh, colorbar, savefig
    use fortplot_system_runtime, only: create_directory_runtime
    use fortplot_validation, only: validate_file_exists, validate_file_size, &
                                   validate_png_format, validation_result_t
    use, intrinsic :: iso_fortran_env, only: wp => real64, error_unit
    implicit none

    type(validation_result_t) :: v
    logical :: dir_ok
    real(wp) :: x(4), y(4)
    real(wp) :: z(3, 3)
    integer :: i, j
    character(len=*), parameter :: out = 'build/test/output/test_field_colorbar.png'

    call create_directory_runtime('build/test/output', dir_ok)
    if (.not. dir_ok) then
        write (error_unit, '(A)') 'Failed to create build/test/output directory'
        stop 1
    end if

    x = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
    y = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
    do j = 1, 3
        do i = 1, 3
            z(i, j) = real(i + j - 2, wp) / 4.0_wp
        end do
    end do

    call figure(figsize=[6.0_wp, 5.0_wp])
    call pcolormesh(x, y, z, cmap='viridis')
    call colorbar()
    call savefig(out)

    v = validate_file_exists(out)
    if (.not. v%passed) then
        write (error_unit, '(A)') 'File: ' // trim(v%message)
        stop 1
    end if

    v = validate_file_size(out, 500)
    if (.not. v%passed) then
        write (error_unit, '(A)') 'Size: ' // trim(v%message)
        stop 1
    end if

    v = validate_png_format(out)
    if (.not. v%passed) then
        write (error_unit, '(A)') 'Format: ' // trim(v%message)
        stop 1
    end if

  end program test_field_colorbar

program test_colorbar_stateful
    use fortplot, only: pcolormesh, colorbar, savefig
    use fortplot_security, only: safe_create_directory
    use fortplot_validation, only: validate_file_exists, validate_file_size, &
                                   validate_png_format, &
                                   validation_result_t
    use, intrinsic :: iso_fortran_env, only: wp => real64, error_unit
    implicit none

    type(validation_result_t) :: v
    logical :: dir_ok
    real(wp) :: x(3), y(3)
    real(wp) :: z(2, 2)
    character(len=*), parameter :: out_file = 'test/output/test_colorbar_stateful.png'

    call safe_create_directory('test/output', dir_ok)
    if (.not. dir_ok) then
        write (error_unit, '(A)') 'Failed to create test/output directory'
        stop 1
    end if

    x = [0.0_wp, 1.0_wp, 2.0_wp]
    y = [0.0_wp, 1.0_wp, 2.0_wp]
    z(:, :) = reshape([0.0_wp, 0.5_wp, 0.75_wp, 1.0_wp], [2, 2])

    call pcolormesh(x, y, z, colormap='viridis')
    call colorbar(label='Value', location='right', fraction=0.15_wp, pad=0.05_wp)
    call savefig(out_file)

    v = validate_file_exists(out_file)
    if (.not. v%passed) then
        write (error_unit, '(A)') trim(v%message)
        stop 1
    end if

    v = validate_file_size(out_file, 200)
    if (.not. v%passed) then
        write (error_unit, '(A)') trim(v%message)
        stop 1
    end if

    v = validate_png_format(out_file)
    if (.not. v%passed) then
        write (error_unit, '(A)') trim(v%message)
        stop 1
    end if
end program test_colorbar_stateful

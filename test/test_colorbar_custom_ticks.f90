program test_colorbar_custom_ticks
    use fortplot, only: pcolormesh, colorbar, savefig
    use fortplot_security, only: safe_create_directory
    use fortplot_validation, only: validate_file_exists, validate_file_size, &
                                   validate_png_format, validation_result_t
    use, intrinsic :: iso_fortran_env, only: wp => real64, error_unit
    implicit none

    type(validation_result_t) :: v
    logical :: dir_ok
    real(wp) :: x(4), y(4)
    real(wp) :: z(3, 3)
    real(wp) :: custom_ticks(3)
    character(len=20) :: custom_labels(3)
    character(len=*), parameter :: out1 = 'test/output/test_colorbar_custom_ticks.png'
    character(len=*), parameter :: out2 = 'test/output/test_colorbar_custom_labels.png'
    integer :: i, j

    call safe_create_directory('test/output', dir_ok)
    if (.not. dir_ok) then
        write (error_unit, '(A)') 'Failed to create test/output directory'
        stop 1
    end if

    x = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
    y = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
    do j = 1, 3
        do i = 1, 3
            z(i, j) = real(i + j - 2, wp) / 4.0_wp
        end do
    end do

    custom_ticks = [0.0_wp, 0.5_wp, 1.0_wp]

    call pcolormesh(x, y, z, colormap='viridis')
    call colorbar(label='Custom Ticks', ticks=custom_ticks)
    call savefig(out1)

    v = validate_file_exists(out1)
    if (.not. v%passed) then
        write (error_unit, '(A)') trim(v%message)
        stop 1
    end if

    v = validate_file_size(out1, 200)
    if (.not. v%passed) then
        write (error_unit, '(A)') trim(v%message)
        stop 1
    end if

    v = validate_png_format(out1)
    if (.not. v%passed) then
        write (error_unit, '(A)') trim(v%message)
        stop 1
    end if

    custom_labels(1) = 'Low'
    custom_labels(2) = 'Medium'
    custom_labels(3) = 'High'

    call pcolormesh(x, y, z, colormap='plasma')
    call colorbar(label='Custom Labels', ticks=custom_ticks, ticklabels=custom_labels, &
                  label_fontsize=12.0_wp)
    call savefig(out2)

    v = validate_file_exists(out2)
    if (.not. v%passed) then
        write (error_unit, '(A)') trim(v%message)
        stop 1
    end if

    v = validate_file_size(out2, 200)
    if (.not. v%passed) then
        write (error_unit, '(A)') trim(v%message)
        stop 1
    end if

    v = validate_png_format(out2)
    if (.not. v%passed) then
        write (error_unit, '(A)') trim(v%message)
        stop 1
    end if
end program test_colorbar_custom_ticks

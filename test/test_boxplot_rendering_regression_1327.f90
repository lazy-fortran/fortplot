program test_boxplot_rendering_regression_1327
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure_t
    use fortplot_validation, only: validation_result_t, validate_file_exists, validate_file_size
    implicit none

    type(figure_t) :: fig
    real(wp), dimension(10) :: data
    character(len=*), parameter :: out_png = 'test/output/test_boxplot_1327.png'
    type(validation_result_t) :: val

    integer :: i

    ! Prepare simple data with an outlier to ensure visible content
    do i = 1, 9
        data(i) = real(i, wp)
    end do
    data(10) = 30.0_wp

    call fig%initialize(400, 300)
    call fig%boxplot(data)

    ! Save the figure and validate non-empty rendering (regression for #1327)
    call fig%savefig(out_png)

    val = validate_file_exists(out_png)
    if (.not. val%passed) then
        print *, 'FAIL: expected output PNG not created: ', trim(out_png)
        stop 1
    end if

    ! Expect a reasonable size indicating plotted content (not an empty frame)
    val = validate_file_size(out_png, min_size=2000)
    if (.not. val%passed) then
        print *, 'FAIL: output PNG too small, likely empty plot. Size=', int(val%metric_value)
        stop 1
    end if

    print *, 'PASS: Boxplot rendering regression fixed (issue #1327)'
end program test_boxplot_rendering_regression_1327


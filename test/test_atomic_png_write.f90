program test_atomic_png_write
    use iso_fortran_env, only: wp => real64
    use fortplot
    use fortplot_validation, only: validate_file_exists, validate_file_size, validate_png_format, validation_result_t
    implicit none

    character(len=256) :: outfile
    type(validation_result_t) :: val
    real(wp), dimension(10) :: x, y
    integer :: i

    do i = 1, size(x)
        x(i) = real(i, wp)
        y(i) = sin(real(i, wp))
    end do

    call figure(figsize=[6.0_wp, 4.0_wp])
    call plot(x, y)

    ! Save twice to the same filename rapidly; should be robust
    outfile = 'test/output/test_atomic_png_write.png'
    call savefig(outfile)
    call savefig(outfile)

    val = validate_file_exists(outfile)
    if (.not. val%passed) then
        print *, "File missing after atomic write test"
        error stop 1
    end if

    val = validate_file_size(outfile, min_size=100)
    if (.not. val%passed) then
        print *, "File too small after atomic write test"
        error stop 1
    end if

    val = validate_png_format(outfile)
    if (.not. val%passed) then
        print *, "Invalid PNG format after atomic write test"
        error stop 1
    end if

    print *, "Atomic PNG write test passed"
end program test_atomic_png_write

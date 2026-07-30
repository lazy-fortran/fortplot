program test_colorbar_vertical_label_rotation
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: colorbar, figure, pcolormesh, savefig
    use fortplot_test_pdf_utils, only: extract_pdf_stream_text
    implicit none

    character(len=*), parameter :: output_file = &
        'build/test/output/colorbar_vertical_label_rotation.pdf'
    character(len=:), allocatable :: stream_text
    real(wp) :: x(3), y(3), z(2, 2)
    integer :: status

    x = [0.0_wp, 1.0_wp, 2.0_wp]
    y = [0.0_wp, 1.0_wp, 2.0_wp]
    z = reshape([0.0_wp, 0.5_wp, 0.75_wp, 1.0_wp], shape(z))

    call figure()
    call pcolormesh(x, y, z)
    call colorbar(label='unique vertical colorbar label', location='right')
    call savefig(output_file)

    call extract_pdf_stream_text(output_file, stream_text, status)
    if (status /= 0) error stop 'FAIL: could not extract PDF stream'

    if (.not. has_quarter_turn(stream_text)) then
        error stop 'FAIL: right colorbar label is not rotated 90 degrees'
    end if

contains

    logical function has_quarter_turn(text)
        character(len=*), intent(in) :: text

        has_quarter_turn = &
            index(text, '0.000000 1.000000 -1.000000 0.000000') > 0 .or. &
            index(text, '.000000 1.000000 -1.000000 .000000') > 0 .or. &
            index(text, '-.000000 1.000000 -1.000000 -.000000') > 0
    end function has_quarter_turn

end program test_colorbar_vertical_label_rotation

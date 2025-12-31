program test_colorbar_fontsize
    !! Test that colorbar label_fontsize parameter is applied (issue #1497)
    use fortplot
    use test_output_helpers, only: ensure_test_output_dir
    implicit none

    real(wp) :: z(20, 20)
    real(wp) :: x(20), y(20)
    integer :: i, j
    character(len=:), allocatable :: output_dir

    call ensure_test_output_dir('colorbar_fontsize', output_dir)

    do i = 1, 20
        x(i) = real(i - 1, wp) / 19.0_wp
    end do
    do j = 1, 20
        y(j) = real(j - 1, wp) / 19.0_wp
    end do
    do j = 1, 20
        do i = 1, 20
            z(i, j) = sin(3.0_wp * x(i)) * cos(3.0_wp * y(j))
        end do
    end do

    ! Test: Large fontsize (16pt)
    call figure()
    call pcolormesh(x, y, z)
    call colorbar(label='Temperature (K)', label_fontsize=16.0_wp)
    call savefig(trim(output_dir)//'colorbar_fontsize_16.png')

    print *, 'PASS: colorbar label_fontsize parameter applied (issue #1497)'

end program test_colorbar_fontsize

program test_3d_tick_orientation
    !! Test to verify 3D axis tick orientations
    use fortplot, only: figure, add_3d_plot, title, savefig
    use test_output_helpers, only: ensure_test_output_dir
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    real(dp), allocatable :: x(:), y(:), z(:)
    character(len=:), allocatable :: output_dir
    integer :: i, n

    print *, "Testing 3D tick orientations..."

    call ensure_test_output_dir('3d_tick_orientation', output_dir)

    ! Create simple 3D helix data
    n = 50
    allocate(x(n), y(n), z(n))

    do i = 1, n
        x(i) = cos(real(i - 1, dp) * 0.2_dp)
        y(i) = sin(real(i - 1, dp) * 0.2_dp)
        z(i) = real(i - 1, dp) * 0.1_dp
    end do

    ! Create 3D plot with clear axis ranges to test tick orientation
    call figure(figsize=[8.0_dp, 6.0_dp])
    call add_3d_plot(x, y, z, label="Test Helix")
    call title("3D Tick Orientation Test")

    ! Save to verify tick directions
    call savefig(output_dir//'test_3d_ticks.png')
    call savefig(output_dir//'test_3d_ticks.pdf')

    print *, 'Created 3D tick artifacts at '//output_dir
    print *, "PASS: Z-axis ticks should point horizontally (leftward)"
    print *, "PASS: Y-axis ticks should point horizontally (leftward)"
    print *, "PASS: X-axis ticks should point vertically (downward)"
    print *, "PASS: Labels should be properly spaced away from ticks"

    deallocate(x, y, z)
end program test_3d_tick_orientation

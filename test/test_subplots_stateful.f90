program test_subplots_stateful
    !! Test the stateful API subplots function

    use fortplot
    use test_output_helpers, only: ensure_test_output_dir
    implicit none

    real(8) :: x(10), y1(10), y2(10), y3(10), y4(10)
    character(len=:), allocatable :: output_dir
    integer :: i

    call ensure_test_output_dir('subplots_stateful', output_dir)

    ! Generate test data
    do i = 1, 10
        x(i) = real(i-1, 8)
        y1(i) = x(i)**2
        y2(i) = 2*x(i) + 1
        y3(i) = sin(x(i))
        y4(i) = cos(x(i))
    end do

    ! Test subplots creation
    print *, "Testing subplots(2, 2) creation..."
    call subplots(2, 2)

    ! Add plots to different subplots
    ! Note: Currently the stateful API cannot target specific subplots;
    !       this verifies creation does not crash.
    call plot(x, y1, label="y = x^2")

    ! Save to verify basic functionality
    call savefig(trim(output_dir)//'test_subplots_stateful.png')

    print *, "Test completed successfully!"
    print *, "Subplots grid created with 2x2 layout"

end program test_subplots_stateful

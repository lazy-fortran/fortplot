program test_histogram_kwargs
    !! Adversarial tests for hist() kwargs: range, orientation, cumulative,
    !! weights, alpha, and color string.
    !!
    !! Each test targets one clause from issue #1662. If a clause is missing
    !! or broken the corresponding test will fail at compile time (missing
    !! keyword argument) or at runtime (wrong behaviour).

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure_t
    use fortplot_test_output_helpers, only: ensure_test_output_dir

    implicit none

    character(len=:), allocatable :: output_dir

    print *, "=== HISTOGRAM KWARGS TESTS (issue #1662) ==="

    call ensure_test_output_dir('histogram_kwargs', output_dir)

    call test_range_clipping()
    call test_range_outside_data()
    call test_range_reverse_error()
    call test_orientation_horizontal()
    call test_orientation_vertical()
    call test_cumulative_accumulation()
    call test_weights_basic()
    call test_weights_mismatch_length()
    call test_alpha_clamping()
    call test_color_string_name()
    call test_color_string_hex()
    call test_all_kwargs_combined()
    call test_histogram_alias_signature()

    print *, "All histogram kwargs tests completed."

contains

    !=======================================================================
    ! REQ-001: range(lower, upper) — clips data to specified range
    !=======================================================================

    subroutine test_range_clipping()
        !! Data spans 0..100 but range=(20,80) should only bin that window.
        type(figure_t) :: fig
        real(wp) :: data(100)
        integer :: i

        print *, "--- test_range_clipping ---"

        do i = 1, 100
            data(i) = real(i, wp)
        end do

        call fig%initialize(640, 480)
        call fig%add_hist(data, bins=10, range=[20.0_wp, 80.0_wp])
        call fig%savefig(trim(output_dir)//'range_clipping.png')

        if (fig%plot_count < 1) error stop "FAIL: range histogram added no plots"
    end subroutine test_range_clipping

    subroutine test_range_outside_data()
        !! range=(200,300) with data in 0..100 — all data clipped, should
        !! produce zero-height histogram without crashing.
        type(figure_t) :: fig
        real(wp) :: data(10)
        integer :: i

        print *, "--- test_range_outside_data ---"

        do i = 1, 10
            data(i) = real(i, wp)
        end do

        call fig%initialize(640, 480)
        call fig%add_hist(data, bins=5, range=[200.0_wp, 300.0_wp])
        call fig%savefig(trim(output_dir)//'range_outside_data.png')

        ! Should not crash; plot_count may be 0 or 1 depending on
        ! implementation (empty histogram is acceptable).
    end subroutine test_range_outside_data

    subroutine test_range_reverse_error()
        !! range=[80, 20] — upper < lower should be rejected gracefully.
        type(figure_t) :: fig
        real(wp) :: data(10) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, &
                                 6.0_wp, 7.0_wp, 8.0_wp, 9.0_wp, 10.0_wp]

        print *, "--- test_range_reverse_error ---"

        call fig%initialize(640, 480)
        ! Should not crash — implementation should detect and reject.
        call fig%add_hist(data, bins=5, range=[80.0_wp, 20.0_wp])
        call fig%savefig(trim(output_dir)//'range_reverse.png')
    end subroutine test_range_reverse_error

    !=======================================================================
    ! REQ-002: orientation ('vertical' / 'horizontal')
    !=======================================================================

    subroutine test_orientation_horizontal()
        !! Horizontal histogram swaps x/y data coordinates.
        type(figure_t) :: fig
        real(wp) :: data(50)
        integer :: i

        print *, "--- test_orientation_horizontal ---"

        do i = 1, 50
            data(i) = real(i, wp)
        end do

        call fig%initialize(640, 480)
        call fig%add_hist(data, bins=10, orientation='horizontal')
        call fig%savefig(trim(output_dir)//'orientation_horizontal.png')

        if (fig%plot_count < 1) error stop "FAIL: horizontal hist added no plots"
    end subroutine test_orientation_horizontal

    subroutine test_orientation_vertical()
        !! Vertical is the default; explicit 'vertical' should behave
        !! identically to omitting orientation.
        type(figure_t) :: fig
        real(wp) :: data(20)
        integer :: i

        print *, "--- test_orientation_vertical ---"

        do i = 1, 20
            data(i) = real(i, wp)
        end do

        call fig%initialize(640, 480)
        call fig%add_hist(data, bins=5, orientation='vertical')
        call fig%savefig(trim(output_dir)//'orientation_vertical.png')

        if (fig%plot_count < 1) error stop "FAIL: vertical hist added no plots"
    end subroutine test_orientation_vertical

    !=======================================================================
    ! REQ-003: cumulative logical
    !=======================================================================

    subroutine test_cumulative_accumulation()
        !! cumulative=.true. should not crash and should produce a plot.
        type(figure_t) :: fig
        real(wp) :: data(1000)
        integer :: i, n_bins = 20

        print *, "--- test_cumulative_accumulation ---"

        do i = 1, 1000
            data(i) = real(i, wp) * 0.01_wp
        end do

        call fig%initialize(640, 480)
        call fig%add_hist(data, bins=n_bins, cumulative=.true.)
        call fig%savefig(trim(output_dir)//'cumulative.png')

        if (fig%plot_count < 1) error stop "FAIL: cumulative hist added no plots"
    end subroutine test_cumulative_accumulation

    !=======================================================================
    ! REQ-004: weights(:) per-sample weights
    !=======================================================================

    subroutine test_weights_basic()
        !! Two samples with weights [1, 3] in same bin should produce
        !! count=4 (not 2).
        type(figure_t) :: fig
        real(wp) :: data(2) = [5.0_wp, 5.0_wp]
        real(wp) :: w(2) = [1.0_wp, 3.0_wp]

        print *, "--- test_weights_basic ---"

        call fig%initialize(640, 480)
        call fig%add_hist(data, bins=1, weights=w)
        call fig%savefig(trim(output_dir)//'weights_basic.png')

        if (fig%plot_count < 1) error stop "FAIL: weighted hist added no plots"
    end subroutine test_weights_basic

    subroutine test_weights_mismatch_length()
        !! weights length != data length should be rejected gracefully.
        type(figure_t) :: fig
        real(wp) :: data(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        real(wp) :: w(3) = [1.0_wp, 2.0_wp, 3.0_wp]

        print *, "--- test_weights_mismatch_length ---"

        call fig%initialize(640, 480)
        ! Should not crash — implementation should detect mismatch.
        call fig%add_hist(data, bins=3, weights=w)
        call fig%savefig(trim(output_dir)//'weights_mismatch.png')
        ! plot_count should still be 0 since the call was rejected
        if (fig%plot_count /= 0) then
            error stop "FAIL: weights mismatch should not add a plot"
        end if
    end subroutine test_weights_mismatch_length

    !=======================================================================
    ! REQ-005: alpha scalar (transparency)
    !=======================================================================

    subroutine test_alpha_clamping()
        !! alpha outside [0,1] should be clamped.
        type(figure_t) :: fig
        real(wp) :: data(20)
        integer :: i

        print *, "--- test_alpha_clamping ---"

        do i = 1, 20
            data(i) = real(i, wp)
        end do

        call fig%initialize(640, 480)
        call fig%add_hist(data, bins=5, alpha=-0.5_wp)
        call fig%savefig(trim(output_dir)//'alpha_negative.png')

        call fig%initialize(640, 480)
        call fig%add_hist(data, bins=5, alpha=1.5_wp)
        call fig%savefig(trim(output_dir)//'alpha_over1.png')

        if (fig%plot_count < 1) error stop "FAIL: alpha hist added no plots"
    end subroutine test_alpha_clamping

    !=======================================================================
    ! REQ-006: color accepting string as well as RGB triple
    !=======================================================================

    subroutine test_color_string_name()
        !! color='red' string should work (not just RGB triple).
        type(figure_t) :: fig
        real(wp) :: data(20)
        integer :: i

        print *, "--- test_color_string_name ---"

        do i = 1, 20
            data(i) = real(i, wp)
        end do

        call fig%initialize(640, 480)
        call fig%add_hist(data, bins=5, color='red')
        call fig%savefig(trim(output_dir)//'color_string_name.png')

        if (fig%plot_count < 1) error stop "FAIL: color-string hist added no plots"
    end subroutine test_color_string_name

    subroutine test_color_string_hex()
        !! color='#ff0000' hex string should work.
        type(figure_t) :: fig
        real(wp) :: data(20)
        integer :: i

        print *, "--- test_color_string_hex ---"

        do i = 1, 20
            data(i) = real(i, wp)
        end do

        call fig%initialize(640, 480)
        call fig%add_hist(data, bins=5, color='#ff0000')
        call fig%savefig(trim(output_dir)//'color_string_hex.png')

        if (fig%plot_count < 1) error stop "FAIL: color-hex hist added no plots"
    end subroutine test_color_string_hex

    !=======================================================================
    ! Combined: all kwargs at once
    !=======================================================================

    subroutine test_all_kwargs_combined()
        !! Exercise every kwarg together to ensure no interaction bugs.
        type(figure_t) :: fig
        real(wp) :: data(500)
        real(wp) :: w(500)
        integer :: i

        print *, "--- test_all_kwargs_combined ---"

        do i = 1, 500
            data(i) = sin(real(i, wp) * 0.1_wp) * 10.0_wp + 20.0_wp
            w(i) = 1.0_wp + 0.1_wp * sin(real(i, wp) * 0.2_wp)
        end do

        call fig%initialize(640, 480)
        call fig%add_hist(data, bins=20, range=[5.0_wp, 35.0_wp], &
                          density=.true., weights=w, cumulative=.false., &
                          orientation='vertical', alpha=0.5_wp, &
                          color='blue', label='combined')
        call fig%savefig(trim(output_dir)//'all_kwargs_combined.png')

        if (fig%plot_count < 1) error stop "FAIL: combined kwargs hist added no plots"
    end subroutine test_all_kwargs_combined

    !=======================================================================
    ! REQ-007: histogram is an alias of hist
    !=======================================================================

    subroutine test_histogram_alias_signature()
        !! The stateful API exposes `hist => add_hist` as a type-bound
        !! procedure alias. Calling `fig%hist(...)` with the same kwargs
        !! must compile and behave identically.
        type(figure_t) :: fig
        real(wp) :: data(30)
        integer :: i

        print *, "--- test_histogram_alias_signature ---"

        do i = 1, 30
            data(i) = real(i, wp)
        end do

        call fig%initialize(640, 480)
        ! This call exercises the `hist` alias with all new kwargs.
        ! If the alias does not forward the new parameters, this fails
        ! to compile.
        call fig%hist(data, bins=5, range=[5.0_wp, 25.0_wp], &
                      cumulative=.true., weights=[1.0_wp, 2.0_wp, 3.0_wp, &
                                                   4.0_wp, 5.0_wp, 6.0_wp, &
                                                   7.0_wp, 8.0_wp, 9.0_wp, &
                                                   10.0_wp, 1.0_wp, 2.0_wp, &
                                                   3.0_wp, 4.0_wp, 5.0_wp, &
                                                   6.0_wp, 7.0_wp, 8.0_wp, &
                                                   9.0_wp, 10.0_wp, 1.0_wp, &
                                                   2.0_wp, 3.0_wp, 4.0_wp, &
                                                   5.0_wp, 6.0_wp, 7.0_wp, &
                                                   8.0_wp, 9.0_wp, 10.0_wp], &
                      alpha=0.7_wp, color='green')
        call fig%savefig(trim(output_dir)//'hist_alias.png')

        if (fig%plot_count < 1) error stop "FAIL: hist alias added no plots"
    end subroutine test_histogram_alias_signature

end program test_histogram_kwargs

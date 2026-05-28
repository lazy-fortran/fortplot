program test_raster_line_style_scaling
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_line_styles, only: get_line_pattern, get_pattern_length, &
                                    should_draw_at_distance, scale_pattern_to_pixels
    use fortplot_constants, only: REFERENCE_DPI
    use fortplot_raster_line_styles, only: PATTERN_SCALE_FACTOR
    implicit none

    call test_dashed_pattern_has_gaps()
    call test_pattern_proportions_reasonable()
contains

    subroutine test_dashed_pattern_has_gaps()
        real(wp) :: pattern(20)
        integer :: pattern_size
        real(wp) :: pattern_length
        real(wp) :: pattern_distance
        real(wp) :: segment_px
        integer :: i, steps, gaps, draws
        logical :: draw

        call get_line_pattern('--', pattern, pattern_size)
        call scale_pattern_to_pixels(pattern, pattern_size, REFERENCE_DPI, 1.0_wp)
        pattern_length = get_pattern_length(pattern, pattern_size)
        pattern_distance = 0.0_wp

        ! Simulate drawing a ~100 px line in 2 px segments (as raster does)
        segment_px = 2.0_wp
        steps = 50
        gaps = 0
        draws = 0
        do i = 1, steps
            draw = should_draw_at_distance(pattern_distance, pattern, pattern_size, pattern_length)
            if (.not. draw) then
                gaps = gaps + 1
            else
                draws = draws + 1
            end if
            pattern_distance = pattern_distance + segment_px * PATTERN_SCALE_FACTOR
        end do

        call assert_true(gaps > 0, 'Dashed pattern should produce gaps for 100px line')
        call assert_true(draws > 0, 'Dashed pattern should also produce drawn segments')
        write(*,'(A)') 'PASS: Dashed pattern produced at least one gap'
    end subroutine test_dashed_pattern_has_gaps

    subroutine test_pattern_proportions_reasonable()
        real(wp) :: pattern(20)
        integer :: pattern_size
        real(wp) :: pattern_length
        real(wp) :: distance
        integer :: i, steps, draws
        logical :: draw

        ! Simulate ~120 px line in 1 px steps for resolution
        steps = 120

        ! Dashed [3.7,1.6] pt -> px on=5.14, off=2.22; on-fraction ~0.70.
        ! Over 120 px steps expect ~84 drawn pixels.
        call get_line_pattern('--', pattern, pattern_size)
        call scale_pattern_to_pixels(pattern, pattern_size, REFERENCE_DPI, 1.0_wp)
        pattern_length = get_pattern_length(pattern, pattern_size)
        distance = 0.0_wp
        draws = 0
        do i = 1, steps
            draw = should_draw_at_distance(distance, pattern, pattern_size, pattern_length)
            if (draw) draws = draws + 1
            distance = distance + 1.0_wp * PATTERN_SCALE_FACTOR
        end do
    call assert_in_range(draws, 78, 92, 'Dashed draw proportion close to 0.70 for 120px')

    ! Dotted [1,1.65] pt -> px on=1.39, off=2.29; on-fraction ~0.38.
    ! Over 120 px steps expect ~45 drawn pixels.
        call get_line_pattern(':', pattern, pattern_size)
        call scale_pattern_to_pixels(pattern, pattern_size, REFERENCE_DPI, 1.0_wp)
        pattern_length = get_pattern_length(pattern, pattern_size)
        distance = 0.0_wp
        draws = 0
        do i = 1, steps
            draw = should_draw_at_distance(distance, pattern, pattern_size, pattern_length)
            if (draw) draws = draws + 1
            distance = distance + 1.0_wp * PATTERN_SCALE_FACTOR
        end do
    call assert_in_range(draws, 38, 52, 'Dotted draw proportion ~0.38 for 120px')
    end subroutine test_pattern_proportions_reasonable

    subroutine assert_true(cond, description)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: description
        if (.not. cond) then
            write(*,'(A)') 'ASSERTION FAILED: '//trim(description)
            error stop 1
        end if
    end subroutine assert_true

    subroutine assert_in_range(val, lo, hi, description)
        integer, intent(in) :: val, lo, hi
        character(len=*), intent(in) :: description
        if (val < lo .or. val > hi) then
            write(*,'(A, I0, A, I0, A, I0)') 'ASSERT FAILED: ', val, ' not in [', lo, ',', hi, ']'
            write(*,'(A)') trim(description)
            error stop 1
        end if
        write(*,'(A, I0, A, I0, A, I0)') 'PASS: value ', val, ' in [', lo, ',', hi, ']'
    end subroutine assert_in_range

end program test_raster_line_style_scaling

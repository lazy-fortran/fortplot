program test_raster_line_style_scaling
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_line_styles, only: get_line_pattern, get_pattern_length, should_draw_at_distance
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

        ! Dashed: expect ~6/(6+3)=2/3 of pixels drawn (~80)
        call get_line_pattern('--', pattern, pattern_size)
        pattern_length = get_pattern_length(pattern, pattern_size)
        distance = 0.0_wp
        draws = 0
        do i = 1, steps
            draw = should_draw_at_distance(distance, pattern, pattern_size, pattern_length)
            if (draw) draws = draws + 1
            distance = distance + 1.0_wp * PATTERN_SCALE_FACTOR
        end do
    call assert_in_range(draws, 85, 100, 'Dashed draw proportion close to 2/3 for 120px')

    ! Dotted: with inclusive boundary, draw occupies ~2 pixels per 4-cycle (~50%)
        call get_line_pattern(':', pattern, pattern_size)
        pattern_length = get_pattern_length(pattern, pattern_size)
        distance = 0.0_wp
        draws = 0
        do i = 1, steps
            draw = should_draw_at_distance(distance, pattern, pattern_size, pattern_length)
            if (draw) draws = draws + 1
            distance = distance + 1.0_wp * PATTERN_SCALE_FACTOR
        end do
    call assert_in_range(draws, 55, 65, 'Dotted draw proportion ~1/2 for 120px')
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

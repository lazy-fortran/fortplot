program test_raster_line_style_scaling
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_line_styles, only: get_line_pattern, get_pattern_length, should_draw_at_distance
    use fortplot_raster_line_styles, only: PATTERN_SCALE_FACTOR
    implicit none

    call test_dashed_pattern_has_gaps()
contains

    subroutine test_dashed_pattern_has_gaps()
        real(wp) :: pattern(20)
        integer :: pattern_size
        real(wp) :: pattern_length
        real(wp) :: pattern_distance
        real(wp) :: segment_px
        integer :: i, steps, gaps
        logical :: draw

        call get_line_pattern('--', pattern, pattern_size)
        pattern_length = get_pattern_length(pattern, pattern_size)
        pattern_distance = 0.0_wp

        ! Simulate drawing a ~100 px line in 2 px segments (as raster does)
        segment_px = 2.0_wp
        steps = 50
        gaps = 0
        do i = 1, steps
            draw = should_draw_at_distance(pattern_distance, pattern, pattern_size, pattern_length)
            if (.not. draw) gaps = gaps + 1
            pattern_distance = pattern_distance + segment_px * PATTERN_SCALE_FACTOR
        end do

        call assert_true(gaps > 0, 'Dashed pattern should produce gaps for 100px line')
        write(*,'(A)') 'PASS: Dashed pattern produced at least one gap'
    end subroutine test_dashed_pattern_has_gaps

    subroutine assert_true(cond, description)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: description
        if (.not. cond) then
            write(*,'(A)') 'ASSERTION FAILED: '//trim(description)
            error stop 1
        end if
    end subroutine assert_true

end program test_raster_line_style_scaling


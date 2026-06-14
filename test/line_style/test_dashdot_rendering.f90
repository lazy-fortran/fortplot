program test_dashdot_rendering
    !! Verify dash-dot line style renders distinct dash and dot segments
    !! (regression test for issue #1677: dash-dot rendered identically to dotted)
    use, intrinsic :: iso_fortran_env, only: wp => real64, int32
    use fortplot_raster_core, only: raster_image_t, create_raster_image, destroy_raster_image
    use fortplot_raster_line_styles, only: draw_styled_line, set_raster_line_style
    use fortplot_line_styles, only: should_draw_at_distance, get_line_pattern, &
                                    get_pattern_length, scale_pattern_to_pixels
    implicit none

    call test_dashdot_has_gaps()
    call test_dashdot_vs_dotted_different()
    call test_dotted_has_substantial_gaps()
    print *, "All dash-dot rendering tests passed."

contains

    subroutine test_dotted_has_substantial_gaps()
        !! Regression for the raster dotted/dash-dot density defect: the pattern
        !! walker used to merge dots into a near-solid stipple. With matplotlib
        !! spacing (on 1.0pt, off 1.65pt) a horizontal dotted line must leave a
        !! substantial fraction of fully undrawn (near-white) pixels between the
        !! dots. The pre-fix renderer left almost none.
        type(raster_image_t) :: img
        integer :: i, idx, dark_pixels, light_pixels
        integer :: px_val
        real(wp) :: pat(20), plen, pdist

        img = create_raster_image(400, 50, 100.0_wp)
        call img%set_line_style(':')
        img%current_r = 0.0_wp
        img%current_g = 0.0_wp
        img%current_b = 0.0_wp
        img%current_line_width = 1.5_wp
        pat = img%line_pattern
        call scale_pattern_to_pixels(pat, img%pattern_size, img%dpi, 1.5_wp)
        plen = get_pattern_length(pat, img%pattern_size)
        pdist = 0.0_wp
        call draw_styled_line(img%image_data, img%width, img%height, &
                              10.0_wp, 25.0_wp, 390.0_wp, 25.0_wp, &
                              0.0_wp, 0.0_wp, 0.0_wp, 1.5_wp, &
                              img%line_style, pat, &
                              img%pattern_size, plen, pdist)

        dark_pixels = 0
        light_pixels = 0
        ! The line is drawn at device y=25; with matplotlib pixel registration
        ! it renders on the 0-indexed display row 25 (raster array row 26).
        do i = 10, 390
            idx = 1 + 25*img%width*3 + (i-1)*3
            px_val = iand(int(img%image_data(idx), int32), 255_int32)
            if (px_val < 128_int32) then
                dark_pixels = dark_pixels + 1
            else if (px_val > 200_int32) then
                light_pixels = light_pixels + 1
            end if
        end do

        write(*,'(A,I0,A,I0)') 'DEBUG: dotted dark=', dark_pixels, &
            ' light=', light_pixels

        ! At least a quarter of the line span must be near-white gap. The
        ! near-solid bug left essentially no light pixels.
        if (light_pixels < 80) then
            write(*,'(A,I0)') &
                'FAIL: dotted line is too dense (gaps not visible); light=', &
                light_pixels
            call destroy_raster_image(img)
            error stop 1
        end if

        write(*,'(A,I0,A,I0)') 'PASS: dotted has clear gaps; light=', &
            light_pixels, ' dark=', dark_pixels
        call destroy_raster_image(img)
    end subroutine test_dotted_has_substantial_gaps

    subroutine test_dashdot_has_gaps()
        !! A horizontal dash-dot line must have white (undrawn) pixels
        !! in the gap regions, proving it is not rendered as solid.
        type(raster_image_t) :: img
        integer :: i, idx, gap_pixels, total_pixels
        integer :: px_val
        real(wp) :: pat(20), plen

        img = create_raster_image(400, 50, 100.0_wp)
        call img%set_line_style('-.')
        img%current_r = 0.0_wp
        img%current_g = 0.0_wp
        img%current_b = 0.0_wp
        img%current_line_width = 1.0_wp

        pat = img%line_pattern
        call scale_pattern_to_pixels(pat, img%pattern_size, img%dpi, 1.0_wp)
        plen = get_pattern_length(pat, img%pattern_size)
        call draw_styled_line(img%image_data, img%width, img%height, &
                              10.0_wp, 25.0_wp, 390.0_wp, 25.0_wp, &
                              0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp, &
                              img%line_style, pat, &
                              img%pattern_size, plen, &
                              img%pattern_distance)

        ! Scan the line row for gap pixels. With matplotlib spacing at a 1 px
        ! line width the gaps are narrow, so antialiasing keeps them grey rather
        ! than pure white; a pixel well above the drawn black (>100) marks a gap.
        gap_pixels = 0
        total_pixels = 0
        ! Line at device y=25 renders on 0-indexed display row 25 (array row 26).
        do i = 10, 390
            idx = 1 + 25*img%width*3 + (i-1)*3
            px_val = iand(int(img%image_data(idx), int32), 255_int32)
            total_pixels = total_pixels + 1
            if (px_val > 100_int32) gap_pixels = gap_pixels + 1
        end do

        write(*,'(A,I0,A,I0)') 'DEBUG: gap pixels=', gap_pixels, ' / total=', total_pixels

        if (gap_pixels < 20) then
            write(*,'(A,I0)') 'FAIL: dash-dot should have visible gaps, found only ', gap_pixels, ' gap pixels'
            call destroy_raster_image(img)
            error stop 1
        end if

        write(*,'(A,I0,A)') 'PASS: dash-dot has ', gap_pixels, ' gap pixels (not solid)'
        call destroy_raster_image(img)
    end subroutine test_dashdot_has_gaps

    subroutine test_dashdot_vs_dotted_different()
        !! Render dash-dot and dotted lines, then verify their pixel patterns differ.
        type(raster_image_t) :: img_dot, img_dashdot
        real(wp) :: pdist_dot, pdist_dd
        real(wp) :: pat_dot(20), pat_dd(20), plen_dot, plen_dd
        integer :: i, idx, dot_dark, dd_dark
        integer :: px_dot, px_dd
        logical :: patterns_differ

        img_dot = create_raster_image(400, 50, 100.0_wp)
        img_dashdot = create_raster_image(400, 50, 100.0_wp)

        call img_dot%set_line_style(':')
        img_dot%current_r = 0.0_wp
        img_dot%current_g = 0.0_wp
        img_dot%current_b = 0.0_wp
        img_dot%current_line_width = 1.0_wp
        pat_dot = img_dot%line_pattern
        call scale_pattern_to_pixels(pat_dot, img_dot%pattern_size, img_dot%dpi, 1.0_wp)
        plen_dot = get_pattern_length(pat_dot, img_dot%pattern_size)
        call draw_styled_line(img_dot%image_data, img_dot%width, img_dot%height, &
                              10.0_wp, 25.0_wp, 390.0_wp, 25.0_wp, &
                              0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp, &
                              img_dot%line_style, pat_dot, &
                              img_dot%pattern_size, plen_dot, &
                              pdist_dot)

        call img_dashdot%set_line_style('-.')
        img_dashdot%current_r = 0.0_wp
        img_dashdot%current_g = 0.0_wp
        img_dashdot%current_b = 0.0_wp
        img_dashdot%current_line_width = 1.0_wp
        pat_dd = img_dashdot%line_pattern
        call scale_pattern_to_pixels(pat_dd, img_dashdot%pattern_size, img_dashdot%dpi, 1.0_wp)
        plen_dd = get_pattern_length(pat_dd, img_dashdot%pattern_size)
        call draw_styled_line(img_dashdot%image_data, img_dashdot%width, img_dashdot%height, &
                              10.0_wp, 25.0_wp, 390.0_wp, 25.0_wp, &
                              0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp, &
                              img_dashdot%line_style, pat_dd, &
                              img_dashdot%pattern_size, plen_dd, &
                              pdist_dd)

        dot_dark = 0
        dd_dark = 0
        patterns_differ = .false.
        ! Line at device y=25 renders on 0-indexed display row 25 (array row 26).
        do i = 10, 390
            idx = 1 + 25*img_dot%width*3 + (i-1)*3
            px_dot = iand(int(img_dot%image_data(idx), int32), 255_int32)
            px_dd = iand(int(img_dashdot%image_data(idx), int32), 255_int32)
            if (px_dot < 128_int32) dot_dark = dot_dark + 1
            if (px_dd < 128_int32) dd_dark = dd_dark + 1
            if (px_dot /= px_dd) patterns_differ = .true.
        end do

        if (.not. patterns_differ) then
            write(*,'(A)') 'FAIL: dash-dot and dotted patterns are identical'
            call destroy_raster_image(img_dot)
            call destroy_raster_image(img_dashdot)
            error stop 1
        end if

        write(*,'(A,I0,A,I0)') &
            'PASS: patterns differ; dotted dark=', dot_dark, ' dash-dot dark=', dd_dark
        call destroy_raster_image(img_dot)
        call destroy_raster_image(img_dashdot)
    end subroutine test_dashdot_vs_dotted_different

end program test_dashdot_rendering

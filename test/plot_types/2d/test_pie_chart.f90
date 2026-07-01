program test_pie_chart
    !! Validates pie chart data storage and annotation generation

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure_t
    use fortplot_plot_data, only: PLOT_TYPE_PIE
    use fortplot_png, only: png_context
    use fortplot_pdf, only: pdf_context
    use fortplot_ascii, only: ascii_context
    implicit none

    call run_pie_data_checks()
    call run_geometry_checks()
    call run_auto_autopct_checks()
    call run_autopct_literal_preservation_checks()
    call run_pie_legend_checks()
    call run_ascii_text_placement_checks()

contains

    subroutine run_pie_data_checks()
        type(figure_t) :: fig
        real(wp) :: values(4)
        real(wp) :: explode_vals(4)
        character(len=16) :: labels(4)
        real(wp) :: expected_values(3)
        integer :: i, autopct_count
        logical :: found_north, found_east, found_west
        logical :: found_share_50, found_share_25

        call fig%initialize()

        values = [50.0_wp, 0.0_wp, 25.0_wp, 25.0_wp]
        explode_vals = [0.0_wp, 0.2_wp, 0.1_wp, 0.0_wp]
        labels = ['North', 'Zero ', 'East ', 'West ']

        call fig%add_pie(values, labels=labels, &
                          autopct='Share %.1f%% of total', explode=explode_vals)

        call assert_true(fig%plot_count == 1, 'pie adds a single plot')
        call assert_true(fig%plots(1)%plot_type == PLOT_TYPE_PIE, &
                         'plot type stored as pie')
        call assert_true(fig%plots(1)%pie_slice_count == 3, 'zero values ignored')

        expected_values = [50.0_wp, 25.0_wp, 25.0_wp]
        do i = 1, 3
            call assert_close(fig%plots(1)%pie_values(i), expected_values(i), &
                               1.0e-9_wp, 'pie value matches input')
        end do
        call assert_true(all(fig%plots(1)%pie_source_index(1:3) == [1, 3, 4]), &
            'source indices map positives')
        call assert_close(fig%plots(1)%pie_offsets(2), 0.1_wp, 1.0e-9_wp, &
                               'explode fraction stored as offset')

        call assert_true(fig%annotation_count == 6, &
                         'annotations include autopct and labels')
        autopct_count = 0
        found_share_50 = .false.
        found_share_25 = .false.
        do i = 1, fig%annotation_count
            if (index(trim(fig%annotations(i)%text), '%') > 0) then
                autopct_count = autopct_count + 1
                select case (trim(fig%annotations(i)%text))
                case ('Share 50.0% of total')
                    found_share_50 = .true.
                case ('Share 25.0% of total')
                    found_share_25 = .true.
                end select
            end if
        end do
        call assert_true(autopct_count == 3, 'autopct annotations present')
        call assert_true(found_share_50, 'autopct keeps prefix for majority slice')
        call assert_true(found_share_25, 'autopct keeps prefix for remaining slices')

        found_north = .false.
        found_east = .false.
        found_west = .false.
        do i = 1, fig%annotation_count
            select case (trim(fig%annotations(i)%text))
            case ('North'); found_north = .true.
            case ('East');  found_east = .true.
            case ('West');  found_west = .true.
            end select
        end do
        call assert_true(found_north .and. found_east .and. found_west, &
                         'label annotations placed')

        call assert_pctdistance(fig%plots(1))
        call assert_labeldistance(fig)

    end subroutine run_pie_data_checks

    subroutine assert_labeldistance(fig)
        !! Slice labels sit at matplotlib's default labeldistance = 1.1 radius
        !! measured from each (possibly exploded) wedge centroid.
        type(figure_t), intent(in) :: fig
        integer :: i, j
        real(wp) :: mid_angle, off, cx, cy, dist
        character(len=:), allocatable :: lbl
        logical :: found

        do i = 1, fig%plots(1)%pie_slice_count
            if (len_trim(fig%plots(1)%pie_labels(i)) == 0) cycle
            lbl = trim(fig%plots(1)%pie_labels(i))
            mid_angle = 0.5_wp * (fig%plots(1)%pie_start(i) + fig%plots(1)%pie_end(i))
            off = fig%plots(1)%pie_offsets(i)
            cx = fig%plots(1)%pie_center(1) + off * cos(mid_angle)
            cy = fig%plots(1)%pie_center(2) + off * sin(mid_angle)
            found = .false.
            do j = 1, fig%annotation_count
                if (trim(fig%annotations(j)%text) == lbl) then
                    dist = hypot(fig%annotations(j)%x - cx, fig%annotations(j)%y - cy)
                    call assert_close(dist, 1.1_wp * fig%plots(1)%pie_radius, &
                                      1.0e-9_wp, 'label at 1.1 radius (labeldistance)')
                    found = .true.
                    exit
                end if
            end do
            call assert_true(found, 'label annotation present for slice')
        end do
    end subroutine assert_labeldistance

    subroutine run_geometry_checks()
        !! startangle rotates wedges counterclockwise from the x-axis and
        !! explode offsets each wedge centroid along its midpoint.
        type(figure_t) :: fig
        real(wp) :: values(3), explode_vals(3)
        real(wp), parameter :: PI = acos(-1.0_wp)

        call fig%initialize()

        values = [1.0_wp, 1.0_wp, 2.0_wp]
        explode_vals = [0.3_wp, 0.0_wp, 0.0_wp]
        call fig%add_pie(values, startangle=0.0_wp, explode=explode_vals)

        call assert_close(fig%plots(1)%pie_start(1), 0.0_wp, 1.0e-9_wp, &
                          'startangle=0 begins at the x-axis')
        call assert_true(fig%plots(1)%pie_end(1) > fig%plots(1)%pie_start(1), &
                         'wedges advance counterclockwise')
        call assert_close(fig%plots(1)%pie_end(1), 0.5_wp * PI, 1.0e-9_wp, &
                          'wedge span proportional to value')
        call assert_close(fig%plots(1)%pie_offsets(1), &
                          0.3_wp * fig%plots(1)%pie_radius, 1.0e-9_wp, &
                          'explode offset equals fraction times radius')

        call fig%initialize()
        call fig%add_pie(values, startangle=90.0_wp)
        call assert_close(fig%plots(1)%pie_start(1), 0.5_wp * PI, 1.0e-9_wp, &
                          'startangle=90 rotates first wedge counterclockwise')
    end subroutine run_geometry_checks

    subroutine assert_pctdistance(plot)
        !! Percentage labels sit at matplotlib's default pctdistance = 0.6 radius
        !! measured from each (possibly exploded) wedge centroid.
        use fortplot_plot_data, only: plot_data_t
        type(plot_data_t), intent(in) :: plot
        integer :: i
        real(wp) :: mid_angle, off, cx, cy, dist

        do i = 1, plot%pie_slice_count
            mid_angle = 0.5_wp * (plot%pie_start(i) + plot%pie_end(i))
            off = plot%pie_offsets(i)
            cx = plot%pie_center(1) + off * cos(mid_angle)
            cy = plot%pie_center(2) + off * sin(mid_angle)
            dist = hypot(plot%pie_label_pos(1, i) - cx, &
                         plot%pie_label_pos(2, i) - cy)
            call assert_close(dist, 0.6_wp * plot%pie_radius, 1.0e-9_wp, &
                              'autopct label at 0.6 radius (pctdistance)')
        end do
    end subroutine assert_pctdistance

    subroutine run_auto_autopct_checks()
        type(figure_t) :: fig
        real(wp) :: values(3)
        integer :: i
        logical :: has_integer_label, has_fractional_label

        call fig%initialize()

        values = [3.0_wp, 2.0_wp, 1.0_wp]
        call fig%add_pie(values, autopct='auto')

        call assert_true(fig%annotation_count == 3, &
                         'auto autopct creates one label per slice')

        has_integer_label = .false.
        has_fractional_label = .false.
        do i = 1, fig%annotation_count
            call assert_true(trim(fig%annotations(i)%text) /= 'auto', &
                             'auto autopct must render formatted percentages')
            select case (trim(fig%annotations(i)%text))
            case ('50%')
                has_integer_label = .true.
            case ('33.3%')
                has_fractional_label = .true.
            case ('16.7%')
                has_fractional_label = .true.
            end select
        end do

        call assert_true(has_integer_label, 'auto autopct formats integer percentages')
        call assert_true(has_fractional_label, 'auto autopct keeps one decimal for fractions')
    end subroutine run_auto_autopct_checks

    subroutine run_autopct_literal_preservation_checks()
        type(figure_t) :: fig
        real(wp) :: values(2)
        integer :: i
        logical :: found_first, found_second

        call fig%initialize()

        values = [40.0_wp, 60.0_wp]
        call fig%add_pie(values, autopct='Share %.1f%% done: %%')

        call assert_true(fig%annotation_count == 2, &
                         'literal autopct creates one label per slice')

        found_first = .false.
        found_second = .false.
        do i = 1, fig%annotation_count
            select case (trim(fig%annotations(i)%text))
            case ('Share 40.0% done: %')
                found_first = .true.
            case ('Share 60.0% done: %')
                found_second = .true.
            end select
        end do

        call assert_true(found_first, &
                         'autopct retains literal prefix/suffix for first slice')
        call assert_true(found_second, &
                         'autopct retains literal prefix/suffix for second slice')
    end subroutine run_autopct_literal_preservation_checks

    subroutine run_pie_legend_checks()
        type(figure_t) :: fig
        real(wp) :: values(4)
        character(len=16) :: labels(4)
        real(wp) :: plot_width_px, plot_height_px
        real(wp) :: range_x, range_y
        real(wp) :: scale_x, scale_y, aspect_diff
        integer :: i
        logical :: found_north, found_east, found_west, found_online

        call fig%initialize()

        values = [30.0_wp, 40.0_wp, 20.0_wp, 10.0_wp]
        labels(1) = 'North'
        labels(2) = 'East'
        labels(3) = 'West'
        labels(4) = 'Online'

        call fig%add_pie(values, labels=labels, autopct='%.1f%%')
        call assert_true(fig%plots(1)%pie_slice_count == 4, &
                         'legend test stores all pie slices')
        call fig%legend(location='east')
        call fig%savefig('build/test/output/test_pie_legend.png')

        call assert_true(fig%state%legend_data%num_entries == 4, &
                         'pie legend includes all slices')

        found_north = .false.
        found_east = .false.
        found_west = .false.
        found_online = .false.
        do i = 1, fig%state%legend_data%num_entries
            associate(entry => fig%state%legend_data%entries(i))
                select case (trim(entry%label))
                case ('North')
                    found_north = .true.
                case ('East')
                    found_east = .true.
                case ('West')
                    found_west = .true.
                case ('Online')
                    found_online = .true.
                end select

                call assert_true(allocated(entry%linestyle), &
                                 'pie legend stores linestyle')
                call assert_true(trim(entry%linestyle) == 'None', &
                                 'pie legend suppresses wedge lines')
                call assert_true(allocated(entry%marker), &
                                 'pie legend stores marker')
            end associate
        end do

        call assert_true(found_north .and. found_east .and. found_west .and. &
                         found_online, 'pie legend retains labels')

        range_x = fig%state%x_max - fig%state%x_min
        range_y = fig%state%y_max - fig%state%y_min
        call assert_true(range_x > 0.0_wp .and. range_y > 0.0_wp, &
                         'pie legend keeps finite axis ranges')

        call get_backend_plot_extent(fig%state%backend, &
            real(fig%state%width, wp), real(fig%state%height, wp), &
            fig%state%margin_left, fig%state%margin_right, fig%state%margin_top, &
            fig%state%margin_bottom, plot_width_px, plot_height_px)
        call assert_true(plot_width_px > 0.0_wp .and. plot_height_px > 0.0_wp, &
                         'pie legend keeps positive plot area')

        scale_x = plot_width_px / range_x
        scale_y = plot_height_px / range_y
        aspect_diff = abs(scale_x - scale_y) / max(scale_x, scale_y)
        call assert_true(aspect_diff <= 1.0e-6_wp, &
                         'pie legend preserves equal axis scaling')
    end subroutine run_pie_legend_checks

    subroutine run_ascii_text_placement_checks()
        !! Text output lists every label and percentage exactly once in the
        !! legend and never embeds them in the wedge glyphs (issue #2073).
        type(figure_t) :: fig
        real(wp) :: values(4)
        character(len=8) :: labels(4)
        character(len=*), parameter :: out_txt = &
            'build/test/output/test_pie_text_placement.txt'
        integer :: status

        call fig%initialize()

        values = [26.0_wp, 25.0_wp, 25.0_wp, 24.0_wp]
        labels(1) = 'Alpha'
        labels(2) = 'Beta'
        labels(3) = 'Gamma'
        labels(4) = 'Delta'

        call fig%add_pie(values, labels=labels, autopct='%.1f%%')
        call fig%legend(location='east')
        call fig%savefig_with_status(out_txt, status)
        call assert_true(status == 0, 'ascii pie text output saved')

        call assert_token_count(out_txt, 'Alpha', 1, 'label Alpha appears once')
        call assert_token_count(out_txt, 'Beta', 1, 'label Beta appears once')
        call assert_token_count(out_txt, 'Gamma', 1, 'label Gamma appears once')
        call assert_token_count(out_txt, 'Delta', 1, 'label Delta appears once')
        call assert_token_count(out_txt, '26.0%', 1, 'percent 26.0% appears once')
        call assert_token_count(out_txt, '24.0%', 1, 'percent 24.0% appears once')
    end subroutine run_ascii_text_placement_checks

    subroutine assert_token_count(path, token, expected, description)
        !! Count how many times token appears across the whole text file.
        character(len=*), intent(in) :: path, token, description
        integer, intent(in) :: expected
        integer :: unit, ios, count_found, pos, start
        character(len=512) :: line

        count_found = 0
        open(newunit=unit, file=path, status='old', action='read', iostat=ios)
        call assert_true(ios == 0, 'ascii pie text output readable')
        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            start = 1
            do
                pos = index(line(start:), trim(token))
                if (pos == 0) exit
                count_found = count_found + 1
                start = start + pos + len_trim(token) - 1
                if (start > len(line)) exit
            end do
        end do
        close(unit)

        if (count_found /= expected) then
            print *, 'Assertion failed: ', trim(description)
            print *, '  expected count:', expected, ' actual:', count_found
            stop 1
        end if
    end subroutine assert_token_count

    subroutine assert_true(condition, description)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: description
        if (.not. condition) then
            print *, 'Assertion failed: ', trim(description)
            stop 1
        end if
    end subroutine assert_true

    subroutine assert_close(actual, expected, tolerance, description)
        real(wp), intent(in) :: actual, expected, tolerance
        character(len=*), intent(in) :: description
        if (abs(actual - expected) > tolerance) then
            print *, 'Assertion failed: ', trim(description)
            print *, '  expected:', expected, ' actual:', actual
            stop 1
        end if
    end subroutine assert_close

    subroutine get_backend_plot_extent(backend, fig_width, fig_height, margin_left, &
        margin_right, margin_top, margin_bottom, width_px, height_px)
        class(*), intent(in) :: backend
        real(wp), intent(in) :: fig_width, fig_height
        real(wp), intent(in) :: margin_left, margin_right, margin_top, margin_bottom
        real(wp), intent(out) :: width_px, height_px

        select type (backend)
        type is (png_context)
            width_px = real(max(1, backend%plot_area%width), wp)
            height_px = real(max(1, backend%plot_area%height), wp)
        type is (pdf_context)
            width_px = real(max(1, backend%plot_area%width), wp)
            height_px = real(max(1, backend%plot_area%height), wp)
        type is (ascii_context)
            width_px = real(max(1, backend%plot_width - 3), wp)
            height_px = real(max(1, backend%plot_height - 3), wp)
        class default
            width_px = fig_width * max(0.0_wp, 1.0_wp - margin_left - margin_right)
            height_px = fig_height * max(0.0_wp, 1.0_wp - margin_top - margin_bottom)
        end select
    end subroutine get_backend_plot_extent

end program test_pie_chart

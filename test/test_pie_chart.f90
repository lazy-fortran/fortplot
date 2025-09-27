program test_pie_chart
    !! Validates pie chart data storage and annotation generation

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure_t
    use fortplot_plot_data, only: PLOT_TYPE_PIE
    implicit none

    call run_pie_data_checks()
    call run_auto_autopct_checks()
    call run_autopct_literal_preservation_checks()
    call run_pie_legend_checks()

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

    end subroutine run_pie_data_checks

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
                call assert_true(trim(entry%marker) == 's', &
                                 'pie legend uses square markers')
            end associate
        end do

        call assert_true(found_north .and. found_east .and. found_west .and. &
                         found_online, 'pie legend retains labels')

        range_x = fig%state%x_max - fig%state%x_min
        range_y = fig%state%y_max - fig%state%y_min
        call assert_true(range_x > 0.0_wp .and. range_y > 0.0_wp, &
                         'pie legend keeps finite axis ranges')

        plot_width_px = real(fig%state%width, wp) * &
                        max(0.0_wp, 1.0_wp - fig%state%margin_left - &
                                      fig%state%margin_right)
        plot_height_px = real(fig%state%height, wp) * &
                         max(0.0_wp, 1.0_wp - fig%state%margin_bottom - &
                                       fig%state%margin_top)
        call assert_true(plot_width_px > 0.0_wp .and. plot_height_px > 0.0_wp, &
                         'pie legend keeps positive plot area')

        scale_x = plot_width_px / range_x
        scale_y = plot_height_px / range_y
        aspect_diff = abs(scale_x - scale_y) / max(scale_x, scale_y)
        call assert_true(aspect_diff <= 1.0e-6_wp, &
                         'pie legend preserves equal axis scaling')
    end subroutine run_pie_legend_checks

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

end program test_pie_chart

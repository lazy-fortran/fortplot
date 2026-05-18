module fortplot_figure_legend_setup
    !! Figure legend setup module
    !! Extracted from fortplot_figure_plot_management for size compliance (refs #1694)

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_plot_data, only: plot_data_t, PLOT_TYPE_PIE
    use fortplot_legend, only: legend_t
    implicit none

    private
    public :: setup_figure_legend, add_pie_legend_entries, get_pie_slice_marker_for_index

contains

    subroutine setup_figure_legend(legend_data, show_legend, plots, plot_count, &
                                   location, backend_name)
        type(legend_t), intent(inout) :: legend_data
        logical, intent(inout) :: show_legend
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        character(len=*), intent(in), optional :: location
        character(len=*), intent(in), optional :: backend_name

        character(len=:), allocatable :: loc
        integer :: i

        loc = 'upper right'
        if (present(location)) loc = location

        show_legend = .true.
        call legend_data%clear()
        call legend_data%set_position(loc)

        do i = 1, plot_count
            if (plots(i)%plot_type == PLOT_TYPE_PIE) then
                call add_pie_legend_entries(legend_data, plots(i), backend_name)
                cycle
            end if

            if (allocated(plots(i)%label)) then
                if (len_trim(plots(i)%label) > 0) then
                    if (allocated(plots(i)%linestyle)) then
                        if (allocated(plots(i)%marker)) then
                            call legend_data%add_entry(plots(i)%label, &
                                                       plots(i)%color, &
                                                       plots(i)%linestyle, &
                                                       plots(i)%marker)
                        else
                            call legend_data%add_entry(plots(i)%label, &
                                                       plots(i)%color, &
                                                       plots(i)%linestyle)
                        end if
                    else
                        if (present(backend_name) .and. trim(backend_name) == &
                            'ascii') then
                            call legend_data%add_entry(plots(i)%label, &
                                                       plots(i)%color)
                        else
                            call legend_data%add_entry(plots(i)%label, &
                                                       plots(i)%color, &
                                                       marker='s')
                        end if
                    end if
                end if
            end if
        end do
    end subroutine setup_figure_legend

    subroutine add_pie_legend_entries(legend_data, plot, backend_name)
        type(legend_t), intent(inout) :: legend_data
        type(plot_data_t), intent(in) :: plot
        character(len=*), intent(in), optional :: backend_name

        integer :: slice_count, i
        real(wp) :: color(3)
        real(wp) :: total, percent
        logical :: has_labels, has_values
        character(len=64) :: label_buf

        slice_count = plot%pie_slice_count
        if (slice_count <= 0) return

        has_labels = allocated(plot%pie_labels)
        if (has_labels) then
            if (size(plot%pie_labels) < slice_count) has_labels = .false.
        end if

        has_values = allocated(plot%pie_values)
        if (has_values) then
            if (size(plot%pie_values) < slice_count) has_values = .false.
        end if

        total = 0.0_wp
        if (has_values) total = sum(plot%pie_values(1:slice_count))

        do i = 1, slice_count
            if (allocated(plot%pie_colors)) then
                if (size(plot%pie_colors, 2) >= i) then
                    color = plot%pie_colors(:, i)
                else
                    color = plot%color
                end if
            else
                color = plot%color
            end if

            label_buf = ''
            if (has_labels) label_buf = trim(plot%pie_labels(i))

            if (len_trim(label_buf) == 0) then
                if (has_values .and. total > 0.0_wp) then
                    percent = 100.0_wp*plot%pie_values(i)/ &
                              max(total, tiny(1.0_wp))
                    write (label_buf, '("Slice ",I0," (",F6.1,"%)")') i, percent
                else
                    write (label_buf, '("Slice ",I0)') i
                end if
            end if

            if (present(backend_name)) then
                if (trim(backend_name) == 'ascii') then
                    call legend_data%add_entry(trim(label_buf), color, &
                                               linestyle='None', &
                                               marker=get_pie_slice_marker_for_index(i))
                else
                    call legend_data%add_entry(trim(label_buf), color, &
                                               linestyle='None', marker='s')
                end if
            else
                call legend_data%add_entry(trim(label_buf), color, &
                                           linestyle='None', &
                                           marker=get_pie_slice_marker_for_index(i))
            end if
        end do
    end subroutine add_pie_legend_entries

    pure function get_pie_slice_marker_for_index(slice_index) result(marker)
        integer, intent(in) :: slice_index
        character(len=1) :: marker

        select case (slice_index)
        case (1)
            marker = '-'
        case (2)
            marker = '='
        case (3)
            marker = '%'
        case (4)
            marker = '#'
        case (5)
            marker = '@'
        case default
            marker = '+'
        end select
    end function get_pie_slice_marker_for_index

end module fortplot_figure_legend_setup

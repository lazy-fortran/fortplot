module fortplot_spec_frontend_adapters
    !! Frontend-to-spec adapters for strict render cutover.

    use fortplot_figure_core, only: figure_t
    use fortplot_spec_types, only: spec_t
    implicit none

    private
    public :: figure_to_spec

contains

    subroutine figure_to_spec(fig, spec)
        type(figure_t), intent(in) :: fig
        type(spec_t), intent(out) :: spec

        spec%width = fig%state%width
        spec%height = fig%state%height
        if (allocated(fig%state%title)) spec%title = fig%state%title

        spec%scene%defined = .true.
        spec%scene%state = fig%state
        spec%scene%plot_count = fig%plot_count
        spec%scene%annotation_count = fig%annotation_count
        spec%scene%subplot_rows = fig%subplot_rows
        spec%scene%subplot_cols = fig%subplot_cols

        if (allocated(fig%plots)) then
            allocate (spec%scene%plots(size(fig%plots)))
            spec%scene%plots = fig%plots
        end if

        if (allocated(fig%annotations)) then
            allocate (spec%scene%annotations(size(fig%annotations)))
            spec%scene%annotations = fig%annotations
        end if

        if (allocated(fig%subplots_array)) then
            allocate (spec%scene%subplots_array(size(fig%subplots_array, 1), &
                                                size(fig%subplots_array, 2)))
            spec%scene%subplots_array = fig%subplots_array
        end if
    end subroutine figure_to_spec

end module fortplot_spec_frontend_adapters

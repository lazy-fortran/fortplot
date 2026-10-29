module fortplot_subplot_ticks
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context, only: plot_context
    use fortplot_plot_data, only: subplot_data_t
    use fortplot_raster, only: raster_context
    use fortplot_pdf, only: pdf_context
    use fortplot_ascii, only: ascii_context
    use fortplot_svg, only: svg_context
    implicit none
    private
    public :: apply_subplot_ticks
contains
    subroutine apply_subplot_ticks(backend, sp)
        class(plot_context), intent(inout) :: backend
        type(subplot_data_t), intent(in) :: sp

        select type (bk => backend)
        class is (raster_context)
            if (allocated(bk%raster%config_xtick_values)) &
                deallocate(bk%raster%config_xtick_values)
            if (allocated(bk%raster%config_ytick_values)) &
                deallocate(bk%raster%config_ytick_values)
            if (allocated(sp%custom_xtick_positions)) &
                bk%raster%config_xtick_values = sp%custom_xtick_positions
            if (allocated(sp%custom_ytick_positions)) &
                bk%raster%config_ytick_values = sp%custom_ytick_positions
        class is (pdf_context)
            call copy_ticks(bk%custom_xtick_positions, bk%custom_xtick_labels, &
                sp%custom_xtick_positions, sp%custom_xtick_labels)
            call copy_ticks(bk%custom_ytick_positions, bk%custom_ytick_labels, &
                sp%custom_ytick_positions, sp%custom_ytick_labels)
        class is (svg_context)
            call copy_ticks(bk%custom_xtick_positions, bk%custom_xtick_labels, &
                sp%custom_xtick_positions, sp%custom_xtick_labels)
            call copy_ticks(bk%custom_ytick_positions, bk%custom_ytick_labels, &
                sp%custom_ytick_positions, sp%custom_ytick_labels)
        class is (ascii_context)
            call copy_ticks(bk%custom_xtick_positions, bk%custom_xtick_labels, &
                sp%custom_xtick_positions, sp%custom_xtick_labels)
        end select
    end subroutine apply_subplot_ticks

    subroutine copy_ticks(out_positions, out_labels, positions, labels)
        real(wp), allocatable, intent(out) :: out_positions(:)
        character(len=*), allocatable, intent(out) :: out_labels(:)
        real(wp), intent(in), optional :: positions(:)
        character(len=*), intent(in), optional :: labels(:)

        ! Absent panel ticks clear the preceding panel's backend configuration.
        if (present(positions)) out_positions = positions
        if (present(labels)) out_labels = labels
    end subroutine copy_ticks
end module fortplot_subplot_ticks

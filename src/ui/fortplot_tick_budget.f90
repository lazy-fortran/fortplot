module fortplot_tick_budget
    !! Matplotlib Axis.get_tick_space and MaxNLocator(nbins='auto').
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    private
    public :: axis_tick_budget, raster_tick_budget

contains

    pure integer function axis_tick_budget(length_pt, horizontal, font_pt) result(n)
        real(wp), intent(in) :: length_pt
        logical, intent(in) :: horizontal
        real(wp), intent(in), optional :: font_pt
        real(wp) :: size_pt, spacing

        size_pt = 10.0_wp
        if (present(font_pt)) size_pt = max(font_pt, tiny(1.0_wp))
        spacing = 2.0_wp*size_pt
        if (horizontal) spacing = 3.0_wp*size_pt
        n = max(1, min(9, floor(max(0.0_wp, length_pt)/spacing)))
    end function axis_tick_budget

    pure integer function raster_tick_budget(length_px, dpi, horizontal, &
            configured_size) result(n)
        integer, intent(in) :: length_px
        real(wp), intent(in) :: dpi
        logical, intent(in) :: horizontal
        real(wp), intent(in), optional :: configured_size
        real(wp) :: font_pt

        font_pt = 10.0_wp
        if (present(configured_size)) then
            ! Explicit raster configuration is pixels at the reference 100 DPI.
            if (configured_size > 0.0_wp) font_pt = configured_size*0.72_wp
        end if
        n = axis_tick_budget(real(length_px, wp)*72.0_wp/max(dpi, 1.0_wp), &
            horizontal, font_pt)
    end function raster_tick_budget

end module fortplot_tick_budget

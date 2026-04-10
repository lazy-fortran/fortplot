module fortplot_raster_config
    !! Configurable rendering overrides for the raster backend.
    !!
    !! Set these before calling rendering subroutines. Negative values
    !! mean "use the hardcoded default".

    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    real(wp) :: config_title_font_size = -1.0_wp
    real(wp) :: config_label_font_size = -1.0_wp
    real(wp) :: config_tick_font_size = -1.0_wp

    ! Custom tick values from spec encoding (set before rendering)
    real(wp), allocatable :: config_xtick_values(:)
    real(wp), allocatable :: config_ytick_values(:)

end module fortplot_raster_config

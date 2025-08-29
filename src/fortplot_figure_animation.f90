module fortplot_figure_animation
    !! Figure animation support functionality
    !! Extracted from fortplot_figure_core.f90 for size reduction (SRP compliance)
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_figure_compatibility, only: setup_png_backend_for_animation_compat, &
                                            extract_rgb_data_for_animation_compat, &
                                            extract_png_data_for_animation_compat
    implicit none

    private
    public :: setup_figure_png_backend_for_animation
    public :: extract_figure_rgb_data_for_animation
    public :: extract_figure_png_data_for_animation

contains

    subroutine setup_figure_png_backend_for_animation(state)
        !! Setup PNG backend for animation (temporary method)
        type(figure_state_t), intent(inout) :: state
        call setup_png_backend_for_animation_compat(state)
    end subroutine setup_figure_png_backend_for_animation
    
    subroutine extract_figure_rgb_data_for_animation(state, rgb_data, rendered)
        !! Extract RGB data for animation
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(out) :: rgb_data(:,:,:)
        logical, intent(in) :: rendered
        
        ! Note: rendering check moved to calling layer for better separation
        if (rendered) then
            call extract_rgb_data_for_animation_compat(state, rgb_data)
        end if
    end subroutine extract_figure_rgb_data_for_animation
    
    subroutine extract_figure_png_data_for_animation(state, png_data, status, rendered)
        !! Extract PNG data for animation
        type(figure_state_t), intent(inout) :: state
        integer(1), allocatable, intent(out) :: png_data(:)
        integer, intent(out) :: status
        logical, intent(in) :: rendered
        
        ! Note: rendering check moved to calling layer for better separation
        if (rendered) then
            call extract_png_data_for_animation_compat(state, png_data, status)
        else
            allocate(png_data(0))
            status = -1
        end if
    end subroutine extract_figure_png_data_for_animation

end module fortplot_figure_animation
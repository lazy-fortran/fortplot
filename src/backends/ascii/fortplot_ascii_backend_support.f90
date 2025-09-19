module fortplot_ascii_backend_support
    !! ASCII terminal plotting backend - Backend Support Functions
    !!
    !! This module contains support functions for polymorphic backend operations,
    !! including stub implementations and specialized interface functions.
    !!
    !! Author: fortplot contributors
    
    use fortplot_plot_data, only: plot_data_t
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    private
    public :: extract_ascii_rgb_data, get_ascii_png_data, prepare_ascii_3d_data
    public :: render_ascii_ylabel, render_ascii_axes
    
contains

    subroutine extract_ascii_rgb_data(width, height, rgb_data)
        !! Extract RGB data from ASCII backend (not supported - dummy data)
        integer, intent(in) :: width, height
        real(wp), intent(out) :: rgb_data(width, height, 3)
        
        ! ASCII backend doesn't have RGB data for animation - fill with dummy data
        rgb_data = 0.0_wp  ! Black background
    end subroutine extract_ascii_rgb_data

    subroutine get_ascii_png_data(width, height, png_data, status)
        !! Get PNG data from ASCII backend (not supported)
        integer, intent(in) :: width, height
        integer(1), allocatable, intent(out) :: png_data(:)
        integer, intent(out) :: status
        
        ! Reference unused parameters without unreachable branches
        associate(unused_w => width, unused_h => height); end associate
        
        ! ASCII backend doesn't provide PNG data
        allocate(png_data(0))
        status = -1
    end subroutine get_ascii_png_data

    subroutine prepare_ascii_3d_data(plots)
        !! Prepare 3D data for ASCII backend (no-op - ASCII doesn't use 3D data)
        type(plot_data_t), intent(in) :: plots(:)
        
        ! Reference unused parameters without unreachable branches
        associate(unused_n => size(plots)); end associate
        
        ! ASCII backend doesn't need 3D data preparation - no-op
    end subroutine prepare_ascii_3d_data

    subroutine render_ascii_ylabel(ylabel)
        !! Render Y-axis label for ASCII backend (no-op - handled elsewhere)
        character(len=*), intent(in) :: ylabel
        
        ! Reference unused parameter without unreachable branch
        associate(unused_l => len_trim(ylabel)); end associate
        
        ! ASCII backend handles Y-axis labels differently - no-op
    end subroutine render_ascii_ylabel

    subroutine render_ascii_axes(title_text, xlabel_text, ylabel_text)
        !! Render axes for ASCII context (stub implementation)
        character(len=*), intent(in), optional :: title_text, xlabel_text, ylabel_text
        
        if (present(title_text)) then; associate(unused_lt => len_trim(title_text)); end associate; end if
        if (present(xlabel_text)) then; associate(unused_lx => len_trim(xlabel_text)); end associate; end if
        if (present(ylabel_text)) then; associate(unused_ly => len_trim(ylabel_text)); end associate; end if
        
        ! ASCII axes are rendered as part of draw_axes_and_labels_backend
        ! This is a stub to satisfy the interface
    end subroutine render_ascii_axes

end module fortplot_ascii_backend_support

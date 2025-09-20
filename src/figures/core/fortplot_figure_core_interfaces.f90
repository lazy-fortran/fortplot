module fortplot_figure_core_interfaces
    !! Interface definitions for figure_t methods
    !! Groups related method interfaces to reduce main module size

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_plot_data, only: plot_data_t, subplot_data_t, arrow_data_t
    use fortplot_annotations, only: text_annotation_t
    implicit none

    private
    public :: figure_core_plot_methods, figure_core_property_methods
    public :: figure_core_specialized_methods, figure_core_animation_methods
    public :: figure_core_subplot_methods

    abstract interface
        !! Core plotting methods
        subroutine figure_core_plot_methods
        end subroutine

        !! Property management methods
        subroutine figure_core_property_methods
        end subroutine

        !! Specialized plotting methods
        subroutine figure_core_specialized_methods
        end subroutine

        !! Animation and backend methods
        subroutine figure_core_animation_methods
        end subroutine

        !! Subplot management methods
        subroutine figure_core_subplot_methods
        end subroutine
    end interface

end module fortplot_figure_core_interfaces
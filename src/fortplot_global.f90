module fortplot_global
    !! Global figure instance management for fortplot
    !! Provides access to the shared global figure used by pyplot-style functions
    !!
    !! This module separates the global figure instance management from the
    !! matplotlib-compatibility wrapper functions, allowing clean access to
    !! the global figure from different parts of the library.
    
    use fortplot_figure_core, only: figure_t
    
    implicit none
    private
    
    ! Export the global figure instance for modules that need direct access
    public :: global_figure
    
    ! Global figure instance used by pyplot-style API
    type(figure_t), save, target :: global_figure
    
end module fortplot_global
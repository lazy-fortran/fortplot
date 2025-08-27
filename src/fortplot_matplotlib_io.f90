module fortplot_matplotlib_io
    !! File I/O and display operations for matplotlib-compatible API
    !! Contains figure management, saving, and showing functions
    
    use iso_fortran_env, only: wp => real64
    use fortplot_figure_core, only: figure_t
    use fortplot_global, only: fig => global_figure
    use fortplot_logging, only: log_error, log_warning, log_info
    use fortplot_security, only: safe_launch_viewer, safe_remove_file
    
    implicit none
    private
    
    ! Export I/O and display functions
    public :: figure, subplot
    public :: savefig, savefig_with_status
    public :: show, show_viewer
    public :: ensure_global_figure_initialized
    public :: get_global_figure
    
    ! Overloaded show interface
    interface show
        module procedure show_data, show_figure
    end interface show
    
contains

    subroutine ensure_global_figure_initialized()
        !! Ensure global figure is initialized before use (matplotlib compatibility)
        !! Auto-initializes with default dimensions if not already initialized
        if (.not. allocated(fig)) then
            allocate(figure_t :: fig)
        end if
        if (.not. fig%backend_associated()) then
            call fig%initialize()
        end if
    end subroutine ensure_global_figure_initialized
    
    function get_global_figure() result(global_fig)
        !! Get reference to the global figure for testing access to arrow data
        !! This allows tests to access fig%arrow_data without making fig public
        class(figure_t), pointer :: global_fig
        call ensure_global_figure_initialized()
        global_fig => fig
    end function get_global_figure

    subroutine figure(num, figsize, dpi)
        !! Create or activate a figure with optional size and DPI settings
        integer, intent(in), optional :: num
        real(8), dimension(2), intent(in), optional :: figsize
        integer, intent(in), optional :: dpi
        
        integer :: fig_num, fig_dpi
        real(8), dimension(2) :: size
        character(len=256) :: msg
        
        ! Set defaults
        fig_num = 1
        if (present(num)) fig_num = num
        
        size = [8.0d0, 6.0d0]  ! Default figure size in inches
        if (present(figsize)) size = figsize
        
        fig_dpi = 100
        if (present(dpi)) fig_dpi = dpi
        
        ! Validate inputs
        if (size(1) <= 0.0d0 .or. size(2) <= 0.0d0) then
            call log_error("figure: Invalid figure size")
            return
        end if
        
        if (fig_dpi <= 0) then
            call log_error("figure: Invalid DPI value")
            return
        end if
        
        ! Log figure creation
        write(msg, '(A,I0,A,F6.2,A,F6.2,A,I0)') &
            "Creating figure ", fig_num, " with size ", size(1), "x", size(2), &
            " inches at ", fig_dpi, " DPI"
        call log_info(trim(msg))
        
        ! Re-initialize global figure with new settings
        if (allocated(fig)) then
            deallocate(fig)
        end if
        
        allocate(figure_t :: fig)
        call fig%initialize(width=nint(size(1)*fig_dpi), &
                          height=nint(size(2)*fig_dpi))
    end subroutine figure

    subroutine subplot(nrows, ncols, index)
        !! Create subplot arrangement (simplified for single axis)
        !! Currently implements single plot behavior
        integer, intent(in) :: nrows, ncols, index
        
        character(len=256) :: msg
        
        call ensure_global_figure_initialized()
        
        ! Validate inputs
        if (nrows <= 0 .or. ncols <= 0) then
            call log_error("subplot: Invalid grid dimensions")
            return
        end if
        
        if (index <= 0 .or. index > nrows*ncols) then
            call log_error("subplot: Invalid subplot index")
            return
        end if
        
        ! Log subplot creation
        write(msg, '(A,I0,A,I0,A,I0)') &
            "Creating subplot ", index, " in ", nrows, "x", ncols, " grid"
        call log_info(trim(msg))
        
        ! Note: Current implementation doesn't support multiple subplots
        ! This is a stub for API compatibility
        if (index > 1) then
            call log_warning("subplot: Multiple subplots not yet implemented")
        end if
    end subroutine subplot

    subroutine savefig(filename, dpi, transparent, bbox_inches)
        !! Save the global figure to a file (pyplot-style)
        character(len=*), intent(in) :: filename
        integer, intent(in), optional :: dpi
        logical, intent(in), optional :: transparent
        character(len=*), intent(in), optional :: bbox_inches
        
        call ensure_global_figure_initialized()
        call fig%savefig(filename)
    end subroutine savefig

    subroutine savefig_with_status(filename, status, dpi, transparent, bbox_inches)
        !! Save figure with status return (error handling version)
        character(len=*), intent(in) :: filename
        integer, intent(out) :: status
        integer, intent(in), optional :: dpi
        logical, intent(in), optional :: transparent
        character(len=*), intent(in), optional :: bbox_inches
        
        call ensure_global_figure_initialized()
        call fig%savefig_with_status(filename, status)
    end subroutine savefig_with_status

    subroutine show_data(x, y, label, title_text, xlabel_text, ylabel_text, blocking)
        !! Show data as a simple plot with optional labels
        real(8), dimension(:), intent(in) :: x, y
        character(len=*), intent(in), optional :: label, title_text
        character(len=*), intent(in), optional :: xlabel_text, ylabel_text
        logical, intent(in), optional :: blocking
        
        call ensure_global_figure_initialized()
        
        ! Add the plot
        call fig%add_plot(x, y, label=label)
        
        ! Set labels if provided
        if (present(title_text)) call fig%set_title(title_text)
        if (present(xlabel_text)) call fig%set_xlabel(xlabel_text)
        if (present(ylabel_text)) call fig%set_ylabel(ylabel_text)
        
        ! Show the figure
        call fig%show(blocking=blocking)
    end subroutine show_data

    subroutine show_figure(blocking)
        !! Display the global figure (pyplot-style)
        logical, intent(in), optional :: blocking
        
        call ensure_global_figure_initialized()
        call fig%show(blocking=blocking)
    end subroutine show_figure

    subroutine show_viewer(blocking)
        !! Display figure using system viewer (cross-platform)
        logical, intent(in), optional :: blocking
        
        call ensure_global_figure_initialized()
        call show_viewer_implementation(blocking)
    end subroutine show_viewer

    function is_gui_available() result(gui_available)
        !! Check if GUI display is available
        logical :: gui_available
        character(len=256) :: display_var
        integer :: status
        
        gui_available = .false.
        
        ! Check for display environment
        call get_environment_variable("DISPLAY", display_var, status=status)
        if (status == 0 .and. len_trim(display_var) > 0) then
            gui_available = .true.
        end if
        
        ! Check for SSH without X forwarding
        call get_environment_variable("SSH_CLIENT", display_var, status=status)
        if (status == 0 .and. .not. gui_available) then
            call log_warning("SSH session detected without X forwarding")
        end if
    end function is_gui_available

    subroutine show_viewer_implementation(blocking)
        !! Internal implementation of viewer display
        logical, intent(in), optional :: blocking
        
        character(len=512) :: temp_file
        logical :: is_blocking, success
        integer :: status, unit
        real :: start_time, current_time
        
        is_blocking = .false.
        if (present(blocking)) is_blocking = blocking
        
        ! Check GUI availability
        if (.not. is_gui_available()) then
            call log_warning("No GUI available, saving to show_output.png instead")
            call fig%savefig("show_output.png")
            return
        end if
        
        ! Create temporary file
        call get_environment_variable("TMPDIR", temp_file, status=status)
        if (status /= 0) temp_file = "/tmp"
        
        write(temp_file, '(A,A,I0,A)') trim(temp_file), "/fortplot_", &
                                       int(rand(0)*1000000), ".png"
        
        ! Save figure to temporary file
        call fig%savefig_with_status(trim(temp_file), status)
        if (status /= 0) then
            call log_error("Failed to save figure for viewing")
            return
        end if
        
        ! Launch viewer
        call safe_launch_viewer(trim(temp_file), success)
        
        if (.not. success) then
            call log_error("Failed to launch image viewer")
            call safe_remove_file(trim(temp_file), success)
            return
        end if
        
        ! Handle blocking mode
        if (is_blocking) then
            call log_info("Viewer launched in blocking mode. Close viewer to continue.")
            ! Wait for viewer to close
            call cpu_time(start_time)
            do
                call cpu_time(current_time)
                if (current_time - start_time > 30.0) exit  ! 30 second timeout
                call execute_command_line("sleep 0.1", wait=.true.)
            end do
        else
            ! Give viewer time to load file before deletion
            call execute_command_line("sleep 1", wait=.true.)
        end if
        
        ! Clean up temporary file
        call safe_remove_file(trim(temp_file), success)
    end subroutine show_viewer_implementation

end module fortplot_matplotlib_io
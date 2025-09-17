module fortplot_matplotlib_session
    !! Global figure session helpers shared across matplotlib-compatible wrappers
    !!
    !! Provides lifecycle management for the singleton figure used by the
    !! matplotlib facade. Responsibilities include creating and reusing the
    !! global figure, grid/subplot helpers, and viewer integration utilities.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_core, only: figure_t
    use fortplot_figure_initialization, only: configure_figure_dimensions
    use fortplot_global, only: fig => global_figure
    use fortplot_logging, only: log_error, log_warning, log_info
    use fortplot_security, only: safe_launch_viewer, safe_remove_file

    implicit none
    private

    public :: ensure_fig_init
    public :: ensure_global_figure_initialized
    public :: get_global_figure
    public :: figure
    public :: subplot
    public :: subplots
    public :: subplots_grid
    public :: savefig
    public :: savefig_with_status
    public :: show_data
    public :: show_figure
    public :: show_viewer

contains

    subroutine ensure_fig_init()
        !! Ensure the global figure exists and is initialized
        if (.not. allocated(fig)) then
            allocate(figure_t :: fig)
        end if
        if (.not. fig%backend_associated()) then
            call fig%initialize()
        end if
    end subroutine ensure_fig_init

    subroutine ensure_global_figure_initialized()
        !! Public wrapper to guarantee the singleton is ready for use
        call ensure_fig_init()
    end subroutine ensure_global_figure_initialized

    function get_global_figure() result(global_fig)
        !! Return the global figure pointer, initializing on demand
        class(figure_t), pointer :: global_fig
        call ensure_fig_init()
        global_fig => fig
    end function get_global_figure

    subroutine figure(num, figsize, dpi)
        !! Create a matplotlib-style figure using the shared singleton
        integer, intent(in), optional :: num
        real(wp), dimension(2), intent(in), optional :: figsize
        integer, intent(in), optional :: dpi

        integer :: fig_num, fig_dpi
        real(wp), dimension(2) :: requested_size, safe_size
        integer :: width_px, height_px
        character(len=256) :: msg

        fig_num = 1
        if (present(num)) fig_num = num

        requested_size = [8.0_wp, 6.0_wp]
        if (present(figsize)) requested_size = figsize

        fig_dpi = 100
        if (present(dpi)) fig_dpi = dpi

        if (requested_size(1) <= 0.0_wp .or. requested_size(2) <= 0.0_wp) then
            call log_error("figure: Invalid figure size")
            return
        end if

        if (fig_dpi <= 0) then
            call log_error("figure: Invalid DPI value")
            return
        end if

        width_px = nint(requested_size(1) * real(fig_dpi, wp))
        height_px = nint(requested_size(2) * real(fig_dpi, wp))
        safe_size = requested_size

        if (width_px > 10000 .or. height_px > 10000) then
            call log_warning("figure: Large figure size may cause memory issues")
        end if

        write(msg, '(A,I0)') "figure: Creating figure ", fig_num
        call log_info(trim(msg))

        if (allocated(fig)) then
            deallocate(fig)
        end if
        allocate(figure_t :: fig)
        call fig%initialize()
        call configure_figure_dimensions(fig%state, width=width_px, height=height_px)
    end subroutine figure

    subroutine subplot(nrows, ncols, index)
        !! Create a subplot placeholder matching matplotlib's subplot API
        integer, intent(in) :: nrows, ncols, index
        character(len=256) :: msg
        logical, save :: subplot_warning_shown = .false.

        call ensure_global_figure_initialized()

        if (nrows <= 0 .or. ncols <= 0) then
            call log_error("subplot: Invalid grid dimensions")
            return
        end if

        if (index <= 0 .or. index > nrows * ncols) then
            call log_error("subplot: Invalid subplot index")
            return
        end if

        write(msg, '(A,I0,A,I0,A,I0,A)') &
            "Creating subplot ", index, " in ", nrows, "x", ncols, " grid"
        call log_info(trim(msg))

        if (index > 1 .and. .not. subplot_warning_shown) then
            call log_warning("subplot: Multiple subplots not yet implemented")
            subplot_warning_shown = .true.
        end if
    end subroutine subplot

    subroutine subplots(nrows, ncols)
        !! Initialize a subplot grid using the global figure
        integer, intent(in) :: nrows, ncols

        call ensure_global_figure_initialized()

        if (nrows <= 0 .or. ncols <= 0) then
            call log_error("subplots: Invalid grid dimensions")
            return
        end if

        call fig%subplots(nrows, ncols)
    end subroutine subplots

    function subplots_grid(nrows, ncols) result(axes)
        !! Create subplot grid and return axis indices in row-major order
        integer, intent(in) :: nrows, ncols
        integer, allocatable :: axes(:,:)
        integer :: i, j

        call ensure_global_figure_initialized()

        if (nrows <= 0 .or. ncols <= 0) then
            call log_error("subplots_grid: Invalid grid dimensions")
            allocate(axes(0, 0))
            return
        end if

        call fig%subplots(nrows, ncols)
        allocate(axes(nrows, ncols))

        do i = 1, nrows
            do j = 1, ncols
                axes(i, j) = (i - 1) * ncols + j
            end do
        end do
    end function subplots_grid

    subroutine savefig(filename, dpi, transparent, bbox_inches)
        !! Save current figure using matplotlib-compatible API
        character(len=*), intent(in) :: filename
        integer, intent(in), optional :: dpi
        logical, intent(in), optional :: transparent
        character(len=*), intent(in), optional :: bbox_inches

        call ensure_global_figure_initialized()
        call fig%savefig(filename)
        if (present(dpi) .or. present(transparent) .or. present(bbox_inches)) then
            call log_warning("savefig: backend ignores dpi/transparent/bbox settings")
        end if
    end subroutine savefig

    subroutine savefig_with_status(filename, status, dpi, transparent, bbox_inches)
        !! Save figure and return status code for testing scenarios
        character(len=*), intent(in) :: filename
        integer, intent(out) :: status
        integer, intent(in), optional :: dpi
        logical, intent(in), optional :: transparent
        character(len=*), intent(in), optional :: bbox_inches

        call ensure_global_figure_initialized()
        call fig%savefig_with_status(filename, status)
        if (present(dpi) .or. present(transparent) .or. present(bbox_inches)) then
            call log_warning( &
                "savefig_with_status: backend ignores dpi/transparent/bbox settings")
        end if
    end subroutine savefig_with_status

    subroutine show_data(x, y, label, title_text, xlabel_text, ylabel_text, blocking)
        !! Convenience routine mirroring matplotlib.pyplot.show signature with data
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label, title_text
        character(len=*), intent(in), optional :: xlabel_text, ylabel_text
        logical, intent(in), optional :: blocking

        call ensure_global_figure_initialized()
        call fig%add_plot(x, y, label=label)
        if (present(title_text)) call fig%set_title(title_text)
        if (present(xlabel_text)) call fig%set_xlabel(xlabel_text)
        if (present(ylabel_text)) call fig%set_ylabel(ylabel_text)
        call fig%show(blocking=blocking)
    end subroutine show_data

    subroutine show_figure(blocking)
        !! Show the global figure via backend implementation
        logical, intent(in), optional :: blocking

        call ensure_global_figure_initialized()
        call fig%show(blocking=blocking)
    end subroutine show_figure

    subroutine show_viewer(blocking)
        !! Launch external viewer with saved figure artifact when available
        logical, intent(in), optional :: blocking

        call ensure_global_figure_initialized()
        call show_viewer_implementation(blocking)
    end subroutine show_viewer

    subroutine show_viewer_implementation(blocking)
        logical, intent(in), optional :: blocking
        character(len=512) :: temp_file
        logical :: is_blocking, success
        integer :: status
        real :: start_time, current_time, random_val
        logical, save :: no_gui_warning_shown = .false.

        is_blocking = .false.
        if (present(blocking)) is_blocking = blocking

        if (.not. is_gui_available()) then
            if (.not. no_gui_warning_shown) then
                call log_warning("No GUI available, saving to show_output.png instead")
                no_gui_warning_shown = .true.
            end if
            call fig%savefig("show_output.png")
            return
        end if

        call random_number(random_val)
        call get_environment_variable("TMPDIR", temp_file, status=status)
        if (status /= 0) temp_file = "/tmp"
        write(temp_file, '(A,A,I0,A)') trim(temp_file), "/fortplot_", &
            int(random_val * 1000000), ".png"

        call fig%savefig_with_status(trim(temp_file), status)
        if (status /= 0) then
            call log_error("Failed to save figure for viewing")
            return
        end if

        call safe_launch_viewer(trim(temp_file), success)
        if (.not. success) then
            call log_error("Failed to launch image viewer")
            call safe_remove_file(trim(temp_file), success)
            return
        end if

        if (is_blocking) then
            call log_info("Viewer launched in blocking mode. Close viewer to continue.")
            call cpu_time(start_time)
            do
                call cpu_time(current_time)
                if (current_time - start_time > 30.0) exit
                call sleep_fortran(100)
            end do
        else
            call sleep_fortran(1000)
        end if

        call safe_remove_file(trim(temp_file), success)
    end subroutine show_viewer_implementation

    function is_gui_available() result(gui_available)
        logical :: gui_available
        character(len=256) :: display_var
        integer :: status
        logical, save :: ssh_warning_shown = .false.

        gui_available = .false.
        call get_environment_variable("DISPLAY", display_var, status=status)
        if (status == 0 .and. len_trim(display_var) > 0) then
            gui_available = .true.
        end if

        call get_environment_variable("SSH_CLIENT", display_var, status=status)
        if (status == 0 .and. .not. gui_available .and. .not. ssh_warning_shown) then
            call log_warning("SSH session detected without X forwarding")
            ssh_warning_shown = .true.
        end if
    end function is_gui_available

    subroutine sleep_fortran(milliseconds)
        integer, intent(in) :: milliseconds
        real :: seconds
        integer :: start_count, end_count, count_rate, target_count

        seconds = real(milliseconds) / 1000.0
        call system_clock(start_count, count_rate)
        target_count = int(seconds * real(count_rate))
        do
            call system_clock(end_count)
            if (end_count - start_count >= target_count) exit
        end do
    end subroutine sleep_fortran

end module fortplot_matplotlib_session

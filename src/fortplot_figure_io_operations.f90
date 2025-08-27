module fortplot_figure_io_operations
    !! Figure I/O operations module
    !! 
    !! This module contains save and show operations extracted from
    !! fortplot_figure_core to achieve QADS compliance (<500 lines).
    !!
    !! Single Responsibility: Handle figure saving and display operations

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_utils, only: get_backend_from_filename
    use fortplot_figure_initialization, only: setup_figure_backend
    use fortplot_errors, only: SUCCESS, ERROR_FILE_IO, is_error
    use fortplot_logging, only: log_error, log_warning
    use fortplot_png, only: png_context
    use fortplot_pdf, only: pdf_context
    use fortplot_ascii, only: ascii_context
    use fortplot_figure_io, only: save_backend_with_status
    implicit none

    private
    public :: figure_savefig, figure_savefig_with_status, figure_show

contains

    subroutine figure_savefig(state, filename, blocking, render_proc)
        !! Save figure to file (backward compatibility version)
        !! This version logs errors but doesn't return status
        type(figure_state_t), intent(inout) :: state
        character(len=*), intent(in) :: filename
        logical, intent(in), optional :: blocking
        
        interface
            subroutine render_proc()
            end subroutine render_proc
        end interface
        
        integer :: status
        
        ! Delegate to version with status reporting
        call figure_savefig_with_status(state, filename, status, blocking, render_proc)
        
        ! Log error if save failed (maintains existing behavior)
        if (status /= SUCCESS) then
            call log_error("Failed to save figure to '" // trim(filename) // "'")
        end if
    end subroutine figure_savefig
    
    subroutine figure_savefig_with_status(state, filename, status, blocking, render_proc)
        !! Save figure to file with error status reporting
        type(figure_state_t), intent(inout) :: state
        character(len=*), intent(in) :: filename
        integer, intent(out) :: status
        logical, intent(in), optional :: blocking
        
        interface
            subroutine render_proc()
            end subroutine render_proc
        end interface
        
        character(len=20) :: required_backend, current_backend
        logical :: block, need_backend_switch
        
        ! Initialize success status
        status = SUCCESS
        
        block = .true.
        if (present(blocking)) block = blocking
        
        ! Determine required backend from filename extension
        required_backend = get_backend_from_filename(filename)
        
        ! Determine current backend type
        select type (backend => state%backend)
        type is (png_context)
            current_backend = 'png'
        type is (pdf_context)
            current_backend = 'pdf'
        type is (ascii_context)
            current_backend = 'ascii'
        class default
            current_backend = 'unknown'
        end select
        
        ! Check if we need to switch backends
        need_backend_switch = (trim(required_backend) /= trim(current_backend))
        
        if (need_backend_switch) then
            call setup_figure_backend(state, required_backend)
        end if
        
        ! Render if not already rendered
        if (.not. state%rendered) then
            call render_proc()
        end if
        
        ! Save the figure with status checking
        call save_backend_with_status(state%backend, filename, status)
    end subroutine figure_savefig_with_status

    subroutine figure_show(state, blocking, render_proc)
        !! Display the figure
        type(figure_state_t), intent(inout) :: state
        logical, intent(in), optional :: blocking
        
        interface
            subroutine render_proc()
            end subroutine render_proc
        end interface
        
        logical :: block
        
        block = .true.
        if (present(blocking)) block = blocking
        
        ! Render if not already rendered
        if (.not. state%rendered) then
            call render_proc()
        end if
        
        ! Display the figure
        call state%backend%save("terminal")
    end subroutine figure_show

end module fortplot_figure_io_operations
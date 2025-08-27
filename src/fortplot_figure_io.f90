module fortplot_figure_io
    !! File I/O operations for figure backends
    !! Contains save operations for PNG, PDF, and ASCII formats
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_errors, only: SUCCESS, ERROR_FILE_IO
    use fortplot_logging, only: log_error
    use fortplot_png, only: png_context
    use fortplot_pdf, only: pdf_context
    use fortplot_ascii, only: ascii_context
    
    implicit none
    private
    
    public :: save_backend_with_status
    public :: save_png_with_status
    public :: save_pdf_with_status
    public :: save_ascii_with_status
    
contains

    subroutine save_backend_with_status(backend, filename, status)
        !! Save the figure with appropriate backend based on file extension
        class(plot_context), intent(inout) :: backend
        character(len=*), intent(in) :: filename
        integer, intent(out) :: status
        
        character(len=3) :: ext
        integer :: ext_start
        
        status = SUCCESS
        
        ! Get file extension
        ext_start = index(filename, '.', back=.true.)
        if (ext_start > 0 .and. ext_start < len_trim(filename)) then
            ext = filename(ext_start+1:min(ext_start+3, len_trim(filename)))
        else
            ext = 'png'  ! Default to PNG
        end if
        
        ! Save based on backend type
        select type(backend)
        type is (png_context)
            call save_png_with_status(backend, filename, status)
        type is (pdf_context)
            call save_pdf_with_status(backend, filename, status)
        type is (ascii_context)
            call save_ascii_with_status(backend, filename, status)
        class default
            call log_error("save_backend_with_status: Unknown backend type")
            status = ERROR_FILE_IO
        end select
    end subroutine save_backend_with_status

    subroutine save_png_with_status(backend, filename, status)
        !! Save PNG file with status reporting
        type(png_context), intent(inout) :: backend
        character(len=*), intent(in) :: filename
        integer, intent(out) :: status
        
        logical :: file_exists
        integer :: final_size
        
        status = SUCCESS
        
        ! Save the PNG file
        call backend%save(filename)
        
        ! Verify the file was created (unless it's terminal output)
        if (len_trim(filename) == 0 .or. trim(filename) == "terminal") then
            return
        end if
        
        inquire(file=filename, exist=file_exists, size=final_size)
        if (.not. file_exists .or. final_size <= 0) then
            status = ERROR_FILE_IO
            call log_error("save_png_with_status: Failed to save PNG file")
        end if
    end subroutine save_png_with_status

    subroutine save_pdf_with_status(backend, filename, status)
        !! Save PDF file with status reporting
        type(pdf_context), intent(inout) :: backend
        character(len=*), intent(in) :: filename
        integer, intent(out) :: status
        
        logical :: file_exists
        integer :: final_size
        
        status = SUCCESS
        
        ! Save the PDF file
        call backend%save(filename)
        
        ! For terminal output (empty filename), consider it successful
        if (len_trim(filename) == 0 .or. trim(filename) == "terminal") then
            return
        end if
        
        ! Verify the file was created
        inquire(file=filename, exist=file_exists, size=final_size)
        if (.not. file_exists .or. final_size <= 0) then
            status = ERROR_FILE_IO
            call log_error("save_pdf_with_status: Failed to save PDF file")
        end if
    end subroutine save_pdf_with_status

    subroutine save_ascii_with_status(backend, filename, status)
        !! Save ASCII file with status reporting
        type(ascii_context), intent(inout) :: backend
        character(len=*), intent(in) :: filename
        integer, intent(out) :: status
        
        logical :: file_exists
        integer :: final_size
        
        status = SUCCESS
        
        ! Save the ASCII file
        call backend%save(filename)
        
        ! For terminal output (empty filename), consider it successful
        if (len_trim(filename) == 0 .or. trim(filename) == "terminal") then
            return
        end if
        
        ! Verify the file was created
        inquire(file=filename, exist=file_exists, size=final_size)
        if (.not. file_exists .or. final_size <= 0) then
            status = ERROR_FILE_IO
            call log_error("save_ascii_with_status: Failed to save ASCII file")
        end if
    end subroutine save_ascii_with_status

end module fortplot_figure_io
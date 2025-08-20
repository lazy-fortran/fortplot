!> Error handling module for fortplot library
!> Provides centralized error management with status codes and messages
module fortplot_errors
    use iso_fortran_env, only: error_unit
    implicit none
    private
    
    public :: fortplot_error_t
    public :: fortplot_status_t
    public :: is_error
    public :: get_error_message
    public :: log_error
    public :: SUCCESS
    public :: ERROR_INVALID_INPUT
    public :: ERROR_DIMENSION_MISMATCH
    public :: ERROR_RESOURCE_LIMIT
    public :: ERROR_INTERNAL
    public :: ERROR_FILE_IO
    
    ! Error status codes
    integer, parameter :: SUCCESS = 0
    integer, parameter :: ERROR_INVALID_INPUT = 1
    integer, parameter :: ERROR_DIMENSION_MISMATCH = 2
    integer, parameter :: ERROR_RESOURCE_LIMIT = 3
    integer, parameter :: ERROR_INTERNAL = 4
    integer, parameter :: ERROR_FILE_IO = 5
    
    ! Maximum error message length
    integer, parameter :: MAX_ERROR_MESSAGE = 256
    
    !> Error result type containing status and message
    type :: fortplot_error_t
        integer :: status = SUCCESS
        character(len=MAX_ERROR_MESSAGE) :: message = ""
    contains
        procedure :: set_error
        procedure :: clear_error
        procedure :: is_error => error_has_error
    end type fortplot_error_t
    
    !> Status type for simpler error handling
    type :: fortplot_status_t
        integer :: code = SUCCESS
    end type fortplot_status_t

contains

    !> Set error status and message
    subroutine set_error(this, status, message)
        class(fortplot_error_t), intent(inout) :: this
        integer, intent(in) :: status
        character(len=*), intent(in) :: message
        
        this%status = status
        this%message = trim(message)
    end subroutine set_error

    !> Clear error status
    subroutine clear_error(this)
        class(fortplot_error_t), intent(inout) :: this
        
        this%status = SUCCESS
        this%message = ""
    end subroutine clear_error

    !> Check if error object has an error
    function error_has_error(this) result(has_error)
        class(fortplot_error_t), intent(in) :: this
        logical :: has_error
        
        has_error = (this%status /= SUCCESS)
    end function error_has_error

    !> Check if status code indicates an error
    function is_error(status) result(has_error)
        integer, intent(in) :: status
        logical :: has_error
        
        has_error = (status /= SUCCESS)
    end function is_error

    !> Get descriptive error message for status code
    function get_error_message(status) result(message)
        integer, intent(in) :: status
        character(len=MAX_ERROR_MESSAGE) :: message
        
        select case (status)
        case (SUCCESS)
            message = "Success"
        case (ERROR_INVALID_INPUT)
            message = "Invalid input parameters"
        case (ERROR_DIMENSION_MISMATCH)
            message = "Array dimension mismatch"
        case (ERROR_RESOURCE_LIMIT)
            message = "Resource limit exceeded"
        case (ERROR_INTERNAL)
            message = "Internal error"
        case (ERROR_FILE_IO)
            message = "File I/O error"
        case default
            write(message, '("Unknown error code: ", I0)') status
        end select
    end function get_error_message

    !> Log error message to error unit
    subroutine log_error(status, context)
        integer, intent(in) :: status
        character(len=*), intent(in), optional :: context
        
        character(len=MAX_ERROR_MESSAGE) :: message
        
        if (is_error(status)) then
            message = get_error_message(status)
            if (present(context)) then
                write(error_unit, '("Error in ", A, ": ", A)') trim(context), trim(message)
            else
                write(error_unit, '("Error: ", A)') trim(message)
            end if
        end if
    end subroutine log_error

end module fortplot_errors
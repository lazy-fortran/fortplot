program fortplot_python_bridge
    !! Fortran program that acts as a bridge for Python calls
    !! This reads commands from stdin and executes fortplot functions
    use fortplot_matplotlib
    use iso_fortran_env, only: wp => real64
    use fortplot_logging, only: log_error, log_warning
    use fortplot_parameter_validation, only: validate_numeric_parameters, &
         is_finite_safe, parameter_validation_result_t
    use fortplot_validation_core, only: validate_file_path
    implicit none
    
    integer, parameter :: MAX_CMD_LEN = 64
    integer, parameter :: MAX_STR_LEN = 256
    integer, parameter :: MAX_INPUT_ARRAY = 1000000
    
    character(len=MAX_STR_LEN) :: command
    real(wp), allocatable :: x(:), y(:), data(:)
    integer :: ios
    
    ! Initialize figure
    call figure()
    
    ! Main command processing loop
    do
        if (.not. safe_read_line(MAX_CMD_LEN, command)) exit
        
        command = adjustl(trim(command))
        if (len_trim(command) == 0) cycle
        
        select case (trim(command))
        case ('FIGURE')
            call process_figure_command()
        case ('PLOT')
            call process_plot_command(x, y)
        case ('SCATTER')
            call process_scatter_command(x, y)
        case ('HISTOGRAM')
            call process_histogram_command(data)
        case ('TITLE')
            call process_title_command()
        case ('XLABEL')
            call process_xlabel_command()
        case ('YLABEL')
            call process_ylabel_command()
        case ('LEGEND')
            call legend()
        case ('SAVEFIG')
            call process_savefig_command()
        case ('SHOW')
            call process_show_command()
        case ('XLIM')
            call process_xlim_command()
        case ('YLIM')
            call process_ylim_command()
        case ('QUIT', 'EXIT')
            exit
        case default
            ! Unknown command, skip
            cycle
        end select
    end do
    
    ! Cleanup
    if (allocated(x)) deallocate(x)
    if (allocated(y)) deallocate(y)
    if (allocated(data)) deallocate(data)
    
contains
    
    subroutine process_figure_command()
        integer :: ios
        read(*, *, iostat=ios) ! width, height (ignored for now)
        call figure()
    end subroutine
    
    subroutine process_plot_command(x_arr, y_arr)
        real(wp), allocatable, intent(inout) :: x_arr(:), y_arr(:)
        character(len=MAX_STR_LEN) :: label_str
        integer :: n, ios
        type(parameter_validation_result_t) :: v
        
        read(*, *, iostat=ios) n
        if (ios /= 0) return
        if (n <= 0 .or. n > MAX_INPUT_ARRAY) then
            call log_error('Invalid or excessive array size for PLOT')
            return
        end if
        
        if (allocated(x_arr)) deallocate(x_arr)
        if (allocated(y_arr)) deallocate(y_arr)
        allocate(x_arr(n), y_arr(n))
        
        call read_xy_data(x_arr, y_arr, n)
        
        ! Validate numeric inputs (NaN/infinity handling)
        v = validate_numeric_parameters(x_arr, 'x', 'python_bridge_plot')
        if (.not. v%is_valid) return
        v = validate_numeric_parameters(y_arr, 'y', 'python_bridge_plot')
        if (.not. v%is_valid) return
        
        if (safe_read_line(MAX_STR_LEN, label_str) .and. len_trim(label_str) > 0) then
            call plot(x_arr, y_arr, label=trim(label_str))
        else
            call plot(x_arr, y_arr)
        end if
    end subroutine
    
    subroutine process_scatter_command(x_arr, y_arr)
        real(wp), allocatable, intent(inout) :: x_arr(:), y_arr(:)
        character(len=MAX_STR_LEN) :: label_str
        integer :: n, ios
        type(parameter_validation_result_t) :: v
        
        read(*, *, iostat=ios) n
        if (ios /= 0) return
        if (n <= 0 .or. n > MAX_INPUT_ARRAY) then
            call log_error('Invalid or excessive array size for SCATTER')
            return
        end if
        
        if (allocated(x_arr)) deallocate(x_arr)
        if (allocated(y_arr)) deallocate(y_arr)
        allocate(x_arr(n), y_arr(n))
        
        call read_xy_data(x_arr, y_arr, n)
        
        ! Validate numeric inputs (NaN/infinity handling)
        v = validate_numeric_parameters(x_arr, 'x', 'python_bridge_scatter')
        if (.not. v%is_valid) return
        v = validate_numeric_parameters(y_arr, 'y', 'python_bridge_scatter')
        if (.not. v%is_valid) return
        
        if (safe_read_line(MAX_STR_LEN, label_str) .and. len_trim(label_str) > 0) then
            call scatter(x_arr, y_arr, label=trim(label_str))
        else
            call scatter(x_arr, y_arr)
        end if
    end subroutine
    
    subroutine process_histogram_command(data_arr)
        real(wp), allocatable, intent(inout) :: data_arr(:)
        character(len=MAX_STR_LEN) :: label_str
        integer :: n, i, ios
        type(parameter_validation_result_t) :: v
        
        read(*, *, iostat=ios) n
        if (ios /= 0) return
        if (n <= 0 .or. n > MAX_INPUT_ARRAY) then
            call log_error('Invalid or excessive array size for HISTOGRAM')
            return
        end if
        
        if (allocated(data_arr)) deallocate(data_arr)
        allocate(data_arr(n))
        
        do i = 1, n
            read(*, *, iostat=ios) data_arr(i)
            if (ios /= 0) exit
        end do
        
        v = validate_numeric_parameters(data_arr, 'data', 'python_bridge_hist')
        if (.not. v%is_valid) return
        
        if (safe_read_line(MAX_STR_LEN, label_str) .and. len_trim(label_str) > 0) then
            call hist(data_arr, label=trim(label_str))
        else
            call hist(data_arr)
        end if
    end subroutine
    
    subroutine read_xy_data(x_arr, y_arr, n)
        real(wp), intent(out) :: x_arr(:), y_arr(:)
        integer, intent(in) :: n
        integer :: i, ios
        
        do i = 1, n
            read(*, *, iostat=ios) x_arr(i)
            if (ios /= 0) exit
        end do
        
        do i = 1, n
            read(*, *, iostat=ios) y_arr(i)
            if (ios /= 0) exit
        end do
    end subroutine
    
    subroutine process_title_command()
        character(len=MAX_STR_LEN) :: title_str
        if (safe_read_line(MAX_STR_LEN, title_str)) call title(trim(title_str))
    end subroutine
    
    subroutine process_xlabel_command()
        character(len=MAX_STR_LEN) :: xlabel_str
        if (safe_read_line(MAX_STR_LEN, xlabel_str)) call xlabel(trim(xlabel_str))
    end subroutine
    
    subroutine process_ylabel_command()
        character(len=MAX_STR_LEN) :: ylabel_str
        if (safe_read_line(MAX_STR_LEN, ylabel_str)) call ylabel(trim(ylabel_str))
    end subroutine
    
    subroutine process_savefig_command()
        character(len=MAX_STR_LEN) :: filename_str
        type(parameter_validation_result_t) :: path_validation
        
        if (.not. safe_read_line(MAX_STR_LEN, filename_str)) return
        
        path_validation = validate_file_path(trim(filename_str), check_parent=.true., context='python_bridge_savefig')
        if (.not. path_validation%is_valid) then
            call log_error('Invalid file path in SAVEFIG')
            return
        end if
        
        call savefig(trim(filename_str))
    end subroutine
    
    subroutine process_show_command()
        logical :: blocking_flag
        integer :: ios
        blocking_flag = .true.
        read(*, *, iostat=ios) blocking_flag
        call show(blocking=blocking_flag)
    end subroutine
    
    subroutine process_xlim_command()
        real(wp) :: xmin_val, xmax_val
        integer :: ios
        read(*, *, iostat=ios) xmin_val, xmax_val
        if (ios /= 0) return
        if (.not. is_finite_safe(xmin_val) .or. .not. is_finite_safe(xmax_val)) then
            call log_error('Invalid numeric range in XLIM')
            return
        end if
        call xlim(xmin_val, xmax_val)
    end subroutine
    
    subroutine process_ylim_command()
        real(wp) :: ymin_val, ymax_val
        integer :: ios
        read(*, *, iostat=ios) ymin_val, ymax_val
        if (ios /= 0) return
        if (.not. is_finite_safe(ymin_val) .or. .not. is_finite_safe(ymax_val)) then
            call log_error('Invalid numeric range in YLIM')
            return
        end if
        call ylim(ymin_val, ymax_val)
    end subroutine
    
    logical function safe_read_line(max_len, out)
        !! Safely read a line from stdin, enforcing maximum length
        integer, intent(in) :: max_len
        character(len=*), intent(inout) :: out
        character(len=4096) :: buf
        integer :: ios
        
        safe_read_line = .false.
        read(*, '(A)', iostat=ios) buf
        if (ios /= 0) return
        if (len_trim(buf) > max_len) then
            call log_error('Input line exceeds maximum allowed length')
            return
        end if
        out = buf(1:min(len(out), len_trim(buf)))
        safe_read_line = .true.
    end function safe_read_line
    
end program fortplot_python_bridge

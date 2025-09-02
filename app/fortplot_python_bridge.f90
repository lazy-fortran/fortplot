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
    ! Command-line handling
    integer :: argc, i
    character(len=64) :: arg

    argc = command_argument_count()
    if (argc > 0) then
        do i = 1, argc
            call get_command_argument(i, arg)
            if (trim(adjustl(arg)) == '--help' .or. trim(adjustl(arg)) == '-h') then
                write(*, '(A)') 'fortplot_python_bridge - stdin-driven plotting bridge'
                write(*, '(A)') 'Usage:'
                write(*, '(A)') '  - Run without arguments and communicate via stdin.'
                write(*, '(A)') '  - Example sequence:'
                write(*, '(A)') '      FIGURE'
                write(*, '(A)') '      640 480'
                write(*, '(A)') '      PLOT'
                write(*, '(A)') '      <n> then n lines of x, then n lines of y'
                write(*, '(A)') '      TITLE'
                write(*, '(A)') '      <text>'
                write(*, '(A)') '      SAVEFIG'
                write(*, '(A)') '      <filename>'
                write(*, '(A)') '      QUIT'
                stop
            end if
        end do
    end if
    
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
        case ('PCOLORMESH')
            call process_pcolormesh_command()
        case ('CONTOUR')
            call process_contour_command(.false.)
        case ('CONTOURF')
            call process_contour_command(.true.)
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
        case ('XSCALE')
            call process_xscale_command()
        case ('YSCALE')
            call process_yscale_command()
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

    subroutine process_pcolormesh_command()
        !! Read pcolormesh inputs and call implementation
        real(wp), allocatable :: x_local(:), y_local(:)
        real(wp), allocatable :: c_local(:,:)
        integer :: nx, ny, mx, my
        integer :: i, j, ios

        read(*, *, iostat=ios) nx, ny
        if (ios /= 0) return
        if (nx <= 1 .or. ny <= 1) return
        if (nx > MAX_INPUT_ARRAY .or. ny > MAX_INPUT_ARRAY) then
            call log_error('Invalid or excessive grid size for PCOLORMESH')
            return
        end if

        allocate(x_local(nx), y_local(ny))
        do i = 1, nx
            read(*, *, iostat=ios) x_local(i)
            if (ios /= 0) return
        end do
        do i = 1, ny
            read(*, *, iostat=ios) y_local(i)
            if (ios /= 0) return
        end do

        ! c has shape (ny-1, nx-1)
        my = ny - 1
        mx = nx - 1
        allocate(c_local(my, mx))
        do i = 1, my
            do j = 1, mx
                read(*, *, iostat=ios) c_local(i, j)
                if (ios /= 0) return
            end do
        end do

        call pcolormesh(x_local, y_local, c_local)

        if (allocated(x_local)) deallocate(x_local)
        if (allocated(y_local)) deallocate(y_local)
        if (allocated(c_local)) deallocate(c_local)
    end subroutine process_pcolormesh_command

    subroutine process_contour_command(filled)
        !! Read contour/contourf inputs and call implementation
        logical, intent(in) :: filled
        real(wp), allocatable :: x_local(:), y_local(:)
        real(wp), allocatable :: z_local(:,:)
        real(wp), allocatable :: levels(:)
        integer :: nx, ny, i, j, ios, nlevels

        read(*, *, iostat=ios) nx, ny
        if (ios /= 0) return
        if (nx <= 1 .or. ny <= 1) return
        if (nx > MAX_INPUT_ARRAY .or. ny > MAX_INPUT_ARRAY) then
            call log_error('Invalid or excessive grid size for CONTOUR')
            return
        end if

        allocate(x_local(nx), y_local(ny))
        do i = 1, nx
            read(*, *, iostat=ios) x_local(i)
            if (ios /= 0) return
        end do
        do i = 1, ny
            read(*, *, iostat=ios) y_local(i)
            if (ios /= 0) return
        end do

        allocate(z_local(ny, nx))
        do i = 1, ny
            do j = 1, nx
                read(*, *, iostat=ios) z_local(i, j)
                if (ios /= 0) return
            end do
        end do

        ! Optional levels: next line is integer count; if <=0 or read fails, skip
        nlevels = 0
        read(*, *, iostat=ios) nlevels
        if (ios == 0 .and. nlevels > 0 .and. nlevels <= MAX_INPUT_ARRAY) then
            allocate(levels(nlevels))
            do i = 1, nlevels
                read(*, *, iostat=ios) levels(i)
                if (ios /= 0) exit
            end do
            if (filled) then
                call contour_filled(x_local, y_local, z_local, levels=levels)
            else
                call contour(x_local, y_local, z_local, levels=levels)
            end if
        else
            if (filled) then
                call contour_filled(x_local, y_local, z_local)
            else
                call contour(x_local, y_local, z_local)
            end if
        end if

        if (allocated(x_local)) deallocate(x_local)
        if (allocated(y_local)) deallocate(y_local)
        if (allocated(z_local)) deallocate(z_local)
        if (allocated(levels)) deallocate(levels)
    end subroutine process_contour_command
    
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

    subroutine process_streamplot_command()
        !! Read streamplot inputs and call implementation
        real(wp), allocatable :: x_local(:), y_local(:)
        real(wp), allocatable :: u_local(:,:), v_local(:,:)
        integer :: nx, ny, i, j, ios
        real(wp) :: density

        read(*, *, iostat=ios) nx, ny
        if (ios /= 0) return
        if (nx <= 0 .or. ny <= 0) return
        if (nx > MAX_INPUT_ARRAY .or. ny > MAX_INPUT_ARRAY) then
            call log_error('Invalid or excessive grid size for STREAMPLOT')
            return
        end if
        allocate(x_local(nx), y_local(ny))
        do i = 1, nx
            read(*, *, iostat=ios) x_local(i)
            if (ios /= 0) return
        end do
        do i = 1, ny
            read(*, *, iostat=ios) y_local(i)
            if (ios /= 0) return
        end do

        allocate(u_local(nx, ny), v_local(nx, ny))
        do i = 1, nx
            do j = 1, ny
                read(*, *, iostat=ios) u_local(i, j)
                if (ios /= 0) return
            end do
        end do
        do i = 1, nx
            do j = 1, ny
                read(*, *, iostat=ios) v_local(i, j)
                if (ios /= 0) return
            end do
        end do

        density = 1.0_wp
        read(*, *, iostat=ios) density
        if (ios /= 0) density = 1.0_wp

        call streamplot(x_local, y_local, u_local, v_local, density)

        if (allocated(x_local)) deallocate(x_local)
        if (allocated(y_local)) deallocate(y_local)
        if (allocated(u_local)) deallocate(u_local)
        if (allocated(v_local)) deallocate(v_local)
    end subroutine process_streamplot_command

    subroutine process_xscale_command()
        character(len=MAX_STR_LEN) :: scale
        real(wp) :: threshold
        integer :: ios
        threshold = 1.0_wp
        if (.not. safe_read_line(MAX_STR_LEN, scale)) return
        read(*, *, iostat=ios) threshold
        if (ios /= 0) then
            call set_xscale(trim(scale))
        else
            call set_xscale(trim(scale), threshold)
        end if
    end subroutine process_xscale_command

    subroutine process_yscale_command()
        character(len=MAX_STR_LEN) :: scale
        real(wp) :: threshold
        integer :: ios
        threshold = 1.0_wp
        if (.not. safe_read_line(MAX_STR_LEN, scale)) return
        read(*, *, iostat=ios) threshold
        if (ios /= 0) then
            call set_yscale(trim(scale))
        else
            call set_yscale(trim(scale), threshold)
        end if
    end subroutine process_yscale_command
    
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

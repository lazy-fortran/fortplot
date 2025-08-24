program fortplot_python_bridge
    !! Fortran program that acts as a bridge for Python calls
    !! This reads commands from stdin and executes fortplot functions
    use fortplot_matplotlib
    use iso_fortran_env, only: wp => real64
    implicit none
    
    character(len=256) :: command
    real(wp), allocatable :: x(:), y(:), data(:)
    integer :: ios
    
    ! Initialize figure
    call figure()
    
    ! Main command processing loop
    do
        read(*, '(A)', iostat=ios) command
        if (ios /= 0) exit
        
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
        character(len=256) :: label_str
        integer :: n, i, ios
        
        read(*, *, iostat=ios) n
        if (ios /= 0) return
        
        if (allocated(x_arr)) deallocate(x_arr)
        if (allocated(y_arr)) deallocate(y_arr)
        allocate(x_arr(n), y_arr(n))
        
        call read_xy_data(x_arr, y_arr, n)
        
        read(*, '(A)', iostat=ios) label_str
        if (ios == 0 .and. len_trim(label_str) > 0) then
            call plot(x_arr, y_arr, label=trim(label_str))
        else
            call plot(x_arr, y_arr)
        end if
    end subroutine
    
    subroutine process_scatter_command(x_arr, y_arr)
        real(wp), allocatable, intent(inout) :: x_arr(:), y_arr(:)
        character(len=256) :: label_str
        integer :: n, ios
        
        read(*, *, iostat=ios) n
        if (ios /= 0) return
        
        if (allocated(x_arr)) deallocate(x_arr)
        if (allocated(y_arr)) deallocate(y_arr)
        allocate(x_arr(n), y_arr(n))
        
        call read_xy_data(x_arr, y_arr, n)
        
        read(*, '(A)', iostat=ios) label_str
        if (ios == 0 .and. len_trim(label_str) > 0) then
            call scatter(x_arr, y_arr, label=trim(label_str))
        else
            call scatter(x_arr, y_arr)
        end if
    end subroutine
    
    subroutine process_histogram_command(data_arr)
        real(wp), allocatable, intent(inout) :: data_arr(:)
        character(len=256) :: label_str
        integer :: n, i, ios
        
        read(*, *, iostat=ios) n
        if (ios /= 0) return
        
        if (allocated(data_arr)) deallocate(data_arr)
        allocate(data_arr(n))
        
        do i = 1, n
            read(*, *, iostat=ios) data_arr(i)
            if (ios /= 0) exit
        end do
        
        read(*, '(A)', iostat=ios) label_str
        if (ios == 0 .and. len_trim(label_str) > 0) then
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
        character(len=256) :: title_str
        integer :: ios
        read(*, '(A)', iostat=ios) title_str
        if (ios == 0) call title(trim(title_str))
    end subroutine
    
    subroutine process_xlabel_command()
        character(len=256) :: xlabel_str
        integer :: ios
        read(*, '(A)', iostat=ios) xlabel_str
        if (ios == 0) call xlabel(trim(xlabel_str))
    end subroutine
    
    subroutine process_ylabel_command()
        character(len=256) :: ylabel_str
        integer :: ios
        read(*, '(A)', iostat=ios) ylabel_str
        if (ios == 0) call ylabel(trim(ylabel_str))
    end subroutine
    
    subroutine process_savefig_command()
        character(len=256) :: filename_str
        integer :: ios
        read(*, '(A)', iostat=ios) filename_str
        if (ios == 0) call savefig(trim(filename_str))
    end subroutine
    
    subroutine process_show_command()
        logical :: blocking_flag
        integer :: ios
        read(*, *, iostat=ios) blocking_flag
        call show(blocking=blocking_flag)
    end subroutine
    
    subroutine process_xlim_command()
        real(wp) :: xmin_val, xmax_val
        integer :: ios
        read(*, *, iostat=ios) xmin_val, xmax_val
        if (ios == 0) call xlim(xmin_val, xmax_val)
    end subroutine
    
    subroutine process_ylim_command()
        real(wp) :: ymin_val, ymax_val
        integer :: ios
        read(*, *, iostat=ios) ymin_val, ymax_val
        if (ios == 0) call ylim(ymin_val, ymax_val)
    end subroutine
    
end program fortplot_python_bridge
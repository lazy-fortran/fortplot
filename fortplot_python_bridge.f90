program fortplot_python_bridge
    !! Fortran program that acts as a bridge for Python calls
    !! This reads commands from stdin and executes fortplot functions
    use fortplot_matplotlib
    use iso_fortran_env, only: wp => real64
    implicit none
    
    character(len=256) :: command, arg1, arg2, arg3
    character(len=:), allocatable :: filename
    real(wp), allocatable :: x(:), y(:), data(:)
    real(wp) :: xmin_val, xmax_val, ymin_val, ymax_val
    integer :: n, i, ios
    logical :: blocking_flag
    
    ! Initialize figure
    call figure()
    
    ! Read commands from stdin
    do
        read(*, '(A)', iostat=ios) command
        if (ios /= 0) exit
        
        command = adjustl(trim(command))
        if (len_trim(command) == 0) cycle
        
        select case (trim(command))
        
        case ('FIGURE')
            read(*, *, iostat=ios) ! width, height (ignored for now)
            call figure()
            
        case ('PLOT')
            read(*, *, iostat=ios) n
            if (ios /= 0) cycle
            
            if (allocated(x)) deallocate(x)
            if (allocated(y)) deallocate(y)
            allocate(x(n), y(n))
            
            ! Read x values
            do i = 1, n
                read(*, *, iostat=ios) x(i)
                if (ios /= 0) exit
            end do
            
            ! Read y values  
            do i = 1, n
                read(*, *, iostat=ios) y(i)
                if (ios /= 0) exit
            end do
            
            ! Read optional label
            read(*, '(A)', iostat=ios) arg1
            if (ios == 0 .and. len_trim(arg1) > 0) then
                call plot(x, y, label=trim(arg1))
            else
                call plot(x, y)
            end if
            
        case ('SCATTER')
            read(*, *, iostat=ios) n
            if (ios /= 0) cycle
            
            if (allocated(x)) deallocate(x)
            if (allocated(y)) deallocate(y)
            allocate(x(n), y(n))
            
            ! Read x values
            do i = 1, n
                read(*, *, iostat=ios) x(i)
                if (ios /= 0) exit
            end do
            
            ! Read y values  
            do i = 1, n
                read(*, *, iostat=ios) y(i)
                if (ios /= 0) exit
            end do
            
            ! Read optional label
            read(*, '(A)', iostat=ios) arg1
            if (ios == 0 .and. len_trim(arg1) > 0) then
                call scatter(x, y, label=trim(arg1))
            else
                call scatter(x, y)
            end if
            
        case ('HISTOGRAM')
            read(*, *, iostat=ios) n
            if (ios /= 0) cycle
            
            if (allocated(data)) deallocate(data)
            allocate(data(n))
            
            ! Read data values
            do i = 1, n
                read(*, *, iostat=ios) data(i)
                if (ios /= 0) exit
            end do
            
            ! Read optional label
            read(*, '(A)', iostat=ios) arg1
            if (ios == 0 .and. len_trim(arg1) > 0) then
                call hist(data, label=trim(arg1))
            else
                call hist(data)
            end if
            
        case ('TITLE')
            read(*, '(A)', iostat=ios) arg1
            if (ios == 0) call title(trim(arg1))
            
        case ('XLABEL')
            read(*, '(A)', iostat=ios) arg1
            if (ios == 0) call xlabel(trim(arg1))
            
        case ('YLABEL')
            read(*, '(A)', iostat=ios) arg1
            if (ios == 0) call ylabel(trim(arg1))
            
        case ('LEGEND')
            call legend()
            
        case ('SAVEFIG')
            read(*, '(A)', iostat=ios) arg1
            if (ios == 0) call savefig(trim(arg1))
            
        case ('SHOW')
            read(*, *, iostat=ios) blocking_flag
            call show(blocking=blocking_flag)
            
        case ('XLIM')
            read(*, *, iostat=ios) xmin_val, xmax_val
            if (ios == 0) call xlim(xmin_val, xmax_val)
            
        case ('YLIM')
            read(*, *, iostat=ios) ymin_val, ymax_val
            if (ios == 0) call ylim(ymin_val, ymax_val)
            
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

end program fortplot_python_bridge
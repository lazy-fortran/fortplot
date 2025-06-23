module fortplot_legend
    !! Legend module following SOLID principles
    !!
    !! Single Responsibility: Legend rendering and positioning
    !! Open/Closed: Extensible legend types via interfaces
    !! Liskov Substitution: Legend renderers work across backends
    !! Interface Segregation: Focused legend interface
    !! Dependency Inversion: Depends on abstractions, not concrete backends
    
    use fortplot_context, only: plot_context
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    private
    public :: legend_t, legend_entry_t, create_legend, initialize_legend, legend_render
    public :: LEGEND_UPPER_LEFT, LEGEND_UPPER_RIGHT, LEGEND_LOWER_LEFT, LEGEND_LOWER_RIGHT
    
    ! Legend position constants
    integer, parameter :: LEGEND_UPPER_LEFT = 1
    integer, parameter :: LEGEND_UPPER_RIGHT = 2  
    integer, parameter :: LEGEND_LOWER_LEFT = 3
    integer, parameter :: LEGEND_LOWER_RIGHT = 4
    
    type :: legend_entry_t
        !! Single Responsibility: Represents one legend entry
        character(len=:), allocatable :: label
        real(wp), dimension(3) :: color
        character(len=:), allocatable :: linestyle
    end type legend_entry_t
    
    type :: legend_t
        !! Single Responsibility: Legend layout and rendering coordination
        type(legend_entry_t), allocatable :: entries(:)
        integer :: position = LEGEND_UPPER_RIGHT
        integer :: num_entries = 0
        real(wp) :: x_offset = 10.0_wp
        real(wp) :: y_offset = 10.0_wp
        real(wp) :: entry_height = 20.0_wp
        real(wp) :: line_length = 30.0_wp
        real(wp) :: text_padding = 10.0_wp
    contains
        procedure :: add_entry => legend_add_entry
        procedure :: render => legend_render
        procedure :: set_position => legend_set_position
    end type legend_t
    
contains

    function create_legend() result(legend)
        !! Factory function following Dependency Inversion
        type(legend_t) :: legend
        
        allocate(legend%entries(0))
        legend%num_entries = 0
    end function create_legend
    
    subroutine initialize_legend(legend)
        !! Initialize legend components
        type(legend_t), intent(inout) :: legend
        
        allocate(legend%entries(0))
        legend%num_entries = 0
        legend%position = LEGEND_UPPER_RIGHT
        legend%x_offset = 10.0_wp
        legend%y_offset = 10.0_wp
        legend%entry_height = 20.0_wp
        legend%line_length = 30.0_wp
        legend%text_padding = 10.0_wp
    end subroutine initialize_legend
    
    subroutine legend_add_entry(this, label, color, linestyle)
        !! Add entry following Open/Closed principle
        class(legend_t), intent(inout) :: this
        character(len=*), intent(in) :: label
        real(wp), dimension(3), intent(in) :: color
        character(len=*), intent(in), optional :: linestyle
        type(legend_entry_t), allocatable :: temp_entries(:)
        integer :: new_size
        
        ! Expand entries array (DRY: could be extracted to utility)
        new_size = this%num_entries + 1
        allocate(temp_entries(new_size))
        
        if (this%num_entries > 0) then
            temp_entries(1:this%num_entries) = this%entries
        end if
        
        ! Add new entry
        temp_entries(new_size)%label = label
        temp_entries(new_size)%color = color
        if (present(linestyle)) then
            temp_entries(new_size)%linestyle = linestyle
        else
            temp_entries(new_size)%linestyle = "-"
        end if
        
        ! Replace entries array
        call move_alloc(temp_entries, this%entries)
        this%num_entries = new_size
    end subroutine legend_add_entry
    
    subroutine legend_set_position(this, location)
        !! Set legend position using string interface
        class(legend_t), intent(inout) :: this
        character(len=*), intent(in) :: location
        
        select case (trim(location))
        case ("upper left")
            this%position = LEGEND_UPPER_LEFT
        case ("upper right")
            this%position = LEGEND_UPPER_RIGHT
        case ("lower left") 
            this%position = LEGEND_LOWER_LEFT
        case ("lower right")
            this%position = LEGEND_LOWER_RIGHT
        case default
            this%position = LEGEND_UPPER_RIGHT  ! Default
        end select
    end subroutine legend_set_position
    
    subroutine legend_render(this, backend)
        !! Render legend following Liskov Substitution
        !! Works with any backend that implements plot_context interface
        class(legend_t), intent(in) :: this
        class(plot_context), intent(inout) :: backend
        real(wp) :: legend_x, legend_y, line_x1, line_x2, line_y
        real(wp) :: text_x, text_y
        integer :: i
        
        if (this%num_entries == 0) return
        
        ! Calculate legend position based on backend dimensions
        call calculate_legend_position(this, backend, legend_x, legend_y)
        
        ! Render each legend entry
        do i = 1, this%num_entries
            ! Calculate line position
            line_x1 = legend_x + this%x_offset
            line_x2 = line_x1 + this%line_length
            line_y = legend_y + real(i-1, wp) * this%entry_height
            
            ! Set color and draw legend line
            call backend%color(this%entries(i)%color(1), &
                              this%entries(i)%color(2), &
                              this%entries(i)%color(3))
            call backend%line(line_x1, line_y, line_x2, line_y)
            
            ! Draw legend text
            text_x = line_x2 + this%text_padding
            text_y = line_y
            call backend%text(text_x, text_y, this%entries(i)%label)
        end do
    end subroutine legend_render
    
    subroutine calculate_legend_position(legend, backend, x, y)
        !! Calculate legend position based on backend and position setting
        !! Interface Segregation: Only depends on backend dimensions
        type(legend_t), intent(in) :: legend
        class(plot_context), intent(in) :: backend
        real(wp), intent(out) :: x, y
        real(wp) :: total_height
        
        total_height = real(legend%num_entries, wp) * legend%entry_height
        
        select case (legend%position)
        case (LEGEND_UPPER_LEFT)
            x = 20.0_wp
            y = 20.0_wp
        case (LEGEND_UPPER_RIGHT)
            x = real(backend%width, wp) - 150.0_wp  ! Estimate legend width
            y = 20.0_wp
        case (LEGEND_LOWER_LEFT)
            x = 20.0_wp
            y = real(backend%height, wp) - total_height - 20.0_wp
        case (LEGEND_LOWER_RIGHT)
            x = real(backend%width, wp) - 150.0_wp
            y = real(backend%height, wp) - total_height - 20.0_wp
        case default
            x = real(backend%width, wp) - 150.0_wp
            y = 20.0_wp
        end select
    end subroutine calculate_legend_position

end module fortplot_legend
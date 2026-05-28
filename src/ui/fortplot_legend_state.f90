module fortplot_legend_state
    !! Legend state and factory functions
    !!
    !! Single Responsibility: Legend type definitions and factory functions

    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: legend_t, legend_entry_t, create_legend, LEGEND_UPPER_LEFT, &
              LEGEND_UPPER_RIGHT, LEGEND_LOWER_LEFT, LEGEND_LOWER_RIGHT, &
              LEGEND_EAST

    ! Legend position constants
    integer, parameter :: LEGEND_UPPER_LEFT = 1
    integer, parameter :: LEGEND_UPPER_RIGHT = 2
    integer, parameter :: LEGEND_LOWER_LEFT = 3
    integer, parameter :: LEGEND_LOWER_RIGHT = 4
    integer, parameter :: LEGEND_EAST = 5

    type :: legend_entry_t
        !! Single Responsibility: Represents one legend entry
        character(len=:), allocatable :: label
        real(wp), dimension(3) :: color = [0.0_wp, 0.0_wp, 0.0_wp]
        character(len=:), allocatable :: linestyle
        character(len=:), allocatable :: marker
        logical :: is_patch = .false.
            !! Render as a filled rectangle swatch (bars), not a line+marker.
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
        procedure :: set_position => legend_set_position
        procedure :: clear => legend_clear
    end type legend_t

contains

    function create_legend() result(legend)
        !! Factory function following Dependency Inversion
        type(legend_t) :: legend

        allocate(legend%entries(0))
        legend%num_entries = 0
    end function create_legend

    subroutine legend_add_entry(this, label, color, linestyle, marker, is_patch)
        !! Add entry following Open/Closed principle
        class(legend_t), intent(inout) :: this
        character(len=*), intent(in) :: label
        real(wp), dimension(3), intent(in) :: color
        character(len=*), intent(in), optional :: linestyle, marker
        logical, intent(in), optional :: is_patch
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
        if (present(marker)) then
            temp_entries(new_size)%marker = marker
        else
            temp_entries(new_size)%marker = "None"
        end if
        if (present(is_patch)) temp_entries(new_size)%is_patch = is_patch

        ! Replace entries array
        call move_alloc(temp_entries, this%entries)
        this%num_entries = new_size
    end subroutine legend_add_entry

    subroutine legend_clear(this)
        !! Clear all legend entries
        class(legend_t), intent(inout) :: this
        type(legend_entry_t), allocatable :: empty(:)

        allocate(empty(0))
        call move_alloc(empty, this%entries)
        this%num_entries = 0
    end subroutine legend_clear

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
        case ("east")
            this%position = LEGEND_EAST
        case default
            this%position = LEGEND_UPPER_RIGHT  ! Default
        end select
    end subroutine legend_set_position

end module fortplot_legend_state

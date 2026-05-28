module fortplot_legend
    !! Legend module following SOLID principles
    !!
    !! This module re-exports procedures from specialized submodules:
    !!   fortplot_legend_state  - type definitions
    !!   fortplot_legend_drawing - legend box and entry drawing
    !!   fortplot_legend_layout - legend layout calculation (external)

    use fortplot_context, only: plot_context
    use fortplot_legend_drawing, only: render_ascii_legend, render_standard_legend, &
                                      calculate_legend_position, backend_is_ascii
    use fortplot_legend_layout, only: calculate_legend_box
    use fortplot_legend_state, only: legend_t, legend_entry_t, &
                                      LEGEND_UPPER_LEFT, LEGEND_UPPER_RIGHT, &
                                      LEGEND_LOWER_LEFT, LEGEND_LOWER_RIGHT, &
                                      LEGEND_EAST, LEGEND_BEST
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: legend_t, legend_entry_t, create_legend, legend_render, render_ascii_legend, render_standard_legend
    public :: LEGEND_UPPER_LEFT, LEGEND_UPPER_RIGHT, LEGEND_LOWER_LEFT, LEGEND_LOWER_RIGHT
    public :: LEGEND_EAST, LEGEND_BEST

contains

    function create_legend() result(legend)
        !! Factory function following Dependency Inversion
        type(legend_t) :: legend

        allocate(legend%entries(0))
        legend%num_entries = 0
    end function create_legend

    subroutine legend_add_entry(this, label, color, linestyle, marker)
        !! Add entry following Open/Closed principle
        class(legend_t), intent(inout) :: this
        character(len=*), intent(in) :: label
        real(wp), dimension(3), intent(in) :: color
        character(len=*), intent(in), optional :: linestyle, marker
        type(legend_entry_t), allocatable :: temp_entries(:)
        integer :: new_size

        new_size = this%num_entries + 1
        allocate(temp_entries(new_size))

        if (this%num_entries > 0) then
            temp_entries(1:this%num_entries) = this%entries
        end if

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
        case ("best")
            this%position = LEGEND_BEST
        case default
            this%position = LEGEND_UPPER_RIGHT
        end select
    end subroutine legend_set_position

    subroutine legend_render(this, backend)
        !! Render legend - delegates to drawing module
        class(legend_t), intent(in) :: this
        class(plot_context), intent(inout) :: backend
        real(wp) :: legend_x, legend_y
        logical :: ascii_mode

        if (this%num_entries == 0) return

        call calculate_legend_position(this, backend, legend_x, legend_y)

        ascii_mode = backend_is_ascii(backend)
        if (ascii_mode) then
            call render_ascii_legend(this, backend, legend_x, legend_y)
        else
            call render_standard_legend(this, backend, legend_x, legend_y)
        end if
    end subroutine legend_render

end module fortplot_legend

module fortplot_mpeg_memory
    use iso_c_binding
    implicit none
    private
    
    ! Memory structure definition
    type :: mem_t
        integer :: len
        integer :: width
        integer :: height
        integer(c_int8_t), allocatable :: data(:)
    end type mem_t
    
    ! I/O buffer structure definition
    type :: iobuf_t
        integer :: hpos
        integer :: vpos
        integer :: hor
        integer :: ver
        integer :: width
        integer :: height
        integer :: flag
        type(mem_t), pointer :: mem => null()
    end type iobuf_t
    
    public :: mem_t, iobuf_t
    public :: mem_create, mem_destroy, mem_resize
    public :: mem_get_bounds, mem_set_bounds
    public :: iobuf_create, iobuf_destroy, iobuf_initialize
    public :: iobuf_set_position, iobuf_get_position
    public :: char_bound, lower_bound, upper_bound
    
contains

    function mem_create(width, height) result(mem)
        integer, intent(in) :: width, height
        type(mem_t) :: mem
        
        mem%width = width
        mem%height = height
        mem%len = width * height
        
        if (mem%len > 0) then
            allocate(mem%data(mem%len))
            mem%data = 0_c_int8_t
        end if
    end function mem_create

    subroutine mem_destroy(mem)
        type(mem_t), intent(inout) :: mem
        
        if (allocated(mem%data)) then
            deallocate(mem%data)
        end if
        
        mem%width = 0
        mem%height = 0
        mem%len = 0
    end subroutine mem_destroy

    subroutine mem_resize(mem, new_width, new_height)
        type(mem_t), intent(inout) :: mem
        integer, intent(in) :: new_width, new_height
        integer :: new_len
        
        new_len = new_width * new_height
        
        if (allocated(mem%data)) then
            deallocate(mem%data)
        end if
        
        mem%width = new_width
        mem%height = new_height
        mem%len = new_len
        
        if (new_len > 0) then
            allocate(mem%data(new_len))
            mem%data = 0_c_int8_t
        end if
    end subroutine mem_resize

    function mem_get_bounds(mem, index) result(bounded_index)
        type(mem_t), intent(in) :: mem
        integer, intent(in) :: index
        integer :: bounded_index
        
        bounded_index = max(1, min(index, mem%len))
    end function mem_get_bounds

    subroutine mem_set_bounds(mem, index, value)
        type(mem_t), intent(inout) :: mem
        integer, intent(in) :: index
        integer(c_int8_t), intent(in) :: value
        integer :: bounded_index
        
        if (.not. allocated(mem%data)) return
        
        bounded_index = mem_get_bounds(mem, index)
        mem%data(bounded_index) = value
    end subroutine mem_set_bounds

    function iobuf_create(width, height, block_hor, block_ver) result(iobuf)
        integer, intent(in) :: width, height, block_hor, block_ver
        type(iobuf_t) :: iobuf
        
        iobuf%width = width
        iobuf%height = height
        iobuf%hor = block_hor
        iobuf%ver = block_ver
        iobuf%hpos = 0
        iobuf%vpos = 0
        iobuf%flag = 0
        
        allocate(iobuf%mem)
        iobuf%mem = mem_create(width, height)
    end function iobuf_create

    subroutine iobuf_destroy(iobuf)
        type(iobuf_t), intent(inout) :: iobuf
        
        if (associated(iobuf%mem)) then
            call mem_destroy(iobuf%mem)
            deallocate(iobuf%mem)
            iobuf%mem => null()
        end if
        
        iobuf%width = 0
        iobuf%height = 0
        iobuf%hor = 0
        iobuf%ver = 0
        iobuf%hpos = 0
        iobuf%vpos = 0
        iobuf%flag = 0
    end subroutine iobuf_destroy

    subroutine iobuf_initialize(iobuf, width, height, block_hor, block_ver)
        type(iobuf_t), intent(inout) :: iobuf
        integer, intent(in) :: width, height, block_hor, block_ver
        
        ! Clean up existing memory if any
        if (associated(iobuf%mem)) then
            call mem_destroy(iobuf%mem)
            deallocate(iobuf%mem)
        end if
        
        ! Initialize new buffer
        iobuf%width = width
        iobuf%height = height
        iobuf%hor = block_hor
        iobuf%ver = block_ver
        iobuf%hpos = 0
        iobuf%vpos = 0
        iobuf%flag = 0
        
        allocate(iobuf%mem)
        iobuf%mem = mem_create(width, height)
    end subroutine iobuf_initialize

    subroutine iobuf_set_position(iobuf, hpos, vpos)
        type(iobuf_t), intent(inout) :: iobuf
        integer, intent(in) :: hpos, vpos
        
        iobuf%hpos = max(0, min(hpos, iobuf%width - iobuf%hor))
        iobuf%vpos = max(0, min(vpos, iobuf%height - iobuf%ver))
    end subroutine iobuf_set_position

    subroutine iobuf_get_position(iobuf, hpos, vpos)
        type(iobuf_t), intent(in) :: iobuf
        integer, intent(out) :: hpos, vpos
        
        hpos = iobuf%hpos
        vpos = iobuf%vpos
    end subroutine iobuf_get_position

    ! Utility functions for bounds checking (MPEG equivalents)
    function char_bound(value) result(bounded_value)
        integer, intent(in) :: value
        integer :: bounded_value
        
        bounded_value = max(0, min(value, 255))
    end function char_bound

    function lower_bound(index, lower_limit) result(bounded_index)
        integer, intent(in) :: index, lower_limit
        integer :: bounded_index
        
        bounded_index = max(index, lower_limit)
    end function lower_bound

    function upper_bound(index, upper_limit) result(bounded_index)
        integer, intent(in) :: index, upper_limit
        integer :: bounded_index
        
        bounded_index = min(index, upper_limit)
    end function upper_bound

end module fortplot_mpeg_memory
module fortplot_memory_backend
    !! Memory-only backend for Windows CI performance testing
    !!
    !! This module provides an in-memory alternative to file-based backends,
    !! specifically designed to eliminate file I/O bottlenecks in Windows CI
    !! environments (Issue #188).
    
    use iso_fortran_env, only: int32, int64, real64, int8
    implicit none
    private
    
    public :: memory_backend_t
    public :: memory_buffer_t
    public :: get_memory_backend
    public :: clear_memory_backend
    
    ! Memory buffer to hold rendered output
    type :: memory_buffer_t
        character(len=:), allocatable :: filename
        integer(int8), dimension(:), allocatable :: data
        integer(int64) :: size = 0
        character(len=16) :: format = ""
        real(real64) :: creation_time = 0.0_real64
    end type memory_buffer_t
    
    ! Memory backend manager
    type :: memory_backend_t
        type(memory_buffer_t), dimension(:), allocatable :: buffers
        integer :: buffer_count = 0
        integer :: max_buffers = 1000
        logical :: enabled = .false.
        real(real64) :: total_memory_used = 0.0_real64
    contains
        procedure :: initialize => memory_backend_initialize
        procedure :: save => memory_backend_save
        procedure :: get_buffer => memory_backend_get_buffer
        procedure :: exists => memory_backend_exists
        procedure :: get_size => memory_backend_get_size
        procedure :: clear => memory_backend_clear
        procedure :: get_stats => memory_backend_get_stats
    end type memory_backend_t
    
    ! Global memory backend instance
    type(memory_backend_t), save, target :: global_memory_backend
    logical, save :: backend_initialized = .false.
    
contains
    
    subroutine memory_backend_initialize(this, max_buffers)
        !! Initialize the memory backend
        class(memory_backend_t), intent(inout) :: this
        integer, intent(in), optional :: max_buffers
        
        if (present(max_buffers)) then
            this%max_buffers = max_buffers
        end if
        
        if (allocated(this%buffers)) deallocate(this%buffers)
        allocate(this%buffers(this%max_buffers))
        
        this%buffer_count = 0
        this%total_memory_used = 0.0_real64
        this%enabled = .true.
        
    end subroutine memory_backend_initialize
    
    subroutine memory_backend_save(this, filename, data, format)
        !! Save data to memory buffer instead of file
        class(memory_backend_t), intent(inout) :: this
        character(len=*), intent(in) :: filename
        integer(int8), dimension(:), intent(in) :: data
        character(len=*), intent(in), optional :: format
        
        integer :: i, existing_idx
        real(real64) :: current_time
        
        if (.not. this%enabled) return
        
        ! Check if buffer already exists for this filename
        existing_idx = 0
        do i = 1, this%buffer_count
            if (allocated(this%buffers(i)%filename)) then
                if (this%buffers(i)%filename == filename) then
                    existing_idx = i
                    exit
                end if
            end if
        end do
        
        call cpu_time(current_time)
        
        if (existing_idx > 0) then
            ! Update existing buffer
            if (allocated(this%buffers(existing_idx)%data)) then
                this%total_memory_used = this%total_memory_used - &
                    real(size(this%buffers(existing_idx)%data), real64)
                deallocate(this%buffers(existing_idx)%data)
            end if
            
            allocate(this%buffers(existing_idx)%data(size(data)))
            this%buffers(existing_idx)%data = data
            this%buffers(existing_idx)%size = size(data, kind=int64)
            this%buffers(existing_idx)%creation_time = current_time
            if (present(format)) this%buffers(existing_idx)%format = format
            
        else
            ! Create new buffer
            if (this%buffer_count >= this%max_buffers) then
                ! Reached maximum buffers, overwrite oldest
                existing_idx = 1
                do i = 2, this%buffer_count
                    if (this%buffers(i)%creation_time < &
                        this%buffers(existing_idx)%creation_time) then
                        existing_idx = i
                    end if
                end do
                
                if (allocated(this%buffers(existing_idx)%data)) then
                    this%total_memory_used = this%total_memory_used - &
                        real(size(this%buffers(existing_idx)%data), real64)
                    deallocate(this%buffers(existing_idx)%data)
                end if
                if (allocated(this%buffers(existing_idx)%filename)) then
                    deallocate(this%buffers(existing_idx)%filename)
                end if
            else
                this%buffer_count = this%buffer_count + 1
                existing_idx = this%buffer_count
            end if
            
            this%buffers(existing_idx)%filename = filename
            allocate(this%buffers(existing_idx)%data(size(data)))
            this%buffers(existing_idx)%data = data
            this%buffers(existing_idx)%size = size(data, kind=int64)
            this%buffers(existing_idx)%creation_time = current_time
            if (present(format)) this%buffers(existing_idx)%format = format
        end if
        
        this%total_memory_used = this%total_memory_used + real(size(data), real64)
        
    end subroutine memory_backend_save
    
    function memory_backend_get_buffer(this, filename) result(buffer)
        !! Retrieve a buffer by filename
        class(memory_backend_t), intent(in) :: this
        character(len=*), intent(in) :: filename
        type(memory_buffer_t) :: buffer
        
        integer :: i
        
        do i = 1, this%buffer_count
            if (allocated(this%buffers(i)%filename)) then
                if (this%buffers(i)%filename == filename) then
                    buffer = this%buffers(i)
                    return
                end if
            end if
        end do
        
        ! Not found - return empty buffer
        buffer%size = 0
        
    end function memory_backend_get_buffer
    
    function memory_backend_exists(this, filename) result(exists)
        !! Check if a file exists in memory backend
        class(memory_backend_t), intent(in) :: this
        character(len=*), intent(in) :: filename
        logical :: exists
        
        integer :: i
        
        exists = .false.
        if (.not. this%enabled) return
        
        do i = 1, this%buffer_count
            if (allocated(this%buffers(i)%filename)) then
                if (this%buffers(i)%filename == filename) then
                    exists = .true.
                    return
                end if
            end if
        end do
        
    end function memory_backend_exists
    
    function memory_backend_get_size(this, filename) result(size)
        !! Get size of a file in memory backend
        class(memory_backend_t), intent(in) :: this
        character(len=*), intent(in) :: filename
        integer(int64) :: size
        
        integer :: i
        
        size = -1
        if (.not. this%enabled) return
        
        do i = 1, this%buffer_count
            if (allocated(this%buffers(i)%filename)) then
                if (this%buffers(i)%filename == filename) then
                    size = this%buffers(i)%size
                    return
                end if
            end if
        end do
        
    end function memory_backend_get_size
    
    subroutine memory_backend_clear(this)
        !! Clear all memory buffers
        class(memory_backend_t), intent(inout) :: this
        
        integer :: i
        
        do i = 1, this%buffer_count
            if (allocated(this%buffers(i)%data)) deallocate(this%buffers(i)%data)
            if (allocated(this%buffers(i)%filename)) deallocate(this%buffers(i)%filename)
        end do
        
        this%buffer_count = 0
        this%total_memory_used = 0.0_real64
        
    end subroutine memory_backend_clear
    
    subroutine memory_backend_get_stats(this, buffer_count, total_memory, enabled)
        !! Get memory backend statistics
        class(memory_backend_t), intent(in) :: this
        integer, intent(out), optional :: buffer_count
        real(real64), intent(out), optional :: total_memory
        logical, intent(out), optional :: enabled
        
        if (present(buffer_count)) buffer_count = this%buffer_count
        if (present(total_memory)) total_memory = this%total_memory_used
        if (present(enabled)) enabled = this%enabled
        
    end subroutine memory_backend_get_stats
    
    function get_memory_backend() result(backend)
        !! Get global memory backend instance
        type(memory_backend_t), pointer :: backend
        
        if (.not. backend_initialized) then
            call global_memory_backend%initialize()
            backend_initialized = .true.
        end if
        
        backend => global_memory_backend
        
    end function get_memory_backend
    
    subroutine clear_memory_backend()
        !! Clear global memory backend
        
        if (backend_initialized) then
            call global_memory_backend%clear()
        end if
        
    end subroutine clear_memory_backend
    
end module fortplot_memory_backend
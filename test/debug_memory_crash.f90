program debug_memory_crash
    !! Debug memory corruption in text rendering
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    type(figure_t) :: fig
    real(wp), dimension(5) :: x, y
    integer :: i
    
    print *, "=== DEBUG: Memory Corruption Investigation ==="
    
    ! Simple test data
    x = [(real(i, wp), i=1, 5)]
    y = x * 2.0_wp
    
    ! Initialize once
    call fig%initialize(640, 480)
    call fig%add_plot(x, y, label="Test Data")
    
    print *, "First legend call..."
    call fig%legend()
    
    print *, "First savefig call..."
    call fig%savefig('/tmp/test_crash_1.png')
    print *, "First savefig completed"
    
    print *, "Second savefig call..."
    call fig%savefig('/tmp/test_crash_2.png')
    print *, "Second savefig completed"
    
    print *, "Third savefig call..."
    call fig%savefig('/tmp/test_crash_3.png')
    print *, "Third savefig completed"
    
    print *, "Fourth savefig call..."
    call fig%savefig('/tmp/test_crash_4.png')
    print *, "Fourth savefig completed"
    
    print *, "=== All operations completed successfully ==="
    
end program debug_memory_crash
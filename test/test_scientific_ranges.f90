program test_scientific_ranges
    !! Test scientific ranges that should work without clamping
    
    use fortplot
    use iso_fortran_env, only: wp => real64
    implicit none
    
    real(wp), allocatable :: x(:), y(:)
    type(figure_t) :: fig
    
    print *, "=== Testing Scientific Ranges (No Clamping Expected) ==="
    
    !---------------------------------------------------------------------
    ! Test 1: Typical scientific computing range
    !---------------------------------------------------------------------
    print *, "Test 1: Typical scientific range (1e-12 to 1e12)"
    
    x = [1.0e-12_wp, 1.0e-6_wp, 1.0_wp, 1.0e6_wp, 1.0e12_wp]
    y = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
    
    call fig%initialize(400, 300, 'png')
    call fig%add_plot(x, y, label="scientific data")
    call fig%set_title("Scientific Range - Should Not Clamp")
    call fig%set_xlabel("X axis (log scale)")
    call fig%set_ylabel("Y axis")
    call fig%set_xscale("log")
    call fig%savefig('test_scientific_range.png')
    
    ! ASCII for verification
    call fig%initialize(80, 24, 'ascii')
    call fig%add_plot(x, y, label="scientific data")
    call fig%set_title("Scientific Range - ASCII")
    call fig%set_xlabel("X axis (log scale)")
    call fig%set_ylabel("Y axis")
    call fig%set_xscale("log")
    call fig%savefig('test_scientific_range.txt')
    
    !---------------------------------------------------------------------
    ! Test 2: Physics/engineering range
    !---------------------------------------------------------------------
    print *, "Test 2: Physics/engineering range (1e-9 to 1e9)"
    
    x = [1.0e-9_wp, 1.0e-6_wp, 1.0e-3_wp, 1.0_wp, 1.0e3_wp, 1.0e6_wp, 1.0e9_wp]
    y = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp, 7.0_wp]
    
    call fig%initialize(400, 300, 'png')
    call fig%add_plot(x, y, label="engineering data")
    call fig%set_title("Engineering Range")
    call fig%set_xlabel("X axis (log scale)")
    call fig%set_ylabel("Y axis")
    call fig%set_xscale("log")
    call fig%savefig('test_engineering_range.png')
    
    !---------------------------------------------------------------------  
    ! Test 3: At the edge of clamping (should just barely not clamp)
    !---------------------------------------------------------------------
    print *, "Test 3: At edge of clamping range (1e-24 to 1e24)"
    
    x = [1.0e-24_wp, 1.0e-12_wp, 1.0_wp, 1.0e12_wp, 1.0e24_wp]
    y = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
    
    call fig%initialize(400, 300, 'png')
    call fig%add_plot(x, y, label="edge case")
    call fig%set_title("Edge of Clamping Range")
    call fig%set_xlabel("X axis (log scale)")
    call fig%set_ylabel("Y axis")
    call fig%set_xscale("log")
    call fig%savefig('test_edge_clamp_range.png')
    
    print *, ""
    print *, "============================================="
    print *, "Scientific range test files generated:"
    print *, "- test_scientific_range.png (24 orders of magnitude)"
    print *, "- test_scientific_range.txt (ASCII version)"  
    print *, "- test_engineering_range.png (18 orders of magnitude)"
    print *, "- test_edge_clamp_range.png (48 orders - should not clamp)"
    print *, ""
    print *, "✓ No clamping messages should appear above"
    print *, "============================================="
    
end program test_scientific_ranges
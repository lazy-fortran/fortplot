program test_figure_state_isolation
    !! Test program to verify that figure() calls properly reset state
    !! Reproduces Issue #434: Legend state contamination between figure calls
    
    use fortplot
    use iso_fortran_env, only: wp => real64
    implicit none
    
    real(wp) :: x1(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
    real(wp) :: y1(5) = [1.0_wp, 4.0_wp, 2.0_wp, 8.0_wp, 3.0_wp]
    real(wp) :: x2(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
    real(wp) :: y2(5) = [2.0_wp, 3.0_wp, 1.0_wp, 5.0_wp, 4.0_wp]
    type(figure_t), pointer :: fig_ptr
    logical :: test_passed
    
    write(*,*) 'Testing figure state isolation...'
    
    ! Test 1: Create first figure with legend
    call figure(figsize=[800.0d0, 600.0d0])
    call plot(x1, y1, label='First Plot')
    call legend()
    
    ! Get reference to global figure and check legend entries
    fig_ptr => get_global_figure()
    if (fig_ptr%state%legend_data%num_entries /= 1) then
        write(*,*) 'FAIL: First figure should have 1 legend entry, got:', fig_ptr%state%legend_data%num_entries
        stop 1
    end if
    write(*,*) 'PASS: First figure has correct legend entries (1)'
    
    ! Test 2: Create second figure - should have clean state
    call figure(figsize=[800.0d0, 600.0d0])
    call plot(x2, y2, label='Second Plot')
    
    ! Check that legend state is clean (no entries from previous figure)
    fig_ptr => get_global_figure()
    if (fig_ptr%state%legend_data%num_entries /= 0) then
        write(*,*) 'FAIL: Second figure should have 0 legend entries before legend(), got:', fig_ptr%state%legend_data%num_entries
        write(*,*) 'This indicates state contamination from first figure'
        stop 1
    end if
    write(*,*) 'PASS: Second figure has clean legend state (0 entries)'
    
    ! Add legend to second figure  
    call legend()
    
    ! Check that second figure now has only its own legend
    if (fig_ptr%state%legend_data%num_entries /= 1) then
        write(*,*) 'FAIL: Second figure should have 1 legend entry after legend(), got:', fig_ptr%state%legend_data%num_entries
        stop 1
    end if
    write(*,*) 'PASS: Second figure has correct legend entries after legend() (1)'
    
    ! Test 3: Create third figure and verify complete isolation
    call figure(figsize=[800.0d0, 600.0d0])
    
    ! Should have no legend entries and no plots
    fig_ptr => get_global_figure()
    if (fig_ptr%state%legend_data%num_entries /= 0) then
        write(*,*) 'FAIL: Third figure should have 0 legend entries, got:', fig_ptr%state%legend_data%num_entries
        stop 1
    end if
    
    if (fig_ptr%plot_count /= 0) then
        write(*,*) 'FAIL: Third figure should have 0 plots, got:', fig_ptr%plot_count
        stop 1
    end if
    
    write(*,*) 'PASS: Third figure has completely clean state'
    
    write(*,*) 'All tests passed! Figure state isolation is working correctly.'

end program test_figure_state_isolation
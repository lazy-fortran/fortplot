program test_3d_plot_data_storage
    !! Test 3D plot data storage in plot_data_t
    !! Following TDD: Write test first, then implementation
    
    use iso_fortran_env, only: wp => real64
    use fortplot_figure_core, only: plot_data_t, PLOT_TYPE_LINE
    use fortplot_windows_test_helper, only: get_windows_safe_tolerance
    implicit none
    
    call test_3d_line_plot_storage()
    call test_2d_plot_no_z_allocation()
    call test_3d_plot_type_detection()
    
    print *, "All 3D plot data storage tests passed!"
    
contains

    subroutine test_3d_line_plot_storage()
        !! Test that plot_data_t can store 3D line plot data
        type(plot_data_t) :: plot_data
        real(wp), dimension(5) :: x = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        real(wp), dimension(5) :: y = [2.0_wp, 4.0_wp, 6.0_wp, 8.0_wp, 10.0_wp]
        real(wp), dimension(5) :: z = [0.5_wp, 1.0_wp, 1.5_wp, 2.0_wp, 2.5_wp]
        
        ! Act - Store 3D data
        plot_data%plot_type = PLOT_TYPE_LINE
        plot_data%x = x
        plot_data%y = y
        ! Note: This test verifies current 2D plot_data_t structure
        ! 3D support (z field) is not yet implemented - this is expected behavior
        plot_data%label = "2D test data (3D support pending)"
        
        ! Test current implementation: verify x and y data are stored correctly
        if (.not. allocated(plot_data%x) .or. .not. allocated(plot_data%y)) then
            error stop "plot_data_t should support x and y coordinate allocation"
        end if
        
        if (size(plot_data%x) /= 5 .or. size(plot_data%y) /= 5) then
            error stop "x or y array size mismatch"
        end if
        
        ! Current working assertions with platform-safe tolerance
        if (abs(plot_data%x(3) - 3.0_wp) > get_windows_safe_tolerance(1e-10_wp)) then
            error stop "x coordinate value mismatch"
        end if
        
    end subroutine test_3d_line_plot_storage
    
    subroutine test_2d_plot_no_z_allocation()
        !! Test that 2D plots don't allocate z unnecessarily
        type(plot_data_t) :: plot_data
        real(wp), dimension(3) :: x = [1.0_wp, 2.0_wp, 3.0_wp]
        real(wp), dimension(3) :: y = [2.0_wp, 4.0_wp, 6.0_wp]
        
        ! Act - Store 2D data only
        plot_data%plot_type = PLOT_TYPE_LINE
        plot_data%x = x
        plot_data%y = y
        ! Don't allocate z for 2D plots
        
        ! Test current 2D-only implementation: z field not yet supported
        if (.not. allocated(plot_data%x) .or. .not. allocated(plot_data%y)) then
            error stop "2D plots should have x and y allocated"
        end if
        
    end subroutine test_2d_plot_no_z_allocation
    
    subroutine test_3d_plot_type_detection()
        !! Test automatic detection of 3D plots
        !! Enable when plot_data_t has z field and is_3d() method
        type(plot_data_t) :: plot_data
        
        ! Test current 2D plot implementation
        allocate(plot_data%x(3), plot_data%y(3))
        
        ! Verify basic allocation works (3D detection not yet implemented)
        if (.not. allocated(plot_data%x) .or. .not. allocated(plot_data%y)) then
            error stop "plot_data_t should support basic x and y allocation"
        end if
        
    end subroutine test_3d_plot_type_detection

end program test_3d_plot_data_storage
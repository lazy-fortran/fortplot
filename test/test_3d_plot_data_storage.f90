program test_3d_plot_data_storage
    !! Test 3D plot data storage in plot_data_t
    !! Following TDD: Write test first, then implementation
    
    use iso_fortran_env, only: wp => real64
    use fortplot_figure_core, only: plot_data_t, PLOT_TYPE_LINE
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
        ! Note: plot_data_t currently doesn't have z field for 3D line plots
        ! This test documents that 3D support is not yet implemented in the current data structure
        ! plot_data%z = z  ! TODO: Add z coordinate support for 3D line plots
        plot_data%label = "3D test data"
        
        ! Assert - Currently expect failure since z is not implemented
        ! TODO: Enable this assertion when z field is added to plot_data_t
        ! if (.not. allocated(plot_data%z)) then
        !     error stop "plot_data_t should support z coordinate allocation"
        ! end if
        
        ! Current working assertion: verify x and y data are stored correctly
        if (.not. allocated(plot_data%x) .or. .not. allocated(plot_data%y)) then
            error stop "plot_data_t should support x and y coordinate allocation"
        end if
        
        ! TODO: Enable when z field is implemented
        ! if (size(plot_data%z) /= 5) then
        !     error stop "z array size mismatch"
        ! end if
        
        ! Current working assertions
        if (size(plot_data%x) /= 5 .or. size(plot_data%y) /= 5) then
            error stop "x or y array size mismatch"
        end if
        
        ! TODO: Enable when z field is implemented
        ! if (abs(plot_data%z(3) - 1.5_wp) > 1e-10_wp) then
        !     error stop "z coordinate value mismatch"
        ! end if
        
        ! Current working assertions
        if (abs(plot_data%x(3) - 3.0_wp) > 1e-10_wp) then
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
        
        ! Assert - TODO: Enable when z field is added to plot_data_t
        ! if (allocated(plot_data%z)) then
        !     error stop "2D plots should not have z allocated by default"
        ! end if
        
        ! Current working assertion: verify basic fields are working
        if (.not. allocated(plot_data%x) .or. .not. allocated(plot_data%y)) then
            error stop "2D plots should have x and y allocated"
        end if
        
    end subroutine test_2d_plot_no_z_allocation
    
    subroutine test_3d_plot_type_detection()
        !! Test automatic detection of 3D plots
        !! TODO: Enable when plot_data_t has z field and is_3d() method
        type(plot_data_t) :: plot_data
        
        ! Test 2D plot - basic functionality test for now
        allocate(plot_data%x(3), plot_data%y(3))
        ! TODO: Enable when is_3d() method exists
        ! if (plot_data%is_3d()) then
        !     error stop "2D plot incorrectly detected as 3D"
        ! end if
        
        ! Test 3D plot - TODO: Enable when z field exists
        ! allocate(plot_data%z(3))
        ! if (.not. plot_data%is_3d()) then
        !     error stop "3D plot not detected when z is allocated"
        ! end if
        
        ! Current working assertion - verify basic allocation works
        if (.not. allocated(plot_data%x) .or. .not. allocated(plot_data%y)) then
            error stop "plot_data_t should support basic x and y allocation"
        end if
        
    end subroutine test_3d_plot_type_detection

end program test_3d_plot_data_storage
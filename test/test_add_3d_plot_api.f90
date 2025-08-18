program test_add_3d_plot_api
    !! Test add_3d_plot API behavior
    !! Following TDD: Write test first, then implementation
    !! API follows pyplot-fortran conventions
    
    use iso_fortran_env, only: wp => real64
    use fortplot_figure_core, only: figure_t
    implicit none
    
    call test_add_3d_plot_basic()
    call test_add_3d_plot_with_label()
    call test_add_3d_plot_with_all_options()
    call test_figure_detects_3d()
    
    print *, "All add_3d_plot API tests passed!"
    
contains

    subroutine test_add_3d_plot_basic()
        !! Test basic add_3d_plot functionality
        type(figure_t) :: fig
        real(wp), dimension(5) :: x = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        real(wp), dimension(5) :: y = [2.0_wp, 4.0_wp, 6.0_wp, 8.0_wp, 10.0_wp]
        real(wp), dimension(5) :: z = [0.5_wp, 1.0_wp, 1.5_wp, 2.0_wp, 2.5_wp]
        
        ! Initialize figure normally - no special 3D flag
        call fig%initialize(640, 480)
        
        ! Add 3D plot using pyplot-fortran compatible API
        call fig%add_3d_plot(x, y, z)
        
        ! Assert
        if (fig%plot_count /= 1) then
            error stop "add_3d_plot should increment plot count"
        end if
        
        if (.not. allocated(fig%plots(1)%z)) then
            error stop "add_3d_plot should allocate z coordinates"
        end if
        
        if (.not. fig%plots(1)%is_3d()) then
            error stop "Plot should be detected as 3D"
        end if
        
    end subroutine test_add_3d_plot_basic
    
    subroutine test_add_3d_plot_with_label()
        !! Test add_3d_plot with label parameter
        type(figure_t) :: fig
        real(wp), dimension(3) :: x = [1.0_wp, 2.0_wp, 3.0_wp]
        real(wp), dimension(3) :: y = [1.0_wp, 4.0_wp, 9.0_wp]
        real(wp), dimension(3) :: z = [0.0_wp, 1.0_wp, 0.0_wp]
        
        call fig%initialize(640, 480)
        
        ! Add with label
        call fig%add_3d_plot(x, y, z, label="3D curve")
        
        ! Assert
        if (fig%plots(1)%label /= "3D curve") then
            error stop "Label not set correctly"
        end if
        
    end subroutine test_add_3d_plot_with_label
    
    subroutine test_add_3d_plot_with_all_options()
        !! Test add_3d_plot with all pyplot-fortran compatible options
        type(figure_t) :: fig
        real(wp), dimension(4) :: x = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
        real(wp), dimension(4) :: y = [0.0_wp, 1.0_wp, 0.0_wp, -1.0_wp]
        real(wp), dimension(4) :: z = [0.0_wp, 0.5_wp, 1.0_wp, 0.5_wp]
        
        call fig%initialize(640, 480)
        
        ! Add with all options matching pyplot-fortran signature
        call fig%add_3d_plot(x, y, z, label="test", linestyle="--", &
                            markersize=8.0_wp, linewidth=2.0_wp)
        
        ! Assert
        if (fig%plots(1)%linestyle /= "--") then
            error stop "Linestyle not set correctly"
        end if
        
        ! Note: markersize and linewidth stored in backend-specific way
        
    end subroutine test_add_3d_plot_with_all_options
    
    subroutine test_figure_detects_3d()
        !! Test that figure automatically detects 3D plots
        type(figure_t) :: fig
        real(wp), dimension(2) :: x = [0.0_wp, 1.0_wp]
        real(wp), dimension(2) :: y = [0.0_wp, 1.0_wp]
        real(wp), dimension(2) :: z = [0.0_wp, 1.0_wp]
        
        call fig%initialize(640, 480)
        
        ! Initially no 3D plots
        if (fig%has_3d_plots()) then
            error stop "Empty figure should not have 3D plots"
        end if
        
        ! Add regular 2D plot
        call fig%add_plot(x, y)
        if (fig%has_3d_plots()) then
            error stop "2D plot should not trigger 3D mode"
        end if
        
        ! Add 3D plot
        call fig%add_3d_plot(x, y, z)
        if (.not. fig%has_3d_plots()) then
            error stop "Figure should detect 3D plots"
        end if
        
    end subroutine test_figure_detects_3d

end program test_add_3d_plot_api
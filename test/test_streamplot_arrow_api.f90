program test_streamplot_arrow_api
    !! RED Phase: Failing tests for streamplot arrow API extensions (Issue #22)
    !! Tests arrowsize and arrowstyle parameter handling in all API layers
    use fortplot
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    
    call test_figure_streamplot_arrow_parameters()
    call test_global_streamplot_arrow_parameters()
    call test_python_interface_arrow_parameters()
    call test_arrow_parameter_defaults()
    call test_arrow_parameter_validation()
    
    print *, "All streamplot arrow API tests passed!"
    
contains
    
    subroutine test_figure_streamplot_arrow_parameters()
        !! Given: Figure streamplot method
        !! When: Called with arrowsize and arrowstyle parameters
        !! Then: Should accept parameters without error
        type(figure_t) :: fig
        real(real64), dimension(3) :: x = [0.0, 1.0, 2.0]
        real(real64), dimension(3) :: y = [0.0, 1.0, 2.0]
        real(real64), dimension(3,3) :: u, v
        
        u = 1.0_real64
        v = 0.0_real64
        
        call fig%initialize(800, 600)
        
        ! Test will fail until figure%streamplot signature is extended
        call fig%streamplot(x, y, u, v, arrowsize=1.5_real64, arrowstyle='->')
        
        if (fig%has_error) then
            error stop "Figure streamplot does not accept arrow parameters"
        end if
        
        ! Should have generated arrows
        if (.not. allocated(fig%arrow_data)) then
            error stop "Figure streamplot with arrow parameters did not generate arrows"
        end if
    end subroutine
    
    subroutine test_global_streamplot_arrow_parameters()
        !! Given: Global streamplot function
        !! When: Called with arrowsize and arrowstyle parameters  
        !! Then: Should accept parameters and create arrows
        real(real64), dimension(3) :: x = [0.0, 1.0, 2.0]
        real(real64), dimension(3) :: y = [0.0, 1.0, 2.0]
        real(real64), dimension(3,3) :: u, v
        type(figure_t), pointer :: global_fig
        
        u = 1.0_real64
        v = 0.0_real64
        
        ! Initialize global figure
        call figure(800, 600)
        global_fig => get_global_figure()
        
        ! Test will fail until global streamplot signature is extended
        call streamplot(x, y, u, v, arrowsize=2.0_real64, arrowstyle='->')
        
        ! Check that global figure received arrow parameters
        if (.not. allocated(global_fig%arrow_data)) then
            error stop "Global streamplot with arrow parameters did not generate arrows"
        end if
        
        if (global_fig%arrow_data(1)%size /= 2.0_real64) then
            error stop "Global streamplot arrow parameters not properly passed"
        end if
    end subroutine
    
    subroutine test_python_interface_arrow_parameters()
        !! Given: Python interface streamplot function
        !! When: Called with arrowsize and arrowstyle parameters
        !! Then: Should accept parameters via C interface
        use fortplot_python_interface, only: python_streamplot => streamplot
        real(real64), dimension(3) :: x_grid = [0.0, 1.0, 2.0]
        real(real64), dimension(3) :: y_grid = [0.0, 1.0, 2.0]
        real(real64), dimension(3,3) :: u_grid, v_grid
        real(real64) :: test_arrowsize, test_density
        integer :: i, j
        type(figure_t), pointer :: global_fig
        
        ! Create uniform flow field
        do j = 1, 3
            do i = 1, 3
                u_grid(i,j) = 1.0_real64
                v_grid(i,j) = 0.0_real64
            end do
        end do
        
        test_density = 1.0_real64
        test_arrowsize = 1.2_real64
        
        ! Initialize global figure
        call figure(800, 600)
        global_fig => get_global_figure()
        
        ! Test will fail until Python interface accepts arrow parameters
        call python_streamplot(x_grid, y_grid, u_grid, v_grid, 3, 3, test_density, test_arrowsize)
        
        ! Should have created arrows in global figure
        if (.not. allocated(global_fig%arrow_data)) then
            error stop "Python interface streamplot did not generate arrows"
        end if
        
        if (abs(global_fig%arrow_data(1)%size - test_arrowsize) > 1e-10) then
            error stop "Python interface arrow parameters not properly handled"
        end if
    end subroutine
    
    subroutine test_arrow_parameter_defaults()
        !! Given: Streamplot called without arrow parameters
        !! When: Default values should be used
        !! Then: Should generate arrows with matplotlib-compatible defaults
        type(figure_t) :: fig
        real(real64), dimension(3) :: x = [0.0, 1.0, 2.0]
        real(real64), dimension(3) :: y = [0.0, 1.0, 2.0]
        real(real64), dimension(3,3) :: u, v
        real(real64) :: default_arrowsize = 1.0_real64
        
        u = 1.0_real64
        v = 0.0_real64
        
        call fig%initialize(800, 600)
        
        ! Call without arrow parameters - should use defaults
        call fig%streamplot(x, y, u, v)
        
        ! Test will fail until default arrow behavior is implemented
        if (.not. allocated(fig%arrow_data)) then
            error stop "Default arrow generation not implemented"
        end if
        
        if (size(fig%arrow_data) == 0) then
            error stop "No arrows generated with default parameters"
        end if
        
        ! Should use default arrowsize = 1.0 (matplotlib compatible)
        if (abs(fig%arrow_data(1)%size - default_arrowsize) > 1e-10) then
            error stop "Default arrowsize not matplotlib-compatible"
        end if
    end subroutine
    
    subroutine test_arrow_parameter_validation()
        !! Given: Invalid arrow parameters
        !! When: Streamplot is called
        !! Then: Should validate parameters and set error flags appropriately
        type(figure_t) :: fig
        real(real64), dimension(3) :: x = [0.0, 1.0, 2.0]
        real(real64), dimension(3) :: y = [0.0, 1.0, 2.0]
        real(real64), dimension(3,3) :: u, v
        
        u = 1.0_real64
        v = 0.0_real64
        
        ! Test negative arrowsize
        call fig%initialize(800, 600)
        call fig%streamplot(x, y, u, v, arrowsize=-1.0_real64)
        
        ! Test will fail until parameter validation is implemented
        if (.not. fig%has_error) then
            error stop "Negative arrowsize not properly validated"
        end if
        
        ! Test zero arrowsize (should be valid - no arrows)
        call fig%initialize(800, 600)
        call fig%streamplot(x, y, u, v, arrowsize=0.0_real64)
        
        if (fig%has_error) then
            error stop "Zero arrowsize should be valid (no arrows case)"
        end if
        
        ! Should generate no arrows for zero size
        if (allocated(fig%arrow_data) .and. size(fig%arrow_data) > 0) then
            error stop "Zero arrowsize should generate no arrows"
        end if
        
        ! Test invalid arrowstyle
        call fig%initialize(800, 600)
        call fig%streamplot(x, y, u, v, arrowstyle='invalid')
        
        if (.not. fig%has_error) then
            error stop "Invalid arrowstyle not properly validated"
        end if
    end subroutine
    
end program test_streamplot_arrow_api
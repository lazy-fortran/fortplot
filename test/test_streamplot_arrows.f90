program test_streamplot_arrows
    !! RED Phase: Failing tests for streamplot arrow functionality (Issue #22)
    !! Tests arrow data structure, placement algorithm, and parameter handling
    use fortplot
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    
    call test_arrow_data_structure()
    call test_arrow_placement_algorithm()
    call test_arrow_parameter_validation()
    call test_arrow_size_scaling()
    call test_arrow_direction_calculation()
    
    print *, "All streamplot arrow tests passed!"
    
contains
    
    subroutine test_arrow_data_structure()
        !! Given: Arrow data structure should contain position, direction, size, style
        !! When: Arrow data is created for streamplot
        !! Then: Structure should have all required fields with proper types
        type(figure_t) :: fig
        real(real64), dimension(3) :: x = [0.0, 1.0, 2.0]
        real(real64), dimension(3) :: y = [0.0, 1.0, 2.0]
        real(real64), dimension(3,3) :: u, v
        integer :: i, j
        
        ! Simple circular flow field
        do j = 1, 3
            do i = 1, 3
                u(i,j) = -real(j-2, real64)  ! -y component for circular flow
                v(i,j) = real(i-2, real64)   ! x component for circular flow
            end do
        end do
        
        call fig%initialize(800, 600)
        call fig%streamplot(x, y, u, v, arrowsize=1.0_real64, arrowstyle='->')
        
        ! Test will fail until arrow data structure is implemented
        if (.not. allocated(fig%arrow_data)) then
            error stop "Arrow data structure not implemented"
        end if
        
        if (size(fig%arrow_data) == 0) then
            error stop "No arrows generated for streamplot"
        end if
        
        ! Check first arrow has required fields
        if (fig%arrow_data(1)%size <= 0.0) then
            error stop "Arrow size not properly set"
        end if
    end subroutine
    
    subroutine test_arrow_placement_algorithm()
        !! Given: Streamlines exist with flow field
        !! When: Arrows are placed along streamlines
        !! Then: Arrows should be positioned at regular intervals
        type(figure_t) :: fig
        real(real64), dimension(5) :: x = [0.0, 1.0, 2.0, 3.0, 4.0]
        real(real64), dimension(5) :: y = [0.0, 1.0, 2.0, 3.0, 4.0]
        real(real64), dimension(5,5) :: u, v
        integer :: i, j, expected_arrows, actual_arrows
        real(real64) :: density, avg_spacing
        
        ! Linear flow field (rightward flow)
        do j = 1, 5
            do i = 1, 5
                u(i,j) = 1.0_real64
                v(i,j) = 0.0_real64
            end do
        end do
        
        density = 1.0_real64
        call fig%initialize(800, 600)
        call fig%streamplot(x, y, u, v, density=density, arrowsize=1.0_real64)
        
        ! Test will fail until arrow placement algorithm is implemented
        if (.not. allocated(fig%arrow_data)) then
            error stop "Arrow placement algorithm not implemented"
        end if
        
        actual_arrows = size(fig%arrow_data)
        if (actual_arrows == 0) then
            error stop "No arrows placed by placement algorithm"
        end if
        
        ! Arrows should be spaced appropriately based on density
        ! This will fail until proper spacing calculation is implemented
        expected_arrows = int(density * 10)  ! Rough estimate
        if (actual_arrows < expected_arrows / 2) then
            error stop "Too few arrows placed - placement algorithm incorrect"
        end if
    end subroutine
    
    subroutine test_arrow_parameter_validation()
        !! Given: Streamplot with arrow parameters
        !! When: arrowsize and arrowstyle parameters are provided
        !! Then: Parameters should be validated and stored properly
        type(figure_t) :: fig
        real(real64), dimension(3) :: x = [0.0, 1.0, 2.0]
        real(real64), dimension(3) :: y = [0.0, 1.0, 2.0]
        real(real64), dimension(3,3) :: u, v
        real(real64) :: test_arrowsize
        character(len=2) :: test_arrowstyle
        
        u = 1.0_real64
        v = 0.0_real64
        test_arrowsize = 2.5_real64
        test_arrowstyle = '->'
        
        call fig%initialize(800, 600)
        call fig%streamplot(x, y, u, v, arrowsize=test_arrowsize, arrowstyle=test_arrowstyle)
        
        ! Test will fail until parameter handling is implemented
        if (fig%has_error) then
            error stop "Arrow parameters not accepted - API not extended"
        end if
        
        if (.not. allocated(fig%arrow_data)) then
            error stop "Arrow parameter handling not implemented"
        end if
        
        ! Check that parameters are properly stored/used
        if (abs(fig%arrow_data(1)%size - test_arrowsize) > 1e-10) then
            error stop "Arrow size parameter not properly handled"
        end if
    end subroutine
    
    subroutine test_arrow_size_scaling()
        !! Given: Different arrowsize values
        !! When: Arrows are generated
        !! Then: Arrow sizes should scale proportionally
        type(figure_t) :: fig1, fig2
        real(real64), dimension(3) :: x = [0.0, 1.0, 2.0]
        real(real64), dimension(3) :: y = [0.0, 1.0, 2.0]
        real(real64), dimension(3,3) :: u, v
        real(real64) :: size1, size2, ratio
        
        u = 1.0_real64
        v = 0.0_real64
        
        ! Test with arrowsize = 1.0
        call fig1%initialize(800, 600)
        call fig1%streamplot(x, y, u, v, arrowsize=1.0_real64)
        
        ! Test with arrowsize = 2.0
        call fig2%initialize(800, 600)
        call fig2%streamplot(x, y, u, v, arrowsize=2.0_real64)
        
        ! Test will fail until size scaling is implemented
        if (.not. allocated(fig1%arrow_data) .or. .not. allocated(fig2%arrow_data)) then
            error stop "Arrow size scaling not implemented"
        end if
        
        size1 = fig1%arrow_data(1)%size
        size2 = fig2%arrow_data(1)%size
        ratio = size2 / size1
        
        if (abs(ratio - 2.0) > 0.1) then
            error stop "Arrow size scaling not proportional"
        end if
    end subroutine
    
    subroutine test_arrow_direction_calculation()
        !! Given: Flow field with known directions
        !! When: Arrows are placed
        !! Then: Arrow directions should match local flow field
        type(figure_t) :: fig
        real(real64), dimension(3) :: x = [0.0, 1.0, 2.0]
        real(real64), dimension(3) :: y = [0.0, 1.0, 2.0]
        real(real64), dimension(3,3) :: u, v
        real(real64) :: expected_angle, actual_angle
        integer :: i, j
        
        ! Create a simple rotation field (circular flow)
        do j = 1, 3
            do i = 1, 3
                u(i,j) = -real(j-2, real64)  ! -y for rotation
                v(i,j) = real(i-2, real64)   ! x for rotation
            end do
        end do
        
        call fig%initialize(800, 600)
        call fig%streamplot(x, y, u, v, arrowsize=1.0_real64)
        
        ! Test will fail until direction calculation is implemented
        if (.not. allocated(fig%arrow_data)) then
            error stop "Arrow direction calculation not implemented"
        end if
        
        if (size(fig%arrow_data) == 0) then
            error stop "No arrows to test direction calculation"
        end if
        
        ! Check that arrow direction vectors are normalized and non-zero
        if (abs(fig%arrow_data(1)%dx) < 1e-10 .and. abs(fig%arrow_data(1)%dy) < 1e-10) then
            error stop "Arrow direction vector is zero - calculation failed"
        end if
        
        ! Direction vector should be normalized
        actual_angle = sqrt(fig%arrow_data(1)%dx**2 + fig%arrow_data(1)%dy**2)
        if (abs(actual_angle - 1.0) > 0.1) then
            error stop "Arrow direction vector not properly normalized"
        end if
    end subroutine
    
end program test_streamplot_arrows
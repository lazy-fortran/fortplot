program test_streamplot_arrow_rendering
    !! RED Phase: Failing tests for streamplot arrow rendering (Issue #22)
    !! Tests arrow rendering across PNG, PDF, and ASCII backends
    use fortplot
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    
    call test_png_arrow_rendering()
    call test_pdf_arrow_rendering()
    call test_ascii_arrow_rendering()
    call test_backend_arrow_consistency()
    call test_arrow_vector_calculations()
    
    print *, "All streamplot arrow rendering tests passed!"
    
contains
    
    subroutine test_png_arrow_rendering()
        !! Given: PNG backend with streamplot arrows
        !! When: Arrow heads are rendered
        !! Then: Should draw triangular arrow shapes at correct positions
        type(figure_t) :: fig
        real(real64), dimension(5) :: x = [0.0, 1.0, 2.0, 3.0, 4.0]
        real(real64), dimension(5) :: y = [0.0, 1.0, 2.0, 3.0, 4.0]
        real(real64), dimension(5,5) :: u, v
        integer :: i, j
        
        ! Simple rightward flow
        do j = 1, 5
            do i = 1, 5
                u(i,j) = 1.0_real64
                v(i,j) = 0.0_real64
            end do
        end do
        
        call fig%initialize(800, 600)
        call fig%streamplot(x, y, u, v, arrowsize=1.5_real64, arrowstyle='->')
        call fig%savefig('test_png_arrows.png')
        
        ! Test will fail until PNG backend implements arrow rendering
        if (fig%has_error) then
            error stop "PNG backend cannot render arrows"
        end if
        
        ! Check that backend received arrow data for rendering
        if (.not. allocated(fig%arrow_data)) then
            error stop "PNG backend did not receive arrow data"
        end if
        
        ! Check that arrows were processed by backend
        if (.not. fig%backend%has_rendered_arrows) then
            error stop "PNG backend did not render arrows"
        end if
    end subroutine
    
    subroutine test_pdf_arrow_rendering()
        !! Given: PDF backend with streamplot arrows
        !! When: Arrow heads are rendered
        !! Then: Should draw vector arrows using PDF primitives
        type(figure_t) :: fig
        real(real64), dimension(4) :: x = [0.0, 1.0, 2.0, 3.0]
        real(real64), dimension(4) :: y = [0.0, 1.0, 2.0, 3.0]
        real(real64), dimension(4,4) :: u, v
        integer :: i, j
        
        ! Circular flow field
        do j = 1, 4
            do i = 1, 4
                u(i,j) = -real(j-2.5, real64)
                v(i,j) = real(i-2.5, real64)
            end do
        end do
        
        call fig%initialize(800, 600)
        call fig%streamplot(x, y, u, v, arrowsize=2.0_real64, arrowstyle='->')
        call fig%savefig('test_pdf_arrows.pdf')
        
        ! Test will fail until PDF backend implements arrow rendering
        if (fig%has_error) then
            error stop "PDF backend cannot render arrows"
        end if
        
        if (.not. allocated(fig%arrow_data)) then
            error stop "PDF backend did not receive arrow data"
        end if
        
        if (.not. fig%backend%has_rendered_arrows) then
            error stop "PDF backend did not render arrows"
        end if
        
        ! PDF should handle vector graphics arrows differently than PNG
        if (.not. fig%backend%uses_vector_arrows) then
            error stop "PDF backend should use vector arrow rendering"
        end if
    end subroutine
    
    subroutine test_ascii_arrow_rendering()
        !! Given: ASCII backend with streamplot arrows
        !! When: Arrow directions are rendered
        !! Then: Should use Unicode arrow characters for direction indication
        type(figure_t) :: fig
        real(real64), dimension(6) :: x = [0.0, 1.0, 2.0, 3.0, 4.0, 5.0]
        real(real64), dimension(6) :: y = [0.0, 1.0, 2.0, 3.0, 4.0, 5.0]
        real(real64), dimension(6,6) :: u, v
        integer :: i, j
        character(len=1000) :: ascii_output
        
        ! Create a known flow pattern
        do j = 1, 6
            do i = 1, 6
                u(i,j) = 1.0_real64  ! Rightward
                v(i,j) = 0.0_real64
            end do
        end do
        
        call fig%initialize(80, 24)  ! ASCII dimensions
        call fig%streamplot(x, y, u, v, arrowsize=1.0_real64, arrowstyle='->')
        call fig%show()  ! ASCII output
        
        ! Test will fail until ASCII backend implements arrow characters
        if (fig%has_error) then
            error stop "ASCII backend cannot render arrows"
        end if
        
        if (.not. allocated(fig%arrow_data)) then
            error stop "ASCII backend did not receive arrow data"
        end if
        
        ! ASCII should use directional characters
        ascii_output = fig%backend%get_ascii_output()
        if (index(ascii_output, '→') == 0 .and. index(ascii_output, '>') == 0) then
            error stop "ASCII backend did not render rightward arrow characters"
        end if
    end subroutine
    
    subroutine test_backend_arrow_consistency()
        !! Given: Same streamplot data across different backends
        !! When: Arrows are rendered
        !! Then: Arrow positions and directions should be consistent
        type(figure_t) :: fig_png, fig_pdf, fig_ascii
        real(real64), dimension(4) :: x = [0.0, 1.0, 2.0, 3.0]
        real(real64), dimension(4) :: y = [0.0, 1.0, 2.0, 3.0]
        real(real64), dimension(4,4) :: u, v
        integer :: i, j
        
        ! Simple upward flow
        do j = 1, 4
            do i = 1, 4
                u(i,j) = 0.0_real64
                v(i,j) = 1.0_real64
            end do
        end do
        
        ! Test PNG backend
        call fig_png%initialize(800, 600)
        call fig_png%streamplot(x, y, u, v, arrowsize=1.0_real64)
        call fig_png%savefig('test_consistency.png')
        
        ! Test PDF backend
        call fig_pdf%initialize(800, 600)
        call fig_pdf%streamplot(x, y, u, v, arrowsize=1.0_real64)
        call fig_pdf%savefig('test_consistency.pdf')
        
        ! Test ASCII backend
        call fig_ascii%initialize(80, 24)
        call fig_ascii%streamplot(x, y, u, v, arrowsize=1.0_real64)
        
        ! Test will fail until arrow data consistency is implemented
        if (.not. allocated(fig_png%arrow_data) .or. &
            .not. allocated(fig_pdf%arrow_data) .or. &
            .not. allocated(fig_ascii%arrow_data)) then
            error stop "Not all backends generated arrow data"
        end if
        
        ! Arrow positions should be identical across backends
        if (size(fig_png%arrow_data) /= size(fig_pdf%arrow_data)) then
            error stop "Arrow count inconsistent between PNG and PDF backends"
        end if
        
        ! Arrow directions should be consistent (upward flow = dy > 0)
        if (fig_png%arrow_data(1)%dy <= 0.0) then
            error stop "PNG backend arrow direction incorrect for upward flow"
        end if
        
        if (abs(fig_png%arrow_data(1)%dy - fig_pdf%arrow_data(1)%dy) > 1e-10) then
            error stop "Arrow directions inconsistent between backends"
        end if
    end subroutine
    
    subroutine test_arrow_vector_calculations()
        !! Given: Flow field with known directions
        !! When: Arrow head vectors are calculated  
        !! Then: Arrow geometry should be mathematically correct
        type(figure_t) :: fig
        real(real64), dimension(3) :: x = [0.0, 1.0, 2.0]
        real(real64), dimension(3) :: y = [0.0, 1.0, 2.0]
        real(real64), dimension(3,3) :: u, v
        real(real64) :: arrow_angle, expected_angle, tolerance
        integer :: i, j
        
        ! Diagonal flow (45 degrees)
        do j = 1, 3
            do i = 1, 3
                u(i,j) = 1.0_real64  ! Equal u and v gives 45 degree angle
                v(i,j) = 1.0_real64
            end do
        end do
        
        call fig%initialize(800, 600)
        call fig%streamplot(x, y, u, v, arrowsize=1.0_real64)
        
        ! Test will fail until arrow vector calculations are implemented
        if (.not. allocated(fig%arrow_data)) then
            error stop "Arrow vector calculations not implemented"
        end if
        
        if (size(fig%arrow_data) == 0) then
            error stop "No arrows generated for vector calculation test"
        end if
        
        ! Check arrow angle calculation for diagonal flow
        arrow_angle = atan2(fig%arrow_data(1)%dy, fig%arrow_data(1)%dx)
        expected_angle = atan2(1.0_real64, 1.0_real64)  ! 45 degrees
        tolerance = 0.1_real64  ! 0.1 radian tolerance
        
        if (abs(arrow_angle - expected_angle) > tolerance) then
            error stop "Arrow vector angle calculation incorrect"
        end if
        
        ! Arrow head should have proper triangular geometry
        if (.not. fig%backend%has_triangular_arrows) then
            error stop "Arrow heads not rendered as proper triangular shapes"
        end if
    end subroutine
    
end program test_streamplot_arrow_rendering
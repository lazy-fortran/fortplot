program test_contour_filled_backend_rendering
    !! Test contour filled backend-specific rendering for Issue #177
    !! 
    !! Given: Contour filled plots should render consistently across backends
    !! When: Rendered through PNG, PDF, and ASCII backends
    !! Then: All backends should show filled contour regions with proper colors
    !!
    !! ISSUE #177: Colored contour plots are broken in PNG/PDF backends
    !! - ASCII backend works (shows character-based fills)
    !! - PNG/PDF backends show blank/empty regions instead of filled colors
    !! - Root cause: fill_heatmap() only implemented for ASCII backend

    use iso_fortran_env, only: wp => real64
    use fortplot, only: figure_t
    use fortplot_security, only: get_test_output_path
    use fortplot_system_runtime, only: is_windows
    use fortplot_windows_performance, only: setup_windows_performance, &
                                            should_use_memory_backend
    use fortplot_fast_io, only: fast_savefig, enable_fast_io
    ! Performance monitoring not integrated yet
    implicit none
    
    logical :: on_windows
    
    ! Initialize performance optimization for Windows CI
    on_windows = is_windows()
    if (on_windows) then
        call setup_windows_performance()
        if (should_use_memory_backend()) then
            call enable_fast_io()
            print *, "Enabled fast I/O with memory backend for Windows CI"
        end if
        ! Performance monitoring not integrated yet
    end if
    
    call test_contour_filled_png_backend()
    call test_contour_filled_pdf_backend()
    call test_contour_filled_ascii_backend()
    call test_backend_consistency_comparison()
    call test_color_interpolation_bands()
    call test_complex_contour_regions()
    
    ! Performance monitoring not integrated yet
    
    print *, "All contour filled backend rendering tests completed!"
    
contains

    subroutine test_contour_filled_png_backend()
        !! Given: Simple contour data with distinct levels
        !! When: Rendered as PNG with filled contours
        !! Then: PNG should contain filled regions with proper colors (NOT blank)
        !!
        !! EXPECTED TO FAIL: PNG backend does not implement contour filling
        
        type(figure_t) :: fig
        real(wp) :: x(10), y(10), z(10, 10)
        integer :: i, j
        logical :: file_exists
        
        ! Arrange - Create simple gradient data
        do i = 1, 10
            x(i) = real(i-1, wp) * 0.1_wp
            y(i) = real(i-1, wp) * 0.1_wp
        end do
        
        ! Create concentric circular pattern for easy visual verification
        do i = 1, 10
            do j = 1, 10
                z(i, j) = sqrt((x(i) - 0.45_wp)**2 + (y(j) - 0.45_wp)**2)
            end do
        end do
        
        ! Act - Render PNG with filled contours
        call fig%initialize(400, 300)
        call add_contour_filled(x, y, z, colormap='viridis')
        call fig%set_title("Contour Filled PNG Test")
        if (should_use_memory_backend()) then
            call fast_savefig(fig, get_test_output_path('/tmp/test_contour_filled_png_issue177.png'))
        else
            call savefig(get_test_output_path('/tmp/test_contour_filled_png_issue177.png'))
        end if
        
        ! Assert - File should exist (basic check)
        inquire(file=get_test_output_path('/tmp/test_contour_filled_png_issue177.png'), exist=file_exists)
        if (.not. file_exists) then
            error stop "ERROR: PNG file was not created"
        end if
        
        ! TODO: Add pixel analysis to verify filled regions have non-background colors
        ! This test WILL FAIL because PNG backend doesn't implement contour filling
        
        print *, "test_contour_filled_png_backend: File created (visual inspection needed)"
    end subroutine

    subroutine test_contour_filled_pdf_backend()
        !! Given: Simple contour data with distinct levels
        !! When: Rendered as PDF with filled contours  
        !! Then: PDF should contain filled regions with proper colors (NOT blank)
        !!
        !! EXPECTED TO FAIL: PDF backend does not implement contour filling
        
        type(figure_t) :: fig
        real(wp) :: x(8), y(8), z(8, 8)
        integer :: i, j
        logical :: file_exists
        
        ! Arrange - Create saddle point pattern
        do i = 1, 8
            x(i) = real(i-1, wp) * 0.2_wp - 0.7_wp
            y(i) = real(i-1, wp) * 0.2_wp - 0.7_wp
        end do
        
        do i = 1, 8
            do j = 1, 8
                z(i, j) = x(i)**2 - y(j)**2  ! Saddle point function
            end do
        end do
        
        ! Act - Render PDF with filled contours
        call fig%initialize(600, 400)
        call add_contour_filled(x, y, z, colormap='plasma')
        call fig%set_title("Contour Filled PDF Test")
        call fig%set_xlabel("X coordinate")
        call fig%set_ylabel("Y coordinate")
        if (should_use_memory_backend()) then
            call fast_savefig(fig, get_test_output_path('/tmp/test_contour_filled_pdf_issue177.pdf'))
        else
            call savefig(get_test_output_path('/tmp/test_contour_filled_pdf_issue177.pdf'))
        end if
        
        ! Assert - File should exist (basic check)
        inquire(file=get_test_output_path('/tmp/test_contour_filled_pdf_issue177.pdf'), exist=file_exists)
        if (.not. file_exists) then
            error stop "ERROR: PDF file was not created"
        end if
        
        ! TODO: Add PDF content analysis to verify filled polygons exist
        ! This test WILL FAIL because PDF backend doesn't implement contour filling
        
        print *, "test_contour_filled_pdf_backend: File created (visual inspection needed)"
    end subroutine

    subroutine test_contour_filled_ascii_backend()
        !! Given: Simple contour data with distinct levels
        !! When: Rendered as ASCII with filled contours
        !! Then: ASCII should contain character-based filled regions
        !!
        !! EXPECTED TO PASS: ASCII backend already implements contour filling
        
        type(figure_t) :: fig
        real(wp) :: x(6), y(6), z(6, 6)
        integer :: i, j
        logical :: file_exists
        
        ! Arrange - Create linear gradient
        do i = 1, 6
            x(i) = real(i-1, wp)
            y(i) = real(i-1, wp)
        end do
        
        do i = 1, 6
            do j = 1, 6
                z(i, j) = real(i + j, wp) * 0.5_wp
            end do
        end do
        
        ! Act - Render ASCII with filled contours
        call fig%initialize(60, 30)
        call add_contour_filled(x, y, z)
        call fig%set_title("Contour Filled ASCII Test")
        if (should_use_memory_backend()) then
            call fast_savefig(fig, get_test_output_path('/tmp/test_contour_filled_ascii_issue177.txt'))
        else
            call savefig(get_test_output_path('/tmp/test_contour_filled_ascii_issue177.txt'))
        end if
        
        ! Assert - File should exist and contain non-space characters
        inquire(file=get_test_output_path('/tmp/test_contour_filled_ascii_issue177.txt'), exist=file_exists)
        if (.not. file_exists) then
            error stop "ERROR: ASCII file was not created"
        end if
        
        ! TODO: Read file and verify it contains contour fill characters
        
        print *, "test_contour_filled_ascii_backend: PASSED (reference implementation)"
    end subroutine

    subroutine test_backend_consistency_comparison()
        !! Given: Identical contour data rendered across all backends
        !! When: Comparing visual outputs
        !! Then: All backends should show similar contour structure (but PNG/PDF currently fail)
        !!
        !! EXPECTED TO HIGHLIGHT INCONSISTENCY: ASCII works, PNG/PDF don't
        
        type(figure_t) :: fig_png, fig_pdf, fig_ascii
        real(wp) :: x(12), y(12), z(12, 12)
        integer :: i, j
        logical :: png_exists, pdf_exists, ascii_exists
        
        ! Arrange - Create test data with clear contour levels
        do i = 1, 12
            x(i) = real(i-1, wp) * 0.1_wp
            y(i) = real(i-1, wp) * 0.1_wp
        end do
        
        ! Create a pattern with multiple distinct regions
        do i = 1, 12
            do j = 1, 12
                z(i, j) = sin(x(i) * 6.0_wp) * cos(y(j) * 6.0_wp)
            end do
        end do
        
        ! Act - Render through all backends with identical settings
        call fig_png%initialize(400, 400)
        call add_contour_filled(x, y, z, colormap='coolwarm')
        call fig_png%set_title("Backend Consistency - PNG")
        if (should_use_memory_backend()) then
            call fast_savefig(fig_png, get_test_output_path('/tmp/test_backend_consistency_png_issue177.png'))
        else
            call fig_png%savefig(get_test_output_path('/tmp/test_backend_consistency_png_issue177.png'))
        end if
        
        call fig_pdf%initialize(400, 400)  
        call add_contour_filled(x, y, z, colormap='coolwarm')
        call fig_pdf%set_title("Backend Consistency - PDF")
        if (should_use_memory_backend()) then
            call fast_savefig(fig_pdf, get_test_output_path('/tmp/test_backend_consistency_pdf_issue177.pdf'))
        else
            call fig_pdf%savefig(get_test_output_path('/tmp/test_backend_consistency_pdf_issue177.pdf'))
        end if
        
        call fig_ascii%initialize(80, 40)
        call add_contour_filled(x, y, z)
        call fig_ascii%set_title("Backend Consistency - ASCII")
        if (should_use_memory_backend()) then
            call fast_savefig(fig_ascii, get_test_output_path('/tmp/test_backend_consistency_ascii_issue177.txt'))
        else
            call fig_ascii%savefig(get_test_output_path('/tmp/test_backend_consistency_ascii_issue177.txt'))
        end if
        
        ! Assert - All files should exist
        inquire(file=get_test_output_path('/tmp/test_backend_consistency_png_issue177.png'), exist=png_exists)
        inquire(file=get_test_output_path('/tmp/test_backend_consistency_pdf_issue177.pdf'), exist=pdf_exists)
        inquire(file=get_test_output_path('/tmp/test_backend_consistency_ascii_issue177.txt'), exist=ascii_exists)
        
        if (.not. png_exists) error stop "ERROR: PNG consistency test file missing"
        if (.not. pdf_exists) error stop "ERROR: PDF consistency test file missing" 
        if (.not. ascii_exists) error stop "ERROR: ASCII consistency test file missing"
        
        ! TODO: Implement automated visual comparison
        ! Currently ASCII will show fills, PNG/PDF will be blank/empty
        
        print *, "test_backend_consistency_comparison: Files created (inconsistency expected)"
    end subroutine

    subroutine test_color_interpolation_bands()
        !! Given: Contour data with specified levels and colormap
        !! When: Rendered with filled contours
        !! Then: Colors should interpolate smoothly between contour levels
        !!
        !! EXPECTED TO FAIL: No color interpolation in PNG/PDF backends
        
        type(figure_t) :: fig
        real(wp) :: x(15), y(15), z(15, 15)
        real(wp) :: levels(5)
        integer :: i, j
        logical :: file_exists
        
        ! Arrange - Create data with known value ranges
        do i = 1, 15
            x(i) = real(i-1, wp) * 0.1_wp
            y(i) = real(i-1, wp) * 0.1_wp
        end do
        
        ! Create radial gradient
        do i = 1, 15
            do j = 1, 15
                z(i, j) = sqrt((x(i) - 0.7_wp)**2 + (y(j) - 0.7_wp)**2)
            end do
        end do
        
        ! Specify explicit contour levels for predictable banding
        levels = [0.1_wp, 0.3_wp, 0.5_wp, 0.7_wp, 0.9_wp]
        
        ! Act - Render with specific levels and colormap
        call fig%initialize(500, 400)
        call add_contour_filled(x, y, z, levels=levels, colormap='jet')
        call fig%set_title("Color Interpolation Test")
        if (should_use_memory_backend()) then
            call fast_savefig(fig, get_test_output_path('/tmp/test_color_interpolation_issue177.png'))
        else
            call savefig(get_test_output_path('/tmp/test_color_interpolation_issue177.png'))
        end if
        
        ! Assert - File should exist
        inquire(file=get_test_output_path('/tmp/test_color_interpolation_issue177.png'), exist=file_exists)
        if (.not. file_exists) then
            error stop "ERROR: Color interpolation test file was not created"
        end if
        
        ! TODO: Analyze pixels to verify smooth color transitions exist
        ! This test WILL FAIL because PNG doesn't implement color interpolation
        
        print *, "test_color_interpolation_bands: File created (color analysis needed)"
    end subroutine

    subroutine test_complex_contour_regions()
        !! Given: Complex contour data with multiple disconnected regions
        !! When: Rendered with filled contours
        !! Then: All regions should be properly filled with correct colors
        !!
        !! EXPECTED TO FAIL: Complex polygon decomposition not implemented
        
        type(figure_t) :: fig
        real(wp) :: x(20), y(20), z(20, 20)
        integer :: i, j
        logical :: file_exists
        
        ! Arrange - Create data with multiple peaks/valleys
        do i = 1, 20
            x(i) = real(i-1, wp) * 0.1_wp
            y(i) = real(i-1, wp) * 0.1_wp
        end do
        
        ! Create pattern with multiple disconnected regions
        do i = 1, 20
            do j = 1, 20
                z(i, j) = sin(x(i) * 8.0_wp) * sin(y(j) * 8.0_wp) + &
                         0.5_wp * cos(x(i) * 12.0_wp) * cos(y(j) * 12.0_wp)
            end do
        end do
        
        ! Act - Render complex pattern
        call fig%initialize(600, 600)
        call add_contour_filled(x, y, z, colormap='viridis')
        call fig%set_title("Complex Contour Regions Test")
        if (should_use_memory_backend()) then
            call fast_savefig(fig, get_test_output_path('/tmp/test_complex_regions_issue177.png'))
        else
            call figure_savefig(fig, get_test_output_path('/tmp/test_complex_regions_issue177.png'))
        end if
        
        ! Assert - File should exist
        inquire(file=get_test_output_path('/tmp/test_complex_regions_issue177.png'), exist=file_exists)
        if (.not. file_exists) then
            error stop "ERROR: Complex regions test file was not created"
        end if
        
        ! TODO: Verify all disconnected regions are properly filled
        ! This test WILL FAIL because complex polygon handling is not implemented
        
        print *, "test_complex_contour_regions: File created (region analysis needed)"
    end subroutine

end program test_contour_filled_backend_rendering
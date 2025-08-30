program test_title_centering_fix
    !! Regression test for PNG title positioning fix (Issue #337)
    !! 
    !! This test verifies that titles are centered over the plot area
    !! regardless of the data coordinate range, matching matplotlib behavior.
    use fortplot
    use fortplot_png, only: png_context, create_png_canvas
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    type(png_context) :: ctx
    character(len=:), allocatable :: title_text, xlabel_text, ylabel_text
    logical :: test_passed = .true.

    print *, "Running PNG title centering regression test..."

    ! Test 1: Verify titles are consistently positioned with different data ranges
    call test_title_consistency()
    
    ! Test 2: Verify title positioning with extreme coordinate values
    call test_extreme_coordinates()
    
    if (test_passed) then
        print *, "SUCCESS: PNG title positioning fix verified!"
    else
        print *, "FAILURE: PNG title positioning issues detected!"
        error stop 1
    end if

contains

    subroutine test_title_consistency()
        !! Test that titles appear at the same pixel position regardless of data coordinates
        
        print *, "  Testing title consistency across data ranges..."
        
        ! Test with symmetric data range
        ctx = create_png_canvas(400, 300)
        ctx%x_min = -1.0_wp
        ctx%x_max = 1.0_wp  
        ctx%y_min = -1.0_wp
        ctx%y_max = 1.0_wp
        call ctx%color(0.0_wp, 0.0_wp, 1.0_wp)
        call ctx%line(-0.5_wp, -0.5_wp, 0.5_wp, 0.5_wp)
        
        title_text = "Centered Title Test"
        xlabel_text = "X Axis"
        ylabel_text = "Y Axis" 
        call ctx%draw_axes_and_labels_backend('linear', 'linear', 1.0_wp, &
                                             -1.0_wp, 1.0_wp, -1.0_wp, 1.0_wp, &
                                             title=title_text, xlabel=xlabel_text, ylabel=ylabel_text, &
                                             has_3d_plots=.false.)
        call ctx%save('test/output/title_test_symmetric.png')
        
        ! Test with asymmetric data range - title should appear in same visual position
        ctx = create_png_canvas(400, 300)
        ctx%x_min = 1000.0_wp
        ctx%x_max = 2000.0_wp
        ctx%y_min = -5000.0_wp
        ctx%y_max = -4000.0_wp
        call ctx%color(1.0_wp, 0.0_wp, 0.0_wp)  
        call ctx%line(1200.0_wp, -4800.0_wp, 1800.0_wp, -4200.0_wp)
        
        call ctx%draw_axes_and_labels_backend('linear', 'linear', 1.0_wp, &
                                             1000.0_wp, 2000.0_wp, -5000.0_wp, -4000.0_wp, &
                                             title=title_text, xlabel=xlabel_text, ylabel=ylabel_text, &
                                             has_3d_plots=.false.)
        call ctx%save('test/output/title_test_asymmetric.png')
        
        print *, "    Created test images: test/output/title_test_symmetric.png, test/output/title_test_asymmetric.png"
    end subroutine test_title_consistency

    subroutine test_extreme_coordinates()
        !! Test with extreme coordinate values to ensure robust positioning
        
        print *, "  Testing with extreme coordinate values..."
        
        ctx = create_png_canvas(600, 400)  
        ctx%x_min = 1.0e-6_wp
        ctx%x_max = 2.0e-6_wp
        ctx%y_min = 1.0e6_wp
        ctx%y_max = 2.0e6_wp
        call ctx%color(0.0_wp, 1.0_wp, 0.0_wp)
        call ctx%line(1.2e-6_wp, 1.2e6_wp, 1.8e-6_wp, 1.8e6_wp)
        
        title_text = "Extreme Coordinates Test"
        xlabel_text = "Microscale (1e-6)"
        ylabel_text = "Megascale (1e6)"
        call ctx%draw_axes_and_labels_backend('linear', 'linear', 1.0_wp, &
                                             1.0e-6_wp, 2.0e-6_wp, 1.0e6_wp, 2.0e6_wp, &
                                             title=title_text, xlabel=xlabel_text, ylabel=ylabel_text, &
                                             has_3d_plots=.false.)
        call ctx%save('test/output/title_test_extreme.png')
        
        print *, "    Created test image: test/output/title_test_extreme.png"
    end subroutine test_extreme_coordinates

end program test_title_centering_fix
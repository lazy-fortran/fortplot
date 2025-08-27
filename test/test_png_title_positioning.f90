program test_png_title_positioning
    use fortplot
    use fortplot_png, only: png_context, create_png_canvas
    use fortplot_test_helpers, only: test_get_temp_path, test_cleanup_all
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    ! Test to demonstrate PNG title positioning issue compared to matplotlib
    type(png_context) :: ctx
    real(wp), parameter :: x_data(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
    real(wp), parameter :: y_data(5) = [2.0_wp, 4.0_wp, 1.0_wp, 3.0_wp, 2.5_wp]
    character(len=:), allocatable :: sym_title, sym_xlabel, sym_ylabel
    character(len=:), allocatable :: asym_title, asym_xlabel, asym_ylabel
    character(len=:), allocatable :: neg_title, neg_xlabel, neg_ylabel
    
    ! Create PNG context
    ctx = create_png_canvas(800, 600)
    
    ! Test case 1: Symmetric data range around zero
    print *, "Testing title positioning with symmetric data range..."
    ctx%x_min = -10.0_wp
    ctx%x_max = 10.0_wp
    ctx%y_min = -5.0_wp
    ctx%y_max = 5.0_wp
    call ctx%color(0.0_wp, 0.0_wp, 1.0_wp)
    call ctx%line(-8.0_wp, -3.0_wp, 8.0_wp, 3.0_wp)
    sym_title = 'Symmetric Data Range'
    sym_xlabel = 'X Label'
    sym_ylabel = 'Y Label'
    call ctx%draw_axes_and_labels_backend('linear', 'linear', 1.0_wp, &
                                         -10.0_wp, 10.0_wp, -5.0_wp, 5.0_wp, &
                                         title=sym_title, &
                                         xlabel=sym_xlabel, ylabel=sym_ylabel, &
                                         has_3d_plots=.false.)
    call ctx%save(test_get_temp_path('test_symmetric_title.png'))
    
    ! Test case 2: Asymmetric data range
    print *, "Testing title positioning with asymmetric data range..."
    ctx = create_png_canvas(800, 600)  ! Reset context
    ctx%x_min = 100.0_wp
    ctx%x_max = 200.0_wp
    ctx%y_min = 1000.0_wp
    ctx%y_max = 2000.0_wp
    call ctx%color(1.0_wp, 0.0_wp, 0.0_wp)
    call ctx%line(120.0_wp, 1200.0_wp, 180.0_wp, 1800.0_wp)
    asym_title = 'Asymmetric Data Range'
    asym_xlabel = 'X Label'
    asym_ylabel = 'Y Label'
    call ctx%draw_axes_and_labels_backend('linear', 'linear', 1.0_wp, &
                                         100.0_wp, 200.0_wp, 1000.0_wp, 2000.0_wp, &
                                         title=asym_title, &
                                         xlabel=asym_xlabel, ylabel=asym_ylabel, &
                                         has_3d_plots=.false.)
    call ctx%save(test_get_temp_path('test_asymmetric_title.png'))
    
    ! Test case 3: Negative range
    print *, "Testing title positioning with negative data range..."
    ctx = create_png_canvas(800, 600)  ! Reset context  
    ctx%x_min = -200.0_wp
    ctx%x_max = -100.0_wp
    ctx%y_min = -1000.0_wp
    ctx%y_max = -500.0_wp
    call ctx%color(0.0_wp, 1.0_wp, 0.0_wp)
    call ctx%line(-180.0_wp, -800.0_wp, -120.0_wp, -600.0_wp)
    neg_title = 'Negative Data Range'
    neg_xlabel = 'X Label'
    neg_ylabel = 'Y Label'
    call ctx%draw_axes_and_labels_backend('linear', 'linear', 1.0_wp, &
                                         -200.0_wp, -100.0_wp, -1000.0_wp, -500.0_wp, &
                                         title=neg_title, &
                                         xlabel=neg_xlabel, ylabel=neg_ylabel, &
                                         has_3d_plots=.false.)
    call ctx%save(test_get_temp_path('test_negative_title.png'))
    
    print *, "Title positioning test complete!"
    print *, "Generated test files in temporary directory"
    print *, "- test_symmetric_title.png (should be centered)"
    print *, "- test_asymmetric_title.png (may appear off-center)"
    print *, "- test_negative_title.png (may appear off-center)"
    print *, ""
    print *, "In all cases, the title should be visually centered over the plot area"
    print *, "regardless of the data coordinate range."
    
    ! Clean up test files
    call test_cleanup_all()

end program test_png_title_positioning
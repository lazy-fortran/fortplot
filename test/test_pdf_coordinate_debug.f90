program test_pdf_coordinate_debug
    !! Diagnostic program to identify PDF coordinate system issues
    !! Uses low-level PDF API to test coordinate transformations
    
    use fortplot_pdf
    use fortplot_png
    use fortplot_test_helpers, only: test_get_temp_path, test_cleanup_all
    use iso_fortran_env, only: wp => real64
    implicit none
    
    type(pdf_context) :: pdf_ctx
    type(png_context) :: png_ctx
    real(wp), parameter :: x_data(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
    real(wp), parameter :: y_data(5) = [2.0_wp, 4.0_wp, 1.0_wp, 5.0_wp, 3.0_wp]
    integer :: i
    
    ! Create contexts
    pdf_ctx = create_pdf_canvas(400, 300)
    png_ctx = create_png_canvas(400, 300)
    
    ! Set coordinate system for both
    call pdf_ctx%set_coordinates(0.0_wp, 6.0_wp, 0.0_wp, 6.0_wp)
    png_ctx%x_min = 0.0_wp
    png_ctx%x_max = 6.0_wp
    png_ctx%y_min = 0.0_wp
    png_ctx%y_max = 6.0_wp
    
    ! Draw coordinate system test - PDF
    call pdf_ctx%color(0.0_wp, 0.0_wp, 0.0_wp)  ! Black
    call pdf_ctx%set_line_width(1.0_wp)
    
    ! Draw data points as lines - PDF
    call pdf_ctx%color(1.0_wp, 0.0_wp, 0.0_wp)  ! Red
    call pdf_ctx%set_line_width(2.0_wp)
    do i = 1, size(x_data) - 1
        call pdf_ctx%line(x_data(i), y_data(i), x_data(i+1), y_data(i+1))
    end do
    
    ! Add axes manually - PDF
    call pdf_ctx%render_axes('PDF Debug Test', 'X Axis', 'Y Axis')
    
    ! Do the same for PNG
    call png_ctx%color(0.0_wp, 0.0_wp, 0.0_wp)  ! Black
    call png_ctx%set_line_width(1.0_wp)
    
    ! Draw data points as lines - PNG
    call png_ctx%color(1.0_wp, 0.0_wp, 0.0_wp)  ! Red
    call png_ctx%set_line_width(2.0_wp)
    do i = 1, size(x_data) - 1
        call png_ctx%line(x_data(i), y_data(i), x_data(i+1), y_data(i+1))
    end do
    
    ! Save both files to temp directory
    call pdf_ctx%save(test_get_temp_path('debug_coordinates_pdf.pdf'))
    call png_ctx%save(test_get_temp_path('debug_coordinates_png.png'))
    
    print *, 'Generated test files in temporary directory'
    print *, 'Compare both files to identify coordinate system issues:'
    print *, '1. Check if X axis appears at bottom (correct) or top (incorrect)'
    print *, '2. Check if axes are blue (incorrect) or black (correct)'
    print *, '3. Check if plot appears within frame boundaries'
    print *, '4. Verify data line positions match between formats'
    
    ! Clean up test files
    call test_cleanup_all()
    
end program test_pdf_coordinate_debug
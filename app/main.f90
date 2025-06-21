program plot_generator
    use plotting_module
    use png_context_module
    use pdf_module
    implicit none
    
    integer, parameter :: width = 640, height = 480
    type(png_context) :: png_canvas
    type(pdf_context) :: pdf_canvas
    
    png_canvas = create_png_canvas(width, height)
    pdf_canvas = create_pdf_canvas(width, height)
    
    call draw_sine_wave(png_canvas, 0.4, 4)
    call draw_coordinate_axes(png_canvas)
    call save_plot(png_canvas, 'output.png')
    
    call draw_sine_wave(pdf_canvas, 0.4, 4)
    call draw_coordinate_axes(pdf_canvas)
    call save_plot(pdf_canvas, 'output.pdf')
    
end program plot_generator

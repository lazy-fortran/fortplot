program test_backend_type
    use fortplot
    use fortplot_raster, only: raster_context
    use fortplot_png, only: png_context
    implicit none
    
    type(figure_t) :: fig
    
    call figure(fig)
    
    ! Check what type of backend we have
    select type (backend => fig%backend)
    type is (png_context)
        print *, 'Backend is png_context'
    type is (raster_context) 
        print *, 'Backend is raster_context'
    class default
        print *, 'Backend is something else'
    end select
    
end program test_backend_type
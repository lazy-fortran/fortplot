module stb_image_write_wrapper
    !! Fortran interface to STB image write C wrapper
    !! Provides iso_c_binding interface to stb_image_write functions
    use iso_c_binding
    use, intrinsic :: iso_fortran_env, only: int8
    implicit none
    
    private
    public :: stb_write_jpeg_wrapper, stb_write_jpeg_to_memory
    
    ! C wrapper interfaces
    interface
        ! Write JPEG to file
        function stb_write_jpeg_wrapper(filename, width, height, components, data, quality) &
                bind(C, name="stb_write_jpeg_wrapper")
            import :: c_int, c_char, c_ptr
            character(c_char), intent(in) :: filename(*)
            integer(c_int), value :: width, height, components, quality
            type(c_ptr), value :: data
            integer(c_int) :: stb_write_jpeg_wrapper
        end function stb_write_jpeg_wrapper
        
        ! Write JPEG to memory buffer
        function stb_write_jpeg_to_memory(output_data, output_size, width, height, components, data, quality) &
                bind(C, name="stb_write_jpeg_to_memory")
            import :: c_int, c_ptr
            type(c_ptr), intent(out) :: output_data
            integer(c_int), intent(out) :: output_size
            integer(c_int), value :: width, height, components, quality
            type(c_ptr), value :: data
            integer(c_int) :: stb_write_jpeg_to_memory
        end function stb_write_jpeg_to_memory
    end interface
    
end module stb_image_write_wrapper
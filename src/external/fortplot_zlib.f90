module fortplot_zlib_bridge
    !! Bridge module to re-export from the main fortplot_zlib_core
    !! This maintains compatibility while using the working full implementation
    use fortplot_zlib_core, only: zlib_compress, zlib_compress_into, crc32_calculate
    implicit none
    
    private  
    public :: zlib_compress, zlib_compress_into, crc32_calculate
    
end module fortplot_zlib_bridge

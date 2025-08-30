module fortplot_zlib
    !! Pure Fortran implementation of zlib compression, decompression, and CRC32  
    !! Refactored for file size compliance (Issue #884) - delegates to specialized modules
    use fortplot_zlib_core, only: zlib_compress, crc32_calculate
    implicit none
    
    private
    public :: zlib_compress, crc32_calculate
    
end module fortplot_zlib
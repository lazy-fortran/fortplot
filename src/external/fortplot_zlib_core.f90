module fortplot_zlib_core
    !! Re-export facade for zlib compress and decompress modules.
    !!
    !! Compression lives in fortplot_zlib_compress; decompression in
    !! fortplot_zlib_decompress. This module re-exports both for
    !! backwards compatibility (Issue #1694).

    use fortplot_zlib_compress, only: crc32_calculate, zlib_compress, &
                                      zlib_compress_into
    use fortplot_zlib_decompress, only: zlib_decompress
    implicit none

    private
    public :: zlib_compress, zlib_compress_into, zlib_decompress
    public :: crc32_calculate

end module fortplot_zlib_core

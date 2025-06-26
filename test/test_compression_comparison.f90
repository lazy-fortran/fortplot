program test_compression_comparison
    !! Test Fortran zlib compression against STB compression
    use fortplot_zlib, only: zlib_compress
    use iso_c_binding, only: c_ptr, c_loc, c_int, c_f_pointer, c_associated
    use, intrinsic :: iso_fortran_env, only: int8
    implicit none
    
    call test_compression_ratios()
    call test_typical_image_data()
    
contains

    subroutine test_compression_ratios()
        !! Compare compression ratios between Fortran and STB implementations
        integer(int8), target :: test_data(10000)
        integer(int8), allocatable :: fortran_compressed(:)
        integer(int8), pointer :: stb_compressed(:)
        type(c_ptr) :: stb_ptr
        integer :: fortran_len, i
        integer, target :: stb_len
        real :: fortran_ratio, stb_ratio
        
        interface
            function stb_compress_data(data, data_len, compressed_len, quality) bind(C, name="stb_compress_data")
                import :: c_ptr, c_int
                type(c_ptr), value :: data
                integer(c_int), value :: data_len
                type(c_ptr), value :: compressed_len
                integer(c_int), value :: quality
                type(c_ptr) :: stb_compress_data
            end function stb_compress_data
        end interface
        
        ! Create test data with repeated patterns (should compress well)
        do i = 1, 10000
            test_data(i) = int(mod(i, 16), int8)  ! Pattern: 0,1,2,...,15,0,1,2,...
        end do
        
        print *, "=== Compression Comparison Test ==="
        print *, "Input data size:", size(test_data), "bytes"
        
        ! Test Fortran compression
        fortran_compressed = zlib_compress(test_data, size(test_data), fortran_len)
        fortran_ratio = real(fortran_len) / real(size(test_data))
        
        print *, "Fortran zlib compressed size:", fortran_len, "bytes"
        print *, "Fortran compression ratio:", fortran_ratio
        
        ! Test STB compression
        stb_ptr = stb_compress_data(c_loc(test_data), int(size(test_data), c_int), c_loc(stb_len), 8_c_int)
        if (c_associated(stb_ptr)) then
            call c_f_pointer(stb_ptr, stb_compressed, [stb_len])
            stb_ratio = real(stb_len) / real(size(test_data))
            
            print *, "STB zlib compressed size:", stb_len, "bytes"
            print *, "STB compression ratio:", stb_ratio
            
            print *, ""
            print *, "Compression efficiency:"
            print *, "  STB achieves", int((1.0 - stb_ratio) * 100), "% reduction"
            print *, "  Fortran achieves", int((1.0 - fortran_ratio) * 100), "% reduction"
            
            if (fortran_ratio > 1.0) then
                print *, "WARNING: Fortran implementation is expanding data!"
            end if
            
            if (stb_ratio < fortran_ratio) then
                print *, "STB compression is", int(fortran_ratio / stb_ratio), "x better than Fortran"
            else
                print *, "Fortran compression is comparable to STB"
            end if
        else
            print *, "STB compression failed"
        end if
        
        deallocate(fortran_compressed)
        print *, ""
    end subroutine test_compression_ratios

    subroutine test_typical_image_data()
        !! Test with data typical of PNG image content
        integer(int8), target :: image_like_data(65536)  ! 256x256 pixel equivalent
        integer(int8), allocatable :: fortran_compressed(:)
        integer(int8), pointer :: stb_compressed(:)
        type(c_ptr) :: stb_ptr
        integer :: fortran_len, i, j
        integer, target :: stb_len
        real :: fortran_ratio, stb_ratio
        integer :: seed
        
        interface
            function stb_compress_data(data, data_len, compressed_len, quality) bind(C, name="stb_compress_data")
                import :: c_ptr, c_int
                type(c_ptr), value :: data
                integer(c_int), value :: data_len
                type(c_ptr), value :: compressed_len
                integer(c_int), value :: quality
                type(c_ptr) :: stb_compress_data
            end function stb_compress_data
        end interface
        
        ! Create image-like data (lots of similar values, some patterns)
        seed = 12345
        do i = 1, size(image_like_data)
            ! Create pseudo-realistic image data with some patterns
            j = mod(i-1, 256) + 1  ! Column position
            if (j < 50 .or. j > 200) then
                image_like_data(i) = -1_int8  ! White background areas
            else
                ! Add some variation in content area
                seed = mod(seed * 1103515245 + 12345, 2147483647)
                image_like_data(i) = int(mod(seed, 64) - 32, int8)  ! Gray values
            end if
        end do
        
        print *, "=== Image-like Data Compression Test ==="
        print *, "Input data size:", size(image_like_data), "bytes (like 256x256 image)"
        
        ! Test Fortran compression
        fortran_compressed = zlib_compress(image_like_data, size(image_like_data), fortran_len)
        fortran_ratio = real(fortran_len) / real(size(image_like_data))
        
        print *, "Fortran zlib compressed size:", fortran_len, "bytes"
        print *, "Fortran compression ratio:", fortran_ratio
        
        ! Test STB compression
        stb_ptr = stb_compress_data(c_loc(image_like_data), int(size(image_like_data), c_int), c_loc(stb_len), 8_c_int)
        if (c_associated(stb_ptr)) then
            call c_f_pointer(stb_ptr, stb_compressed, [stb_len])
            stb_ratio = real(stb_len) / real(size(image_like_data))
            
            print *, "STB zlib compressed size:", stb_len, "bytes"
            print *, "STB compression ratio:", stb_ratio
            
            print *, ""
            print *, "For typical image data:"
            print *, "  STB would save", int((1.0 - stb_ratio) * 100), "% space"
            print *, "  Fortran saves", int((1.0 - fortran_ratio) * 100), "% space"
            
            if (fortran_len > size(image_like_data)) then
                print *, "PROBLEM: Fortran implementation makes files LARGER!"
                print *, "This explains the 1MB PNG files."
            end if
        else
            print *, "STB compression failed"
        end if
        
        deallocate(fortran_compressed)
    end subroutine test_typical_image_data

end program test_compression_comparison
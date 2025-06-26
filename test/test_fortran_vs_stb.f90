program test_fortran_vs_stb
    !! Test Fortran zlib functions against STB implementations
    use fortplot_zlib, only: crc32_calculate
    use iso_c_binding, only: c_ptr, c_loc, c_int, c_int32_t
    use, intrinsic :: iso_fortran_env, only: int8, int32
    implicit none
    
    call test_crc32_vs_stb()
    print *, "Fortran vs STB implementation tests PASSED!"
    
contains

    subroutine test_crc32_vs_stb()
        !! Test that our Fortran CRC32 matches STB implementation
        integer(int8), target :: test_data(10)
        integer(int32) :: fortran_crc, stb_crc
        integer :: i
        
        interface
            function stb_crc32(data, data_len) bind(C, name="stb_crc32")
                import :: c_ptr, c_int, c_int32_t
                type(c_ptr), value :: data
                integer(c_int), value :: data_len
                integer(c_int32_t) :: stb_crc32
            end function stb_crc32
        end interface
        
        ! Test 1: Simple sequential data
        do i = 1, 10
            test_data(i) = int(i, int8)
        end do
        
        fortran_crc = crc32_calculate(test_data, 10)
        stb_crc = stb_crc32(c_loc(test_data), 10)
        
        if (fortran_crc /= stb_crc) then
            print *, "FAIL: CRC32 mismatch for sequential data"
            print *, "Fortran:", fortran_crc, "STB:", stb_crc
            stop 1
        end if
        
        ! Test 2: Known "123456789" test
        test_data(1:9) = [49, 50, 51, 52, 53, 54, 55, 56, 57]  ! ASCII "123456789"
        
        fortran_crc = crc32_calculate(test_data, 9)
        stb_crc = stb_crc32(c_loc(test_data), 9)
        
        if (fortran_crc /= stb_crc) then
            print *, "FAIL: CRC32 mismatch for known test string"
            print *, "Fortran:", fortran_crc, "STB:", stb_crc
            stop 1
        end if
        
        ! Test 3: All zeros
        test_data = 0_int8
        
        fortran_crc = crc32_calculate(test_data, 10)
        stb_crc = stb_crc32(c_loc(test_data), 10)
        
        if (fortran_crc /= stb_crc) then
            print *, "FAIL: CRC32 mismatch for all zeros"
            print *, "Fortran:", fortran_crc, "STB:", stb_crc
            stop 1
        end if
        
        print *, "PASS: CRC32 matches STB implementation for all test cases"
    end subroutine test_crc32_vs_stb

end program test_fortran_vs_stb
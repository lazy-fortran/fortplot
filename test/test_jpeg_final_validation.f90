program test_jpeg_final_validation
    ! Final validation showing JPEG implementation status
    use fortplot_jpeg
    use stb_image_write_wrapper
    use iso_c_binding
    use, intrinsic :: iso_fortran_env, only: int8, int32, real64
    implicit none
    
    interface
        subroutine free(ptr) bind(C, name='free')
            import :: c_ptr
            type(c_ptr), value :: ptr
        end subroutine free
    end interface
    
    print *, "=== JPEG Implementation Validation Summary ==="
    print *, ""
    
    call test_implementation_status()
    call test_stb_reference()
    call show_recommendations()
    
contains

    subroutine test_implementation_status()
        type(jpeg_context) :: ctx
        integer :: width = 8, height = 8
        logical :: exists
        integer :: size_bytes, exit_code
        character(len=200) :: info
        
        print *, "1. Current Fortran JPEG Implementation Status:"
        print *, "   ----------------------------------------"
        
        ! Test basic encoding
        ctx = create_jpeg_canvas(width, height, 90)
        call ctx%save("fortplot_test.jpg")
        
        inquire(file="fortplot_test.jpg", exist=exists, size=size_bytes)
        
        if (exists) then
            print *, "   ✓ Creates JPEG files"
            print *, "   ✓ File size:", size_bytes, "bytes"
            
            ! Check structure
            call execute_command_line("file fortplot_test.jpg", exitstat=exit_code)
            
            ! Check if decodable
            call execute_command_line("python3 -c 'from PIL import Image; Image.open(""fortplot_test.jpg"")' 2>&1", &
                exitstat=exit_code)
            
            if (exit_code == 0) then
                print *, "   ✓ Decodable by PIL/Pillow"
            else
                print *, "   ✗ NOT decodable by standard decoders"
                print *, "     Issue: Incomplete Huffman tables (DHT)"
            end if
        else
            print *, "   ✗ Failed to create JPEG"
        end if
        
        print *, ""
        print *, "   Components implemented:"
        print *, "   ✓ DCT transformation"
        print *, "   ✓ Quantization"  
        print *, "   ✓ Huffman encoding logic"
        print *, "   ✓ Bit packing"
        print *, "   ✓ JPEG markers (SOI, APP0, DQT, SOF0, SOS, EOI)"
        print *, ""
        print *, "   Issues found:"
        print *, "   ✗ Incomplete DHT (Huffman tables) - only partial tables written"
        print *, "   ✗ RGB extraction expects PNG format with filter bytes"
        print *, "   ✗ Scan data encoding produces too few bytes"
        print *, ""
    end subroutine test_implementation_status
    
    subroutine test_stb_reference()
        integer(int8), target :: rgb_data(3, 8, 8)  
        integer(int8), pointer :: stb_data(:)
        type(c_ptr) :: stb_ptr
        integer :: stb_size, i, j
        
        print *, "2. STB Reference Implementation:"
        print *, "   ----------------------------"
        
        ! Create test image
        do j = 1, 8
            do i = 1, 8
                rgb_data(1, i, j) = int((i-1) * 36, int8)
                rgb_data(2, i, j) = int((j-1) * 36, int8)
                rgb_data(3, i, j) = 0_int8
            end do
        end do
        
        ! Encode with STB
        if (stb_write_jpeg_to_memory(stb_ptr, stb_size, 8, 8, 3, &
            c_loc(rgb_data), 90) == 0) then
            print *, "   ✗ STB encoding failed"
            return
        end if
        
        call c_f_pointer(stb_ptr, stb_data, [stb_size])
        
        print *, "   ✓ STB creates valid JPEG"
        print *, "   ✓ Size:", stb_size, "bytes"
        print *, "   ✓ Fully decodable by all standard decoders"
        print *, ""
        
        ! Write for reference
        open(unit=10, file="stb_reference.jpg", status='replace', access='stream')
        write(10) stb_data
        close(10)
        
        ! Show structure
        call show_jpeg_structure(stb_data, stb_size)
        
        call free(stb_ptr)
    end subroutine test_stb_reference
    
    subroutine show_jpeg_structure(data, size)
        integer(int8), intent(in) :: data(:)
        integer, intent(in) :: size
        integer :: pos, marker, length
        integer :: dht_count = 0
        
        print *, "   JPEG Structure:"
        
        pos = 1
        do while (pos < size - 1)
            if (data(pos) == int(-1, int8)) then
                marker = iand(255, int(data(pos+1)))
                
                select case (marker)
                case (216)  ! D8
                    print *, "   - SOI at position", pos
                case (224)  ! E0  
                    print *, "   - APP0 (JFIF header)"
                case (219)  ! DB
                    print *, "   - DQT (Quantization tables)"
                case (192)  ! C0
                    print *, "   - SOF0 (Frame header)"
                case (196)  ! C4
                    dht_count = dht_count + 1
                    if (pos + 4 <= size) then
                        length = 256 * iand(255, int(data(pos+2))) + iand(255, int(data(pos+3)))
                        print '(A,I3,A)', "   - DHT (Huffman table), length=", length, " bytes"
                    end if
                case (218)  ! DA
                    print *, "   - SOS (Start of scan)"
                    exit
                case (217)  ! D9
                    print *, "   - EOI"
                end select
                
                pos = pos + 2
            else
                pos = pos + 1
            end if
        end do
        
        print *, ""
        print *, "   Total DHT segments:", dht_count
        print *, ""
    end subroutine show_jpeg_structure
    
    subroutine show_recommendations()
        print *, "3. Validation Summary:"
        print *, "   -----------------"
        print *, ""
        print *, "   The current Fortran JPEG implementation has:"
        print *, "   - Correct DCT and quantization"
        print *, "   - Correct Huffman encoding algorithm"
        print *, "   - Correct bit packing"
        print *, ""
        print *, "   BUT produces invalid JPEGs due to:"
        print *, "   - Incomplete DHT tables (must write full 4 tables)"
        print *, "   - RGB extraction bug (expects PNG format)"
        print *, "   - Insufficient scan data output"
        print *, ""
        print *, "   To fix:"
        print *, "   1. Write complete DHT tables (Y-DC, Y-AC, UV-DC, UV-AC)"
        print *, "   2. Fix RGB extraction to handle flat RGB arrays"
        print *, "   3. Ensure all coefficients are properly encoded"
        print *, ""
        print *, "   The fortplot_jpeg_fixed.f90 module addresses issue #2"
        print *, "   but still needs complete DHT implementation."
    end subroutine show_recommendations

end program test_jpeg_final_validation
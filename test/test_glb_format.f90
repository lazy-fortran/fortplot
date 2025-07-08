program test_glb_format
    !! Test GLB binary format generation
    !! Following TDD: Write test first, then implementation
    
    use iso_fortran_env, only: wp => real64, int8
    use fortplot
    implicit none
    
    call test_glb_file_created()
    call test_glb_magic_header()
    call test_glb_chunks_structure()
    call test_glb_binary_data()
    
    print *, "All GLB format tests passed!"
    
contains

    subroutine test_glb_file_created()
        !! Test that .glb file is created
        type(figure_t) :: fig
        real(wp), dimension(3) :: x = [0.0_wp, 1.0_wp, 2.0_wp]
        real(wp), dimension(3) :: y = [0.0_wp, 1.0_wp, 0.0_wp]
        real(wp), dimension(3) :: z = [0.0_wp, 0.5_wp, 1.0_wp]
        character(len=256) :: filename
        logical :: file_exists
        
        filename = "test_glb_format.glb"
        
        call fig%initialize(640, 480)
        call fig%add_3d_plot(x, y, z)
        call fig%savefig(filename)
        
        ! Check file exists
        inquire(file=filename, exist=file_exists)
        if (.not. file_exists) then
            error stop "GLB file was not created"
        end if
        
        ! Clean up
        call execute_command_line('rm -f ' // trim(filename))
        
    end subroutine test_glb_file_created
    
    subroutine test_glb_magic_header()
        !! Test GLB file has correct magic header
        type(figure_t) :: fig
        real(wp), dimension(2) :: x = [0.0_wp, 1.0_wp]
        real(wp), dimension(2) :: y = [0.0_wp, 1.0_wp]
        real(wp), dimension(2) :: z = [0.0_wp, 1.0_wp]
        character(len=256) :: filename
        integer :: unit, iostat
        integer(int8) :: magic_bytes(4)
        integer :: magic_value
        
        filename = "test_glb_magic.glb"
        
        call fig%initialize(640, 480)
        call fig%add_3d_plot(x, y, z)
        call fig%savefig(filename)
        
        ! Read magic header
        open(newunit=unit, file=filename, status='old', &
             action='read', access='stream', iostat=iostat)
        if (iostat /= 0) then
            error stop "Failed to open GLB file"
        end if
        
        read(unit) magic_bytes
        close(unit)
        
        ! GLB magic is 0x46546C67 ('glTF' in ASCII)
        magic_value = int(magic_bytes(1)) + ishft(int(magic_bytes(2)), 8) + &
                     ishft(int(magic_bytes(3)), 16) + ishft(int(magic_bytes(4)), 24)
        
        ! 0x46546C67 = 1179937895 in decimal
        if (magic_value /= 1179937895) then
            error stop "GLB magic header is incorrect"
        end if
        
        ! Clean up
        call execute_command_line('rm -f ' // trim(filename))
        
    end subroutine test_glb_magic_header
    
    subroutine test_glb_chunks_structure()
        !! Test GLB has JSON and BIN chunks
        type(figure_t) :: fig
        real(wp), dimension(3) :: x = [0.0_wp, 1.0_wp, 2.0_wp]
        real(wp), dimension(3) :: y = [0.0_wp, 1.0_wp, 0.0_wp]
        real(wp), dimension(3) :: z = [0.0_wp, 0.5_wp, 1.0_wp]
        character(len=256) :: filename
        integer :: unit, chunk_length, chunk_type
        integer(int8) :: header(12), chunk_header(8)
        
        filename = "test_glb_chunks.glb"
        
        call fig%initialize(640, 480)
        call fig%add_3d_plot(x, y, z)
        call fig%savefig(filename)
        
        ! Read file header and first chunk
        open(newunit=unit, file=filename, status='old', &
             action='read', access='stream')
        
        ! Skip header (12 bytes)
        read(unit) header
        
        ! Read first chunk header
        read(unit) chunk_header
        
        ! Extract chunk length and type
        chunk_length = int(chunk_header(1)) + ishft(int(chunk_header(2)), 8) + &
                      ishft(int(chunk_header(3)), 16) + ishft(int(chunk_header(4)), 24)
        chunk_type = int(chunk_header(5)) + ishft(int(chunk_header(6)), 8) + &
                    ishft(int(chunk_header(7)), 16) + ishft(int(chunk_header(8)), 24)
        
        ! First chunk should be JSON (0x4E4F534A = 1313821514)
        if (chunk_type /= 1313821514) then
            error stop "First chunk is not JSON chunk"
        end if
        
        close(unit)
        
        ! Clean up
        call execute_command_line('rm -f ' // trim(filename))
        
    end subroutine test_glb_chunks_structure
    
    subroutine test_glb_binary_data()
        !! Test GLB contains binary vertex data
        type(figure_t) :: fig
        real(wp), dimension(4) :: x = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
        real(wp), dimension(4) :: y = [0.0_wp, 1.0_wp, 0.0_wp, -1.0_wp]
        real(wp), dimension(4) :: z = [0.0_wp, 0.5_wp, 1.0_wp, 0.5_wp]
        character(len=256) :: filename
        integer :: unit, file_size
        logical :: has_binary_chunk
        
        filename = "test_glb_binary.glb"
        
        call fig%initialize(640, 480)
        call fig%add_3d_plot(x, y, z)
        call fig%savefig(filename)
        
        ! Check file size indicates binary data
        inquire(file=filename, size=file_size)
        
        ! GLB with binary data should be reasonably sized
        ! 4 vertices * 3 coords * 4 bytes = 48 bytes of vertex data minimum
        if (file_size < 100) then
            error stop "GLB file too small to contain binary data"
        end if
        
        ! Clean up
        call execute_command_line('rm -f ' // trim(filename))
        
    end subroutine test_glb_binary_data

end program test_glb_format
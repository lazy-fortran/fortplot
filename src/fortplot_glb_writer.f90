module fortplot_glb_writer
    !! Module for writing GLB binary format
    !! Following SRP - handles GLB file structure
    !! Following KISS - straightforward binary writing
    
    use iso_fortran_env, only: wp => real64, int8, int32
    implicit none
    
    private
    public :: write_glb_file
    
    ! GLB constants
    integer(int32), parameter :: GLB_MAGIC = int(z'46546C67', int32)  ! 'glTF'
    integer(int32), parameter :: GLB_VERSION = 2
    integer(int32), parameter :: GLB_CHUNK_JSON = int(z'4E4F534A', int32)  ! 'JSON'
    integer(int32), parameter :: GLB_CHUNK_BIN = int(z'004E4942', int32)   ! 'BIN\0'
    
contains

    subroutine write_glb_file(filename, json_data, binary_data)
        !! Write complete GLB file with JSON and binary chunks
        !! Following KISS - simple sequential write
        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: json_data
        integer(int8), intent(in) :: binary_data(:)
        
        integer :: unit, json_length, bin_length, total_length
        integer :: json_padded_length, bin_padded_length
        character(len=:), allocatable :: padded_json
        integer(int8), allocatable :: padded_bin(:)
        
        ! Calculate padded lengths (must be multiple of 4)
        json_length = len(json_data)
        json_padded_length = ((json_length + 3) / 4) * 4
        
        bin_length = size(binary_data)
        bin_padded_length = ((bin_length + 3) / 4) * 4
        
        ! Total file length
        total_length = 12 + 8 + json_padded_length + 8 + bin_padded_length
        
        ! Pad JSON with spaces
        padded_json = json_data // repeat(' ', json_padded_length - json_length)
        
        ! Pad binary with zeros
        allocate(padded_bin(bin_padded_length))
        padded_bin = 0
        padded_bin(1:bin_length) = binary_data
        
        ! Write GLB file
        open(newunit=unit, file=filename, status='replace', &
             action='write', access='stream')
        
        ! Write header
        call write_glb_header(unit, total_length)
        
        ! Write JSON chunk
        call write_glb_chunk(unit, GLB_CHUNK_JSON, &
                           json_padded_length, padded_json)
        
        ! Write binary chunk
        call write_glb_binary_chunk(unit, GLB_CHUNK_BIN, &
                                  bin_padded_length, padded_bin)
        
        close(unit)
        
    end subroutine write_glb_file
    
    subroutine write_glb_header(unit, total_length)
        !! Write GLB file header
        !! Following SRP - only writes header
        integer, intent(in) :: unit, total_length
        
        write(unit) int(GLB_MAGIC, int32)
        write(unit) int(GLB_VERSION, int32)
        write(unit) int(total_length, int32)
        
    end subroutine write_glb_header
    
    subroutine write_glb_chunk(unit, chunk_type, chunk_length, chunk_data)
        !! Write a GLB chunk with string data
        !! Following SRP - handles one chunk
        integer, intent(in) :: unit
        integer(int32), intent(in) :: chunk_type
        integer, intent(in) :: chunk_length
        character(len=*), intent(in) :: chunk_data
        
        write(unit) int(chunk_length, int32)
        write(unit) chunk_type
        write(unit) chunk_data
        
    end subroutine write_glb_chunk
    
    subroutine write_glb_binary_chunk(unit, chunk_type, chunk_length, chunk_data)
        !! Write a GLB chunk with binary data
        !! Following SRP - handles binary chunk
        integer, intent(in) :: unit
        integer(int32), intent(in) :: chunk_type
        integer, intent(in) :: chunk_length
        integer(int8), intent(in) :: chunk_data(:)
        
        write(unit) int(chunk_length, int32)
        write(unit) chunk_type
        write(unit) chunk_data
        
    end subroutine write_glb_binary_chunk

end module fortplot_glb_writer
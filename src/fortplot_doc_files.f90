module fortplot_doc_files
    !! File operations for documentation generation
    use fortplot_doc_constants, only: PATH_MAX_LEN, FILENAME_MAX_LEN, LINE_MAX_LEN
    implicit none
    private
    
    ! Public interface
    public :: check_file_exists, get_file_extension, replace_extension
    public :: copy_file_content, build_file_path
    
contains

    pure logical function check_file_exists(dir, filename)
        character(len=*), intent(in) :: dir, filename
        character(len=PATH_MAX_LEN) :: full_path
        
        call build_file_path(dir, filename, full_path)
        inquire(file=trim(full_path), exist=check_file_exists)
    end function check_file_exists
    
    pure function get_file_extension(filename) result(ext)
        character(len=*), intent(in) :: filename
        character(len=10) :: ext
        integer :: dot_pos
        
        dot_pos = index(filename, '.', back=.true.)
        if (dot_pos > 0 .and. dot_pos < len_trim(filename)) then
            ext = filename(dot_pos+1:)
        else
            ext = ''
        end if
    end function get_file_extension
    
    pure function replace_extension(filename, new_ext) result(new_filename)
        character(len=*), intent(in) :: filename, new_ext
        character(len=FILENAME_MAX_LEN) :: new_filename
        integer :: dot_pos
        
        dot_pos = index(filename, '.', back=.true.)
        if (dot_pos > 0) then
            new_filename = filename(1:dot_pos) // new_ext
        else
            new_filename = trim(filename) // '.' // new_ext
        end if
    end function replace_extension
    
    pure subroutine build_file_path(dir, filename, full_path)
        character(len=*), intent(in) :: dir, filename
        character(len=PATH_MAX_LEN), intent(out) :: full_path
        
        full_path = trim(dir) // '/' // trim(filename)
    end subroutine build_file_path
    
    subroutine copy_file_content(source_file, target_unit)
        character(len=*), intent(in) :: source_file
        integer, intent(in) :: target_unit
        
        character(len=LINE_MAX_LEN) :: line
        integer :: unit_src, ios
        
        open(newunit=unit_src, file=trim(source_file), status='old', iostat=ios)
        if (ios == 0) then
            do
                read(unit_src, '(A)', iostat=ios) line
                if (ios /= 0) exit
                write(target_unit, '(A)') trim(line)
            end do
            close(unit_src)
        end if
    end subroutine copy_file_content

end module fortplot_doc_files
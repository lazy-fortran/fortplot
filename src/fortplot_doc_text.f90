module fortplot_doc_text
    !! Text processing utilities for documentation generation
    use fortplot_doc_constants, only: FILENAME_MAX_LEN
    implicit none
    private
    
    ! Public interface
    public :: title_case, get_output_title, process_image_path
    
contains

    pure function title_case(name) result(title)
        character(len=*), intent(in) :: name
        character(len=FILENAME_MAX_LEN) :: title
        integer :: i
        
        title = name
        call replace_underscores_with_spaces(title)
        call capitalize_first_and_after_spaces(title)
    end function title_case
    
    pure subroutine replace_underscores_with_spaces(text)
        character(len=*), intent(inout) :: text
        integer :: i
        
        do i = 1, len_trim(text)
            if (text(i:i) == '_') text(i:i) = ' '
        end do
    end subroutine replace_underscores_with_spaces
    
    pure subroutine capitalize_first_and_after_spaces(text)
        character(len=*), intent(inout) :: text
        integer :: i
        
        if (len_trim(text) > 0) then
            call capitalize_if_lowercase(text(1:1))
            
            do i = 2, len_trim(text)
                if (text(i-1:i-1) == ' ') then
                    call capitalize_if_lowercase(text(i:i))
                end if
            end do
        end if
    end subroutine capitalize_first_and_after_spaces
    
    pure subroutine capitalize_if_lowercase(char)
        character(len=1), intent(inout) :: char
        if (char >= 'a' .and. char <= 'z') then
            char = char(ichar(char) - 32)
        end if
    end subroutine capitalize_if_lowercase
    
    pure function get_output_title(filename) result(title)
        character(len=*), intent(in) :: filename
        character(len=FILENAME_MAX_LEN) :: title
        character(len=FILENAME_MAX_LEN) :: base
        integer :: dot_pos
        
        dot_pos = index(filename, '.', back=.true.)
        if (dot_pos > 0) then
            base = filename(1:dot_pos-1)
        else
            base = filename
        end if
        
        title = title_case(base)
    end function get_output_title
    
    subroutine process_image_path(line, example_name)
        character(len=*), intent(inout) :: line
        character(len=*), intent(in) :: example_name
        integer :: start_pos, end_pos, path_start, path_end
        character(len=FILENAME_MAX_LEN) :: new_path, filename
        
        start_pos = index(line, '](')
        if (start_pos > 0) then
            call extract_path_from_markdown(line, start_pos, filename, path_start, path_end)
            if (path_end > 0) then
                call build_media_path(example_name, filename, new_path)
                call replace_path_in_line(line, start_pos, path_end, new_path)
            end if
        end if
    end subroutine process_image_path
    
    pure subroutine extract_path_from_markdown(line, start_pos, filename, path_start, path_end)
        character(len=*), intent(in) :: line
        integer, intent(in) :: start_pos
        character(len=FILENAME_MAX_LEN), intent(out) :: filename
        integer, intent(out) :: path_start, path_end
        
        path_start = start_pos + 2
        path_end = index(line(path_start:), ')')
        if (path_end > 0) then
            path_end = path_start + path_end - 2
            filename = adjustl(trim(line(path_start:path_end)))
            call extract_filename_only(filename)
        end if
    end subroutine extract_path_from_markdown
    
    pure subroutine extract_filename_only(filename)
        character(len=*), intent(inout) :: filename
        integer :: slash_pos
        
        slash_pos = index(filename, '/', back=.true.)
        if (slash_pos > 0) then
            filename = filename(slash_pos+1:)
        end if
    end subroutine extract_filename_only
    
    pure subroutine build_media_path(example_name, filename, new_path)
        character(len=*), intent(in) :: example_name, filename
        character(len=FILENAME_MAX_LEN), intent(out) :: new_path
        
        new_path = '../../media/examples/' // trim(example_name) // '/' // trim(filename)
    end subroutine build_media_path
    
    subroutine replace_path_in_line(line, start_pos, path_end, new_path)
        character(len=*), intent(inout) :: line
        integer, intent(in) :: start_pos, path_end
        character(len=*), intent(in) :: new_path
        
        line = line(1:start_pos+1) // trim(new_path) // ')'
    end subroutine replace_path_in_line

end module fortplot_doc_text
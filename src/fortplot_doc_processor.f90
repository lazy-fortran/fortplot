module fortplot_doc_processor
    !! Main documentation processing module
    use fortplot_doc_constants, only: LINE_MAX_LEN, PATH_MAX_LEN
    use fortplot_doc_paths, only: build_readme_path, build_output_path
    use fortplot_doc_text, only: title_case, process_image_path
    use fortplot_doc_source, only: write_source_links
    use fortplot_doc_output, only: write_generated_outputs
    implicit none
    private
    
    ! Public interface
    public :: process_example
    
contains

    subroutine process_example(example_dir, example_name)
        character(len=*), intent(in) :: example_dir, example_name
        
        character(len=PATH_MAX_LEN) :: readme_file, output_file
        integer :: unit_in, unit_out, ios
        logical :: file_exists
        
        print *, "Processing: ", trim(example_name)
        
        call build_readme_path(example_dir, readme_file)
        call build_output_path(example_name, output_file)
        
        call check_readme_exists(readme_file, file_exists)
        if (.not. file_exists) then
            print *, "Warning: README.md not found: ", trim(readme_file)
            return
        end if
        
        call open_output_file(output_file, unit_out, ios)
        if (ios /= 0) return
        
        call open_input_file(readme_file, unit_in, ios)
        if (ios /= 0) then
            close(unit_out)
            return
        end if
        
        call process_readme_content(unit_in, unit_out, example_dir, example_name)
        
        close(unit_in)
        close(unit_out)
    end subroutine process_example
    
    subroutine check_readme_exists(readme_file, file_exists)
        character(len=*), intent(in) :: readme_file
        logical, intent(out) :: file_exists
        
        inquire(file=trim(readme_file), exist=file_exists)
    end subroutine check_readme_exists
    
    subroutine open_output_file(output_file, unit_out, ios)
        character(len=*), intent(in) :: output_file
        integer, intent(out) :: unit_out, ios
        
        open(newunit=unit_out, file=trim(output_file), status='replace', iostat=ios)
        if (ios /= 0) then
            print *, "Error: Cannot create ", trim(output_file)
        end if
    end subroutine open_output_file
    
    subroutine open_input_file(readme_file, unit_in, ios)
        character(len=*), intent(in) :: readme_file
        integer, intent(out) :: unit_in, ios
        
        open(newunit=unit_in, file=trim(readme_file), status='old', iostat=ios)
        if (ios /= 0) then
            print *, "Error: Cannot read ", trim(readme_file)
        end if
    end subroutine open_input_file
    
    subroutine process_readme_content(unit_in, unit_out, example_dir, example_name)
        integer, intent(in) :: unit_in, unit_out
        character(len=*), intent(in) :: example_dir, example_name
        
        character(len=LINE_MAX_LEN) :: line
        logical :: in_output_section
        integer :: ios
        
        in_output_section = .false.
        call write_ford_front_matter(unit_out, example_name)
        
        do
            read(unit_in, '(A)', iostat=ios) line
            if (ios /= 0) exit
            
            call process_line(unit_in, unit_out, line, example_dir, example_name, &
                            in_output_section, ios)
            if (ios /= 0) exit
        end do
    end subroutine process_readme_content
    
    subroutine write_ford_front_matter(unit_out, example_name)
        integer, intent(in) :: unit_out
        character(len=*), intent(in) :: example_name
        
        write(unit_out, '(A)') 'title: ' // trim(title_case(example_name))
    end subroutine write_ford_front_matter
    
    subroutine process_line(unit_in, unit_out, line, example_dir, example_name, &
                          in_output_section, ios)
        integer, intent(in) :: unit_in, unit_out
        character(len=*), intent(inout) :: line
        character(len=*), intent(in) :: example_dir, example_name
        logical, intent(inout) :: in_output_section
        integer, intent(out) :: ios
        
        ios = 0
        
        if (should_skip_title_line(line)) return
        
        if (is_files_section(line)) then
            call handle_files_section(unit_in, unit_out, example_name, ios)
            return
        end if
        
        if (is_output_section(line)) then
            call handle_output_section(unit_out, example_dir, example_name)
            in_output_section = .true.
            return
        end if
        
        if (should_skip_output_content(line, in_output_section)) then
            if (is_new_section(line)) in_output_section = .false.
            return
        end if
        
        if (is_running_section(line)) then
            call skip_running_section(unit_in, ios)
            return
        end if
        
        call process_image_paths(line, example_name)
        write(unit_out, '(A)') trim(line)
    end subroutine process_line
    
    pure logical function should_skip_title_line(line)
        character(len=*), intent(in) :: line
        should_skip_title_line = (index(line, 'title:') == 1)
    end function should_skip_title_line
    
    pure logical function is_files_section(line)
        character(len=*), intent(in) :: line
        is_files_section = (index(line, '## Files') > 0)
    end function is_files_section
    
    pure logical function is_output_section(line)
        character(len=*), intent(in) :: line
        is_output_section = (index(line, '## Output Examples') > 0)
    end function is_output_section
    
    pure logical function is_running_section(line)
        character(len=*), intent(in) :: line
        is_running_section = (index(line, '## Running') > 0)
    end function is_running_section
    
    pure logical function is_new_section(line)
        character(len=*), intent(in) :: line
        is_new_section = (index(line, '##') == 1 .and. index(line, '## Output') == 0)
    end function is_new_section
    
    pure logical function should_skip_output_content(line, in_output_section)
        character(len=*), intent(in) :: line
        logical, intent(in) :: in_output_section
        should_skip_output_content = in_output_section .and. .not. is_new_section(line)
    end function should_skip_output_content
    
    subroutine handle_files_section(unit_in, unit_out, example_name, ios)
        integer, intent(in) :: unit_in, unit_out
        character(len=*), intent(in) :: example_name
        integer, intent(out) :: ios
        
        write(unit_out, '(A)') '## Source Files'
        write(unit_out, '(A)') ''
        call write_source_links(unit_out, example_name)
        call skip_original_files_section(unit_in, ios)
    end subroutine handle_files_section
    
    subroutine handle_output_section(unit_out, example_dir, example_name)
        integer, intent(in) :: unit_out
        character(len=*), intent(in) :: example_dir, example_name
        
        write(unit_out, '(A)') '## Output'
        write(unit_out, '(A)') ''
        call write_generated_outputs(unit_out, example_dir, example_name)
    end subroutine handle_output_section
    
    subroutine skip_original_files_section(unit_in, ios)
        integer, intent(in) :: unit_in
        integer, intent(out) :: ios
        
        character(len=LINE_MAX_LEN) :: line
        
        do
            read(unit_in, '(A)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, '##') == 1 .and. index(line, '## Files') == 0) then
                backspace(unit_in)
                ios = 0
                exit
            end if
        end do
    end subroutine skip_original_files_section
    
    subroutine skip_running_section(unit_in, ios)
        integer, intent(in) :: unit_in
        integer, intent(out) :: ios
        
        character(len=LINE_MAX_LEN) :: line
        
        do
            read(unit_in, '(A)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, '##') == 1 .and. index(line, '## Running') == 0) then
                backspace(unit_in)
                ios = 0
                exit
            end if
        end do
    end subroutine skip_running_section
    
    subroutine process_image_paths(line, example_name)
        character(len=*), intent(inout) :: line
        character(len=*), intent(in) :: example_name
        
        if (index(line, '![') > 0 .and. index(line, '](') > 0) then
            call process_image_path(line, example_name)
        end if
    end subroutine process_image_paths

end module fortplot_doc_processor
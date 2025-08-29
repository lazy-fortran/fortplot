module fortplot_doc_source
    !! Source code writing for documentation
    use fortplot_doc_constants, only: PATH_MAX_LEN, GITHUB_BASE_URL
    use fortplot_doc_examples, only: get_fortran_filename
    use fortplot_doc_paths, only: build_python_path, build_local_fortran_path, build_fortran_url
    use fortplot_doc_files, only: copy_file_content
    implicit none
    private
    
    ! Public interface
    public :: write_source_links
    
contains

    subroutine write_source_links(unit_out, example_name)
        integer, intent(in) :: unit_out
        character(len=*), intent(in) :: example_name
        
        character(len=PATH_MAX_LEN) :: fortran_file, fortran_path, python_path, local_fortran_path
        logical :: python_exists
        
        call get_fortran_filename(example_name, fortran_file)
        call build_fortran_url(example_name, fortran_path)
        call build_python_path(example_name, python_path)
        call build_local_fortran_path(example_name, local_fortran_path)
        
        inquire(file=trim(python_path), exist=python_exists)
        
        call write_source_section_header(unit_out)
        call write_fortran_link(unit_out, fortran_file, fortran_path)
        call write_python_link_if_exists(unit_out, example_name, python_path, python_exists)
        call write_source_code_block(unit_out, local_fortran_path)
    end subroutine write_source_links
    
    subroutine write_source_section_header(unit_out)
        integer, intent(in) :: unit_out
        
        write(unit_out, '(A)') '## Source Code'
        write(unit_out, '(A)') ''
    end subroutine write_source_section_header
    
    subroutine write_fortran_link(unit_out, fortran_file, fortran_path)
        integer, intent(in) :: unit_out
        character(len=*), intent(in) :: fortran_file, fortran_path
        
        write(unit_out, '(A)') '🔷 **Fortran:** [' // trim(fortran_file) // &
                              '](' // trim(fortran_path) // ')'
    end subroutine write_fortran_link
    
    subroutine write_python_link_if_exists(unit_out, example_name, python_path, python_exists)
        integer, intent(in) :: unit_out
        character(len=*), intent(in) :: example_name, python_path
        logical, intent(in) :: python_exists
        
        if (python_exists) then
            write(unit_out, '(A)') ''
            write(unit_out, '(A)') '🐍 **Python:** [' // trim(example_name) // &
                                  '.py](' // GITHUB_BASE_URL // trim(python_path) // ')'
        end if
        write(unit_out, '(A)') ''
    end subroutine write_python_link_if_exists
    
    subroutine write_source_code_block(unit_out, local_fortran_path)
        integer, intent(in) :: unit_out
        character(len=*), intent(in) :: local_fortran_path
        
        write(unit_out, '(A)') '```fortran'
        call copy_file_content(local_fortran_path, unit_out)
        write(unit_out, '(A)') '```'
        write(unit_out, '(A)') ''
    end subroutine write_source_code_block

end module fortplot_doc_source
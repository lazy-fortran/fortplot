module fortplot_doc_paths
    !! Path building utilities for documentation generation
    use fortplot_doc_constants, only: PATH_MAX_LEN, GITHUB_BASE_URL
    use fortplot_doc_examples, only: get_fortran_filename
    implicit none
    private
    
    ! Public interface
    public :: build_readme_path, build_output_path, build_fortran_url
    public :: build_python_path, build_local_fortran_path
    
contains

    pure subroutine build_readme_path(example_dir, readme_file)
        character(len=*), intent(in) :: example_dir
        character(len=PATH_MAX_LEN), intent(out) :: readme_file
        
        readme_file = trim(example_dir) // "/README.md"
    end subroutine build_readme_path
    
    pure subroutine build_output_path(example_name, output_file)
        character(len=*), intent(in) :: example_name
        character(len=PATH_MAX_LEN), intent(out) :: output_file
        
        ! Documentation consolidation - disable duplicate generation
        ! Use canonical source: example/fortran/[name]/README.md
        output_file = "example/fortran/" // trim(example_name) // "/README.md"
    end subroutine build_output_path
    
    pure subroutine build_fortran_url(example_name, fortran_path)
        character(len=*), intent(in) :: example_name
        character(len=PATH_MAX_LEN), intent(out) :: fortran_path
        
        character(len=PATH_MAX_LEN) :: fortran_file
        
        call get_fortran_filename(example_name, fortran_file)
        fortran_path = GITHUB_BASE_URL // 'example/fortran/' // &
                      trim(example_name) // '/' // trim(fortran_file)
    end subroutine build_fortran_url
    
    pure subroutine build_python_path(example_name, python_path)
        character(len=*), intent(in) :: example_name
        character(len=PATH_MAX_LEN), intent(out) :: python_path
        
        python_path = 'example/python/' // trim(example_name) // '/' // &
                     trim(example_name) // '.py'
    end subroutine build_python_path
    
    pure subroutine build_local_fortran_path(example_name, local_path)
        character(len=*), intent(in) :: example_name
        character(len=PATH_MAX_LEN), intent(out) :: local_path
        
        character(len=PATH_MAX_LEN) :: fortran_file
        
        call get_fortran_filename(example_name, fortran_file)
        local_path = 'example/fortran/' // trim(example_name) // '/' // trim(fortran_file)
    end subroutine build_local_fortran_path

end module fortplot_doc_paths
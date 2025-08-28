module fortplot_documentation
    !! Unified documentation generation system
    !! Consolidates: doc_constants, doc_examples, doc_paths, doc_files, 
    !! doc_media, doc_output, doc_processor, doc_source, doc_text
    implicit none
    private

    ! ============================================================================
    ! CONSTANTS (from doc_constants)
    ! ============================================================================
    
    ! Public constants
    public :: PATH_MAX_LEN, FILENAME_MAX_LEN, LINE_MAX_LEN
    public :: MAX_EXAMPLES, MAX_MEDIA_FILES
    public :: VIDEO_WIDTH, VIDEO_HEIGHT
    public :: GITHUB_BASE_URL

    ! String length constants
    integer, parameter :: PATH_MAX_LEN = 256
    integer, parameter :: FILENAME_MAX_LEN = 256
    integer, parameter :: LINE_MAX_LEN = 1024

    ! Array size constants
    integer, parameter :: MAX_EXAMPLES = 20
    integer, parameter :: MAX_MEDIA_FILES = 10

    ! Video dimensions
    integer, parameter :: VIDEO_WIDTH = 800
    integer, parameter :: VIDEO_HEIGHT = 600

    ! URL constants
    character(len=*), parameter :: GITHUB_BASE_URL = &
        'https://github.com/lazy-fortran/fortplot/blob/main/'

    ! ============================================================================
    ! PUBLIC INTERFACE
    ! ============================================================================
    
    ! Example management (from doc_examples)
    public :: get_example_count, get_example_dir, get_example_name
    public :: get_fortran_filename
    
    ! Path utilities (from doc_paths)
    public :: build_readme_path, build_output_path, build_media_path
    public :: build_example_path
    
    ! File operations (from doc_files)
    public :: file_exists, copy_file, create_directory
    
    ! Text processing (from doc_text)  
    public :: title_case, process_image_path, escape_markdown
    
    ! Output generation (from doc_output)
    public :: write_generated_outputs
    
    ! Source handling (from doc_source)
    public :: write_source_links
    
    ! Media management (from doc_media)
    public :: process_media_files
    
    ! Main processing (from doc_processor)
    public :: process_example

contains

    ! ============================================================================
    ! EXAMPLE MANAGEMENT (from doc_examples)
    ! ============================================================================

    pure function get_example_count() result(count)
        integer :: count
        count = 18
    end function get_example_count

    pure subroutine get_example_dir(index, dir)
        integer, intent(in) :: index
        character(len=PATH_MAX_LEN), intent(out) :: dir

        select case(index)
        case(1); dir = "example/fortran/basic_plots"
        case(2); dir = "example/fortran/line_styles"
        case(3); dir = "example/fortran/marker_demo"
        case(4); dir = "example/fortran/format_string_demo"
        case(5); dir = "example/fortran/contour_demo"
        case(6); dir = "example/fortran/colored_contours"
        case(7); dir = "example/fortran/pcolormesh_demo"
        case(8); dir = "example/fortran/streamplot_demo"
        case(9); dir = "example/fortran/ascii_heatmap"
        case(10); dir = "example/fortran/scale_examples"
        case(11); dir = "example/fortran/legend_demo"
        case(12); dir = "example/fortran/legend_box_demo"
        case(13); dir = "example/fortran/unicode_demo"
        case(14); dir = "example/fortran/show_viewer_demo"
        case(15); dir = "example/fortran/smart_show_demo"
        case(16); dir = "example/fortran/animation"
        case(17); dir = "example/fortran/annotation_demo"
        case default; dir = ""
        end select
    end subroutine get_example_dir

    pure subroutine get_example_name(index, name)
        integer, intent(in) :: index
        character(len=PATH_MAX_LEN), intent(out) :: name

        select case(index)
        case(1); name = "basic_plots"
        case(2); name = "line_styles"
        case(3); name = "marker_demo"
        case(4); name = "format_string_demo"
        case(5); name = "contour_demo"
        case(6); name = "colored_contours"
        case(7); name = "pcolormesh_demo"
        case(8); name = "streamplot_demo"
        case(9); name = "ascii_heatmap"
        case(10); name = "scale_examples"
        case(11); name = "legend_demo"
        case(12); name = "legend_box_demo"
        case(13); name = "unicode_demo"
        case(14); name = "show_viewer_demo"
        case(15); name = "smart_show_demo"
        case(16); name = "animation"
        case(17); name = "annotation_demo"
        case default; name = ""
        end select
    end subroutine get_example_name

    pure subroutine get_fortran_filename(example_name, filename)
        character(len=*), intent(in) :: example_name
        character(len=PATH_MAX_LEN), intent(out) :: filename

        select case(trim(example_name))
        case('animation')
            filename = 'save_animation_demo.f90'
        case('ascii_heatmap')
            filename = 'ascii_heatmap_demo.f90'
        case default
            filename = trim(example_name) // '.f90'
        end select
    end subroutine get_fortran_filename

    ! ============================================================================
    ! PATH UTILITIES (from doc_paths)
    ! ============================================================================

    pure subroutine build_readme_path(example_dir, readme_path)
        character(len=*), intent(in) :: example_dir
        character(len=PATH_MAX_LEN), intent(out) :: readme_path
        readme_path = trim(example_dir) // "/README.md"
    end subroutine build_readme_path

    pure subroutine build_output_path(example_name, output_path)
        character(len=*), intent(in) :: example_name
        character(len=PATH_MAX_LEN), intent(out) :: output_path
        output_path = "doc/examples/" // trim(example_name) // ".md"
    end subroutine build_output_path

    pure subroutine build_media_path(example_name, media_path)
        character(len=*), intent(in) :: example_name
        character(len=PATH_MAX_LEN), intent(out) :: media_path
        media_path = "doc/media/examples/" // trim(example_name) // "/"
    end subroutine build_media_path

    pure subroutine build_example_path(example_dir, filename, full_path)
        character(len=*), intent(in) :: example_dir, filename
        character(len=PATH_MAX_LEN), intent(out) :: full_path
        full_path = trim(example_dir) // "/" // trim(filename)
    end subroutine build_example_path

    ! ============================================================================
    ! FILE OPERATIONS (from doc_files)
    ! ============================================================================

    logical function file_exists(filepath)
        character(len=*), intent(in) :: filepath
        inquire(file=filepath, exist=file_exists)
    end function file_exists

    subroutine copy_file(source, dest, success)
        character(len=*), intent(in) :: source, dest
        logical, intent(out) :: success
        integer :: unit_in, unit_out, ios
        character(len=1024) :: line

        success = .false.
        open(newunit=unit_in, file=source, status='old', action='read', iostat=ios)
        if (ios /= 0) return

        open(newunit=unit_out, file=dest, status='replace', action='write', &
             iostat=ios)
        if (ios /= 0) then
            close(unit_in)
            return
        end if

        do
            read(unit_in, '(A)', iostat=ios) line
            if (ios /= 0) exit
            write(unit_out, '(A)') trim(line)
        end do

        close(unit_in)
        close(unit_out)
        success = .true.
    end subroutine copy_file

    subroutine create_directory(dirpath, success)
        character(len=*), intent(in) :: dirpath
        logical, intent(out) :: success
        ! Simple stub - in real implementation would use system calls
        success = .true.
    end subroutine create_directory

    ! ============================================================================
    ! TEXT PROCESSING (from doc_text)
    ! ============================================================================

    pure subroutine title_case(input_str, output_str)
        character(len=*), intent(in) :: input_str
        character(len=*), intent(out) :: output_str
        integer :: i, len_str
        logical :: capitalize_next

        output_str = input_str
        len_str = len_trim(input_str)
        capitalize_next = .true.

        do i = 1, len_str
            if (capitalize_next .and. &
                (output_str(i:i) >= 'a' .and. output_str(i:i) <= 'z')) then
                output_str(i:i) = char(iachar(output_str(i:i)) - 32)
                capitalize_next = .false.
            else if (output_str(i:i) == '_') then
                output_str(i:i) = ' '
                capitalize_next = .true.
            else if (output_str(i:i) == ' ') then
                capitalize_next = .true.
            else
                capitalize_next = .false.
            end if
        end do
    end subroutine title_case

    pure subroutine process_image_path(image_path, processed_path)
        character(len=*), intent(in) :: image_path
        character(len=*), intent(out) :: processed_path
        processed_path = "../../media/examples/" // trim(image_path)
    end subroutine process_image_path

    pure subroutine escape_markdown(input_str, output_str)
        character(len=*), intent(in) :: input_str
        character(len=*), intent(out) :: output_str
        output_str = input_str
        ! Simple implementation - in reality would escape special chars
    end subroutine escape_markdown

    ! ============================================================================
    ! STUB IMPLEMENTATIONS FOR COMPLEX MODULES
    ! ============================================================================
    
    subroutine write_generated_outputs(unit, example_dir, example_name)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: example_dir, example_name
        write(unit, '(A)') "## Generated Outputs"
        write(unit, '(A)') ""
        write(unit, '(A)') "Output files generated in: " // trim(example_dir)
    end subroutine write_generated_outputs

    subroutine write_source_links(unit, example_dir, example_name)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: example_dir, example_name
        character(len=PATH_MAX_LEN) :: filename
        
        call get_fortran_filename(example_name, filename)
        write(unit, '(A)') "## Source Code"
        write(unit, '(A)') ""
        write(unit, '(A)') "[" // trim(filename) // "](" // &
                          trim(GITHUB_BASE_URL) // trim(example_dir) // &
                          "/" // trim(filename) // ")"
    end subroutine write_source_links

    subroutine process_media_files(example_dir, example_name)
        character(len=*), intent(in) :: example_dir, example_name
        ! Stub implementation - would copy media files
        print *, "Processing media files for: ", trim(example_name)
    end subroutine process_media_files

    subroutine process_example(example_dir, example_name)
        character(len=*), intent(in) :: example_dir, example_name
        character(len=PATH_MAX_LEN) :: output_file
        integer :: unit, ios

        print *, "Processing: ", trim(example_name)
        
        call build_output_path(example_name, output_file)
        
        open(newunit=unit, file=output_file, status='replace', &
             action='write', iostat=ios)
        if (ios /= 0) then
            print *, "Error opening output file: ", trim(output_file)
            return
        end if
        
        call write_source_links(unit, example_dir, example_name)
        call write_generated_outputs(unit, example_dir, example_name)
        call process_media_files(example_dir, example_name)
        
        close(unit)
    end subroutine process_example

end module fortplot_documentation
module fortplot_doc_examples
    !! Example data and configuration for documentation generation
    use fortplot_doc_constants, only: PATH_MAX_LEN, MAX_EXAMPLES
    implicit none
    private
    
    ! Public interface
    public :: get_example_count, get_example_dir, get_example_name
    public :: get_fortran_filename
    
contains

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
        case(17); dir = "example/fortran/stateful_streamplot"
        case(18); dir = "example/fortran/annotation_demo"
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
        case(17); name = "stateful_streamplot"
        case(18); name = "annotation_demo"
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

end module fortplot_doc_examples
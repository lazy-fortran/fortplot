module fortplot_doc_media
    !! Media file scanning for documentation generation
    use fortplot_doc_constants, only: FILENAME_MAX_LEN, PATH_MAX_LEN, MAX_MEDIA_FILES
    implicit none
    private
    
    ! Public interface
    public :: scan_directory_for_media, add_if_exists
    
contains

    subroutine scan_directory_for_media(dir, media_files, n_media)
        character(len=*), intent(in) :: dir
        character(len=*), intent(out) :: media_files(:)
        integer, intent(out) :: n_media
        
        n_media = 0
        
        select case(trim(dir))
        case('output/example/fortran/animation')
            call add_animation_files(dir, media_files, n_media)
        case('output/example/fortran/basic_plots')
            call add_basic_plot_files(dir, media_files, n_media)
        case('output/example/fortran/line_styles')
            call add_line_style_files(dir, media_files, n_media)
        case('output/example/fortran/contour_demo')
            call add_contour_files(dir, media_files, n_media)
        case('output/example/fortran/scale_examples')
            call add_scale_files(dir, media_files, n_media)
        case('output/example/fortran/marker_demo')
            call add_marker_files(dir, media_files, n_media)
        case('output/example/fortran/colored_contours')
            call add_colored_contour_files(dir, media_files, n_media)
        case('output/example/fortran/legend_demo')
            call add_legend_files(dir, media_files, n_media)
        case('output/example/fortran/legend_box_demo')
            call add_legend_box_files(dir, media_files, n_media)
        case('output/example/fortran/format_string_demo')
            call add_format_string_files(dir, media_files, n_media)
        case('output/example/fortran/pcolormesh_demo')
            call add_pcolormesh_files(dir, media_files, n_media)
        case('output/example/fortran/streamplot_demo')
            call add_streamplot_files(dir, media_files, n_media)
        case('output/example/fortran/unicode_demo')
            call add_unicode_files(dir, media_files, n_media)
        case('output/example/fortran/stateful_streamplot')
            call add_stateful_streamplot_files(dir, media_files, n_media)
        case default
            call add_default_pattern_files(dir, media_files, n_media)
        end select
    end subroutine scan_directory_for_media
    
    subroutine add_animation_files(dir, media_files, n_media)
        character(len=*), intent(in) :: dir
        character(len=*), intent(out) :: media_files(:)
        integer, intent(out) :: n_media
        
        call add_if_exists(dir, 'wave_animation.mp4', media_files, n_media)
    end subroutine add_animation_files
    
    subroutine add_basic_plot_files(dir, media_files, n_media)
        character(len=*), intent(in) :: dir
        character(len=*), intent(out) :: media_files(:)
        integer, intent(out) :: n_media
        
        call add_if_exists(dir, 'simple_plot.png', media_files, n_media)
        call add_if_exists(dir, 'multi_line.png', media_files, n_media)
    end subroutine add_basic_plot_files
    
    subroutine add_line_style_files(dir, media_files, n_media)
        character(len=*), intent(in) :: dir
        character(len=*), intent(out) :: media_files(:)
        integer, intent(out) :: n_media
        
        call add_if_exists(dir, 'line_styles.png', media_files, n_media)
    end subroutine add_line_style_files
    
    subroutine add_contour_files(dir, media_files, n_media)
        character(len=*), intent(in) :: dir
        character(len=*), intent(out) :: media_files(:)
        integer, intent(out) :: n_media
        
        call add_if_exists(dir, 'contour_gaussian.png', media_files, n_media)
        call add_if_exists(dir, 'mixed_plot.png', media_files, n_media)
    end subroutine add_contour_files
    
    subroutine add_scale_files(dir, media_files, n_media)
        character(len=*), intent(in) :: dir
        character(len=*), intent(out) :: media_files(:)
        integer, intent(out) :: n_media
        
        call add_if_exists(dir, 'log_scale.png', media_files, n_media)
        call add_if_exists(dir, 'symlog_scale.png', media_files, n_media)
    end subroutine add_scale_files
    
    subroutine add_marker_files(dir, media_files, n_media)
        character(len=*), intent(in) :: dir
        character(len=*), intent(out) :: media_files(:)
        integer, intent(out) :: n_media
        
        call add_if_exists(dir, 'scatter_plot.png', media_files, n_media)
        call add_if_exists(dir, 'all_marker_types.png', media_files, n_media)
        call add_if_exists(dir, 'marker_colors.png', media_files, n_media)
    end subroutine add_marker_files
    
    subroutine add_colored_contour_files(dir, media_files, n_media)
        character(len=*), intent(in) :: dir
        character(len=*), intent(out) :: media_files(:)
        integer, intent(out) :: n_media
        
        call add_if_exists(dir, 'gaussian_default.png', media_files, n_media)
        call add_if_exists(dir, 'ripple_jet.png', media_files, n_media)
        call add_if_exists(dir, 'ripple_coolwarm.png', media_files, n_media)
        call add_if_exists(dir, 'ripple_inferno.png', media_files, n_media)
        call add_if_exists(dir, 'saddle_plasma.png', media_files, n_media)
    end subroutine add_colored_contour_files
    
    subroutine add_legend_files(dir, media_files, n_media)
        character(len=*), intent(in) :: dir
        character(len=*), intent(out) :: media_files(:)
        integer, intent(out) :: n_media
        
        call add_if_exists(dir, 'basic_legend.png', media_files, n_media)
        call add_if_exists(dir, 'legend_upper_left.png', media_files, n_media)
        call add_if_exists(dir, 'legend_upper_right.png', media_files, n_media)
        call add_if_exists(dir, 'legend_lower_left.png', media_files, n_media)
        call add_if_exists(dir, 'legend_lower_right.png', media_files, n_media)
        call add_if_exists(dir, 'multi_function_legend.png', media_files, n_media)
    end subroutine add_legend_files
    
    subroutine add_legend_box_files(dir, media_files, n_media)
        character(len=*), intent(in) :: dir
        character(len=*), intent(out) :: media_files(:)
        integer, intent(out) :: n_media
        
        call add_if_exists(dir, 'legend_box_demo_default.png', media_files, n_media)
        call add_if_exists(dir, 'legend_box_demo_upper_left.png', media_files, n_media)
        call add_if_exists(dir, 'legend_box_demo_lower_right.png', media_files, n_media)
    end subroutine add_legend_box_files
    
    subroutine add_format_string_files(dir, media_files, n_media)
        character(len=*), intent(in) :: dir
        character(len=*), intent(out) :: media_files(:)
        integer, intent(out) :: n_media
        
        call add_if_exists(dir, 'format_string_demo.png', media_files, n_media)
    end subroutine add_format_string_files
    
    subroutine add_pcolormesh_files(dir, media_files, n_media)
        character(len=*), intent(in) :: dir
        character(len=*), intent(out) :: media_files(:)
        integer, intent(out) :: n_media
        
        call add_if_exists(dir, 'pcolormesh_basic.png', media_files, n_media)
        call add_if_exists(dir, 'pcolormesh_plasma.png', media_files, n_media)
        call add_if_exists(dir, 'pcolormesh_sinusoidal.png', media_files, n_media)
    end subroutine add_pcolormesh_files
    
    subroutine add_streamplot_files(dir, media_files, n_media)
        character(len=*), intent(in) :: dir
        character(len=*), intent(out) :: media_files(:)
        integer, intent(out) :: n_media
        
        call add_if_exists(dir, 'streamplot_demo.png', media_files, n_media)
    end subroutine add_streamplot_files
    
    subroutine add_unicode_files(dir, media_files, n_media)
        character(len=*), intent(in) :: dir
        character(len=*), intent(out) :: media_files(:)
        integer, intent(out) :: n_media
        
        call add_if_exists(dir, 'unicode_demo.png', media_files, n_media)
        call add_if_exists(dir, 'math_examples.png', media_files, n_media)
    end subroutine add_unicode_files
    
    subroutine add_stateful_streamplot_files(dir, media_files, n_media)
        character(len=*), intent(in) :: dir
        character(len=*), intent(out) :: media_files(:)
        integer, intent(out) :: n_media
        
        call add_if_exists(dir, 'stateful_streamplot.png', media_files, n_media)
    end subroutine add_stateful_streamplot_files
    
    subroutine add_default_pattern_files(dir, media_files, n_media)
        character(len=*), intent(in) :: dir
        character(len=*), intent(out) :: media_files(:)
        integer, intent(out) :: n_media
        
        character(len=FILENAME_MAX_LEN) :: test_file
        
        test_file = dir(index(dir, '/', back=.true.)+1:) // '.png'
        call add_if_exists(dir, test_file, media_files, n_media)
    end subroutine add_default_pattern_files
    
    subroutine add_if_exists(dir, filename, files, n)
        character(len=*), intent(in) :: dir, filename
        character(len=*), intent(inout) :: files(:)
        integer, intent(inout) :: n
        
        character(len=PATH_MAX_LEN) :: full_path
        logical :: exists
        
        full_path = trim(dir) // '/' // trim(filename)
        inquire(file=trim(full_path), exist=exists)
        if (exists .and. n < size(files)) then
            n = n + 1
            files(n) = filename
        end if
    end subroutine add_if_exists

end module fortplot_doc_media
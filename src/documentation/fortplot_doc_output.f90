module fortplot_doc_output
    !! Output generation for documentation
    use fortplot_doc_core, only: &
        FILENAME_MAX_LEN, LINE_MAX_LEN, VIDEO_WIDTH, VIDEO_HEIGHT, &
        get_file_extension, replace_extension, copy_file_content, get_output_title, &
        PATH_MAX_LEN
    implicit none
    private

    ! Public interface
    public :: write_generated_outputs

    ! Internal helper made public for consolidation within this module
    public :: scan_directory_for_media

contains

    subroutine write_generated_outputs(unit_out, example_dir, example_name)
        integer, intent(in) :: unit_out
        character(len=*), intent(in) :: example_dir, example_name
        
        character(len=FILENAME_MAX_LEN) :: media_files(10)
        integer :: n_media, j
        
        call find_output_files(example_dir, example_name, media_files, n_media)
        
        do j = 1, n_media
            call write_media_output(unit_out, example_name, media_files(j))
            write(unit_out, '(A)') ''
        end do
    end subroutine write_generated_outputs
    
    subroutine find_output_files(example_dir, example_name, media_files, n_media)
        character(len=*), intent(in) :: example_dir, example_name
        character(len=*), intent(out) :: media_files(:)
        integer, intent(out) :: n_media
        
        character(len=FILENAME_MAX_LEN) :: output_dir
        
        output_dir = 'output/example/fortran/' // trim(example_name)
        call scan_directory_for_media(output_dir, media_files, n_media)
    end subroutine find_output_files
    
    subroutine write_media_output(unit_out, example_name, media_file)
        integer, intent(in) :: unit_out
        character(len=*), intent(in) :: example_name, media_file
        
        character(len=10) :: extension
        
        extension = get_file_extension(media_file)
        
        call write_output_title(unit_out, media_file)
        
        if (extension == 'mp4') then
            call write_video_output(unit_out, example_name, media_file)
        else
            call write_image_output(unit_out, example_name, media_file)
        end if
    end subroutine write_media_output
    
    subroutine write_output_title(unit_out, media_file)
        integer, intent(in) :: unit_out
        character(len=*), intent(in) :: media_file
        
        write(unit_out, '(A)') '### ' // trim(get_output_title(media_file))
        write(unit_out, '(A)') ''
    end subroutine write_output_title
    
    subroutine write_video_output(unit_out, example_name, media_file)
        integer, intent(in) :: unit_out
        character(len=*), intent(in) :: example_name, media_file
        
        call write_video_html(unit_out, example_name, media_file)
        call write_video_download_link(unit_out, example_name, media_file)
    end subroutine write_video_output
    
    subroutine write_video_html(unit_out, example_name, media_file)
        integer, intent(in) :: unit_out
        character(len=*), intent(in) :: example_name, media_file
        
        write(unit_out, '(A,I0,A,I0,A)') '<video width="', VIDEO_WIDTH, &
                                       '" height="', VIDEO_HEIGHT, '" controls>'
        write(unit_out, '(A)') '  <source src="../../media/examples/' // &
                              trim(example_name) // '/' // trim(media_file) // &
                              '" type="video/mp4">'
        write(unit_out, '(A)') '  Your browser does not support the video tag.'
        write(unit_out, '(A)') '</video>'
        write(unit_out, '(A)') ''
    end subroutine write_video_html
    
    subroutine write_video_download_link(unit_out, example_name, media_file)
        integer, intent(in) :: unit_out
        character(len=*), intent(in) :: example_name, media_file
        
        write(unit_out, '(A)') '[Download MP4](../../media/examples/' // &
                              trim(example_name) // '/' // trim(media_file) // ')'
    end subroutine write_video_download_link
    
    subroutine write_image_output(unit_out, example_name, media_file)
        integer, intent(in) :: unit_out
        character(len=*), intent(in) :: example_name, media_file
        
        call write_image_link(unit_out, example_name, media_file)
        call write_ascii_output_if_exists(unit_out, example_name, media_file)
        call write_pdf_download_link(unit_out, example_name, media_file)
    end subroutine write_image_output
    
    subroutine write_image_link(unit_out, example_name, media_file)
        integer, intent(in) :: unit_out
        character(len=*), intent(in) :: example_name, media_file
        
        write(unit_out, '(A)') '![' // trim(media_file) // &
                              '](../../media/examples/' // trim(example_name) // &
                              '/' // trim(media_file) // ')'
        write(unit_out, '(A)') ''
    end subroutine write_image_link
    
    subroutine write_ascii_output_if_exists(unit_out, example_name, media_file)
        integer, intent(in) :: unit_out
        character(len=*), intent(in) :: example_name, media_file
        
        character(len=FILENAME_MAX_LEN) :: ascii_file
        logical :: file_exists
        
        call build_ascii_file_path(example_name, media_file, ascii_file)
        inquire(file=trim(ascii_file), exist=file_exists)
        
        if (file_exists) then
            call write_ascii_content(unit_out, ascii_file)
        end if
    end subroutine write_ascii_output_if_exists
    
    subroutine build_ascii_file_path(example_name, media_file, ascii_file)
        character(len=*), intent(in) :: example_name, media_file
        character(len=FILENAME_MAX_LEN), intent(out) :: ascii_file
        
        ascii_file = 'output/example/fortran/' // trim(example_name) // '/' // &
                    replace_extension(media_file, 'txt')
    end subroutine build_ascii_file_path
    
    subroutine write_ascii_content(unit_out, ascii_file)
        integer, intent(in) :: unit_out
        character(len=*), intent(in) :: ascii_file
        
        write(unit_out, '(A)') 'ASCII output:'
        write(unit_out, '(A)') '```'
        call copy_file_content(ascii_file, unit_out)
        write(unit_out, '(A)') '```'
        write(unit_out, '(A)') ''
    end subroutine write_ascii_content
    
    subroutine write_pdf_download_link(unit_out, example_name, media_file)
        integer, intent(in) :: unit_out
        character(len=*), intent(in) :: example_name, media_file

        write(unit_out, '(A)') '[Download PDF](../../media/examples/' // &
                              trim(example_name) // '/' // &
                              trim(replace_extension(media_file, 'pdf')) // ')'
    end subroutine write_pdf_download_link

    ! --------------------
    ! Media file scanning (inlined from former media module)
    ! --------------------

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
        case('output/example/fortran/annotation_demo')
            call add_annotation_demo_files(dir, media_files, n_media)
        case default
            call add_default_pattern_files(dir, media_files, n_media)
        end select
    end subroutine scan_directory_for_media

    subroutine add_animation_files(dir, media_files, n_media)
        character(len=*), intent(in) :: dir
        character(len=*), intent(out) :: media_files(:)
        integer, intent(out) :: n_media

        call add_if_exists(dir, 'animation.mp4', media_files, n_media)
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

    subroutine add_annotation_demo_files(dir, media_files, n_media)
        character(len=*), intent(in) :: dir
        character(len=*), intent(out) :: media_files(:)
        integer, intent(out) :: n_media

        call add_if_exists(dir, 'annotation_demo.png', media_files, n_media)
    end subroutine add_annotation_demo_files

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

end module fortplot_doc_output

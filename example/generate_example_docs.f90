program generate_example_docs
    !! Generate FORD documentation pages for fortplotlib examples
    implicit none
    
    character(len=256) :: example_dirs(20)
    character(len=256) :: example_names(20)
    integer :: n_examples, i
    
    ! List of examples to document
    n_examples = 17
    example_dirs(1) = "example/fortran/basic_plots"
    example_names(1) = "basic_plots"
    
    example_dirs(2) = "example/fortran/line_styles"
    example_names(2) = "line_styles"
    
    example_dirs(3) = "example/fortran/marker_demo"
    example_names(3) = "marker_demo"
    
    example_dirs(4) = "example/fortran/format_string_demo"
    example_names(4) = "format_string_demo"
    
    example_dirs(5) = "example/fortran/contour_demo"
    example_names(5) = "contour_demo"
    
    example_dirs(6) = "example/fortran/colored_contours"
    example_names(6) = "colored_contours"
    
    example_dirs(7) = "example/fortran/pcolormesh_demo"
    example_names(7) = "pcolormesh_demo"
    
    example_dirs(8) = "example/fortran/streamplot_demo"
    example_names(8) = "streamplot_demo"
    
    example_dirs(9) = "example/fortran/ascii_heatmap"
    example_names(9) = "ascii_heatmap"
    
    example_dirs(10) = "example/fortran/scale_examples"
    example_names(10) = "scale_examples"
    
    example_dirs(11) = "example/fortran/legend_demo"
    example_names(11) = "legend_demo"
    
    example_dirs(12) = "example/fortran/legend_box_demo"
    example_names(12) = "legend_box_demo"
    
    example_dirs(13) = "example/fortran/unicode_demo"
    example_names(13) = "unicode_demo"
    
    example_dirs(14) = "example/fortran/show_viewer_demo"
    example_names(14) = "show_viewer_demo"
    
    example_dirs(15) = "example/fortran/smart_show_demo"
    example_names(15) = "smart_show_demo"
    
    example_dirs(16) = "example/fortran/animation"
    example_names(16) = "animation"
    
    example_dirs(17) = "example/fortran/stateful_streamplot"
    example_names(17) = "stateful_streamplot"
    
    print *, "Generating example documentation..."
    
    ! Process each example
    do i = 1, n_examples
        call process_example(trim(example_dirs(i)), trim(example_names(i)))
    end do
    
    print *, "Documentation generation complete!"
    
contains

    subroutine process_example(example_dir, example_name)
        character(len=*), intent(in) :: example_dir
        character(len=*), intent(in) :: example_name
        
        character(len=1024) :: line
        character(len=256) :: readme_file, output_file
        integer :: unit_in, unit_out, ios
        logical :: file_exists, in_output_section
        
        print *, "Processing: ", trim(example_name)
        
        ! Build file paths
        readme_file = trim(example_dir) // "/README.md"
        output_file = "doc/examples/" // trim(example_name) // ".md"
        
        ! Check if README exists
        inquire(file=trim(readme_file), exist=file_exists)
        if (.not. file_exists) then
            print *, "Warning: README.md not found: ", trim(readme_file)
            return
        end if
        
        ! Open output markdown file
        open(newunit=unit_out, file=trim(output_file), status='replace', iostat=ios)
        if (ios /= 0) then
            print *, "Error: Cannot create ", trim(output_file)
            return
        end if
        
        ! Process README and copy content with modifications
        open(newunit=unit_in, file=trim(readme_file), status='old', iostat=ios)
        if (ios /= 0) then
            print *, "Error: Cannot read ", trim(readme_file)
            close(unit_out)
            return
        end if
        
        in_output_section = .false.
        
        ! Write FORD front matter first
        write(unit_out, '(A)') 'title: ' // trim(title_case(example_name))
        
        do
            read(unit_in, '(A)', iostat=ios) line
            if (ios /= 0) exit
            
            ! Skip the title line from README
            if (index(line, 'title:') == 1) cycle
            
            ! Check if we're entering the files section
            if (index(line, '## Files') > 0) then
                ! Write enhanced files section with links
                write(unit_out, '(A)') '## Source Files'
                write(unit_out, '(A)') ''
                call write_source_links(unit_out, example_name)
                ! Skip original files section
                do
                    read(unit_in, '(A)', iostat=ios) line
                    if (ios /= 0) exit
                    if (index(line, '##') == 1 .and. index(line, '## Files') == 0) then
                        ! Process this line in the main loop
                        backspace(unit_in)
                        exit
                    end if
                end do
                cycle
            end if
            
            ! Check if we're entering the output examples section
            if (index(line, '## Output Examples') > 0) then
                in_output_section = .true.
                ! Write the output section with generated content
                write(unit_out, '(A)') '## Output'
                write(unit_out, '(A)') ''
                call write_generated_outputs(unit_out, example_dir, example_name)
                ! Skip until next section
                cycle
            end if
            
            ! Skip content in output section
            if (in_output_section) then
                if (index(line, '##') == 1 .and. index(line, '## Output') == 0) then
                    in_output_section = .false.
                else
                    cycle
                end if
            end if
            
            ! Skip "Running" section
            if (index(line, '## Running') > 0) then
                ! Skip until next section
                do
                    read(unit_in, '(A)', iostat=ios) line
                    if (ios /= 0) exit
                    if (index(line, '##') == 1 .and. index(line, '## Running') == 0) then
                        backspace(unit_in)
                        exit
                    end if
                end do
                cycle
            end if
            
            ! Replace relative image paths with correct paths for docs
            if (index(line, '![') > 0 .and. index(line, '](') > 0) then
                call process_image_path(line)
            end if
            
            ! Write the line
            write(unit_out, '(A)') trim(line)
        end do
        
        close(unit_in)
        close(unit_out)
        
    end subroutine process_example
    
    subroutine write_source_links(unit_out, example_name)
        integer, intent(in) :: unit_out
        character(len=*), intent(in) :: example_name
        
        character(len=256) :: fortran_path, fortran_file, python_path, local_fortran_path
        character(len=1024) :: line
        logical :: python_exists
        integer :: unit_src, ios
        
        ! Build paths
        select case(example_name)
        case('animation')
            fortran_file = 'save_animation_demo.f90'
            fortran_path = 'https://github.com/krystophny/fortplotlib/blob/main/example/fortran/' // &
                           trim(example_name) // '/' // trim(fortran_file)
            local_fortran_path = 'example/fortran/' // trim(example_name) // '/' // trim(fortran_file)
        case('ascii_heatmap')
            fortran_file = 'ascii_heatmap_demo.f90'
            fortran_path = 'https://github.com/krystophny/fortplotlib/blob/main/example/fortran/' // &
                           trim(example_name) // '/' // trim(fortran_file)
            local_fortran_path = 'example/fortran/' // trim(example_name) // '/' // trim(fortran_file)
        case default
            fortran_file = trim(example_name) // '.f90'
            fortran_path = 'https://github.com/krystophny/fortplotlib/blob/main/example/fortran/' // &
                           trim(example_name) // '/' // trim(fortran_file)
            local_fortran_path = 'example/fortran/' // trim(example_name) // '/' // trim(fortran_file)
        end select
        python_path = 'example/python/' // trim(example_name) // '/' // trim(example_name) // '.py'
        
        ! Check if Python example exists
        inquire(file=trim(python_path), exist=python_exists)
        
        ! Write section header
        write(unit_out, '(A)') '## Source Code'
        write(unit_out, '(A)') ''
        
        ! Write Fortran link
        write(unit_out, '(A)') '### Fortran Implementation'
        write(unit_out, '(A)') ''
        write(unit_out, '(A)') '📄 View on GitHub: [' // trim(fortran_file) // '](' // trim(fortran_path) // ')'
        write(unit_out, '(A)') ''
        
        ! Write Python link if exists
        if (python_exists) then
            write(unit_out, '(A)') '🐍 Python equivalent: [' // trim(example_name) // '.py](https://github.com/krystophny/fortplotlib/blob/main/' // &
                                   trim(python_path) // ')'
            write(unit_out, '(A)') ''
        end if
        
        ! Include the source code
        write(unit_out, '(A)') '```fortran'
        open(newunit=unit_src, file=trim(local_fortran_path), status='old', iostat=ios)
        if (ios == 0) then
            do
                read(unit_src, '(A)', iostat=ios) line
                if (ios /= 0) exit
                write(unit_out, '(A)') trim(line)
            end do
            close(unit_src)
        end if
        write(unit_out, '(A)') '```'
        write(unit_out, '(A)') ''
        
    end subroutine write_source_links
    
    subroutine write_generated_outputs(unit_out, example_dir, example_name)
        integer, intent(in) :: unit_out
        character(len=*), intent(in) :: example_dir, example_name
        
        character(len=256) :: media_files(10), pdf_files(10), txt_files(10)
        character(len=256) :: ascii_file
        character(len=1024) :: line
        integer :: n_media, n_pdf, n_txt, j, unit_ascii, ios
        logical :: file_exists
        character(len=10) :: extension
        
        ! Find output files
        call find_output_files(example_dir, example_name, media_files, n_media, &
                              pdf_files, n_pdf, txt_files, n_txt)
        
        ! Add media outputs (PNG or MP4)
        do j = 1, n_media
            ! Get file extension
            extension = get_file_extension(media_files(j))
            
            write(unit_out, '(A)') '### ' // trim(get_output_title(media_files(j)))
            write(unit_out, '(A)') ''
            
            if (extension == 'mp4') then
                ! Embed video using HTML
                write(unit_out, '(A)') '<video width="800" height="600" controls>'
                write(unit_out, '(A)') '  <source src="../../media/examples/' // trim(media_files(j)) // '" type="video/mp4">'
                write(unit_out, '(A)') '  Your browser does not support the video tag.'
                write(unit_out, '(A)') '</video>'
                write(unit_out, '(A)') ''
                write(unit_out, '(A)') '[Download MP4](../../media/examples/' // trim(media_files(j)) // ')'
            else
                ! Regular image
                write(unit_out, '(A)') '![' // trim(media_files(j)) // '](../../media/examples/' // trim(media_files(j)) // ')'
                write(unit_out, '(A)') ''
                
                ! Add corresponding ASCII output
                ascii_file = trim(example_dir) // '/' // replace_extension(media_files(j), 'txt')
                inquire(file=trim(ascii_file), exist=file_exists)
                if (file_exists) then
                    write(unit_out, '(A)') 'ASCII output:'
                    write(unit_out, '(A)') '```'
                    
                    open(newunit=unit_ascii, file=trim(ascii_file), status='old', iostat=ios)
                    if (ios == 0) then
                        do
                            read(unit_ascii, '(A)', iostat=ios) line
                            if (ios /= 0) exit
                            write(unit_out, '(A)') trim(line)
                        end do
                        close(unit_ascii)
                    end if
                    
                    write(unit_out, '(A)') '```'
                    write(unit_out, '(A)') ''
                end if
                
                ! Add PDF link
                write(unit_out, '(A)') '[Download PDF](../../media/examples/' // &
                    trim(replace_extension(media_files(j), 'pdf')) // ')'
            end if
            write(unit_out, '(A)') ''
        end do
        
    end subroutine write_generated_outputs
    
    subroutine process_image_path(line)
        character(len=*), intent(inout) :: line
        integer :: start_pos, end_pos, path_start, path_end
        character(len=256) :: new_path, filename
        
        ! Find image markdown pattern ![...](...) 
        start_pos = index(line, '](')
        if (start_pos > 0) then
            path_start = start_pos + 2
            path_end = index(line(path_start:), ')')
            if (path_end > 0) then
                path_end = path_start + path_end - 2
                ! Extract just the filename
                filename = adjustl(trim(line(path_start:path_end)))
                ! Get just the filename if it's a path
                if (index(filename, '/') > 0) then
                    filename = filename(index(filename, '/', back=.true.)+1:)
                end if
                ! Replace with media path
                new_path = '../../media/examples/' // trim(filename)
                line = line(1:start_pos+1) // trim(new_path) // ')'
            end if
        end if
    end subroutine process_image_path
    
    subroutine find_output_files(dir, base_name, media_files, n_media, pdf_files, n_pdf, txt_files, n_txt)
        character(len=*), intent(in) :: dir, base_name
        character(len=*), intent(out) :: media_files(:), pdf_files(:), txt_files(:)
        integer, intent(out) :: n_media, n_pdf, n_txt
        
        character(len=256) :: test_file
        logical :: exists
        integer :: i
        
        n_media = 0
        n_pdf = 0
        n_txt = 0
        
        ! Use a more comprehensive approach - scan directory for all media files
        call scan_directory_for_media(dir, media_files, n_media)
        
    end subroutine find_output_files
    
    subroutine scan_directory_for_media(dir, media_files, n_media)
        character(len=*), intent(in) :: dir
        character(len=*), intent(out) :: media_files(:)
        integer, intent(out) :: n_media
        
        ! Hardcode the known patterns for each example
        character(len=256) :: test_file
        
        n_media = 0
        
        ! Check each known pattern
        select case(dir)
        case('example/fortran/animation')
            call add_if_exists(dir, 'wave_animation.mp4', media_files, n_media)
        case('example/fortran/basic_plots')
            call add_if_exists(dir, 'simple_plot.png', media_files, n_media)
            call add_if_exists(dir, 'multi_line.png', media_files, n_media)
        case('example/fortran/line_styles')
            call add_if_exists(dir, 'line_styles.png', media_files, n_media)
        case('example/fortran/contour_demo')
            call add_if_exists(dir, 'contour_gaussian.png', media_files, n_media)
            call add_if_exists(dir, 'mixed_plot.png', media_files, n_media)
        case('example/fortran/scale_examples')
            call add_if_exists(dir, 'log_scale.png', media_files, n_media)
            call add_if_exists(dir, 'symlog_scale.png', media_files, n_media)
        case('example/fortran/marker_demo')
            call add_if_exists(dir, 'scatter_plot.png', media_files, n_media)
            call add_if_exists(dir, 'all_marker_types.png', media_files, n_media)
            call add_if_exists(dir, 'marker_colors.png', media_files, n_media)
        case('example/fortran/colored_contours')
            call add_if_exists(dir, 'gaussian_default.png', media_files, n_media)
            call add_if_exists(dir, 'ripple_jet.png', media_files, n_media)
            call add_if_exists(dir, 'ripple_coolwarm.png', media_files, n_media)
            call add_if_exists(dir, 'ripple_inferno.png', media_files, n_media)
            call add_if_exists(dir, 'saddle_plasma.png', media_files, n_media)
        case('example/fortran/legend_demo')
            call add_if_exists(dir, 'basic_legend.png', media_files, n_media)
            call add_if_exists(dir, 'legend_upper_left.png', media_files, n_media)
            call add_if_exists(dir, 'legend_upper_right.png', media_files, n_media)
            call add_if_exists(dir, 'legend_lower_left.png', media_files, n_media)
            call add_if_exists(dir, 'legend_lower_right.png', media_files, n_media)
            call add_if_exists(dir, 'multi_function_legend.png', media_files, n_media)
        case('example/fortran/legend_box_demo')
            call add_if_exists(dir, 'legend_box_demo_default.png', media_files, n_media)
            call add_if_exists(dir, 'legend_box_demo_upper_left.png', media_files, n_media)
            call add_if_exists(dir, 'legend_box_demo_lower_right.png', media_files, n_media)
        case('example/fortran/format_string_demo')
            call add_if_exists(dir, 'format_string_demo.png', media_files, n_media)
        case('example/fortran/pcolormesh_demo')
            call add_if_exists(dir, 'pcolormesh_basic.png', media_files, n_media)
            call add_if_exists(dir, 'pcolormesh_plasma.png', media_files, n_media)
            call add_if_exists(dir, 'pcolormesh_sinusoidal.png', media_files, n_media)
        case('example/fortran/streamplot_demo')
            call add_if_exists(dir, 'streamplot_demo.png', media_files, n_media)
        case('example/fortran/unicode_demo')
            call add_if_exists(dir, 'unicode_demo.png', media_files, n_media)
            call add_if_exists(dir, 'math_examples.png', media_files, n_media)
        case('example/fortran/stateful_streamplot')
            call add_if_exists(dir, 'stateful_streamplot.png', media_files, n_media)
        case default
            ! For other examples, try the standard pattern
            test_file = dir(index(dir, '/', back=.true.)+1:) // '.png'
            call add_if_exists(dir, test_file, media_files, n_media)
        end select
        
    end subroutine scan_directory_for_media
    
    subroutine add_if_exists(dir, filename, files, n)
        character(len=*), intent(in) :: dir, filename
        character(len=*), intent(inout) :: files(:)
        integer, intent(inout) :: n
        
        character(len=512) :: full_path
        logical :: exists
        
        full_path = trim(dir) // '/' // trim(filename)
        inquire(file=trim(full_path), exist=exists)
        if (exists .and. n < size(files)) then
            n = n + 1
            files(n) = filename
        end if
    end subroutine add_if_exists
    
    logical function check_file_exists(dir, filename)
        character(len=*), intent(in) :: dir, filename
        character(len=512) :: full_path
        
        full_path = trim(dir) // '/' // trim(filename)
        inquire(file=trim(full_path), exist=check_file_exists)
    end function check_file_exists
    
    function replace_extension(filename, new_ext) result(new_filename)
        character(len=*), intent(in) :: filename, new_ext
        character(len=256) :: new_filename
        integer :: dot_pos
        
        dot_pos = index(filename, '.', back=.true.)
        if (dot_pos > 0) then
            new_filename = filename(1:dot_pos) // new_ext
        else
            new_filename = trim(filename) // '.' // new_ext
        end if
    end function replace_extension
    
    function title_case(name) result(title)
        character(len=*), intent(in) :: name
        character(len=256) :: title
        integer :: i
        
        title = name
        
        ! Replace underscores with spaces
        do i = 1, len_trim(title)
            if (title(i:i) == '_') title(i:i) = ' '
        end do
        
        ! Capitalize first letter and after spaces
        if (len_trim(title) > 0) then
            if (title(1:1) >= 'a' .and. title(1:1) <= 'z') then
                title(1:1) = char(ichar(title(1:1)) - 32)
            end if
            
            do i = 2, len_trim(title)
                if (title(i-1:i-1) == ' ' .and. title(i:i) >= 'a' .and. title(i:i) <= 'z') then
                    title(i:i) = char(ichar(title(i:i)) - 32)
                end if
            end do
        end if
    end function title_case
    
    function get_output_title(filename) result(title)
        character(len=*), intent(in) :: filename
        character(len=256) :: title
        character(len=256) :: base
        integer :: dot_pos
        
        ! Remove extension
        dot_pos = index(filename, '.', back=.true.)
        if (dot_pos > 0) then
            base = filename(1:dot_pos-1)
        else
            base = filename
        end if
        
        title = title_case(base)
    end function get_output_title
    
    function get_file_extension(filename) result(ext)
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

end program generate_example_docs
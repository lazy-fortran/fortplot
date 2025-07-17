program generate_example_docs
    !! Generate FORD documentation pages for fortplotlib examples
    implicit none
    
    character(len=256) :: example_dirs(20)
    character(len=256) :: example_names(20)
    integer :: n_examples, i
    
    ! List of examples to document
    n_examples = 18
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
    
    example_dirs(16) = "example/fortran/save_animation_demo"
    example_names(16) = "save_animation"
    
    example_dirs(17) = "example/fortran/stateful_streamplot"
    example_names(17) = "stateful_streamplot"
    
    example_dirs(18) = "example/fortran/animation"
    example_names(18) = "animation"
    
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
        
        character(len=1024) :: line, source_content
        character(len=4096) :: ascii_content
        character(len=256) :: source_file, output_file, ascii_file
        character(len=256) :: png_files(10), pdf_files(10), txt_files(10)
        integer :: unit_in, unit_out, unit_ascii, ios, j
        integer :: n_png, n_pdf, n_txt
        logical :: file_exists
        
        print *, "Processing: ", trim(example_name)
        
        ! Build file paths
        source_file = trim(example_dir) // "/" // trim(example_name) // ".f90"
        output_file = "doc/examples/" // trim(example_name) // ".md"
        
        ! Check if source exists
        inquire(file=trim(source_file), exist=file_exists)
        if (.not. file_exists) then
            print *, "Warning: Source file not found: ", trim(source_file)
            return
        end if
        
        ! Find output files
        call find_output_files(example_dir, example_name, png_files, n_png, &
                              pdf_files, n_pdf, txt_files, n_txt)
        
        ! Open output markdown file
        open(newunit=unit_out, file=trim(output_file), status='replace', iostat=ios)
        if (ios /= 0) then
            print *, "Error: Cannot create ", trim(output_file)
            return
        end if
        
        ! Write header
        write(unit_out, '(A)') 'title: ' // title_case(example_name)
        write(unit_out, '(A)') '---'
        write(unit_out, '(A)') ''
        write(unit_out, '(A)') '# ' // title_case(example_name)
        write(unit_out, '(A)') ''
        write(unit_out, '(A)') get_example_description(example_name)
        write(unit_out, '(A)') ''
        write(unit_out, '(A)') '## Source Code'
        write(unit_out, '(A)') ''
        write(unit_out, '(A)') '```fortran'
        
        ! Copy source code
        open(newunit=unit_in, file=trim(source_file), status='old', iostat=ios)
        if (ios == 0) then
            do
                read(unit_in, '(A)', iostat=ios) line
                if (ios /= 0) exit
                write(unit_out, '(A)') trim(line)
            end do
            close(unit_in)
        end if
        
        write(unit_out, '(A)') '```'
        write(unit_out, '(A)') ''
        write(unit_out, '(A)') '## Output'
        write(unit_out, '(A)') ''
        
        ! Add PNG outputs
        do j = 1, n_png
            write(unit_out, '(A)') '### ' // get_output_title(png_files(j))
            write(unit_out, '(A)') ''
            write(unit_out, '(A)') '![' // trim(png_files(j)) // '](../media/examples/' // trim(png_files(j)) // ')'
            write(unit_out, '(A)') ''
            
            ! Add corresponding ASCII output
            ascii_file = trim(example_dir) // '/' // replace_extension(png_files(j), 'txt')
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
            write(unit_out, '(A)') '[Download PDF](../media/examples/' // &
                replace_extension(png_files(j), 'pdf') // ')'
            write(unit_out, '(A)') ''
        end do
        
        ! Add key features section
        write(unit_out, '(A)') '## Key Features Demonstrated'
        write(unit_out, '(A)') ''
        write(unit_out, '(A)') get_key_features(example_name)
        
        close(unit_out)
        
    end subroutine process_example
    
    subroutine find_output_files(dir, base_name, png_files, n_png, pdf_files, n_pdf, txt_files, n_txt)
        character(len=*), intent(in) :: dir, base_name
        character(len=*), intent(out) :: png_files(:), pdf_files(:), txt_files(:)
        integer, intent(out) :: n_png, n_pdf, n_txt
        
        character(len=256) :: test_file
        logical :: exists
        integer :: i
        
        n_png = 0
        n_pdf = 0
        n_txt = 0
        
        ! Check for common output patterns
        ! Pattern 1: base_name.ext
        test_file = trim(base_name) // '.png'
        if (check_file_exists(dir, test_file)) then
            n_png = n_png + 1
            png_files(n_png) = test_file
        end if
        
        ! Pattern 2: simple_plot, multi_line, etc. for basic_plots
        if (base_name == 'basic_plots') then
            if (check_file_exists(dir, 'simple_plot.png')) then
                n_png = n_png + 1
                png_files(n_png) = 'simple_plot.png'
            end if
            if (check_file_exists(dir, 'multi_line.png')) then
                n_png = n_png + 1
                png_files(n_png) = 'multi_line.png'
            end if
        else if (base_name == 'contour_demo') then
            if (check_file_exists(dir, 'contour_gaussian.png')) then
                n_png = n_png + 1
                png_files(n_png) = 'contour_gaussian.png'
            end if
            if (check_file_exists(dir, 'mixed_plot.png')) then
                n_png = n_png + 1
                png_files(n_png) = 'mixed_plot.png'
            end if
        else if (base_name == 'scale_examples') then
            if (check_file_exists(dir, 'log_scale.png')) then
                n_png = n_png + 1
                png_files(n_png) = 'log_scale.png'
            end if
            if (check_file_exists(dir, 'symlog_scale.png')) then
                n_png = n_png + 1
                png_files(n_png) = 'symlog_scale.png'
            end if
        end if
        
    end subroutine find_output_files
    
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
    
    function get_example_description(name) result(desc)
        character(len=*), intent(in) :: name
        character(len=512) :: desc
        
        select case(name)
        case('basic_plots')
            desc = 'This example demonstrates the fundamental plotting capabilities of fortplotlib ' // &
                   'using both the simple functional API and the object-oriented interface.'
        case('line_styles')
            desc = 'This example demonstrates all available line styles in fortplotlib, ' // &
                   'showing how to customize the appearance of plotted lines.'
        case('contour_demo')
            desc = 'This example demonstrates contour plotting capabilities, including basic contours, ' // &
                   'custom levels, and mixing contour plots with line plots.'
        case('scale_examples')
            desc = 'This example demonstrates different axis scaling options including ' // &
                   'logarithmic and symmetric logarithmic (symlog) scales.'
        case('marker_demo')
            desc = 'This example showcases various marker types and scatter plot capabilities in fortplotlib.'
        case('format_string_demo')
            desc = 'This example demonstrates matplotlib-style format strings for quick and intuitive ' // &
                   'plot styling.'
        case('colored_contours')
            desc = 'This example shows filled contour plots with customizable colormaps for ' // &
                   'visualizing 2D scalar fields.'
        case('pcolormesh_demo')
            desc = 'This example demonstrates pseudocolor plots for efficient 2D data visualization.'
        case('streamplot_demo')
            desc = 'This example shows vector field visualization using streamlines.'
        case('legend_demo')
            desc = 'This example demonstrates legend placement and customization options.'
        case default
            desc = 'This example demonstrates ' // trim(title_case(name)) // ' capabilities in fortplotlib.'
        end select
    end function get_example_description
    
    function get_key_features(name) result(features)
        character(len=*), intent(in) :: name
        character(len=1024) :: features
        
        select case(name)
        case('basic_plots')
            features = '- **Functional API**: Simple, matplotlib-like interface with global figure management' // new_line('A') // &
                      '- **Object-Oriented API**: More control through `figure_t` type' // new_line('A') // &
                      '- **Multiple output formats**: PNG, PDF, and ASCII text' // new_line('A') // &
                      '- **Line labeling**: Automatic legend generation' // new_line('A') // &
                      '- **Axis labeling**: Clear axis titles and labels'
        case('line_styles')
            features = '- **Named Constants**: Use predefined constants for better code readability' // new_line('A') // &
                      '- **String Shortcuts**: Compatible with matplotlib-style strings' // new_line('A') // &
                      '- **Marker Combinations**: Combine with markers for scatter plots' // new_line('A') // &
                      '- **Clear Separation**: Data offset vertically for visual clarity'
        case('scale_examples')
            features = '- **Logarithmic scaling**: For exponential growth visualization' // new_line('A') // &
                      '- **Symmetric log**: Handles positive and negative values with log-like behavior' // new_line('A') // &
                      '- **Linear threshold**: Symlog parameter controls transition to linear near zero' // new_line('A') // &
                      '- **Automatic tick generation**: Smart tick placement for non-linear scales'
        case default
            features = '- See source code for detailed feature demonstration'
        end select
    end function get_key_features

end program generate_example_docs
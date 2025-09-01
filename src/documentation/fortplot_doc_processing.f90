module fortplot_doc_processing
    !! Processing utilities for documentation generation
    use fortplot_doc_core, only: &
        PATH_MAX_LEN, LINE_MAX_LEN, FILENAME_MAX_LEN, &
        title_case, build_readme_path, build_output_path, &
        build_python_path, build_local_fortran_path, build_fortran_url, &
        copy_file_content, get_fortran_filename
    implicit none
    private

    ! Public interface
    public :: get_example_count, get_example_dir, get_example_name
    public :: process_example

contains

    ! --------------------
    ! Example listing
    ! --------------------

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

    ! --------------------
    ! Main processing
    ! --------------------

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

        call setup_file_units(readme_file, output_file, unit_in, unit_out, ios)
        if (ios /= 0) return

        call process_readme_content(unit_in, unit_out, example_dir, example_name)

        close(unit_in)
        close(unit_out)
    end subroutine process_example

    subroutine setup_file_units(readme_file, output_file, unit_in, unit_out, ios)
        character(len=*), intent(in) :: readme_file, output_file
        integer, intent(out) :: unit_in, unit_out, ios

        call open_output_file(output_file, unit_out, ios)
        if (ios /= 0) return

        call open_input_file(readme_file, unit_in, ios)
        if (ios /= 0) close(unit_out)
    end subroutine setup_file_units

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
        use fortplot_doc_output, only: write_generated_outputs
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

    ! --------------------
    ! Image path processing (markdown)
    ! --------------------

    subroutine process_image_paths(line, example_name)
        character(len=*), intent(inout) :: line
        character(len=*), intent(in) :: example_name

        if (index(line, '![') > 0 .and. index(line, '](') > 0) then
            call process_image_path(line, example_name)
        end if
    end subroutine process_image_paths

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

    ! --------------------
    ! Source code links section
    ! --------------------

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

        write(unit_out, '(A)') '- Fortran: [' // trim(fortran_file) // &
                              '](' // trim(fortran_path) // ')'
    end subroutine write_fortran_link

    subroutine write_python_link_if_exists(unit_out, example_name, python_path, python_exists)
        use fortplot_doc_core, only: GITHUB_BASE_URL
        integer, intent(in) :: unit_out
        character(len=*), intent(in) :: example_name, python_path
        logical, intent(in) :: python_exists

        if (python_exists) then
            write(unit_out, '(A)') ''
            write(unit_out, '(A)') '**Python:** [' // trim(example_name) // &
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

end module fortplot_doc_processing

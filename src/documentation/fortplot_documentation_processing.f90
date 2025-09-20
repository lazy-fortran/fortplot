module fortplot_documentation_processing
    !! Processing functions for documentation generation
    !! Contains example manifest loading and processing logic

    use fortplot_documentation_core, only: PATH_MAX_LEN, LINE_MAX_LEN, &
                                          check_file_exists, build_file_path, &
                                          build_readme_path, build_output_path, &
                                          build_fortran_url, build_python_path, &
                                          build_local_fortran_path, get_output_title, &
                                          get_fortran_filename, copy_file_content
    implicit none
    private

    public :: get_example_count, get_example_dir, get_example_name
    public :: process_example
    public :: MAX_EXAMPLES

    integer, parameter :: MAX_EXAMPLES = 64

    integer, parameter :: FALLBACK_COUNT = 17
    character(len=32), parameter :: FALLBACK_EXAMPLES(FALLBACK_COUNT) = [ &
        "basic_plots         ", "line_styles         ", &
        "marker_demo         ", "format_string_demo  ", &
        "contour_demo        ", "colored_contours    ", &
        "pcolormesh_demo     ", "streamplot_demo     ", &
        "ascii_heatmap       ", "scale_examples      ", &
        "legend_demo         ", "legend_box_demo     ", &
        "unicode_demo        ", "show_viewer_demo    ", &
        "raster_backend_demo ", "pdf_backend_demo    ", &
        "text_backend_demo   " ]

    logical :: manifest_loaded = .false.
    integer :: manifest_count = 0
    character(len=32) :: manifest_names(MAX_EXAMPLES)

contains

    function get_example_count() result(count)
        integer :: count
        call ensure_example_manifest()
        count = manifest_count
    end function get_example_count

    subroutine get_example_dir(index, dir)
        integer, intent(in) :: index
        character(len=PATH_MAX_LEN), intent(out) :: dir

        call ensure_example_manifest()

        if (index > 0 .and. index <= manifest_count) then
            dir = 'example/fortran/' // trim(manifest_names(index))
        else
            dir = ''
        end if
    end subroutine get_example_dir

    subroutine get_example_name(index, name)
        integer, intent(in) :: index
        character(len=PATH_MAX_LEN), intent(out) :: name

        call ensure_example_manifest()

        if (index > 0 .and. index <= manifest_count) then
            name = trim(manifest_names(index))
        else
            name = ''
        end if
    end subroutine get_example_name

    subroutine ensure_example_manifest()
        if (.not. manifest_loaded) then
            call load_example_manifest()
        end if
    end subroutine ensure_example_manifest

    subroutine load_example_manifest()
        character(len=LINE_MAX_LEN) :: line
        character(len=32) :: slug
        logical :: in_section, has_slug
        integer :: ios, unit, i

        manifest_count = 0
        manifest_loaded = .true.

        open(newunit=unit, file='doc/examples/index.md', status='old', action='read', iostat=ios)
        if (ios /= 0) then
            call load_fallback_manifest()
            return
        end if

        in_section = .false.
        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit

            if (index(line, '<!-- AUTO_EXAMPLES_START -->') > 0) then
                in_section = .true.
                cycle
            end if

            if (index(line, '<!-- AUTO_EXAMPLES_END -->') > 0) then
                exit
            end if

            if (in_section .and. index(line, '- [') == 1) then
                call extract_slug_from_link(line, slug, has_slug)
                if (has_slug .and. manifest_count < MAX_EXAMPLES) then
                    manifest_count = manifest_count + 1
                    manifest_names(manifest_count) = slug
                end if
            end if
        end do

        close(unit)

        if (manifest_count == 0) then
            call load_fallback_manifest()
        end if
    end subroutine load_example_manifest

    subroutine extract_slug_from_link(link, slug, has_slug)
        character(len=*), intent(in) :: link
        character(len=*), intent(out) :: slug
        logical, intent(out) :: has_slug
        integer :: start_pos, end_pos

        has_slug = .false.
        slug = ''

        start_pos = index(link, '(')
        if (start_pos == 0) return

        end_pos = index(link, ')', .true.)
        if (end_pos <= start_pos) return

        start_pos = start_pos + 1
        slug = link(start_pos:end_pos-1)
        slug = adjustl(slug)

        if (len_trim(slug) > 0) then
            has_slug = .true.
        end if
    end subroutine extract_slug_from_link

    subroutine load_fallback_manifest()
        integer :: i
        manifest_count = min(FALLBACK_COUNT, MAX_EXAMPLES)
        do i = 1, manifest_count
            manifest_names(i) = trim(FALLBACK_EXAMPLES(i))
        end do
    end subroutine load_fallback_manifest

    subroutine process_example(example_name, example_dir, output_dir, error_occurred)
        character(len=*), intent(in) :: example_name, example_dir, output_dir
        logical, intent(out) :: error_occurred

        character(len=PATH_MAX_LEN) :: readme_path, output_readme_path
        character(len=PATH_MAX_LEN) :: fortran_name, fortran_path, output_fortran_path
        character(len=:), allocatable :: title

        error_occurred = .false.

        readme_path = build_readme_path(example_dir)
        fortran_name = get_fortran_filename(example_name)
        call build_file_path(example_dir, fortran_name, fortran_path)

        if (.not. check_file_exists('', readme_path)) then
            if (check_file_exists('', fortran_path)) then
                call generate_readme_from_fortran(example_name, example_dir, fortran_name, readme_path)
            else
                call generate_placeholder_readme(example_name, readme_path)
            end if
        end if

        call build_file_path(output_dir, 'README.md', output_readme_path)
        call copy_file_content(readme_path, output_readme_path)

        if (check_file_exists('', fortran_path)) then
            call build_file_path(output_dir, fortran_name, output_fortran_path)
            call copy_file_content(fortran_path, output_fortran_path)
        end if
    end subroutine process_example

    subroutine generate_readme_from_fortran(example_name, example_dir, fortran_name, readme_path)
        character(len=*), intent(in) :: example_name, example_dir, fortran_name, readme_path
        character(len=:), allocatable :: title, fortran_url, run_target
        integer :: unit

        title = get_output_title(example_name)
        fortran_url = build_fortran_url(example_name, fortran_name)
        run_target = 'example/' // trim(example_name)

        open(newunit=unit, file=readme_path, status='replace', action='write')
        write(unit, '(A)') '# ' // title
        write(unit, '(A)') ''
        write(unit, '(A)') 'Source code: [' // trim(fortran_name) // '](' // fortran_url // ')'
        write(unit, '(A)') ''
        write(unit, '(A)') 'Run with:'
        write(unit, '(A)') '```bash'
        write(unit, '(A)') 'make ' // trim(run_target)
        write(unit, '(A)') '```'
        close(unit)
    end subroutine generate_readme_from_fortran

    subroutine generate_placeholder_readme(example_name, readme_path)
        character(len=*), intent(in) :: example_name, readme_path
        character(len=:), allocatable :: title
        integer :: unit

        title = get_output_title(example_name)

        open(newunit=unit, file=readme_path, status='replace', action='write')
        write(unit, '(A)') '# ' // title
        write(unit, '(A)') ''
        write(unit, '(A)') 'This example is under development.'
        close(unit)
    end subroutine generate_placeholder_readme

end module fortplot_documentation_processing
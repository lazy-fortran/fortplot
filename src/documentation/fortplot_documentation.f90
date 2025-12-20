module fortplot_documentation
    !! Consolidated documentation generation module combining core utilities,
    !! processing logic, and output generation
    use fortplot_directory_listing, only: list_directory_entries
    use fortplot_doc_utils, only: &
        build_file_path, check_file_exists, file_exists, get_file_extension, &
        lowercase_string, replace_extension, title_case
    implicit none
    private

    ! =====================
    ! Public constants from core
    ! =====================
    public :: PATH_MAX_LEN, FILENAME_MAX_LEN, LINE_MAX_LEN
    public :: MAX_EXAMPLES, MAX_MEDIA_FILES
    public :: VIDEO_WIDTH, VIDEO_HEIGHT
    public :: GITHUB_BASE_URL

    ! =====================
    ! Public utilities from core
    ! =====================
    public :: check_file_exists, get_file_extension, replace_extension
    public :: copy_file_content, build_file_path
    public :: build_readme_path, build_output_path, build_fortran_url
    public :: build_python_path, build_local_fortran_path
    public :: title_case, get_output_title
    public :: get_fortran_filename, get_example_run_target

    ! =====================
    ! Public processing interface
    ! =====================
    public :: get_example_count, get_example_dir, get_example_name
    public :: process_example

    ! =====================
    ! Public output interface
    ! =====================
    public :: write_generated_outputs
    public :: scan_directory_for_media

    ! =====================
    ! Constants
    ! =====================
    ! String length constants
    integer, parameter :: PATH_MAX_LEN = 256
    integer, parameter :: FILENAME_MAX_LEN = 256
    integer, parameter :: LINE_MAX_LEN = 1024

    ! Array size constants
    integer, parameter :: MAX_EXAMPLES = 64
    integer, parameter :: MAX_MEDIA_FILES = 32

    ! Video dimensions
    integer, parameter :: VIDEO_WIDTH = 800
    integer, parameter :: VIDEO_HEIGHT = 600

    ! URL constants
    character(len=*), parameter :: GITHUB_BASE_URL = &
        'https://github.com/lazy-fortran/fortplot/blob/main/'

    ! Private constants for processing
    character(len=*), parameter :: OUTPUT_BASE_DIR = 'output/example/fortran/'
    character(len=*), parameter :: EXAMPLES_INDEX_PATH = 'doc/examples/index.md'
    character(len=*), parameter :: INDEX_START_MARKER = '<!-- AUTO_EXAMPLES_START -->'
    character(len=*), parameter :: INDEX_END_MARKER   = '<!-- AUTO_EXAMPLES_END -->'

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

    ! ========================================
    ! Core Utilities Section (from doc_core)
    ! ========================================

    subroutine copy_file_content(input_file, output_file)
        character(len=*), intent(in) :: input_file, output_file
        integer :: input_unit, output_unit, ios
        character(len=LINE_MAX_LEN) :: line

        open(newunit=input_unit, file=input_file, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, 'Warning: Could not open input file: ', trim(input_file)
            return
        end if

        open(newunit=output_unit, file=output_file, status='replace', action='write')

        do
            read(input_unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            write(output_unit, '(A)') trim(line)
        end do

        close(input_unit)
        close(output_unit)
    end subroutine copy_file_content

    pure subroutine build_readme_path(example_dir, readme_file)
        character(len=*), intent(in) :: example_dir
        character(len=PATH_MAX_LEN), intent(out) :: readme_file

        readme_file = trim(example_dir) // '/README.md'
    end subroutine build_readme_path

    pure subroutine build_output_path(example_name, output_file)
        character(len=*), intent(in) :: example_name
        character(len=PATH_MAX_LEN), intent(out) :: output_file

        output_file = OUTPUT_BASE_DIR // trim(example_name) // '/' // trim(example_name) // '.png'
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

        local_path = 'example/' // trim(example_name) // '.f90'
    end subroutine build_local_fortran_path

    function get_output_title(filename) result(title)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: title
        character(len=:), allocatable :: base_name
        integer :: dot_pos, slash_pos

        ! Extract base name
        slash_pos = index(filename, '/', back=.true.)
        if (slash_pos > 0) then
            base_name = filename(slash_pos+1:)
        else
            base_name = filename
        end if

        ! Remove extension
        dot_pos = index(base_name, '.', back=.true.)
        if (dot_pos > 0) then
            base_name = base_name(1:dot_pos-1)
        end if

        ! Convert to title case
        title = title_case(base_name)
    end function get_output_title

    pure subroutine get_fortran_filename(example_name, filename)
        character(len=*), intent(in) :: example_name
        character(len=PATH_MAX_LEN), intent(out) :: filename

        ! Special cases for examples with different filenames
        select case(trim(example_name))
        case('animation')
            filename = 'save_animation_demo.f90'
        case default
            filename = trim(example_name) // '.f90'
        end select
    end subroutine get_fortran_filename

    pure subroutine get_example_run_target(example_name, run_target)
        character(len=*), intent(in) :: example_name
        character(len=PATH_MAX_LEN), intent(out) :: run_target

        select case(trim(example_name))
        case('animation')
            run_target = 'save_animation_demo'
        case default
            run_target = trim(example_name)
        end select
    end subroutine get_example_run_target

    ! ========================================
    ! Processing Section (from doc_processing)
    ! ========================================

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
        integer :: unit, ios

        manifest_names = ''
        manifest_count = 0
        manifest_loaded = .true.

        open(newunit=unit, file=EXAMPLES_INDEX_PATH, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            call load_fallback_manifest()
            return
        end if

        in_section = .false.
        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit

            if (index(line, INDEX_START_MARKER) > 0) then
                in_section = .true.
                cycle
            end if

            if (index(line, INDEX_END_MARKER) > 0) exit
            if (.not. in_section) cycle

            call parse_example_line(line, slug, has_slug)
            if (has_slug) call add_manifest_entry(slug)
        end do

        close(unit)

        if (manifest_count == 0) then
            call load_fallback_manifest()
        end if
    end subroutine load_example_manifest

    subroutine load_fallback_manifest()
        integer :: n

        manifest_names = ''
        n = min(FALLBACK_COUNT, MAX_EXAMPLES)
        manifest_names(1:n) = FALLBACK_EXAMPLES(1:n)
        manifest_count = n
    end subroutine load_fallback_manifest

    subroutine parse_example_line(line, slug, has_slug)
        character(len=*), intent(in) :: line
        character(len=32), intent(out) :: slug
        logical, intent(out) :: has_slug

        character(len=:), allocatable :: trimmed_line, link
        integer :: link_start, link_end

        slug = ''
        has_slug = .false.

        trimmed_line = adjustl(line)
        if (len_trim(trimmed_line) < 4) return
        if (trimmed_line(1:2) /= '- ') return

        link_start = index(trimmed_line, '](')
        if (link_start <= 0) return

        link_end = index(trimmed_line(link_start+2:), ')')
        if (link_end <= 0) return

        link = trimmed_line(link_start+2:link_start+1+link_end-1)
        call extract_slug_from_link(link, slug, has_slug)
    end subroutine parse_example_line

    subroutine extract_slug_from_link(link, slug, has_slug)
        character(len=*), intent(in) :: link
        character(len=32), intent(out) :: slug
        logical, intent(out) :: has_slug

        character(len=:), allocatable :: work
        integer :: pos

        slug = ''
        has_slug = .false.

        work = trim(link)
        if (len_trim(work) == 0) return

        if (len(work) >= 2 .and. work(1:2) == './') then
            work = work(3:)
        end if

        pos = index(work, '/example/fortran/')
        if (pos > 0) then
            work = work(pos + len('/example/fortran/'):)
        end if

        pos = index(work, '/', back=.true.)
        if (pos > 0) then
            if (pos < len_trim(work)) then
                work = work(pos+1:)
            else
                work = work(:pos-1)
            end if
        end if

        pos = index(work, '.html', back=.true.)
        if (pos > 0) work = work(:pos-1)

        if (len_trim(work) == 0) return

        slug = adjustl(work)
        slug = slug(:len(slug))
        slug = trim(slug)
        if (len_trim(slug) == 0) return

        has_slug = .true.
    end subroutine extract_slug_from_link

    subroutine add_manifest_entry(name)
        character(len=*), intent(in) :: name
        character(len=32) :: candidate
        integer :: i

        candidate = trim(adjustl(name))
        if (len_trim(candidate) == 0) return

        do i = 1, manifest_count
            if (trim(manifest_names(i)) == trim(candidate)) return
        end do

        if (manifest_count >= MAX_EXAMPLES) return

        manifest_count = manifest_count + 1
        manifest_names(manifest_count) = candidate
    end subroutine add_manifest_entry

    subroutine process_example(example_dir, example_name)
        character(len=*), intent(in) :: example_dir, example_name
        character(len=PATH_MAX_LEN) :: readme_file, output_file
        character(len=PATH_MAX_LEN) :: fortran_file, fortran_url
        character(len=PATH_MAX_LEN) :: run_target
        character(len=PATH_MAX_LEN) :: output_dir
        character(len=LINE_MAX_LEN) :: line
        character(len=LINE_MAX_LEN) :: summary_lines(200)
        character(len=FILENAME_MAX_LEN) :: media_files(MAX_MEDIA_FILES)
        integer :: unit_in, unit_out, ios
        integer :: summary_count, n_media
        logical :: readme_exists

        summary_lines = ''
        summary_count = 0

        readme_file = trim(example_dir) // '/README.md'
        output_file = 'doc/examples/' // trim(example_name) // '.md'
        output_dir = OUTPUT_BASE_DIR // trim(example_name)

        call get_fortran_filename(example_name, fortran_file)
        call build_fortran_url(example_name, fortran_url)
        call get_example_run_target(example_name, run_target)

        inquire(file=readme_file, exist=readme_exists)
        if (readme_exists) then
            open(newunit=unit_in, file=readme_file, status='old', action='read', iostat=ios)
            if (ios == 0) then
                do
                    read(unit_in, '(A)', iostat=ios) line
                    if (ios /= 0) exit
                    call append_summary_line(line, summary_lines, summary_count)
                end do
                close(unit_in)
            end if
        end if

        call trim_summary(summary_lines, summary_count)
        call scan_directory_for_media(output_dir, media_files, n_media)

        print '(A,A)', '  Processing example: ', trim(example_name)

        open(newunit=unit_out, file=output_file, status='replace', action='write', iostat=ios)
        if (ios /= 0) return

        call write_example_header(unit_out, example_name, fortran_file, fortran_url)
        call write_summary_section(unit_out, summary_lines, summary_count)
        call write_files_section(unit_out, example_name, fortran_file, n_media)
        call write_running_section(unit_out, trim(run_target))
        call write_output_section(unit_out, example_name, media_files, n_media)

        close(unit_out)
    end subroutine process_example

    subroutine append_summary_line(line, summary_lines, summary_count)
        character(len=*), intent(in) :: line
        character(len=LINE_MAX_LEN), intent(inout) :: summary_lines(:)
        integer, intent(inout) :: summary_count

        character(len=:), allocatable :: trimmed, lower

        if (summary_count >= size(summary_lines)) return

        trimmed = trim(line)
        if (len_trim(trimmed) == 0) then
            if (summary_count > 0) then
                if (summary_lines(summary_count) /= '') then
                    summary_count = summary_count + 1
                    if (summary_count <= size(summary_lines)) then
                        summary_lines(summary_count) = ''
                    end if
                end if
            end if
            return
        end if

        if (trimmed(1:1) == '#') return
        if (len_trim(trimmed) >= 3) then
            if (trimmed(1:3) == '```') return
        end if

        lower = lowercase_string(trimmed)
        if (len(lower) >= 6) then
            if (lower(1:6) == 'title:') return
        end if
        if (len(lower) >= 12) then
            if (lower(1:12) == 'make example') return
        end if
        if (trimmed == '---') return

        summary_count = summary_count + 1
        if (summary_count <= size(summary_lines)) then
            summary_lines(summary_count) = trimmed
        end if
    end subroutine append_summary_line

    subroutine trim_summary(summary_lines, summary_count)
        character(len=LINE_MAX_LEN), intent(inout) :: summary_lines(:)
        integer, intent(inout) :: summary_count

        do while (summary_count > 0)
            if (len_trim(summary_lines(summary_count)) == 0) then
                summary_lines(summary_count) = ''
                summary_count = summary_count - 1
            else
                exit
            end if
        end do
    end subroutine trim_summary

    subroutine write_example_header(unit_out, example_name, fortran_file, fortran_url)
        integer, intent(in) :: unit_out
        character(len=*), intent(in) :: example_name, fortran_file, fortran_url

        write(unit_out, '(A)') 'title: ' // title_case(example_name)
        write(unit_out, '(A)') '---'
        write(unit_out, '(A)') ''
        write(unit_out, '(A)') '# ' // title_case(example_name)
        write(unit_out, '(A)') ''
        write(unit_out, '(A)') 'Source: [' // trim(fortran_file) // '](' // trim(fortran_url) // ')'
        write(unit_out, '(A)') ''
    end subroutine write_example_header

    subroutine write_summary_section(unit_out, summary_lines, summary_count)
        integer, intent(in) :: unit_out
        character(len=LINE_MAX_LEN), intent(in) :: summary_lines(:)
        integer, intent(in) :: summary_count
        integer :: i

        if (summary_count == 0) then
            write(unit_out, '(A)') 'See source and outputs below.'
            write(unit_out, '(A)') ''
        else
            do i = 1, summary_count
                write(unit_out, '(A)') trim(summary_lines(i))
            end do
            write(unit_out, '(A)') ''
        end if
    end subroutine write_summary_section

    subroutine write_files_section(unit_out, example_name, fortran_file, n_media)
        integer, intent(in) :: unit_out
        character(len=*), intent(in) :: example_name, fortran_file
        integer, intent(in) :: n_media

        write(unit_out, '(A)') '## Files'
        write(unit_out, '(A)') ''
        write(unit_out, '(A,A,A)') '- `', trim(fortran_file), '` - Source code'
        if (n_media > 0) then
            write(unit_out, '(A,A,A)') '- Generated media in `output/example/fortran/', &
                trim(example_name), '/`'
        else
            write(unit_out, '(A,A,A)') '- Run the example to populate `output/example/fortran/', &
                trim(example_name), '/`'
        end if
        write(unit_out, '(A)') ''
    end subroutine write_files_section

    subroutine write_running_section(unit_out, run_target)
        integer, intent(in) :: unit_out
        character(len=*), intent(in) :: run_target

        write(unit_out, '(A)') '## Running'
        write(unit_out, '(A)') ''
        write(unit_out, '(A)') '```bash'
        write(unit_out, '(A,A,A)') 'make example ARGS="', trim(run_target), '"'
        write(unit_out, '(A)') '```'
        write(unit_out, '(A)') ''
    end subroutine write_running_section

    subroutine write_output_section(unit_out, example_name, media_files, n_media)
        integer, intent(in) :: unit_out
        character(len=*), intent(in) :: example_name
        character(len=FILENAME_MAX_LEN), intent(in) :: media_files(:)
        integer, intent(in) :: n_media

        character(len=FILENAME_MAX_LEN) :: sorted_media(MAX_MEDIA_FILES)
        character(len=FILENAME_MAX_LEN) :: group_names(MAX_MEDIA_FILES)
        character(len=FILENAME_MAX_LEN) :: image_files(MAX_MEDIA_FILES)
        character(len=FILENAME_MAX_LEN) :: pdf_files(MAX_MEDIA_FILES)
        character(len=FILENAME_MAX_LEN) :: txt_files(MAX_MEDIA_FILES)
        character(len=FILENAME_MAX_LEN) :: video_files(MAX_MEDIA_FILES)
        character(len=FILENAME_MAX_LEN) :: other_links(MAX_MEDIA_FILES)
        logical :: has_image(MAX_MEDIA_FILES), has_pdf(MAX_MEDIA_FILES)
        logical :: has_txt(MAX_MEDIA_FILES), has_video(MAX_MEDIA_FILES)
        logical :: has_other(MAX_MEDIA_FILES)
        integer :: n_groups, i

        write(unit_out, '(A)') '## Output'
        write(unit_out, '(A)') ''

        if (n_media <= 0) then
            write(unit_out, '(A)') 'Run this example to generate plots and other media assets.'
            write(unit_out, '(A)') ''
            return
        end if

        sorted_media = ''
        sorted_media(1:n_media) = media_files(1:n_media)
        call sort_string_array(sorted_media, n_media)

        call group_media_files(sorted_media, n_media, group_names, n_groups, image_files, &
            has_image, pdf_files, has_pdf, txt_files, has_txt, video_files, has_video, &
            other_links, has_other)

        if (n_groups <= 0) then
            write(unit_out, '(A)') 'Run this example to generate plots and other media assets.'
            write(unit_out, '(A)') ''
            return
        end if

        call sort_media_groups(group_names, n_groups, image_files, has_image, pdf_files, &
            has_pdf, txt_files, has_txt, video_files, has_video, other_links, has_other)

        do i = 1, n_groups
            call write_media_group(unit_out, example_name, group_names(i), image_files(i), &
                has_image(i), pdf_files(i), has_pdf(i), txt_files(i), has_txt(i), &
                video_files(i), has_video(i), other_links(i), has_other(i))
        end do
    end subroutine write_output_section

    subroutine write_media_group(unit_out, example_name, group_name, image_file, has_image, &
        pdf_file, has_pdf, txt_file, has_txt, video_file, has_video, other_links, has_other)
        integer, intent(in) :: unit_out
        character(len=*), intent(in) :: example_name, group_name
        character(len=*), intent(in) :: image_file, pdf_file, txt_file, video_file
        character(len=*), intent(in) :: other_links
        logical, intent(in) :: has_image, has_pdf, has_txt, has_video, has_other
        character(len=:), allocatable :: heading
        character(len=PATH_MAX_LEN) :: txt_path
        character(len=LINE_MAX_LEN) :: line
        logical :: txt_exists
        integer :: unit_txt
        integer :: ios
        integer :: line_count
        integer, parameter :: MAX_ASCII_LINES = 60

        heading = title_case(group_name)

        write(unit_out, '(A)') '### ' // trim(heading)
        write(unit_out, '(A)') ''

        if (has_image) then
            write(unit_out, '(A,A,A)') '![', trim(image_file), &
                '](../../media/examples/' // trim(example_name) // '/' // trim(image_file) // ')'
            write(unit_out, '(A)') ''
        end if

        if (has_txt) then
            write(unit_out, '(A)') 'ASCII output:'
            write(unit_out, '(A)') '```'
            txt_path = OUTPUT_BASE_DIR // trim(example_name) // '/' // trim(txt_file)
            inquire(file=txt_path, exist=txt_exists)
            if (txt_exists) then
                open(newunit=unit_txt, file=txt_path, status='old', action='read', iostat=ios)
                if (ios == 0) then
                    line_count = 0
                    do
                        read(unit_txt, '(A)', iostat=ios) line
                        if (ios /= 0) exit
                        line_count = line_count + 1
                        if (line_count > MAX_ASCII_LINES) exit
                        write(unit_out, '(A)') trim(line)
                    end do
                    if (line_count > MAX_ASCII_LINES) then
                        write(unit_out, '(A)') '... (truncated)'
                    end if
                    close(unit_txt)
                else
                    write(unit_out, '(A)') 'See download link.'
                end if
            else
                write(unit_out, '(A)') 'See download link.'
            end if
            write(unit_out, '(A)') '```'
            write(unit_out, '(A)') ''
            write(unit_out, '(A,A,A)') '[Download ASCII](../../media/examples/' // &
                trim(example_name) // '/' // trim(txt_file) // ')'
            write(unit_out, '(A)') ''
        end if

        if (has_pdf) then
            write(unit_out, '(A,A,A)') '[Download PDF](../../media/examples/' // &
                trim(example_name) // '/' // trim(pdf_file) // ')'
            write(unit_out, '(A)') ''
        end if

        if (has_video) then
            write(unit_out, '(A,A,A)') '[Download Video](../../media/examples/' // &
                trim(example_name) // '/' // trim(video_file) // ')'
            write(unit_out, '(A)') ''
        end if

        if (has_other) then
            write(unit_out, '(A,A)') 'Additional files: ', trim(other_links)
            write(unit_out, '(A)') ''
        end if
    end subroutine write_media_group

    subroutine sort_string_array(values, n)
        character(len=*), intent(inout) :: values(:)
        integer, intent(in) :: n
        integer :: i, j
        character(len=len(values(1))) :: temp

        if (n <= 1) return
        do i = 2, n
            temp = values(i)
            j = i - 1
            do while (j >= 1)
                if (trim(values(j)) <= trim(temp)) exit
                values(j + 1) = values(j)
                j = j - 1
            end do
            values(j + 1) = temp
        end do
    end subroutine sort_string_array

    subroutine group_media_files(media_files, n_media, group_names, n_groups, image_files, has_image, &
        pdf_files, has_pdf, txt_files, has_txt, video_files, has_video, other_links, has_other)
        character(len=FILENAME_MAX_LEN), intent(in) :: media_files(:)
        integer, intent(in) :: n_media
        character(len=FILENAME_MAX_LEN), intent(out) :: group_names(:)
        integer, intent(out) :: n_groups
        character(len=FILENAME_MAX_LEN), intent(out) :: image_files(:), pdf_files(:), txt_files(:)
        character(len=FILENAME_MAX_LEN), intent(out) :: video_files(:), other_links(:)
        logical, intent(out) :: has_image(:), has_pdf(:), has_txt(:), has_video(:), has_other(:)

        integer :: i, idx
        character(len=:), allocatable :: ext
        character(len=FILENAME_MAX_LEN) :: base
        character(len=FILENAME_MAX_LEN) :: filename
        integer :: dot_pos

        group_names = ''
        image_files = ''
        pdf_files = ''
        txt_files = ''
        video_files = ''
        other_links = ''
        has_image = .false.
        has_pdf = .false.
        has_txt = .false.
        has_video = .false.
        has_other = .false.
        n_groups = 0

        do i = 1, n_media
            filename = trim(media_files(i))
            if (len_trim(filename) == 0) cycle

            ext = lowercase_string(trim(get_file_extension(filename)))

            base = filename
            dot_pos = index(base, '.', back=.true.)
            if (dot_pos > 0) then
                base = base(1:dot_pos-1)
            end if
            base = trim(base)
            if (len_trim(base) == 0) base = trim(filename)

            idx = find_group_index(group_names, n_groups, base)
            if (idx == 0) then
                if (n_groups >= size(group_names)) cycle
                n_groups = n_groups + 1
                group_names(n_groups) = base
                idx = n_groups
            end if

            select case (trim(ext))
            case('png','jpg','jpeg','svg')
                if (.not. has_image(idx)) image_files(idx) = filename
                has_image(idx) = .true.
            case('pdf')
                if (.not. has_pdf(idx)) pdf_files(idx) = filename
                has_pdf(idx) = .true.
            case('txt')
                if (.not. has_txt(idx)) txt_files(idx) = filename
                has_txt(idx) = .true.
            case('mp4','gif','webm','avi')
                if (.not. has_video(idx)) video_files(idx) = filename
                has_video(idx) = .true.
            case default
                has_other(idx) = .true.
                if (len_trim(other_links(idx)) == 0) then
                    other_links(idx) = filename
                else
                    other_links(idx) = trim(other_links(idx)) // ', ' // filename
                end if
            end select
        end do
    end subroutine group_media_files

    integer function find_group_index(group_names, n_groups, name) result(idx)
        character(len=FILENAME_MAX_LEN), intent(in) :: group_names(:)
        integer, intent(in) :: n_groups
        character(len=*), intent(in) :: name
        integer :: i

        idx = 0
        do i = 1, n_groups
            if (trim(group_names(i)) == trim(name)) then
                idx = i
                return
            end if
        end do
    end function find_group_index

    subroutine sort_media_groups(group_names, n_groups, image_files, has_image, pdf_files, has_pdf, &
        txt_files, has_txt, video_files, has_video, other_links, has_other)
        character(len=FILENAME_MAX_LEN), intent(inout) :: group_names(:)
        integer, intent(in) :: n_groups
        character(len=FILENAME_MAX_LEN), intent(inout) :: image_files(:), pdf_files(:)
        character(len=FILENAME_MAX_LEN), intent(inout) :: txt_files(:), video_files(:)
        character(len=FILENAME_MAX_LEN), intent(inout) :: other_links(:)
        logical, intent(inout) :: has_image(:), has_pdf(:), has_txt(:), has_video(:), has_other(:)
        integer :: i, j
        character(len=FILENAME_MAX_LEN) :: temp_name, temp_file
        logical :: temp_logical

        if (n_groups <= 1) return

        do i = 1, n_groups - 1
            do j = i + 1, n_groups
                if (trim(group_names(j)) < trim(group_names(i))) then
                    temp_name = group_names(i)
                    group_names(i) = group_names(j)
                    group_names(j) = temp_name

                    temp_file = image_files(i)
                    image_files(i) = image_files(j)
                    image_files(j) = temp_file

                    temp_logical = has_image(i)
                    has_image(i) = has_image(j)
                    has_image(j) = temp_logical

                    temp_file = pdf_files(i)
                    pdf_files(i) = pdf_files(j)
                    pdf_files(j) = temp_file

                    temp_logical = has_pdf(i)
                    has_pdf(i) = has_pdf(j)
                    has_pdf(j) = temp_logical

                    temp_file = txt_files(i)
                    txt_files(i) = txt_files(j)
                    txt_files(j) = temp_file

                    temp_logical = has_txt(i)
                    has_txt(i) = has_txt(j)
                    has_txt(j) = temp_logical

                    temp_file = video_files(i)
                    video_files(i) = video_files(j)
                    video_files(j) = temp_file

                    temp_logical = has_video(i)
                    has_video(i) = has_video(j)
                    has_video(j) = temp_logical

                    temp_file = other_links(i)
                    other_links(i) = other_links(j)
                    other_links(j) = temp_file

                    temp_logical = has_other(i)
                    has_other(i) = has_other(j)
                    has_other(j) = temp_logical
                end if
            end do
        end do
    end subroutine sort_media_groups


    ! ========================================
    ! Output Section (from doc_output)
    ! ========================================

    subroutine write_generated_outputs(unit_out, example_dir, example_name)
        integer, intent(in) :: unit_out
        character(len=*), intent(in) :: example_dir, example_name
        character(len=FILENAME_MAX_LEN) :: media_files(MAX_MEDIA_FILES)
        character(len=PATH_MAX_LEN) :: output_dir
        integer :: n_media

        ! Reference unused directory parameter to keep interface stable
        associate(unused_dir_len => len_trim(example_dir)); end associate

        ! Build output directory path
        output_dir = OUTPUT_BASE_DIR // trim(example_name)

        ! Scan for media files
        call scan_directory_for_media(output_dir, media_files, n_media)

        call write_output_section(unit_out, example_name, media_files, n_media)
    end subroutine write_generated_outputs

    subroutine scan_directory_for_media(dir_path, media_files, n_media)
        character(len=*), intent(in) :: dir_path
        character(len=*), intent(out) :: media_files(MAX_MEDIA_FILES)
        integer, intent(out) :: n_media

        integer, parameter :: TEMP_CAPACITY = MAX_MEDIA_FILES * 4
        character(len=FILENAME_MAX_LEN) :: entries(TEMP_CAPACITY)
        character(len=:), allocatable :: extension_lower
        character(len=PATH_MAX_LEN) :: full_path
        integer :: entry_count, status, i
        logical :: exists

        media_files = ''
        n_media = 0

        call list_directory_entries(trim(dir_path), entries, entry_count, status)
        if (status /= 0) return

        do i = 1, entry_count
            if (n_media >= MAX_MEDIA_FILES) exit
            if (len_trim(entries(i)) == 0) cycle

            extension_lower = lowercase_string(trim(get_file_extension(trim(entries(i)))))
            if (.not. is_media_file(trim(extension_lower))) cycle

            write(full_path, '(A,"/",A)') trim(dir_path), trim(entries(i))
            inquire(file=full_path, exist=exists)
            if (.not. exists) cycle

            n_media = n_media + 1
            media_files(n_media) = trim(entries(i))
        end do

        call sort_string_array(media_files, n_media)
    end subroutine scan_directory_for_media

    logical function is_media_file(extension)
        character(len=*), intent(in) :: extension

        select case(extension)
        case('png', 'jpg', 'jpeg', 'gif', 'svg', 'pdf', 'txt', 'dat', 'mp4', 'webm', 'avi')
            is_media_file = .true.
        case default
            is_media_file = .false.
        end select
    end function is_media_file


end module fortplot_documentation

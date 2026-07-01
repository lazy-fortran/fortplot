module fortplot_doc_processing
    !! Example processing logic for documentation generation.
    !!
    !! Handles example manifest loading, line parsing, and per-example
    !! markdown document creation.

    use fortplot_doc_constants, only: PATH_MAX_LEN, FILENAME_MAX_LEN, &
                                      LINE_MAX_LEN, MAX_EXAMPLES, &
                                      MAX_MEDIA_FILES, OUTPUT_BASE_DIR, &
                                      EXAMPLES_INDEX_PATH, INDEX_START_MARKER, &
                                      INDEX_END_MARKER, FALLBACK_COUNT, &
                                      FALLBACK_EXAMPLES
    use fortplot_doc_utils, only: build_file_path, check_file_exists, &
                                  get_file_extension, lowercase_string, &
                                  replace_extension, title_case, &
                                  build_readme_path, build_output_path, &
                                  build_fortran_url, build_local_fortran_path, &
                                  get_output_title, get_fortran_filename, &
                                  get_example_run_target
    use fortplot_directory_listing, only: list_directory_entries
    use fortplot_logging, only: log_warning
    use fortplot_doc_output, only: write_output_section, scan_directory_for_media
    implicit none
    private

    public :: get_example_count, get_example_dir, get_example_name
    public :: process_example

    ! Manifest state
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
        logical :: readme_exists, in_code_block

        summary_lines = ''
        summary_count = 0
        in_code_block = .false.

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
                    call append_summary_line(line, summary_lines, summary_count, in_code_block)
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

    subroutine append_summary_line(line, summary_lines, summary_count, in_code_block)
        character(len=*), intent(in) :: line
        character(len=LINE_MAX_LEN), intent(inout) :: summary_lines(:)
        integer, intent(inout) :: summary_count
        logical, intent(inout) :: in_code_block

        character(len=:), allocatable :: trimmed, lower

        if (summary_count >= size(summary_lines)) return

        trimmed = trim(line)
        if (len_trim(trimmed) >= 3) then
            if (trimmed(1:3) == '```') then
                in_code_block = .not. in_code_block
                summary_count = summary_count + 1
                if (summary_count <= size(summary_lines)) then
                    summary_lines(summary_count) = trimmed
                end if
                return
            end if
        end if

        if (in_code_block) then
            summary_count = summary_count + 1
            if (summary_count <= size(summary_lines)) then
                summary_lines(summary_count) = trimmed
            end if
            return
        end if

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

end module fortplot_doc_processing

module fortplot_doc_processing
    !! Example processing logic for documentation generation.
    !!
    !! Handles example manifest loading, line parsing, and per-example
    !! markdown document creation.

    use fortplot_doc_constants, only: PATH_MAX_LEN, FILENAME_MAX_LEN, &
                                      LINE_MAX_LEN, MAX_EXAMPLES, &
                                      MAX_MEDIA_FILES, OUTPUT_BASE_DIR, &
                                      EXAMPLES_ROOT
    use fortplot_doc_utils, only: lowercase_string, &
                                  replace_extension, &
                                  title_case, &
                                  build_readme_path, &
                                  build_output_path, &
                                  build_fortran_url, &
                                  build_local_fortran_path, &
                                  get_output_title, &
                                  get_fortran_filename, &
                                  get_example_run_target
    use fortplot_doc_output, only: write_output_section, scan_directory_for_media
    use fortplot_directory_listing, only: list_directory_entries
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
        !! Discover examples from the source tree.
        !!
        !! This used to scrape the generated gallery page for '- [Name](...)'
        !! bullets, which coupled the generator to the presentation of a file it
        !! also generates: changing the gallery to render cards silently matched
        !! nothing, and the empty result fell through to a hardcoded fallback
        !! list naming examples deleted long ago, which were then published.
        !! example/fortran/ is the actual source of truth.
        character(len=FILENAME_MAX_LEN), allocatable :: entries(:)
        character(len=PATH_MAX_LEN) :: source_path
        integer :: entry_count, status, i
        logical :: exists

        manifest_names = ''
        manifest_count = 0
        manifest_loaded = .true.

        allocate(entries(MAX_EXAMPLES))
        entries = ''
        call list_directory_entries(EXAMPLES_ROOT, entries, entry_count, status)
        if (status /= 0) return

        call sort_names(entries, min(entry_count, MAX_EXAMPLES))

        do i = 1, min(entry_count, MAX_EXAMPLES)
            if (len_trim(entries(i)) == 0) cycle
            ! A directory holding any Fortran source is an example; a plain file
            ! in example/fortran (README.md, say) is not. The source is not
            ! required to share the directory's name: animation/ holds
            ! save_animation_demo.f90 and ascii_heatmap/ holds
            ! ascii_heatmap_demo.f90.
            if (.not. holds_fortran_source(trim(entries(i)))) cycle
            call add_manifest_entry(entries(i))
        end do

    end subroutine load_example_manifest

    logical function holds_fortran_source(name) result(is_example)
        !! True when example/fortran/<name>/ contains at least one .f90 file.
        character(len=*), intent(in) :: name

        character(len=FILENAME_MAX_LEN), allocatable :: files(:)
        character(len=:), allocatable :: candidate
        integer :: count, status, i

        is_example = .false.
        allocate(files(MAX_MEDIA_FILES))
        files = ''
        call list_directory_entries(EXAMPLES_ROOT//'/'//name, files, count, status)
        if (status /= 0) return

        do i = 1, min(count, MAX_MEDIA_FILES)
            candidate = trim(files(i))
            if (len(candidate) < 5) cycle
            if (candidate(len(candidate) - 3:) == '.f90') then
                is_example = .true.
                return
            end if
        end do
    end function holds_fortran_source

    subroutine sort_names(values, n)
        !! Alphabetical order, so generated pages appear in a stable sequence
        !! regardless of the order the filesystem hands entries back.
        character(len=*), intent(inout) :: values(:)
        integer, intent(in) :: n
        character(len=len(values(1))) :: temp
        integer :: i, j

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
    end subroutine sort_names




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

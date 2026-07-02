module fortplot_doc_output
    !! Output generation for documentation markdown files.
    !!
    !! Handles media scanning, grouping, sorting, and writing
    !! media groups to output markdown.

    use fortplot_doc_constants, only: PATH_MAX_LEN, FILENAME_MAX_LEN, &
                                      LINE_MAX_LEN, MAX_MEDIA_FILES, &
                                      OUTPUT_BASE_DIR
    use fortplot_doc_utils, only: get_file_extension, title_case, &
                                  lowercase_string
    use fortplot_directory_listing, only: list_directory_entries
    implicit none
    private

    public :: write_output_section
    public :: write_media_group
    public :: sort_string_array
    public :: group_media_files
    public :: find_group_index
    public :: sort_media_groups
    public :: is_media_file
    public :: scan_directory_for_media
    public :: write_generated_outputs

contains

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
        logical :: has_ansi
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
            txt_path = OUTPUT_BASE_DIR // trim(example_name) // '/' // trim(txt_file)
            inquire(file=txt_path, exist=txt_exists)
            has_ansi = txt_exists .and. text_file_has_ansi(txt_path)
            if (has_ansi) then
                write(unit_out, '(A)') '<pre><code>'
            else
                write(unit_out, '(A)') '```'
            end if
            if (txt_exists) then
                open(newunit=unit_txt, file=txt_path, status='old', action='read', iostat=ios)
                if (ios == 0) then
                    line_count = 0
                    do
                        read(unit_txt, '(A)', iostat=ios) line
                        if (ios /= 0) exit
                        line_count = line_count + 1
                        if (line_count > MAX_ASCII_LINES) exit
                        if (has_ansi) then
                            write(unit_out, '(A)') ansi_line_to_html(trim(line))
                        else
                            write(unit_out, '(A)') trim(line)
                        end if
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
            if (has_ansi) then
                write(unit_out, '(A)') '</code></pre>'
            else
                write(unit_out, '(A)') '```'
            end if
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

    logical function text_file_has_ansi(path) result(has_ansi)
        character(len=*), intent(in) :: path
        character(len=LINE_MAX_LEN) :: line
        integer :: unit_txt, ios

        has_ansi = .false.
        open(newunit=unit_txt, file=path, status='old', action='read', iostat=ios)
        if (ios /= 0) return
        do
            read(unit_txt, '(A)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, achar(27)//'[') > 0) then
                has_ansi = .true.
                exit
            end if
        end do
        close(unit_txt)
    end function text_file_has_ansi

    function ansi_line_to_html(line) result(html)
        character(len=*), intent(in) :: line
        character(len=:), allocatable :: html
        integer :: i, seq_end
        logical :: span_open
        character(len=:), allocatable :: code

        html = ''
        span_open = .false.
        i = 1
        do while (i <= len_trim(line))
            if (line(i:i) == achar(27) .and. i < len_trim(line) .and. line(i+1:i+1) == '[') then
                seq_end = index(line(i:len_trim(line)), 'm')
                if (seq_end > 0) then
                    code = line(i+2:i+seq_end-2)
                    call append_ansi_span(html, code, span_open)
                    i = i + seq_end
                    cycle
                end if
            end if
            html = html // html_escape_char(line(i:i))
            i = i + 1
        end do
        if (span_open) html = html // '</span>'
    end function ansi_line_to_html

    subroutine append_ansi_span(html, code, span_open)
        character(len=:), allocatable, intent(inout) :: html
        character(len=*), intent(in) :: code
        logical, intent(inout) :: span_open

        select case (trim(code))
        case ('0')
            if (span_open) html = html // '</span>'
            span_open = .false.
        case ('31', '91')
            call open_ansi_span(html, '#d62728', span_open)
        case ('32', '92')
            call open_ansi_span(html, '#2ca02c', span_open)
        case ('34', '94')
            call open_ansi_span(html, '#1f77b4', span_open)
        case default
        end select
    end subroutine append_ansi_span

    subroutine open_ansi_span(html, color, span_open)
        character(len=:), allocatable, intent(inout) :: html
        character(len=*), intent(in) :: color
        logical, intent(inout) :: span_open

        if (span_open) html = html // '</span>'
        html = html // '<span style="color:' // trim(color) // '">'
        span_open = .true.
    end subroutine open_ansi_span

    function html_escape_char(ch) result(out)
        character(len=1), intent(in) :: ch
        character(len=:), allocatable :: out

        select case (ch)
        case ('&')
            out = '&amp;'
        case ('<')
            out = '&lt;'
        case ('>')
            out = '&gt;'
        case default
            out = ch
        end select
    end function html_escape_char

    logical function is_media_file(extension)
        character(len=*), intent(in) :: extension

        select case(extension)
        case('png', 'jpg', 'jpeg', 'gif', 'svg', 'pdf', 'txt', 'dat', 'mp4', 'webm', 'avi')
            is_media_file = .true.
        case default
            is_media_file = .false.
        end select
    end function is_media_file

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

end module fortplot_doc_output

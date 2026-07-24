program update_example_index
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortplot_directory_listing, only: list_directory_entries
    use fortplot_doc_utils, only: file_exists, title_case
    implicit none

    type :: example_entry_t
        character(len=:), allocatable :: name
        character(len=:), allocatable :: title
        character(len=:), allocatable :: description
        character(len=:), allocatable :: source_link
        logical :: has_doc
    end type example_entry_t

    character(len=*), parameter :: marker_start = &
                                   '<!-- AUTO_EXAMPLES_START -->'
    character(len=*), parameter :: marker_end = &
                                   '<!-- AUTO_EXAMPLES_END -->'
    character(len=*), parameter :: examples_root = 'example/fortran'
    character(len=*), parameter :: media_root = 'output/example/fortran'

    ! Inline styles rather than a stylesheet: FORD owns the CSS in build/doc,
    ! and a generated page that carries its own layout cannot be broken by a
    ! future theme change.
    character(len=*), parameter :: GALLERY_OPEN = &
        '<div style="display:flex;flex-wrap:wrap;gap:1.5rem;margin:1rem 0">'
    character(len=*), parameter :: CARD_OPEN = &
        '<div style="flex:1 1 300px;max-width:360px">'
    character(len=*), parameter :: THUMB_STYLE = &
        'width:100%;height:auto;border:1px solid #d0d0d0;border-radius:3px'
    character(len=*), parameter :: DESC_STYLE = &
        'display:block;font-size:0.9em;color:#555'
    character(len=*), parameter :: &
        source_prefix = &
        'https://github.com/lazy-fortran/fortplot/tree/main/' &
        //'example/fortran/'

    type(example_entry_t), allocatable :: entries(:)

    ! Only the gallery carries the example list. doc/index.md used to repeat the
    ! same 30 entries, so every example appeared three times across the site
    ! (home, Documentation, gallery); it now links here instead.
    entries = collect_entries()
    call update_page('doc/examples/index.md', entries)

contains

    function collect_entries() result(result_entries)
        type(example_entry_t), allocatable :: result_entries(:)
        character(len=:), allocatable :: names(:)
        integer :: i

        call list_examples(names)
        if (size(names) == 0) then
            allocate (result_entries(0))
            return
        end if

        allocate (result_entries(size(names)))
        do i = 1, size(names)
            call build_entry(result_entries(i), names(i))
        end do
    end function collect_entries

    subroutine list_examples(names)
        character(len=:), allocatable, intent(out) :: names(:)
        integer, parameter :: entry_len = 256
        integer, parameter :: initial_capacity = 64
        integer, parameter :: max_capacity = 4096
        character(len=entry_len), allocatable :: entries(:)
        character(len=entry_len), allocatable :: filtered(:)
        character(len=entry_len) :: candidate
        character(len=512) :: full_path
        integer :: capacity
        integer :: entry_count
        integer :: status
        integer :: valid_count
        integer :: max_len
        integer :: i

        capacity = initial_capacity
        call ensure_entry_capacity(entries, entry_len, capacity)
        do
            entries = ''
            call list_directory_entries(examples_root, entries, entry_count, status)
            if (status /= -6) exit
            if (capacity >= max_capacity) then
                call fatal('Too many example directories discovered (capacity=' &
                           //to_string(capacity)//')')
                return
            end if
            capacity = min(max_capacity, capacity*2)
            call ensure_entry_capacity(entries, entry_len, capacity)
        end do

        if (status /= 0) then
            call fatal('Unable to enumerate examples (status='// &
                       to_string(status)//')')
        end if

        if (entry_count <= 0) then
            allocate (character(len=1) :: names(0))
            return
        end if

        allocate (character(len=entry_len) :: filtered(entry_count))
        filtered = ''

        valid_count = 0
        do i = 1, entry_count
            candidate = trim(entries(i))
            if (len_trim(candidate) == 0) cycle
            if (candidate(1:1) == '.') cycle
            if (trim(candidate) == 'output') cycle

            full_path = trim(examples_root)//'/'//trim(candidate)
            if (.not. path_is_directory(trim(full_path))) cycle

            valid_count = valid_count + 1
            if (valid_count > size(filtered)) then
                call fatal('Example filtering exceeded buffer capacity')
                return
            end if
            filtered(valid_count) = trim(candidate)
        end do

        if (valid_count == 0) then
            allocate (character(len=1) :: names(0))
            return
        end if

        call sort_names(filtered, valid_count)

        max_len = 0
        do i = 1, valid_count
            max_len = max(max_len, len_trim(filtered(i)))
        end do
        if (max_len <= 0) max_len = 1

        allocate (character(len=max_len) :: names(valid_count))
        do i = 1, valid_count
            names(i) = trim(filtered(i))
        end do
    end subroutine list_examples

    subroutine sort_names(values, count)
        character(len=*), intent(inout) :: values(:)
        integer, intent(in) :: count
        integer :: i
        integer :: j
        character(len=len(values(1))) :: key

        if (count <= 1) return

        do i = 2, count
            key = values(i)
            j = i - 1
            do
                if (j < 1) exit
                if (.not. (values(j) > key)) exit
                values(j + 1) = values(j)
                j = j - 1
            end do
            values(j + 1) = key
        end do
    end subroutine sort_names

    logical function path_is_directory(path)
        character(len=*), intent(in) :: path
        logical :: exists
        ! Use the trailing-slash inquire trick: only directories report
        ! `<path>/.` as existing. Plain files would otherwise pass the old
        ! `ls path` probe and be misclassified as directories.
        path_is_directory = .false.
        inquire(file=trim(path)//"/.", exist=exists)
        if (exists) then
            path_is_directory = .true.
            return
        end if
        inquire(file=trim(path)//"/", exist=exists)
        if (exists) path_is_directory = .true.
    end function path_is_directory

    subroutine ensure_entry_capacity(buffer, string_len, new_capacity)
        integer, intent(in) :: string_len
        integer, intent(in) :: new_capacity
        character(len=string_len), allocatable, intent(inout) :: buffer(:)
        integer :: capacity_to_allocate
        integer :: i

        capacity_to_allocate = max(1, new_capacity)
        buffer = [character(len=string_len) ::('', i=1, capacity_to_allocate)]
    end subroutine ensure_entry_capacity

    subroutine build_entry(entry, raw_name)
        type(example_entry_t), intent(out) :: entry
        character(len=*), intent(in) :: raw_name
        character(len=:), allocatable :: desc
        character(len=512) :: doc_path
        character(len=512) :: readme_path
        integer :: name_len, desc_len, link_len

        name_len = len_trim(raw_name)
        allocate (character(len=name_len) :: entry%name)
        entry%name = trim(raw_name)

        entry%title = title_case(entry%name)

        doc_path = 'doc/examples/'//entry%name//'.md'
        entry%has_doc = file_exists(doc_path)

        readme_path = 'example/fortran/'//entry%name//'/README.md'
        if (file_exists(readme_path)) then
            desc = extract_description(readme_path)
        else if (entry%has_doc) then
            desc = extract_description(doc_path)
        else
            desc = 'Documentation pending; browse the source tree.'
        end if

        desc_len = max(1, len_trim(desc))
        allocate (character(len=desc_len) :: entry%description)
        if (len_trim(desc) == 0) then
            entry%description = ''
        else
            entry%description = trim(desc)
        end if

        link_len = len(source_prefix) + len_trim(entry%name)
        allocate (character(len=link_len) :: entry%source_link)
        entry%source_link = source_prefix//entry%name
    end subroutine build_entry

    subroutine update_page(path, entries)
        character(len=*), intent(in) :: path
        type(example_entry_t), intent(in) :: entries(:)
        character(len=1024), allocatable :: lines(:)
        character(len=1024), allocatable :: updated(:)
        character(len=512), allocatable :: content(:)
        character(len=256) :: desc
        character(len=256) :: link
        character(len=256) :: thumb
        integer :: count, idx_start, idx_end
        integer :: new_size, position, i, n_entries, n_content

        call read_file_lines(path, lines, count)
        idx_start = find_marker(lines, count, marker_start)
        idx_end = find_marker(lines, count, marker_end)
        if (idx_start <= 0 .or. idx_end <= 0 .or. idx_end <= idx_start) then
            call fatal('Missing auto-generation markers in '//path)
        end if

        n_entries = size(entries)
        if (n_entries == 0) then
            allocate (content(3))
            content = ''
            content(2) = '- _No examples discovered._'
            n_content = 3
        else
            ! At most 5 lines per card plus the flex wrapper and its blank
            ! separators. No blank lines may appear between the wrapper tags:
            ! a blank line closes a raw HTML block in python-markdown and the
            ! remaining cards would render as literal angle brackets.
            allocate (content(5*n_entries + 4))
            content = ''
            ! Leave content(1) blank: a raw HTML block must be preceded by an
            ! empty line to be passed through rather than escaped.
            n_content = 2
            call append(content, n_content, GALLERY_OPEN)
            do i = 1, n_entries
                if (entries(i)%has_doc) then
                    link = './'//entries(i)%name//'.html'
                    desc = truncate_desc(entries(i)%description, 120)
                    if (len_trim(desc) == 0) then
                        desc = 'See documentation page for details.'
                    end if
                else
                    link = entries(i)%source_link
                    desc = 'Documentation pending; browse the source tree.'
                end if
                call append(content, n_content, CARD_OPEN)
                call find_thumbnail(entries(i)%name, thumb)
                if (len_trim(thumb) > 0) then
                    call append(content, n_content, &
                        '<a href="'//trim(link)//'"><img src="../../media/examples/'// &
                        entries(i)%name//'/'//trim(thumb)//'" alt="'// &
                        trim(entries(i)%title)//'" loading="lazy" style="'// &
                        THUMB_STYLE//'"></a>')
                end if
                call append(content, n_content, &
                    '<a href="'//trim(link)//'"><strong>'// &
                    trim(entries(i)%title)//'</strong></a>')
                call append(content, n_content, &
                    '<span style="'//DESC_STYLE//'">'// &
                    trim(html_text(desc))//'</span>')
                call append(content, n_content, '</div>')
            end do
            call append(content, n_content, '</div>')
            ! n_content now indexes the trailing blank line that closes the
            ! raw HTML block, so it is also the line count.
        end if

        new_size = count - (idx_end - idx_start - 1) + n_content
        allocate (updated(new_size))

        position = 0
        do i = 1, idx_start
            position = position + 1
            updated(position) = lines(i)
        end do

        do i = 1, n_content
            position = position + 1
            updated(position) = content(i)
        end do

        do i = idx_end, count
            position = position + 1
            updated(position) = lines(i)
        end do

        call write_file_lines(path, updated, position)
    end subroutine update_page

    subroutine append(content, position, text)
        !! Store text at position and advance to the next free slot.
        character(len=*), intent(inout) :: content(:)
        integer, intent(inout) :: position
        character(len=*), intent(in) :: text

        if (position > size(content)) then
            call fatal('gallery content overflow')
        end if
        content(position) = text
        position = position + 1
    end subroutine append

    subroutine find_thumbnail(name, thumb)
        !! Pick the PNG that best represents an example.
        !!
        !! Prefers <name>.png, then the alphabetically first PNG. Examples whose
        !! only output is video or text (the animation demos, ascii_heatmap)
        !! legitimately have none, and get a card without an image.
        character(len=*), intent(in) :: name
        character(len=*), intent(out) :: thumb

        integer, parameter :: entry_len = 256
        integer, parameter :: capacity = 512
        character(len=entry_len), allocatable :: files(:)
        integer :: entry_count, status, i
        character(len=:), allocatable :: candidate

        thumb = ''
        allocate (character(len=entry_len) :: files(capacity))
        files = ''
        call list_directory_entries(media_root//'/'//trim(name), files, &
                                    entry_count, status)
        if (status /= 0) return

        do i = 1, min(entry_count, capacity)
            candidate = trim(files(i))
            if (len(candidate) < 5) cycle
            if (candidate(len(candidate) - 3:) /= '.png') cycle
            if (candidate == trim(name)//'.png') then
                thumb = candidate
                return
            end if
            if (len_trim(thumb) == 0) then
                thumb = candidate
            else if (candidate < trim(thumb)) then
                thumb = candidate
            end if
        end do
    end subroutine find_thumbnail

    function html_text(text) result(escaped)
        !! Escape the characters that would break out of an HTML attribute or
        !! element, and drop the backticks descriptions carry: the card sits in
        !! a raw HTML block, where markdown spans are not processed.
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: escaped
        integer :: i

        escaped = ''
        do i = 1, len_trim(text)
            select case (text(i:i))
            case ('`')
                cycle
            case ('&')
                escaped = escaped//'&amp;'
            case ('<')
                escaped = escaped//'&lt;'
            case ('>')
                escaped = escaped//'&gt;'
            case ('"')
                escaped = escaped//'&quot;'
            case default
                escaped = escaped//text(i:i)
            end select
        end do
    end function html_text

    subroutine read_file_lines(path, lines, count)
        character(len=*), intent(in) :: path
        character(len=1024), allocatable, intent(out) :: lines(:)
        integer, intent(out) :: count
        character(len=1024) :: buffer
        integer :: unit, ios, idx

        call open_file(path, unit)
        count = 0
        do
            read (unit, '(A)', iostat=ios) buffer
            if (ios /= 0) exit
            count = count + 1
        end do
        rewind (unit)
        allocate (lines(count))
        idx = 0
        do
            read (unit, '(A)', iostat=ios) buffer
            if (ios /= 0) exit
            idx = idx + 1
            lines(idx) = buffer
        end do
        close (unit)
    end subroutine read_file_lines

    subroutine write_file_lines(path, lines, count)
        character(len=*), intent(in) :: path
        character(len=1024), intent(in) :: lines(:)
        integer, intent(in) :: count
        integer :: unit, i

        open (newunit=unit, file=path, status='replace', action='write', &
              encoding='UTF-8')
        do i = 1, count
            if (len_trim(lines(i)) == 0) then
                write (unit, '(A)') ''
            else
                write (unit, '(A)') trim(lines(i))
            end if
        end do
        close (unit)
    end subroutine write_file_lines

    integer function find_marker(lines, count, marker)
        character(len=1024), intent(in) :: lines(:)
        integer, intent(in) :: count
        character(len=*), intent(in) :: marker
        integer :: i

        find_marker = -1
        do i = 1, count
            if (trim(lines(i)) == trim(marker)) then
                find_marker = i
                return
            end if
        end do
    end function find_marker

    function extract_description(path) result(description)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: description
        character(len=512) :: line
        character(len=:), allocatable :: normalized
        integer :: unit, ios

        description = ''
        if (.not. file_exists(path)) return

        call open_file(path, unit)
        do
            read (unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            normalized = trim(adjustl(line))
            if (len_trim(normalized) == 0) then
                if (len(description) > 0) exit
                cycle
            end if
            if (starts_with(normalized, 'title:')) cycle
            if (starts_with(normalized, '---')) cycle
            if (normalized(1:1) == '#') cycle
            if (starts_with(normalized, 'Source:')) cycle
            if (starts_with(normalized, 'Output ')) cycle
            if (starts_with(normalized, 'Output:')) cycle
            if (starts_with(normalized, '```')) exit
            if (len(normalized) >= 2) then
                if (normalized(1:2) == '![') cycle
            end if
            if (len(description) == 0) then
                description = normalized
            else
                description = description//' '//normalized
            end if
        end do
        close (unit)
    end function extract_description

    pure function truncate_desc(text, max_len) result(out)
        character(len=*), intent(in) :: text
        integer, intent(in) :: max_len
        character(len=256) :: out
        character(len=:), allocatable :: trimmed
        integer :: eff, target, last_space, idx, i, effective_max

        out = ''
        trimmed = trim(text)
        eff = len_trim(trimmed)
        if (eff == 0) return
        effective_max = min(max_len, len(out))
        if (effective_max <= 0) return
        if (eff <= effective_max) then
            out(1:eff) = trimmed(1:eff)
        else
            if (effective_max <= 3) then
                out(1:effective_max) = trimmed(1:effective_max)
                return
            end if
            target = effective_max - 3
            last_space = 0
            do i = 1, min(target, eff)
                if (trimmed(i:i) == ' ') last_space = i
            end do
            if (last_space > 1) then
                idx = last_space - 1
            else
                idx = target
            end if
            if (idx <= 0) idx = target
            out(1:idx) = trimmed(1:idx)
            out(idx + 1:idx + 3) = '...'
        end if
    end function truncate_desc

    subroutine open_file(path, unit)
        character(len=*), intent(in) :: path
        integer, intent(out) :: unit
        integer :: ios

        open (newunit=unit, file=path, status='old', action='read', &
              encoding='UTF-8', iostat=ios)
        if (ios /= 0) then
            call fatal('Unable to open file: '//trim(path))
        end if
    end subroutine open_file

    subroutine fatal(message)
        character(len=*), intent(in) :: message
        write (error_unit, '(A)') 'update_example_index: '//trim(message)
        stop 1
    end subroutine fatal

    pure logical function starts_with(value, prefix)
        character(len=*), intent(in) :: value
        character(len=*), intent(in) :: prefix
        character(len=:), allocatable :: lhs
        character(len=:), allocatable :: rhs
        integer :: n

        rhs = trim(prefix)
        n = len_trim(rhs)
        lhs = trim(value)
        if (n == 0) then
            starts_with = .true.
            return
        end if
        if (len(lhs) < n) then
            starts_with = .false.
        else
            starts_with = lhs(1:n) == rhs
        end if
    end function starts_with

    pure function to_string(val) result(str)
        integer, intent(in) :: val
        character(len=:), allocatable :: str
        character(len=32) :: buffer

        write (buffer, '(I0)') val
        str = trim(buffer)
    end function to_string

end program update_example_index

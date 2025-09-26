program update_example_index
    use, intrinsic :: iso_fortran_env, only: error_unit
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
    character(len=*), parameter :: temp_dir = 'build/autogen'
    character(len=*), parameter :: list_file = &
        'build/autogen/example_fortran_dirs.txt'
    character(len=*), parameter :: source_prefix = &
        'https://github.com/lazy-fortran/fortplot/tree/main/example/' &
        // 'fortran/'

    type(example_entry_t), allocatable :: entries(:)

    call ensure_directory(temp_dir)
    entries = collect_entries()
    call update_page('doc/index.md', entries, 'index')
    call update_page('doc/examples/index.md', entries, 'examples')
    call cleanup_temp_file(list_file)

contains

    function collect_entries() result(result_entries)
        type(example_entry_t), allocatable :: result_entries(:)
        character(len=:), allocatable :: names(:)
        integer :: i

        call list_examples(names)
        if (size(names) == 0) then
            allocate(result_entries(0))
            return
        end if

        allocate(result_entries(size(names)))
        do i = 1, size(names)
            call build_entry(result_entries(i), names(i))
        end do
    end function collect_entries

    subroutine list_examples(names)
        character(len=:), allocatable, intent(out) :: names(:)

        call run_command('mkdir -p ' // temp_dir, 'create temp directory')
        call run_command( &
            'find example/fortran -mindepth 1 -maxdepth 1 -type d ' // &
            '! -name output -exec basename {} \; | sort > ' // list_file, &
            'enumerate example directories')
        call read_string_list(list_file, names)
    end subroutine list_examples

    subroutine build_entry(entry, raw_name)
        type(example_entry_t), intent(out) :: entry
        character(len=*), intent(in) :: raw_name
        character(len=:), allocatable :: desc
        character(len=512) :: doc_path
        integer :: name_len, desc_len, link_len

        name_len = len_trim(raw_name)
        allocate(character(len=name_len) :: entry%name)
        entry%name = trim(raw_name)

        entry%title = title_case(entry%name)

        doc_path = 'doc/examples/' // entry%name // '.md'
        entry%has_doc = file_exists(doc_path)

        if (entry%has_doc) then
            desc = extract_description(doc_path)
        else
            desc = 'Documentation pending; browse the source tree.'
        end if

        desc_len = max(1, len_trim(desc))
        allocate(character(len=desc_len) :: entry%description)
        if (len_trim(desc) == 0) then
            entry%description = ''
        else
            entry%description = trim(desc)
        end if

        link_len = len(source_prefix) + len_trim(entry%name)
        allocate(character(len=link_len) :: entry%source_link)
        entry%source_link = source_prefix // entry%name
    end subroutine build_entry

    subroutine update_page(path, entries, context)
        character(len=*), intent(in) :: path
        type(example_entry_t), intent(in) :: entries(:)
        character(len=*), intent(in) :: context
        character(len=1024), allocatable :: lines(:)
        character(len=1024), allocatable :: updated(:)
        character(len=512), allocatable :: content(:)
        character(len=256) :: desc
        character(len=256) :: link
        character(len=512) :: bullet
        integer :: count, idx_start, idx_end
        integer :: new_size, position, i, n_entries

        call read_file_lines(path, lines, count)
        idx_start = find_marker(lines, count, marker_start)
        idx_end = find_marker(lines, count, marker_end)
        if (idx_start <= 0 .or. idx_end <= 0 .or. idx_end <= idx_start) then
            call fatal('Missing auto-generation markers in '//path)
        end if

        n_entries = size(entries)
        if (n_entries == 0) then
            allocate(content(3))
            content = ''
            content(2) = '- _No examples discovered._'
        else
            allocate(content(n_entries + 2))
            content = ''
            do i = 1, n_entries
                if (entries(i)%has_doc) then
                    if (context == 'index') then
                        link = './examples/' // entries(i)%name // '.html'
                    else
                        link = './' // entries(i)%name // '.html'
                    end if
                    desc = truncate_desc(entries(i)%description, 120)
                    if (len_trim(desc) == 0) then
                        desc = 'See documentation page for details.'
                    end if
                else
                    link = entries(i)%source_link
                    desc = 'Documentation pending; browse the source tree.'
                end if
                bullet = '- [' // trim(entries(i)%title) // '](' // &
                    trim(link) // ') - ' // trim(desc)
                content(i + 1) = bullet
            end do
        end if

        new_size = count - (idx_end - idx_start - 1) + size(content)
        allocate(updated(new_size))

        position = 0
        do i = 1, idx_start
            position = position + 1
            updated(position) = lines(i)
        end do

        do i = 1, size(content)
            position = position + 1
            updated(position) = content(i)
        end do

        do i = idx_end, count
            position = position + 1
            updated(position) = lines(i)
        end do

        call write_file_lines(path, updated, position)
    end subroutine update_page

    subroutine read_string_list(path, values)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: values(:)
        character(len=512) :: buffer
        character(len=:), allocatable :: trimmed
        integer :: unit, ios, count, idx, max_len

        call open_file(path, unit)
        count = 0
        max_len = 0
        do
            read(unit, '(A)', iostat=ios) buffer
            if (ios /= 0) exit
            trimmed = trim(buffer)
            if (len_trim(trimmed) == 0) cycle
            count = count + 1
            max_len = max(max_len, len_trim(trimmed))
        end do
        rewind(unit)
        if (count == 0) then
            allocate(character(len=1) :: values(0))
            close(unit)
            return
        end if
        if (max_len <= 0) max_len = 1
        allocate(character(len=max_len) :: values(count))
        idx = 0
        do
            read(unit, '(A)', iostat=ios) buffer
            if (ios /= 0) exit
            trimmed = trim(buffer)
            if (len_trim(trimmed) == 0) cycle
            idx = idx + 1
            values(idx) = trimmed
        end do
        close(unit)
    end subroutine read_string_list

    subroutine read_file_lines(path, lines, count)
        character(len=*), intent(in) :: path
        character(len=1024), allocatable, intent(out) :: lines(:)
        integer, intent(out) :: count
        character(len=1024) :: buffer
        integer :: unit, ios, idx

        call open_file(path, unit)
        count = 0
        do
            read(unit, '(A)', iostat=ios) buffer
            if (ios /= 0) exit
            count = count + 1
        end do
        rewind(unit)
        allocate(lines(count))
        idx = 0
        do
            read(unit, '(A)', iostat=ios) buffer
            if (ios /= 0) exit
            idx = idx + 1
            lines(idx) = buffer
        end do
        close(unit)
    end subroutine read_file_lines

    subroutine write_file_lines(path, lines, count)
        character(len=*), intent(in) :: path
        character(len=1024), intent(in) :: lines(:)
        integer, intent(in) :: count
        integer :: unit, i

        open(newunit=unit, file=path, status='replace', action='write', &
            encoding='UTF-8')
        do i = 1, count
            if (len_trim(lines(i)) == 0) then
                write(unit, '(A)') ''
            else
                write(unit, '(A)') trim(lines(i))
            end if
        end do
        close(unit)
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
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            normalized = trim(adjustl(line))
            if (len_trim(normalized) == 0) cycle
            if (starts_with(normalized, 'title:')) cycle
            if (starts_with(normalized, '---')) cycle
            if (normalized(1:1) == '#') cycle
            if (starts_with(normalized, 'Source:')) cycle
            if (starts_with(normalized, 'Output ')) cycle
            if (starts_with(normalized, 'Output:')) cycle
            if (starts_with(normalized, '```')) cycle
            if (len(normalized) >= 2) then
                if (normalized(1:2) == '![') cycle
            end if
            description = normalized
            exit
        end do
        close(unit)
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
            out(idx+1:idx+3) = '...'
        end if
    end function truncate_desc

    subroutine run_command(command, action_desc)
        character(len=*), intent(in) :: command
        character(len=*), intent(in) :: action_desc
        integer :: stat

        call execute_command_line(command, wait=.true., exitstat=stat)
        if (stat /= 0) then
            call fatal('Unable to ' // trim(action_desc) // ' (exit=' // &
                to_string(stat) // ')')
        end if
    end subroutine run_command

    subroutine open_file(path, unit)
        character(len=*), intent(in) :: path
        integer, intent(out) :: unit
        integer :: ios

        open(newunit=unit, file=path, status='old', action='read', &
            encoding='UTF-8', iostat=ios)
        if (ios /= 0) then
            call fatal('Unable to open file: ' // trim(path))
        end if
    end subroutine open_file

    subroutine ensure_directory(dir)
        character(len=*), intent(in) :: dir
        call run_command('mkdir -p ' // trim(dir), 'ensure directory ' // &
            trim(dir))
    end subroutine ensure_directory

    subroutine cleanup_temp_file(path)
        character(len=*), intent(in) :: path
        integer :: stat

        call execute_command_line('rm -f ' // trim(path), wait=.true., &
            exitstat=stat)
        if (stat /= 0) then
            write(error_unit, '(A)') 'update_example_index: warning: unable ' &
                // 'to remove temporary file ' // trim(path)
        end if
    end subroutine cleanup_temp_file

    subroutine fatal(message)
        character(len=*), intent(in) :: message
        write(error_unit, '(A)') 'update_example_index: ' // trim(message)
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

        write(buffer, '(I0)') val
        str = trim(buffer)
    end function to_string

end program update_example_index

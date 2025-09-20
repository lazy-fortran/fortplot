module fortplot_documentation_output
    !! Output generation functions for documentation
    !! Contains media scanning and output writing functionality

    use fortplot_documentation_core, only: FILENAME_MAX_LEN, PATH_MAX_LEN, get_file_extension
    use fortplot_directory_listing, only: list_directory_entries
    implicit none
    private

    public :: write_generated_outputs, scan_directory_for_media
    public :: MAX_MEDIA_FILES

    integer, parameter :: MAX_MEDIA_FILES = 32
    character(len=*), parameter :: OUTPUT_BASE_DIR = 'output/example/fortran/'

contains

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

    function lowercase_string(input) result(output)
        character(len=*), intent(in) :: input
        character(len=:), allocatable :: output
        integer :: i, char_code

        output = input
        do i = 1, len(output)
            char_code = iachar(output(i:i))
            if (char_code >= iachar('A') .and. char_code <= iachar('Z')) then
                output(i:i) = char(char_code + iachar('a') - iachar('A'))
            end if
        end do
    end function lowercase_string

    subroutine sort_string_array(strings, n)
        character(len=*), intent(inout) :: strings(:)
        integer, intent(in) :: n
        integer :: i, j
        character(len=len(strings)) :: temp

        do i = 1, n-1
            do j = i+1, n
                if (strings(i) > strings(j)) then
                    temp = strings(i)
                    strings(i) = strings(j)
                    strings(j) = temp
                end if
            end do
        end do
    end subroutine sort_string_array

    subroutine write_output_section(unit_out, example_name, media_files, n_media)
        integer, intent(in) :: unit_out
        character(len=*), intent(in) :: example_name
        character(len=*), intent(in) :: media_files(:)
        integer, intent(in) :: n_media
        integer :: i

        if (n_media == 0) return

        write(unit_out, '(A)') ''
        write(unit_out, '(A)') '## Output'
        write(unit_out, '(A)') ''

        do i = 1, n_media
            if (len_trim(media_files(i)) == 0) cycle
            call write_media_entry(unit_out, example_name, trim(media_files(i)))
        end do
    end subroutine write_output_section

    subroutine write_media_entry(unit_out, example_name, filename)
        integer, intent(in) :: unit_out
        character(len=*), intent(in) :: example_name, filename
        character(len=:), allocatable :: extension, relative_path

        extension = get_file_extension(filename)
        relative_path = '../../media/examples/' // trim(example_name) // '/' // trim(filename)

        select case(extension)
        case('png', 'jpg', 'jpeg', 'gif', 'svg')
            write(unit_out, '(A)') '![' // trim(filename) // '](' // relative_path // ')'
        case('pdf')
            write(unit_out, '(A)') '[' // trim(filename) // '](' // relative_path // ') (PDF)'
        case('txt', 'dat')
            write(unit_out, '(A)') '[' // trim(filename) // '](' // relative_path // ') (Text)'
        case('mp4', 'webm', 'avi')
            write(unit_out, '(A)') '[' // trim(filename) // '](' // relative_path // ') (Video)'
        case default
            write(unit_out, '(A)') '[' // trim(filename) // '](' // relative_path // ')'
        end select
        write(unit_out, '(A)') ''
    end subroutine write_media_entry

end module fortplot_documentation_output
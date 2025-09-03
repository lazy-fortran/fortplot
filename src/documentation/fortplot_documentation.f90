module fortplot_documentation
    !! Consolidated documentation generation module combining core utilities,
    !! processing logic, and output generation
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
    public :: get_fortran_filename

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
    integer, parameter :: MAX_EXAMPLES = 20
    integer, parameter :: MAX_MEDIA_FILES = 10

    ! Video dimensions
    integer, parameter :: VIDEO_WIDTH = 800
    integer, parameter :: VIDEO_HEIGHT = 600

    ! URL constants
    character(len=*), parameter :: GITHUB_BASE_URL = &
        'https://github.com/lazy-fortran/fortplot/blob/main/'

    ! Private constants for processing
    character(len=*), parameter :: OUTPUT_BASE_DIR = 'output/example/fortran/'

    ! Example data arrays
    integer, parameter :: N_EXAMPLES = 17
    character(len=32), parameter :: EXAMPLE_NAMES(N_EXAMPLES) = [ &
        "basic_plots         ", "line_styles         ", &
        "marker_demo         ", "format_string_demo  ", &
        "contour_demo        ", "colored_contours    ", &
        "pcolormesh_demo     ", "streamplot_demo     ", &
        "ascii_heatmap       ", "scale_examples      ", &
        "legend_demo         ", "legend_box_demo     ", &
        "unicode_demo        ", "show_viewer_demo    ", &
        "raster_backend_demo ", "pdf_backend_demo    ", &
        "text_backend_demo   " ]

contains

    ! ========================================
    ! Core Utilities Section (from doc_core)
    ! ========================================

    logical function check_file_exists(dir, filename)
        character(len=*), intent(in) :: dir, filename
        character(len=PATH_MAX_LEN) :: full_path
        logical :: exists

        ! Build full path
        full_path = trim(adjustl(dir)) // '/' // trim(adjustl(filename))

        ! Check existence
        inquire(file=full_path, exist=exists)
        check_file_exists = exists
    end function check_file_exists

    function get_file_extension(filename) result(extension)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: extension
        integer :: dot_pos

        dot_pos = index(filename, '.', back=.true.)
        if (dot_pos > 0) then
            extension = filename(dot_pos+1:)
        else
            extension = ''
        end if
    end function get_file_extension

    function replace_extension(filename, new_ext) result(new_filename)
        character(len=*), intent(in) :: filename, new_ext
        character(len=:), allocatable :: new_filename
        integer :: dot_pos

        dot_pos = index(filename, '.', back=.true.)
        if (dot_pos > 0) then
            new_filename = filename(1:dot_pos) // trim(new_ext)
        else
            new_filename = trim(filename) // '.' // trim(new_ext)
        end if
    end function replace_extension

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

    pure subroutine build_file_path(dir, filename, full_path)
        character(len=*), intent(in) :: dir, filename
        character(len=PATH_MAX_LEN), intent(out) :: full_path

        full_path = trim(adjustl(dir)) // '/' // trim(adjustl(filename))
    end subroutine build_file_path

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

    function title_case(input_str) result(output_str)
        character(len=*), intent(in) :: input_str
        character(len=:), allocatable :: output_str
        integer :: i
        logical :: capitalize_next

        output_str = input_str
        capitalize_next = .true.

        do i = 1, len(output_str)
            if (capitalize_next .and. output_str(i:i) >= 'a' .and. output_str(i:i) <= 'z') then
                output_str(i:i) = char(ichar(output_str(i:i)) - 32)
                capitalize_next = .false.
            else if (output_str(i:i) == '_' .or. output_str(i:i) == ' ') then
                output_str(i:i) = ' '
                capitalize_next = .true.
            else
                capitalize_next = .false.
            end if
        end do
    end function title_case

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
            filename = 'animation_example.f90'
        case default
            filename = trim(example_name) // '.f90'
        end select
    end subroutine get_fortran_filename

    ! ========================================
    ! Processing Section (from doc_processing)
    ! ========================================

    pure function get_example_count() result(count)
        integer :: count
        count = N_EXAMPLES
    end function get_example_count

    pure subroutine get_example_dir(index, dir)
        integer, intent(in) :: index
        character(len=PATH_MAX_LEN), intent(out) :: dir

        if (index > 0 .and. index <= N_EXAMPLES) then
            dir = "example/fortran/" // trim(EXAMPLE_NAMES(index))
        else
            dir = ""
        end if
    end subroutine get_example_dir

    pure subroutine get_example_name(index, name)
        integer, intent(in) :: index
        character(len=PATH_MAX_LEN), intent(out) :: name

        if (index > 0 .and. index <= N_EXAMPLES) then
            name = trim(EXAMPLE_NAMES(index))
        else
            name = ""
        end if
    end subroutine get_example_name

    subroutine process_example(example_dir, example_name)
        character(len=*), intent(in) :: example_dir, example_name
        character(len=PATH_MAX_LEN) :: readme_file, output_file
        integer :: unit_in, unit_out, ios
        character(len=LINE_MAX_LEN) :: line
        logical :: readme_exists

        ! Build file paths
        readme_file = trim(example_dir) // '/README.md'
        output_file = 'doc/examples/' // trim(example_name) // '.md'

        ! Check if README exists
        inquire(file=readme_file, exist=readme_exists)
        if (readme_exists) then
            print '(A,A)', '  Processing example: ', trim(example_name)

            ! Open files
            open(newunit=unit_in, file=readme_file, status='old', action='read', iostat=ios)
            if (ios /= 0) return

            open(newunit=unit_out, file=output_file, status='replace', action='write')

            ! Write header
            write(unit_out, '(A)') '# ' // title_case(example_name)
            write(unit_out, '(A)') ''

            ! Copy content from README
            do
                read(unit_in, '(A)', iostat=ios) line
                if (ios /= 0) exit
                write(unit_out, '(A)') trim(line)
            end do

            ! Add generated outputs
            write(unit_out, '(A)') ''
            write(unit_out, '(A)') '## Output'
            write(unit_out, '(A)') ''
            call write_generated_outputs(unit_out, example_dir, example_name)

            close(unit_in)
            close(unit_out)
        else
            print '(A,A)', '  Skipping (no README): ', trim(example_name)
        end if
    end subroutine process_example

    ! ========================================
    ! Output Section (from doc_output)
    ! ========================================

    subroutine write_generated_outputs(unit_out, example_dir, example_name)
        integer, intent(in) :: unit_out
        character(len=*), intent(in) :: example_dir, example_name
        character(len=FILENAME_MAX_LEN) :: media_files(MAX_MEDIA_FILES)
        character(len=PATH_MAX_LEN) :: output_dir
        integer :: n_media, i

        ! Reference unused directory parameter to keep interface stable
        associate(unused_dir_len => len_trim(example_dir)); end associate

        ! Build output directory path
        output_dir = OUTPUT_BASE_DIR // trim(example_name)

        ! Scan for media files
        call scan_directory_for_media(output_dir, media_files, n_media)

        ! Write outputs
        do i = 1, n_media
            write(unit_out, '(A)') '### ' // title_case(replace_extension(media_files(i), ''))
            write(unit_out, '(A)') ''
            
            ! Write appropriate markdown based on file type
            if (index(media_files(i), '.png') > 0 .or. index(media_files(i), '.jpg') > 0) then
                write(unit_out, '(A,A,A)') '![', trim(media_files(i)), &
                    '](../../media/examples/' // trim(example_name) // '/' // trim(media_files(i)) // ')'
            else if (index(media_files(i), '.pdf') > 0) then
                write(unit_out, '(A,A,A)') '[Download PDF](../../media/examples/' // &
                    trim(example_name) // '/' // trim(media_files(i)) // ')'
            else if (index(media_files(i), '.txt') > 0) then
                write(unit_out, '(A)') 'ASCII output:'
                write(unit_out, '(A)') '```'
                write(unit_out, '(A)') '(Content embedded here)'
                write(unit_out, '(A)') '```'
            end if
            
            write(unit_out, '(A)') ''
        end do
    end subroutine write_generated_outputs

    subroutine scan_directory_for_media(dir_path, media_files, n_media)
        character(len=*), intent(in) :: dir_path
        character(len=*), intent(out) :: media_files(MAX_MEDIA_FILES)
        integer, intent(out) :: n_media
        character(len=PATH_MAX_LEN) :: command, filename
        character(len=:), allocatable :: ext
        integer :: unit, ios

        n_media = 0

        ! List all files in directory
        write(command, '(A,A,A)') 'ls ', trim(dir_path), '/* 2>/dev/null'

        ! Open pipe to read command output
        open(newunit=unit, file='/tmp/fortplot_media_scan.tmp', status='replace')
        call execute_command_line(trim(command) // ' > /tmp/fortplot_media_scan.tmp')
        close(unit)

        open(newunit=unit, file='/tmp/fortplot_media_scan.tmp', status='old', action='read')

        do
            read(unit, '(A)', iostat=ios) filename
            if (ios /= 0) exit

            ! Extract just the filename from full path
            filename = filename(index(filename, '/', back=.true.)+1:)
            
            ! Check if media file
            ext = get_file_extension(filename)
            if (is_media_file(ext)) then
                n_media = n_media + 1
                if (n_media <= MAX_MEDIA_FILES) then
                    media_files(n_media) = trim(filename)
                end if
            end if
        end do

        close(unit, status='delete')
    end subroutine scan_directory_for_media

    logical function is_media_file(extension)
        character(len=*), intent(in) :: extension

        select case(extension)
        case('png', 'jpg', 'jpeg', 'gif', 'svg', 'pdf', 'txt', 'dat')
            is_media_file = .true.
        case default
            is_media_file = .false.
        end select
    end function is_media_file


end module fortplot_documentation
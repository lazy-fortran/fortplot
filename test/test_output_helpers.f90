module test_output_helpers
    !! Utilities for directing test artifact output into structured directories

    use fortplot_system_runtime, only: create_directory_runtime
    use, intrinsic :: iso_fortran_env, only: error_unit, int64

    implicit none
    private

    character(len=*), parameter :: preferred_output_dir = 'build/test/output/'
    character(len=*), parameter :: fallback_output_dir = 'test/output/'

    public :: ensure_test_output_dir
    public :: assert_pdf_file_valid

contains

    subroutine ensure_test_output_dir(subdir, dir)
        !! Ensure a test output directory exists and return its normalized path
        character(len=*), intent(in) :: subdir
        character(len=:), allocatable, intent(out) :: dir
        character(len=:), allocatable :: target
        character(len=:), allocatable :: fallback_target
        logical :: ok, fallback_ok

        call build_target_path(preferred_output_dir, subdir, target)
        call create_directory_runtime(target, ok)
        if (ok) then
            dir = target
            return
        end if

        call build_target_path(fallback_output_dir, subdir, fallback_target)
        call create_directory_runtime(fallback_target, fallback_ok)
        if (.not. fallback_ok) then
            write (error_unit, '(A)') &
                'ERROR: failed to create test output directory: '// &
                trim(target)
            write (error_unit, '(A)') &
                'ERROR: failed to create fallback test output directory: '// &
                trim(fallback_target)
            error stop 1
        end if

        dir = fallback_target
    contains

        subroutine build_target_path(base_dir, subdir_name, path)
            character(len=*), intent(in) :: base_dir
            character(len=*), intent(in) :: subdir_name
            character(len=:), allocatable, intent(out) :: path
            integer :: last_char

            path = trim(base_dir)
            if (len_trim(subdir_name) > 0) then
                path = path//trim(subdir_name)
            end if

            last_char = len_trim(path)
            if (last_char == 0) then
                write (error_unit, '(A)') &
                    'ERROR: computed output directory path is empty'
                error stop 1
            end if

            if (path(last_char:last_char) /= '/') then
                path = path//'/'
            end if
        end subroutine build_target_path
    end subroutine ensure_test_output_dir

    subroutine assert_pdf_file_valid(path)
        character(len=*), intent(in) :: path

        character(len=5) :: header
        integer :: unit
        integer(int64) :: size_bytes
        integer :: io_read
        integer :: io_stat
        logical :: exists

        inquire (file=trim(path), exist=exists, size=size_bytes)
        if (.not. exists) then
            write (error_unit, '(A)') 'ERROR: expected PDF file missing: '// &
                trim(path)
            error stop 1
        end if

        if (size_bytes < 512_int64) then
            write (error_unit, '(A,I0)') &
                'ERROR: PDF file too small (bytes): ', size_bytes
            write (error_unit, '(A)') 'Path: '//trim(path)
            error stop 1
        end if

        open (newunit=unit, file=trim(path), access='stream', &
              form='unformatted', status='old', action='read', iostat=io_stat)
        if (io_stat /= 0) then
            write (error_unit, '(A,I0)') &
                'ERROR: failed to open PDF file for reading, iostat: ', io_stat
            write (error_unit, '(A)') 'Path: '//trim(path)
            error stop 1
        end if

        read (unit, iostat=io_read) header
        close (unit, iostat=io_stat)
        if (io_read /= 0) then
            write (error_unit, '(A,I0)') &
                'ERROR: failed to read PDF header, iostat: ', io_read
            write (error_unit, '(A)') 'Path: '//trim(path)
            error stop 1
        end if
        if (io_stat /= 0) then
            write (error_unit, '(A,I0)') &
                'ERROR: failed to close PDF file, iostat: ', io_stat
            write (error_unit, '(A)') 'Path: '//trim(path)
            error stop 1
        end if

        if (header /= '%PDF-') then
            write (error_unit, '(A)') &
                'ERROR: file does not look like a PDF: '//trim(path)
            error stop 1
        end if
    end subroutine assert_pdf_file_valid

end module test_output_helpers

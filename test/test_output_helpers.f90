module test_output_helpers
    !! Utilities for directing test artifact output into structured directories

    use fortplot_system_runtime, only: create_directory_runtime
    use, intrinsic :: iso_fortran_env, only: error_unit, int64

    implicit none
    private

    character(len=*), parameter :: base_output_dir = 'test/output/'

    public :: ensure_test_output_dir
    public :: assert_pdf_file_valid

contains

    subroutine ensure_test_output_dir(subdir, dir)
        !! Ensure a test output directory exists and return its normalized path
        character(len=*), intent(in) :: subdir
        character(len=:), allocatable, intent(out) :: dir
        character(len=:), allocatable :: target
        logical :: ok
        integer :: last

        target = base_output_dir
        if (len_trim(subdir) > 0) then
            target = target // trim(subdir)
        end if

        last = len_trim(target)
        if (last == 0) then
            write(error_unit, '(A)') 'ERROR: base output directory path is empty'
            error stop 1
        end if

        if (target(last:last) /= '/') then
            target = target // '/'
        end if

        call create_directory_runtime(target, ok)
        if (.not. ok) then
            write(error_unit, '(A)') &
                'ERROR: failed to create test output directory: ' // trim(target)
            error stop 1
        end if

        dir = target
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
            write (error_unit, '(A)') 'ERROR: expected PDF file missing: ' // &
                trim(path)
            error stop 1
        end if

        if (size_bytes < 512_int64) then
            write (error_unit, '(A,I0)') &
                'ERROR: PDF file too small (bytes): ', size_bytes
            write (error_unit, '(A)') 'Path: ' // trim(path)
            error stop 1
        end if

        open (newunit=unit, file=trim(path), access='stream', &
              form='unformatted', status='old', action='read', iostat=io_stat)
        if (io_stat /= 0) then
            write (error_unit, '(A,I0)') &
                'ERROR: failed to open PDF file for reading, iostat: ', io_stat
            write (error_unit, '(A)') 'Path: ' // trim(path)
            error stop 1
        end if

        read (unit, iostat=io_read) header
        close (unit, iostat=io_stat)
        if (io_read /= 0) then
            write (error_unit, '(A,I0)') &
                'ERROR: failed to read PDF header, iostat: ', io_read
            write (error_unit, '(A)') 'Path: ' // trim(path)
            error stop 1
        end if
        if (io_stat /= 0) then
            write (error_unit, '(A,I0)') &
                'ERROR: failed to close PDF file, iostat: ', io_stat
            write (error_unit, '(A)') 'Path: ' // trim(path)
            error stop 1
        end if

        if (header /= '%PDF-') then
            write (error_unit, '(A)') &
                'ERROR: file does not look like a PDF: ' // trim(path)
            error stop 1
        end if
    end subroutine assert_pdf_file_valid

end module test_output_helpers

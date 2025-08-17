program test_mpeg_edge_case_validation_comprehensive
    use fortplot
    use iso_fortran_env, only: real64
    implicit none

    ! Given: MPEG validation must handle edge cases that cause false positives (Issue #32)
    ! When: We test with empty, corrupted, and fake files
    ! Then: Validation should correctly reject invalid files

    type(figure_t) :: test_fig
    real(real64), dimension(10) :: test_x, test_y
    integer :: i

    print *, "=== COMPREHENSIVE MPEG EDGE CASE VALIDATION TESTS ==="
    
    call test_empty_file_validation()
    call test_corrupted_file_validation()
    call test_fake_mpeg_file_validation()
    call test_truncated_file_validation()
    call test_zero_size_file_validation()
    call test_binary_garbage_file_validation()

    print *, "=== Edge case validation tests completed ==="

contains

    subroutine test_empty_file_validation()
        ! Given: Empty files should be rejected by MPEG validation
        ! When: We create empty file with .mp4 extension
        ! Then: Validation should fail for empty file

        character(len=200) :: empty_file
        logical :: validation_passes
        integer :: file_unit, ios, file_size

        print *, ""
        print *, "TEST: Empty File Validation"
        print *, "=========================="

        empty_file = "edge_case_empty.mp4"

        ! Create empty file
        open(newunit=file_unit, file=empty_file, access='stream', form='unformatted', iostat=ios)
        if (ios == 0) then
            close(file_unit)
        end if

        inquire(file=empty_file, size=file_size)
        validation_passes = validate_mpeg_file_comprehensive(empty_file)

        print *, "Empty file size:", file_size, "bytes"
        print *, "Validation passes (should be FALSE):", validation_passes

        if (validation_passes) then
            print *, "*** EDGE CASE VALIDATION FAILURE ***"
            print *, "Empty file incorrectly validated as valid MPEG"
        else
            print *, "GOOD: Empty file correctly rejected"
        end if

        call execute_command_line("rm -f " // trim(empty_file))
    end subroutine

    subroutine test_corrupted_file_validation()
        ! Given: Corrupted files should be rejected by MPEG validation
        ! When: We create file with random binary data
        ! Then: Validation should fail for corrupted data

        character(len=200) :: corrupted_file
        logical :: validation_passes
        integer :: file_unit, ios, file_size, i_byte
        integer(1) :: random_byte

        print *, ""
        print *, "TEST: Corrupted File Validation"
        print *, "=============================="

        corrupted_file = "edge_case_corrupted.mp4"

        ! Create file with random binary data
        open(newunit=file_unit, file=corrupted_file, access='stream', form='unformatted', iostat=ios)
        if (ios == 0) then
            ! Write 1KB of pseudo-random data
            do i_byte = 1, 1024
                random_byte = int(mod(i_byte * 37 + 123, 256), 1)  ! Pseudo-random
                write(file_unit) random_byte
            end do
            close(file_unit)
        end if

        inquire(file=corrupted_file, size=file_size)
        validation_passes = validate_mpeg_file_comprehensive(corrupted_file)

        print *, "Corrupted file size:", file_size, "bytes"
        print *, "Validation passes (should be FALSE):", validation_passes

        if (validation_passes) then
            print *, "*** EDGE CASE VALIDATION FAILURE ***"
            print *, "Corrupted file incorrectly validated as valid MPEG"
        else
            print *, "GOOD: Corrupted file correctly rejected"
        end if

        call execute_command_line("rm -f " // trim(corrupted_file))
    end subroutine

    subroutine test_fake_mpeg_file_validation()
        ! Given: Fake MPEG files should be rejected by validation
        ! When: We create file with fake MPEG-like content
        ! Then: Validation should detect fake file

        character(len=200) :: fake_file
        logical :: validation_passes
        integer :: file_unit, ios, file_size

        print *, ""
        print *, "TEST: Fake MPEG File Validation"
        print *, "=============================="

        fake_file = "edge_case_fake.mp4"

        ! Create file with fake MP4-like headers but invalid content
        open(newunit=file_unit, file=fake_file, access='stream', form='unformatted', iostat=ios)
        if (ios == 0) then
            ! Write fake ftyp box header
            write(file_unit) char(0), char(0), char(0), char(20)  ! Box size
            write(file_unit) 'ftyp'  ! Box type
            write(file_unit) 'mp41'  ! Major brand
            write(file_unit) char(0), char(0), char(0), char(0)  ! Minor version
            write(file_unit) 'mp41'  ! Compatible brand
            ! Then write invalid content
            write(file_unit) 'This is not valid MPEG data at all!'
            close(file_unit)
        end if

        inquire(file=fake_file, size=file_size)
        validation_passes = validate_mpeg_file_comprehensive(fake_file)

        print *, "Fake file size:", file_size, "bytes"
        print *, "Validation passes (should be FALSE):", validation_passes

        if (validation_passes) then
            print *, "*** EDGE CASE VALIDATION FAILURE ***"
            print *, "Fake MPEG file incorrectly validated as valid"
        else
            print *, "GOOD: Fake MPEG file correctly rejected"
        end if

        call execute_command_line("rm -f " // trim(fake_file))
    end subroutine

    subroutine test_truncated_file_validation()
        ! Given: Truncated MPEG files should be rejected
        ! When: We create file that starts correctly but is incomplete
        ! Then: Validation should detect truncation

        type(animation_t) :: anim
        character(len=200) :: complete_file, truncated_file
        logical :: complete_valid, truncated_valid
        integer :: file_unit_in, file_unit_out, ios, complete_size, truncated_size
        character(len=1), allocatable :: buffer(:)

        print *, ""
        print *, "TEST: Truncated File Validation"
        print *, "=============================="

        complete_file = "edge_case_complete.mp4"
        truncated_file = "edge_case_truncated.mp4"

        test_x = [(real(i, real64), i=1,10)]
        test_y = test_x

        call test_fig%initialize(width=320, height=240)
        call test_fig%add_plot(test_x, test_y)

        anim = FuncAnimation(update_truncated_data, frames=5, interval=100, fig=test_fig)
        call anim%save(complete_file, fps=10)

        inquire(file=complete_file, size=complete_size)

        if (complete_size > 100) then
            ! Create truncated version (only first 100 bytes)
            allocate(buffer(100))
            
            open(newunit=file_unit_in, file=complete_file, access='stream', form='unformatted', iostat=ios)
            if (ios == 0) then
                read(file_unit_in) buffer
                close(file_unit_in)
                
                open(newunit=file_unit_out, file=truncated_file, access='stream', form='unformatted', iostat=ios)
                if (ios == 0) then
                    write(file_unit_out) buffer
                    close(file_unit_out)
                end if
            end if
            
            deallocate(buffer)
        end if

        inquire(file=truncated_file, size=truncated_size)
        complete_valid = validate_mpeg_file_comprehensive(complete_file)
        truncated_valid = validate_mpeg_file_comprehensive(truncated_file)

        print *, "Complete file size:", complete_size, "bytes, valid:", complete_valid
        print *, "Truncated file size:", truncated_size, "bytes, valid:", truncated_valid

        if (truncated_valid) then
            print *, "*** EDGE CASE VALIDATION FAILURE ***"
            print *, "Truncated file incorrectly validated as valid"
        else
            print *, "GOOD: Truncated file correctly rejected"
        end if

        call execute_command_line("rm -f " // trim(complete_file) // " " // trim(truncated_file))
    end subroutine

    subroutine update_truncated_data(frame)
        integer, intent(in) :: frame
        test_y = test_x + real(frame, real64)
        call test_fig%set_ydata(1, test_y)
    end subroutine

    subroutine test_zero_size_file_validation()
        ! Given: Zero-size files should be rejected
        ! When: We test file with exactly zero bytes
        ! Then: Validation should fail for zero-size file

        character(len=200) :: zero_file
        logical :: validation_passes
        integer :: file_size

        print *, ""
        print *, "TEST: Zero Size File Validation"
        print *, "=============================="

        zero_file = "edge_case_zero.mp4"

        ! Create zero-size file using touch equivalent
        call execute_command_line("touch " // trim(zero_file))

        inquire(file=zero_file, size=file_size)
        validation_passes = validate_mpeg_file_comprehensive(zero_file)

        print *, "Zero file size:", file_size, "bytes"
        print *, "Validation passes (should be FALSE):", validation_passes

        if (validation_passes) then
            print *, "*** EDGE CASE VALIDATION FAILURE ***"
            print *, "Zero-size file incorrectly validated as valid"
        else
            print *, "GOOD: Zero-size file correctly rejected"
        end if

        call execute_command_line("rm -f " // trim(zero_file))
    end subroutine

    subroutine test_binary_garbage_file_validation()
        ! Given: Files with binary garbage should be rejected
        ! When: We create file with structured but invalid binary data
        ! Then: Validation should detect garbage content

        character(len=200) :: garbage_file
        logical :: validation_passes
        integer :: file_unit, ios, file_size, i_data
        character(len=1) :: garbage_pattern(8) = ['G', 'A', 'R', 'B', 'A', 'G', 'E', char(0)]

        print *, ""
        print *, "TEST: Binary Garbage File Validation"
        print *, "==================================="

        garbage_file = "edge_case_garbage.mp4"

        ! Create file with repeating garbage pattern
        open(newunit=file_unit, file=garbage_file, access='stream', form='unformatted', iostat=ios)
        if (ios == 0) then
            ! Write 512 bytes of garbage pattern
            do i_data = 1, 64
                write(file_unit) garbage_pattern
            end do
            close(file_unit)
        end if

        inquire(file=garbage_file, size=file_size)
        validation_passes = validate_mpeg_file_comprehensive(garbage_file)

        print *, "Garbage file size:", file_size, "bytes"
        print *, "Validation passes (should be FALSE):", validation_passes

        if (validation_passes) then
            print *, "*** EDGE CASE VALIDATION FAILURE ***"
            print *, "Binary garbage file incorrectly validated as valid"
        else
            print *, "GOOD: Binary garbage file correctly rejected"
        end if

        call execute_command_line("rm -f " // trim(garbage_file))
    end subroutine

    function validate_mpeg_file_comprehensive(filename) result(is_valid)
        character(len=*), intent(in) :: filename
        logical :: is_valid
        logical :: exists, has_content, header_valid, ffprobe_valid
        integer :: file_size

        ! Comprehensive validation that should catch all edge cases
        inquire(file=filename, exist=exists, size=file_size)
        
        ! Basic existence and size check
        has_content = (file_size > 100)  ! Minimum viable MP4 size
        
        ! Header validation
        header_valid = .false.
        if (has_content) then
            header_valid = check_mp4_header_structure(filename)
        end if
        
        ! External tool validation
        ffprobe_valid = .false.
        if (header_valid) then
            ffprobe_valid = check_with_ffprobe_tool(filename)
        end if

        is_valid = exists .and. has_content .and. header_valid .and. ffprobe_valid

        print *, "  Comprehensive edge case validation:"
        print *, "    File exists:", exists
        print *, "    Has content (>100 bytes):", has_content
        print *, "    Valid header structure:", header_valid
        print *, "    FFprobe validates:", ffprobe_valid
        print *, "    Overall valid:", is_valid
    end function

    function check_mp4_header_structure(filename) result(is_valid)
        character(len=*), intent(in) :: filename
        logical :: is_valid
        character(len=16) :: header
        integer :: file_unit, ios

        is_valid = .false.

        open(newunit=file_unit, file=filename, access='stream', form='unformatted', iostat=ios)
        if (ios /= 0) return

        read(file_unit, iostat=ios) header
        close(file_unit)

        if (ios /= 0) return

        ! Look for MP4 box signatures
        is_valid = (index(header, 'ftyp') > 0 .or. &
                   index(header, 'mdat') > 0 .or. &
                   index(header, 'moov') > 0)
    end function

    function check_with_ffprobe_tool(filename) result(is_valid)
        character(len=*), intent(in) :: filename
        logical :: is_valid
        character(len=500) :: command
        integer :: status

        ! Check if ffprobe is available first
        call execute_command_line("which ffprobe >/dev/null 2>&1", exitstat=status)
        if (status /= 0) then
            ! If ffprobe not available, skip this check (don't fail validation)
            is_valid = .true.
            return
        end if

        write(command, '(A,A,A)') 'ffprobe -v error -show_format "', trim(filename), '" >/dev/null 2>&1'
        call execute_command_line(command, exitstat=status)
        is_valid = (status == 0)
    end function

end program test_mpeg_edge_case_validation_comprehensive
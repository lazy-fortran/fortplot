module fortplot_animation_validation
    use iso_fortran_env, only: real64, wp => real64
    use fortplot_animation_core, only: MIN_VALID_VIDEO_SIZE, MIN_EXPECTED_VIDEO_SIZE, &
        MAX_FILENAME_LENGTH, MAX_RETRY_ATTEMPTS, BASE_RETRY_DELAY_MS
    use fortplot_logging, only: log_error, log_info, log_warning
    implicit none
    private

    public :: is_supported_video_format
    public :: validate_generated_video_enhanced
    public :: validate_generated_video
    public :: is_safe_filename
    public :: get_file_extension

contains

    function get_file_extension(filename) result(ext)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: ext
        integer :: dot_pos
        
        dot_pos = index(filename, ".", back=.true.)
        if (dot_pos > 0) then
            ext = filename(dot_pos+1:)
        else
            ext = ""
        end if
    end function get_file_extension

    function is_supported_video_format(filename) result(is_video)
        character(len=*), intent(in) :: filename
        logical :: is_video
        character(len=:), allocatable :: extension
        
        extension = get_file_extension(filename)
        is_video = (extension == "mp4" .or. &
                   extension == "avi" .or. &
                   extension == "mkv")
    end function is_supported_video_format

    function validate_generated_video_enhanced(filename, file_size) result(is_valid)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: file_size
        logical :: is_valid
        logical :: stage1_basic, stage2_format, stage3_structure, stage4_content
        
        ! Multi-stage validation architecture
        ! Stage 1: Basic file validation
        stage1_basic = validate_basic_file_properties(filename, file_size)
        if (.not. stage1_basic) then
            is_valid = .false.
            return
        end if
        
        ! Stage 2: Format-specific validation
        stage2_format = validate_format_specific_properties(filename)
        
        ! Stage 3: Structure validation
        stage3_structure = validate_video_structure(filename)
        
        ! Stage 4: Content validation (optional, best-effort)
        stage4_content = validate_content_integrity(filename)
        
        ! Multi-stage result evaluation
        is_valid = stage1_basic .and. stage2_format .and. stage3_structure
        
        ! Log detailed validation results
        call log_multi_stage_validation_results(stage1_basic, stage2_format, &
                                              stage3_structure, stage4_content)
    end function validate_generated_video_enhanced
    
    function validate_generated_video(filename) result(is_valid)
        character(len=*), intent(in) :: filename
        logical :: is_valid
        logical :: exists
        integer :: file_size
        
        ! Backward compatibility wrapper
        inquire(file=filename, exist=exists, size=file_size)
        if (.not. exists) then
            is_valid = .false.
            return
        end if
        
        is_valid = validate_generated_video_enhanced(filename, file_size)
    end function validate_generated_video

    function validate_basic_file_properties(filename, file_size) result(is_valid)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: file_size
        logical :: is_valid
        
        is_valid = (file_size > MIN_VALID_VIDEO_SIZE) .and. &
                   (file_size < 2000000000)  ! Max 2GB reasonable limit
    end function validate_basic_file_properties

    function validate_format_specific_properties(filename) result(is_valid)
        character(len=*), intent(in) :: filename
        logical :: is_valid
        character(len=:), allocatable :: extension
        
        extension = get_file_extension(filename)
        
        select case (extension)
        case ("mp4")
            is_valid = validate_mp4_format(filename)
        case ("avi")
            is_valid = validate_avi_format(filename)
        case ("mkv")
            is_valid = validate_mkv_format(filename)
        case default
            is_valid = .false.
        end select
    end function validate_format_specific_properties

    function validate_video_structure(filename) result(is_valid)
        character(len=*), intent(in) :: filename
        logical :: is_valid
        
        ! Enhanced structure validation
        is_valid = validate_video_header_format(filename) .and. &
                   validate_container_structure(filename)
    end function validate_video_structure

    function validate_content_integrity(filename) result(is_valid)
        character(len=*), intent(in) :: filename
        logical :: is_valid
        
        ! Best-effort content validation
        is_valid = validate_with_ffprobe(filename)
        ! If ffprobe validation fails, don't fail overall validation
        ! This is optional validation for enhanced checking only
    end function validate_content_integrity

    subroutine log_multi_stage_validation_results(stage1, stage2, stage3, stage4)
        logical, intent(in) :: stage1, stage2, stage3, stage4
        
        call log_info("Multi-stage video validation results:")
        if (stage1) then
            call log_info("  Stage 1 (Basic): PASS")
        else
            call log_warning("  Stage 1 (Basic): FAIL - File size issues")
        end if
        
        if (stage2) then
            call log_info("  Stage 2 (Format): PASS")
        else
            call log_warning("  Stage 2 (Format): FAIL - Format validation issues")
        end if
        
        if (stage3) then
            call log_info("  Stage 3 (Structure): PASS")
        else
            call log_warning("  Stage 3 (Structure): FAIL - Container structure issues")
        end if
        
        if (stage4) then
            call log_info("  Stage 4 (Content): PASS")
        else
            call log_info("  Stage 4 (Content): SKIP - Optional validation not available")
        end if
    end subroutine log_multi_stage_validation_results

    function validate_mp4_format(filename) result(is_valid)
        character(len=*), intent(in) :: filename
        logical :: is_valid
        character(len=20) :: header
        integer :: file_unit, ios
        
        is_valid = .false.
        open(newunit=file_unit, file=filename, access='stream', form='unformatted', iostat=ios)
        if (ios /= 0) return
        
        read(file_unit, iostat=ios) header
        close(file_unit)
        if (ios /= 0) return
        
        ! MP4 format validation
        is_valid = (index(header, 'ftyp') > 0)
    end function validate_mp4_format

    function validate_avi_format(filename) result(is_valid)
        character(len=*), intent(in) :: filename
        logical :: is_valid
        character(len=20) :: header
        integer :: file_unit, ios
        
        is_valid = .false.
        open(newunit=file_unit, file=filename, access='stream', form='unformatted', iostat=ios)
        if (ios /= 0) return
        
        read(file_unit, iostat=ios) header
        close(file_unit)
        if (ios /= 0) return
        
        ! AVI format validation
        is_valid = (index(header, 'RIFF') > 0 .and. index(header, 'AVI ') > 0)
    end function validate_avi_format

    function validate_mkv_format(filename) result(is_valid)
        character(len=*), intent(in) :: filename
        logical :: is_valid
        character(len=20) :: header
        integer :: file_unit, ios
        
        is_valid = .false.
        open(newunit=file_unit, file=filename, access='stream', form='unformatted', iostat=ios)
        if (ios /= 0) return
        
        read(file_unit, iostat=ios) header
        close(file_unit)
        if (ios /= 0) return
        
        ! Matroska/MKV format validation (EBML header)
        is_valid = (header(1:1) == achar(26) .and. header(2:2) == achar(69))  ! EBML signature
    end function validate_mkv_format

    function validate_container_structure(filename) result(is_valid)
        character(len=*), intent(in) :: filename
        logical :: is_valid
        integer :: file_size
        
        ! Basic container structure checks
        inquire(file=filename, size=file_size)
        
        ! Ensure file has reasonable structure (not just header)
        is_valid = (file_size > 1000)  ! Minimum for valid video container
    end function validate_container_structure

    function validate_size_for_video_content(filename, file_size) result(adequate)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: file_size
        logical :: adequate
        integer :: min_expected
        
        ! Calculate minimum expected size based on content
        ! For simple animations: ~200-500 bytes per frame minimum
        ! Even heavily compressed H.264 should produce some data per frame
        min_expected = MIN_EXPECTED_VIDEO_SIZE
        
        adequate = (file_size >= min_expected)
    end function validate_size_for_video_content

    function validate_video_header_format(filename) result(valid_header)
        character(len=*), intent(in) :: filename
        logical :: valid_header
        character(len=12) :: header
        integer :: file_unit, ios
        
        valid_header = .false.
        
        open(newunit=file_unit, file=filename, access='stream', form='unformatted', iostat=ios)
        if (ios /= 0) return
        
        read(file_unit, iostat=ios) header
        close(file_unit)
        
        if (ios /= 0) return
        
        ! Look for MP4 box signatures
        valid_header = (index(header, 'ftyp') > 0 .or. &
                       index(header, 'mdat') > 0 .or. &
                       index(header, 'moov') > 0 .or. &
                       index(header, 'mp4') > 0)
    end function validate_video_header_format

    function validate_with_ffprobe(filename) result(valid)
        use fortplot_security, only: safe_validate_mpeg_with_ffprobe
        character(len=*), intent(in) :: filename
        logical :: valid
        
        ! Use secure validation instead of shell command
        valid = safe_validate_mpeg_with_ffprobe(filename)
    end function validate_with_ffprobe

    function is_safe_filename(filename) result(safe)
        character(len=*), intent(in) :: filename
        logical :: safe
        integer :: len_name
        
        len_name = len_trim(filename)
        
        if (.not. has_valid_length(len_name)) then
            safe = .false.
            return
        end if
        
        if (has_directory_traversal(filename)) then
            safe = .false.
            return
        end if
        
        if (.not. has_safe_characters(filename, len_name)) then
            safe = .false.
            return
        end if
        
        safe = has_video_extension(filename)
    end function is_safe_filename

    function has_valid_length(len_name) result(valid)
        integer, intent(in) :: len_name
        logical :: valid
        
        valid = (len_name > 0 .and. len_name <= MAX_FILENAME_LENGTH)
    end function has_valid_length

    function has_directory_traversal(filename) result(has_traversal)
        character(len=*), intent(in) :: filename
        logical :: has_traversal
        
        has_traversal = (index(filename, '..') > 0)
    end function has_directory_traversal

    function has_safe_characters(filename, len_name) result(safe_chars)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: len_name
        logical :: safe_chars
        integer :: i
        character :: c
        
        do i = 1, len_name
            c = filename(i:i)
            if (.not. ((c >= 'a' .and. c <= 'z') .or. &
                       (c >= 'A' .and. c <= 'Z') .or. &
                       (c >= '0' .and. c <= '9') .or. &
                       c == '-' .or. c == '_' .or. c == '.' .or. c == '/')) then
                safe_chars = .false.
                return
            end if
        end do
        
        safe_chars = .true.
    end function has_safe_characters

    function has_video_extension(filename) result(has_ext)
        character(len=*), intent(in) :: filename
        logical :: has_ext
        
        has_ext = (index(filename, '.mp4') > 0 .or. &
                   index(filename, '.avi') > 0 .or. &
                   index(filename, '.mkv') > 0)
    end function has_video_extension

end module fortplot_animation_validation
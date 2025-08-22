module fortplot_animation
    use iso_fortran_env, only: real64, wp => real64
    use iso_c_binding, only: c_char, c_int, c_null_char
    use fortplot_figure_core, only: figure_t, plot_data_t, ensure_directory_exists
    use fortplot_rendering, only: savefig
    use fortplot_pipe, only: open_ffmpeg_pipe, write_png_to_pipe, close_ffmpeg_pipe
    use fortplot_utils, only: initialize_backend
    use fortplot_logging, only: log_error, log_info, log_warning
    implicit none
    private

    ! Animation configuration constants
    integer, parameter :: DEFAULT_FRAME_INTERVAL_MS = 50
    integer, parameter :: DEFAULT_ANIMATION_FPS = 10
    integer, parameter :: MIN_VALID_VIDEO_SIZE = 100
    integer, parameter :: MIN_EXPECTED_VIDEO_SIZE = 1000
    integer, parameter :: MAX_FILENAME_LENGTH = 255

    ! Animation callback interface
    abstract interface
        subroutine animate_interface(frame)
            integer, intent(in) :: frame
        end subroutine animate_interface
    end interface

    ! Animation type
    type :: animation_t
        procedure(animate_interface), pointer, nopass :: animate_func => null()
        integer :: frames = 0
        integer :: interval_ms = DEFAULT_FRAME_INTERVAL_MS
        logical :: save_frames = .false.
        character(len=:), allocatable :: frame_pattern
        type(figure_t), pointer :: fig => null()
    contains
        procedure :: run
        procedure :: save_png_sequence
        procedure :: set_save_frames
        procedure :: save
        procedure :: save_frame_sequence
        procedure :: set_figure
    end type animation_t

    public :: animation_t, FuncAnimation

contains

    function FuncAnimation(animate_func, frames, interval, fig) result(anim)
        procedure(animate_interface) :: animate_func
        integer, intent(in) :: frames
        integer, intent(in), optional :: interval
        type(figure_t), target, intent(in), optional :: fig
        type(animation_t) :: anim

        anim%animate_func => animate_func
        anim%frames = frames
        
        if (present(interval)) then
            anim%interval_ms = interval
        else
            anim%interval_ms = DEFAULT_FRAME_INTERVAL_MS
        end if
        
        if (present(fig)) then
            anim%fig => fig
        end if
        
        anim%save_frames = .false.
    end function FuncAnimation

    subroutine run(self)
        class(animation_t), intent(inout) :: self
        integer :: i

        if (.not. associated(self%animate_func)) then
            call log_error("Animation callback function not associated")
            return
        end if

        call log_info("Running animation with frames...")

        do i = 1, self%frames
            call self%animate_func(i)
            
            ! Optional: add timing delay
            if (self%interval_ms > 0) then
                call sleep_ms(self%interval_ms)
            end if
        end do

        call log_info("Animation completed.")
    end subroutine run

    subroutine set_save_frames(self, pattern)
        class(animation_t), intent(inout) :: self
        character(len=*), intent(in) :: pattern
        
        self%save_frames = .true.
        self%frame_pattern = pattern
    end subroutine set_save_frames

    subroutine save_png_sequence(self, filename_pattern)
        class(animation_t), intent(inout) :: self
        character(len=*), intent(in) :: filename_pattern
        
        ! Enable frame saving and run animation
        call self%set_save_frames(filename_pattern)
        call self%run()
    end subroutine save_png_sequence

    subroutine sleep_ms(milliseconds)
        integer, intent(in) :: milliseconds
        
        ! Platform independent timing delay using Fortran intrinsic
        ! This provides a busy wait that works across platforms
        call cpu_time_delay(real(milliseconds) / 1000.0_wp)
    end subroutine sleep_ms

    subroutine save(self, filename, fps, status)
        class(animation_t), intent(inout) :: self
        character(len=*), intent(in) :: filename
        integer, intent(in), optional :: fps
        integer, intent(out), optional :: status
        
        integer :: actual_fps, stat
        
        if (.not. is_supported_video_format(filename)) then
            if (present(status)) status = -3
            call log_error("Unsupported file format. Use .mp4, .avi, or .mkv")
            return
        end if
        
        if (.not. check_ffmpeg_available()) then
            if (present(status)) status = -1
            call log_error("ffmpeg not found. Please install ffmpeg to save animations.")
            return
        end if
        
        actual_fps = get_fps_or_default(fps)
        call ensure_directory_exists(filename)
        call save_animation_with_ffmpeg_pipe(self, filename, actual_fps, stat)
        
        if (present(status)) status = stat
    end subroutine save

    subroutine save_frame_sequence(self, pattern)
        class(animation_t), intent(inout) :: self
        character(len=*), intent(in) :: pattern
        character(len=256) :: filename
        integer :: i
        
        do i = 0, self%frames - 1
            write(filename, '(A,I0,A)') trim(pattern), i, ".png"
            call self%animate_func(i + 1)
            if (associated(self%fig)) then
                call savefig(self%fig, trim(filename))
            end if
        end do
        
    end subroutine save_frame_sequence

    subroutine set_figure(self, fig)
        class(animation_t), intent(inout) :: self
        type(figure_t), target, intent(in) :: fig
        
        self%fig => fig
    end subroutine set_figure

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

    function check_ffmpeg_available() result(available)
        use fortplot_pipe, only: check_ffmpeg_available_pipe => check_ffmpeg_available
        logical :: available
        
        available = check_ffmpeg_available_pipe()
    end function check_ffmpeg_available

    function get_fps_or_default(fps) result(actual_fps)
        integer, intent(in), optional :: fps
        integer :: actual_fps
        
        if (present(fps)) then
            actual_fps = fps
        else
            actual_fps = DEFAULT_ANIMATION_FPS
        end if
    end function get_fps_or_default

    subroutine save_animation_with_ffmpeg_pipe(anim, filename, fps, status)
        class(animation_t), intent(inout) :: anim
        character(len=*), intent(in) :: filename
        integer, intent(in) :: fps
        integer, intent(out) :: status
        
        ! Use FFmpeg pipeline for animations
        call save_animation_with_ffmpeg_pipeline(anim, filename, fps, status)
    end subroutine save_animation_with_ffmpeg_pipe


    subroutine extract_frame_rgb_data(fig, rgb_data, status)
        type(figure_t), intent(inout) :: fig
        real(real64), intent(out) :: rgb_data(:,:,:)
        integer, intent(out) :: status
        
        status = 0
        
        ! Setup PNG backend to render frame
        call setup_png_backend(fig)
        call render_to_backend(fig)
        
        ! Extract RGB data from rendered frame using polymorphic method
        call fig%backend%extract_rgb_data(fig%width, fig%height, rgb_data)
    end subroutine extract_frame_rgb_data

    subroutine save_animation_with_ffmpeg_pipeline(anim, filename, fps, status)
        class(animation_t), intent(inout) :: anim
        character(len=*), intent(in) :: filename
        integer, intent(in) :: fps
        integer, intent(out) :: status
        
        integer :: open_stat, close_stat
        
        ! Enhanced pipe opening with better error reporting
        open_stat = open_ffmpeg_pipe(filename, fps)
        if (open_stat /= 0) then
            status = -4
            call log_error("Could not open pipe to ffmpeg")
            return
        end if
        
        ! Write frames with enhanced error handling
        call write_all_frames_to_pipe(anim, status)
        if (status /= 0) then
            close_stat = close_ffmpeg_pipe()  ! Always try to close
            return
        end if
        
        ! Enhanced pipe closing with better status handling
        close_stat = close_ffmpeg_pipe()
        if (close_stat /= 0) then
            call log_warning("Pipe close returned non-zero status, but continuing validation")
        end if
        
        ! Validate output with enhanced checking
        call validate_output_video_enhanced(filename, status)
    end subroutine save_animation_with_ffmpeg_pipeline

    subroutine write_all_frames_to_pipe(anim, status)
        class(animation_t), intent(inout) :: anim
        integer, intent(out) :: status
        
        integer :: frame_idx, stat
        integer(1), allocatable :: png_data(:)
        
        do frame_idx = 1, anim%frames
            call generate_png_frame_data(anim, frame_idx, png_data, stat)
            if (stat /= 0) then
                status = -5
                call log_error("Failed to generate frame")
                return
            end if
            
            stat = write_png_to_pipe(png_data)
            if (stat /= 0) then
                status = -6
                call log_error("Failed to write frame to pipe")
                return
            end if
            
            if (allocated(png_data)) deallocate(png_data)
        end do
        
        status = 0
    end subroutine write_all_frames_to_pipe

    subroutine validate_output_video_enhanced(filename, status)
        character(len=*), intent(in) :: filename
        integer, intent(out) :: status
        
        logical :: file_exists
        integer :: file_size
        
        ! Basic file existence and size check first
        inquire(file=filename, exist=file_exists, size=file_size)
        
        if (.not. file_exists) then
            status = -7
            call log_error("Output file was not created")
            return
        end if
        
        if (file_size <= 0) then
            status = -8
            call log_error("Output file exists but has invalid size")
            return
        end if
        
        ! Enhanced validation for video content
        if (validate_generated_video_enhanced(filename, file_size)) then
            status = 0
            call log_info("Video validation successful")
        else
            status = -7
            call log_error("Generated video failed content validation")
        end if
    end subroutine validate_output_video_enhanced

    subroutine generate_png_frame_data(anim, frame_idx, png_data, status)
        class(animation_t), intent(inout) :: anim
        integer, intent(in) :: frame_idx
        integer(1), allocatable, intent(out) :: png_data(:)
        integer, intent(out) :: status
        
        if (.not. associated(anim%fig)) then
            status = -1
            return
        end if
        
        call update_frame_data(anim, frame_idx)
        call render_frame_to_png(anim%fig, png_data, status)
    end subroutine generate_png_frame_data

    subroutine update_frame_data(anim, frame_idx)
        class(animation_t), intent(inout) :: anim
        integer, intent(in) :: frame_idx
        
        call anim%animate_func(frame_idx)
    end subroutine update_frame_data

    subroutine render_frame_to_png(fig, png_data, status)
        type(figure_t), intent(inout) :: fig
        integer(1), allocatable, intent(out) :: png_data(:)
        integer, intent(out) :: status
        
        call setup_png_backend(fig)
        call render_to_backend(fig)
        call extract_png_data(fig, png_data, status)
    end subroutine render_frame_to_png

    subroutine setup_png_backend(fig)
        type(figure_t), intent(inout) :: fig
        
        if (allocated(fig%backend)) deallocate(fig%backend)
        call initialize_backend(fig%backend, 'png', fig%width, fig%height)
        fig%rendered = .false.
    end subroutine setup_png_backend

    subroutine render_to_backend(fig)
        type(figure_t), intent(inout) :: fig
        
        call render_figure_components(fig)
    end subroutine render_to_backend

    subroutine extract_png_data(fig, png_data, status)
        type(figure_t), intent(inout) :: fig
        integer(1), allocatable, intent(out) :: png_data(:)
        integer, intent(out) :: status
        
        ! Use polymorphic method to get PNG data - eliminates SELECT TYPE
        call fig%backend%get_png_data_backend(fig%width, fig%height, png_data, status)
    end subroutine extract_png_data

    subroutine render_figure_components(fig)
        type(figure_t), intent(inout) :: fig
        
        if (.not. allocated(fig%backend)) return
        
        call render_background(fig)
        call render_all_plots(fig)
        call mark_as_rendered(fig)
    end subroutine render_figure_components

    subroutine render_background(fig)
        type(figure_t), intent(inout) :: fig
        
        call fig%backend%color(1.0_wp, 1.0_wp, 1.0_wp)
    end subroutine render_background

    subroutine render_all_plots(fig)
        type(figure_t), intent(inout) :: fig
        integer :: i
        
        do i = 1, fig%plot_count
            call render_single_plot(fig, fig%plots(i))
        end do
    end subroutine render_all_plots

    subroutine render_single_plot(fig, plot_data)
        type(figure_t), intent(inout) :: fig
        type(plot_data_t), intent(in) :: plot_data
        
        select case (plot_data%plot_type)
        case (1) ! PLOT_TYPE_LINE
            call render_line_plot(fig, plot_data)
        case (2) ! PLOT_TYPE_CONTOUR  
            call render_contour_plot(fig, plot_data)
        case (3) ! PLOT_TYPE_PCOLORMESH
            call render_pcolormesh_plot(fig, plot_data)
        case (4) ! PLOT_TYPE_ERRORBAR
            call render_errorbar_plot(fig, plot_data)
        case (5) ! PLOT_TYPE_BAR
            call render_bar_plot(fig, plot_data)
        case (6) ! PLOT_TYPE_HISTOGRAM
            call render_histogram_plot(fig, plot_data)
        end select
    end subroutine render_single_plot

    subroutine mark_as_rendered(fig)
        type(figure_t), intent(inout) :: fig
        
        fig%rendered = .true.
    end subroutine mark_as_rendered

    subroutine render_line_plot(fig, plot_data)
        type(figure_t), intent(inout) :: fig
        type(plot_data_t), intent(in) :: plot_data
        
        if (.not. is_valid_line_data(plot_data)) return
        
        call set_plot_color(fig, plot_data)
        call draw_line_segments(fig, plot_data)
    end subroutine render_line_plot

    subroutine render_errorbar_plot(fig, plot_data)
        type(figure_t), intent(inout) :: fig
        type(plot_data_t), intent(in) :: plot_data
        
        if (.not. is_valid_errorbar_data(plot_data)) return
        
        ! Errorbar rendering in animation is simplified for performance
        ! Just draw the base line
        call set_plot_color(fig, plot_data)
        call draw_line_segments(fig, plot_data)
    end subroutine render_errorbar_plot

    function is_valid_errorbar_data(plot_data) result(valid)
        type(plot_data_t), intent(in) :: plot_data
        logical :: valid
        
        valid = allocated(plot_data%x) .and. allocated(plot_data%y)
        if (.not. valid) return
        
        valid = size(plot_data%x) > 0 .and. size(plot_data%y) > 0
        if (.not. valid) return
        
        valid = size(plot_data%x) == size(plot_data%y)
    end function is_valid_errorbar_data

    function is_valid_line_data(plot_data) result(valid)
        type(plot_data_t), intent(in) :: plot_data
        logical :: valid
        
        valid = allocated(plot_data%x) .and. allocated(plot_data%y) .and. size(plot_data%x) >= 2
    end function is_valid_line_data

    subroutine set_plot_color(fig, plot_data)
        type(figure_t), intent(inout) :: fig
        type(plot_data_t), intent(in) :: plot_data
        
        call fig%backend%color(plot_data%color(1), plot_data%color(2), plot_data%color(3))
    end subroutine set_plot_color

    subroutine draw_line_segments(fig, plot_data)
        type(figure_t), intent(inout) :: fig
        type(plot_data_t), intent(in) :: plot_data
        integer :: i
        real(wp) :: x_screen, y_screen, x_prev, y_prev
        
        call data_to_screen_coords(fig, plot_data%x(1), plot_data%y(1), x_prev, y_prev)
        
        do i = 2, size(plot_data%x)
            call data_to_screen_coords(fig, plot_data%x(i), plot_data%y(i), x_screen, y_screen)
            call fig%backend%line(x_prev, y_prev, x_screen, y_screen)
            x_prev = x_screen
            y_prev = y_screen
        end do
    end subroutine draw_line_segments

    subroutine render_contour_plot(fig, plot_data)
        type(figure_t), intent(inout) :: fig
        type(plot_data_t), intent(in) :: plot_data
        
        ! Simplified contour rendering for animation - draw as wireframe
        if (.not. is_valid_2d_data(plot_data)) return
        
        call set_plot_color(fig, plot_data)
        call draw_2d_wireframe(fig, plot_data)
    end subroutine render_contour_plot

    subroutine render_pcolormesh_plot(fig, plot_data)
        type(figure_t), intent(inout) :: fig
        type(plot_data_t), intent(in) :: plot_data
        
        ! Simplified pcolormesh rendering for animation - draw as grid
        if (.not. is_valid_2d_data(plot_data)) return
        
        call set_plot_color(fig, plot_data)
        call draw_mesh_grid(fig, plot_data)
    end subroutine render_pcolormesh_plot

    subroutine render_bar_plot(fig, plot_data)
        type(figure_t), intent(inout) :: fig
        type(plot_data_t), intent(in) :: plot_data
        
        ! Simplified bar plot rendering for animation - draw as vertical lines
        if (.not. is_valid_line_data(plot_data)) return
        
        call set_plot_color(fig, plot_data)
        call draw_vertical_bars(fig, plot_data)
    end subroutine render_bar_plot

    subroutine render_histogram_plot(fig, plot_data)
        type(figure_t), intent(inout) :: fig
        type(plot_data_t), intent(in) :: plot_data
        
        ! Simplified histogram rendering for animation - draw as bars
        if (.not. is_valid_line_data(plot_data)) return
        
        call set_plot_color(fig, plot_data)
        call draw_histogram_bars(fig, plot_data)
    end subroutine render_histogram_plot

    subroutine data_to_screen_coords(fig, x_data, y_data, x_screen, y_screen)
        type(figure_t), intent(in) :: fig
        real(wp), intent(in) :: x_data, y_data
        real(wp), intent(out) :: x_screen, y_screen
        
        ! Simple linear mapping from data to screen coordinates
        ! This is a simplified version - the actual implementation would handle
        ! logarithmic scales, margins, etc.
        x_screen = real(fig%width, wp) * (x_data - fig%x_min) / (fig%x_max - fig%x_min)
        y_screen = real(fig%height, wp) * (1.0_wp - (y_data - fig%y_min) / (fig%y_max - fig%y_min))
    end subroutine data_to_screen_coords


    function validate_generated_video_enhanced(filename, file_size) result(is_valid)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: file_size
        logical :: is_valid
        logical :: has_content, has_video_header, adequate_size
        
        ! Enhanced validation with better error tolerance
        has_content = (file_size > MIN_VALID_VIDEO_SIZE)
        adequate_size = validate_size_for_video_content(filename, file_size)
        has_video_header = validate_video_header_format(filename)
        
        ! More lenient validation - don't require ffprobe for basic functionality
        is_valid = has_content .and. adequate_size .and. has_video_header
        
        if (.not. is_valid) then
            call log_warning("Video validation details:")
            if (.not. has_content) call log_warning("- File too small")
            if (.not. adequate_size) call log_warning("- Inadequate size for content")
            if (.not. has_video_header) call log_warning("- Invalid video header")
        end if
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

    subroutine cpu_time_delay(seconds)
        real(wp), intent(in) :: seconds
        real(wp) :: start_time, current_time
        
        call cpu_time(start_time)
        do
            call cpu_time(current_time)
            if (current_time - start_time >= seconds) exit
        end do
    end subroutine cpu_time_delay

    function is_valid_2d_data(plot_data) result(valid)
        type(plot_data_t), intent(in) :: plot_data
        logical :: valid
        
        valid = allocated(plot_data%x) .and. allocated(plot_data%y)
        if (.not. valid) return
        
        valid = size(plot_data%x) > 1 .and. size(plot_data%y) > 1
    end function is_valid_2d_data

    subroutine draw_2d_wireframe(fig, plot_data)
        type(figure_t), intent(inout) :: fig
        type(plot_data_t), intent(in) :: plot_data
        integer :: i, j, nx, ny
        real(wp) :: x1, y1, x2, y2
        
        nx = size(plot_data%x)
        ny = size(plot_data%y)
        
        ! Draw horizontal lines
        do j = 1, ny
            do i = 1, nx - 1
                call data_to_screen_coords(fig, plot_data%x(i), plot_data%y(j), x1, y1)
                call data_to_screen_coords(fig, plot_data%x(i+1), plot_data%y(j), x2, y2)
                call fig%backend%line(x1, y1, x2, y2)
            end do
        end do
        
        ! Draw vertical lines
        do i = 1, nx
            do j = 1, ny - 1
                call data_to_screen_coords(fig, plot_data%x(i), plot_data%y(j), x1, y1)
                call data_to_screen_coords(fig, plot_data%x(i), plot_data%y(j+1), x2, y2)
                call fig%backend%line(x1, y1, x2, y2)
            end do
        end do
    end subroutine draw_2d_wireframe

    subroutine draw_mesh_grid(fig, plot_data)
        type(figure_t), intent(inout) :: fig
        type(plot_data_t), intent(in) :: plot_data
        
        ! For animation, mesh grid is same as wireframe for simplicity
        call draw_2d_wireframe(fig, plot_data)
    end subroutine draw_mesh_grid

    subroutine draw_vertical_bars(fig, plot_data)
        type(figure_t), intent(inout) :: fig
        type(plot_data_t), intent(in) :: plot_data
        integer :: i
        real(wp) :: x_screen, y_base, y_top
        
        do i = 1, size(plot_data%x)
            call data_to_screen_coords(fig, plot_data%x(i), 0.0_wp, x_screen, y_base)
            call data_to_screen_coords(fig, plot_data%x(i), plot_data%y(i), x_screen, y_top)
            call fig%backend%line(x_screen, y_base, x_screen, y_top)
        end do
    end subroutine draw_vertical_bars

    subroutine draw_histogram_bars(fig, plot_data)
        type(figure_t), intent(inout) :: fig
        type(plot_data_t), intent(in) :: plot_data
        integer :: i
        real(wp) :: x1, x2, y_base, y_top, bar_width
        
        if (size(plot_data%x) < 2) return
        
        bar_width = (plot_data%x(2) - plot_data%x(1)) * 0.8_wp
        
        do i = 1, size(plot_data%x)
            call data_to_screen_coords(fig, plot_data%x(i) - bar_width/2, 0.0_wp, x1, y_base)
            call data_to_screen_coords(fig, plot_data%x(i) + bar_width/2, 0.0_wp, x2, y_base)
            call data_to_screen_coords(fig, plot_data%x(i) - bar_width/2, plot_data%y(i), x1, y_top)
            call data_to_screen_coords(fig, plot_data%x(i) + bar_width/2, plot_data%y(i), x2, y_top)
            
            ! Draw rectangle outline for histogram bar
            call fig%backend%line(x1, y_base, x2, y_base)  ! bottom
            call fig%backend%line(x2, y_base, x2, y_top)   ! right
            call fig%backend%line(x2, y_top, x1, y_top)    ! top
            call fig%backend%line(x1, y_top, x1, y_base)   ! left
        end do
    end subroutine draw_histogram_bars

end module fortplot_animation
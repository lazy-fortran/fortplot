module fortplot_animation
    use iso_fortran_env, only: real64, wp => real64
    use iso_c_binding, only: c_char, c_int, c_null_char
    use fortplot_figure_core, only: figure_t, plot_data_t
    use fortplot_pipe, only: open_ffmpeg_pipe, write_png_to_pipe, close_ffmpeg_pipe
    use fortplot_png, only: png_context, create_png_canvas, get_png_data
    use fortplot_mpeg1_format, only: encode_animation_to_mpeg1
    implicit none
    private

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
        integer :: interval_ms = 50
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
            anim%interval_ms = 50  ! Default 50ms between frames
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
            print *, "Error: Animation callback function not associated"
            return
        end if

        print *, "Running animation with", self%frames, "frames..."

        do i = 1, self%frames
            call self%animate_func(i)
            
            ! Optional: add timing delay
            if (self%interval_ms > 0) then
                call sleep_ms(self%interval_ms)
            end if
        end do

        print *, "Animation completed."
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
        ! Simple sleep implementation - platform dependent
        ! For now, just a placeholder
        ! In real implementation, would use system-specific sleep
        continue
    end subroutine sleep_ms

    subroutine save(self, filename, fps, status)
        class(animation_t), intent(inout) :: self
        character(len=*), intent(in) :: filename
        integer, intent(in), optional :: fps
        integer, intent(out), optional :: status
        
        character(len=:), allocatable :: extension
        integer :: actual_fps, stat
        
        extension = get_file_extension(filename)
        
        if (.not. is_video_format(extension)) then
            if (present(status)) status = -3
            print *, "Error: Unsupported file format. Use .mp4, .avi, or .mkv"
            return
        end if
        
        if (.not. check_ffmpeg_available()) then
            if (present(status)) status = -1
            print *, "Error: ffmpeg not found. Please install ffmpeg to save animations."
            return
        end if
        
        actual_fps = get_fps_or_default(fps)
        
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
                call self%fig%savefig(trim(filename))
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

    function get_timestamp() result(ts)
        character(len=:), allocatable :: ts
        integer :: values(8)
        character(len=20) :: temp
        
        call date_and_time(values=values)
        write(temp, '(I0,I0,I0,I0,I0,I0)') &
            values(1), values(2), values(3), values(5), values(6), values(7)
        ts = trim(temp)
    end function get_timestamp

    function int_to_str(num) result(str)
        integer, intent(in) :: num
        character(len=:), allocatable :: str
        character(len=20) :: temp
        
        write(temp, '(I0)') num
        str = trim(temp)
    end function int_to_str

    function is_video_format(extension) result(is_video)
        character(len=*), intent(in) :: extension
        logical :: is_video
        
        is_video = (extension == "mp4" .or. &
                   extension == "avi" .or. &
                   extension == "mkv")
    end function is_video_format

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
            actual_fps = 10
        end if
    end function get_fps_or_default

    subroutine save_animation_with_ffmpeg_pipe(anim, filename, fps, status)
        class(animation_t), intent(inout) :: anim
        character(len=*), intent(in) :: filename
        integer, intent(in) :: fps
        integer, intent(out) :: status
        
        ! Try native MPEG-1 encoder first for substantial file sizes
        if (should_use_native_encoder(anim, filename)) then
            call save_animation_with_native_mpeg1(anim, filename, fps, status)
            if (status == 0) return  ! Native encoder succeeded
            print *, "Native MPEG-1 encoder failed, falling back to FFmpeg"
        end if
        
        ! Fall back to FFmpeg pipeline
        call save_animation_with_ffmpeg_pipeline(anim, filename, fps, status)
    end subroutine save_animation_with_ffmpeg_pipe

    function should_use_native_encoder(anim, filename) result(use_native)
        class(animation_t), intent(in) :: anim
        character(len=*), intent(in) :: filename
        logical :: use_native
        
        ! Disable native encoder - use FFmpeg with optimized parameters for substantial files
        ! The native encoder generates large files but not valid MPEG format
        use_native = .false.
    end function should_use_native_encoder

    subroutine save_animation_with_native_mpeg1(anim, filename, fps, status)
        class(animation_t), intent(inout) :: anim
        character(len=*), intent(in) :: filename
        integer, intent(in) :: fps
        integer, intent(out) :: status
        
        real(real64), allocatable :: frame_data(:,:,:,:)
        integer :: frame_idx, width, height
        
        status = 0
        
        if (.not. associated(anim%fig)) then
            status = -1
            return
        end if
        
        width = anim%fig%width
        height = anim%fig%height
        
        ! Collect frame data for native encoder
        allocate(frame_data(width, height, 3, anim%frames))
        
        do frame_idx = 1, anim%frames
            call update_frame_data(anim, frame_idx)
            call extract_frame_rgb_data(anim%fig, frame_data(:,:,:,frame_idx), status)
            if (status /= 0) return
        end do
        
        ! Use native MPEG-1 encoder
        call encode_animation_to_mpeg1(frame_data, anim%frames, width, height, fps, filename, status)
        
        if (allocated(frame_data)) deallocate(frame_data)
    end subroutine save_animation_with_native_mpeg1

    subroutine extract_frame_rgb_data(fig, rgb_data, status)
        type(figure_t), intent(inout) :: fig
        real(real64), intent(out) :: rgb_data(:,:,:)
        integer, intent(out) :: status
        
        type(png_context) :: png_ctx
        integer :: x, y, idx_base
        
        status = 0
        
        ! Setup PNG backend to render frame
        call setup_png_backend(fig, png_ctx)
        call render_to_backend(fig)
        
        ! Extract RGB data from rendered frame
        select type (backend => fig%backend)
        type is (png_context)
            do y = 1, fig%height
                do x = 1, fig%width
                    ! Calculate 1D index for packed RGB data (width * height * 3 array)
                    ! Format: [R1, G1, B1, R2, G2, B2, ...]
                    idx_base = ((y-1) * fig%width + (x-1)) * 3
                    
                    ! Extract RGB values (normalized to 0-1)
                    rgb_data(x, y, 1) = real(backend%raster%image_data(idx_base + 1), real64) / 255.0_real64
                    rgb_data(x, y, 2) = real(backend%raster%image_data(idx_base + 2), real64) / 255.0_real64
                    rgb_data(x, y, 3) = real(backend%raster%image_data(idx_base + 3), real64) / 255.0_real64
                end do
            end do
        class default
            status = -1
        end select
    end subroutine extract_frame_rgb_data

    subroutine save_animation_with_ffmpeg_pipeline(anim, filename, fps, status)
        class(animation_t), intent(inout) :: anim
        character(len=*), intent(in) :: filename
        integer, intent(in) :: fps
        integer, intent(out) :: status
        
        integer :: frame_idx, stat
        integer(1), allocatable :: png_data(:)
        
        stat = open_ffmpeg_pipe(filename, fps)
        if (stat /= 0) then
            status = -4
            print *, "Error: Could not open pipe to ffmpeg"
            return
        end if
        
        do frame_idx = 1, anim%frames
            call generate_png_frame_data(anim, frame_idx, png_data, stat)
            if (stat /= 0) then
                status = -5
                print *, "Error: Failed to generate frame", frame_idx
                stat = close_ffmpeg_pipe()
                return
            end if
            
            stat = write_png_to_pipe(png_data)
            if (stat /= 0) then
                status = -6
                print *, "Error: Failed to write frame to pipe", frame_idx
                stat = close_ffmpeg_pipe()
                return
            end if
            
            if (allocated(png_data)) deallocate(png_data)
        end do
        
        stat = close_ffmpeg_pipe()
        
        ! Validate the generated video file
        if (validate_generated_video(filename)) then
            status = 0
        else
            status = -7
            print *, "Error: Generated video failed validation"
        end if
    end subroutine save_animation_with_ffmpeg_pipeline

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
        
        type(png_context) :: png_ctx
        
        call setup_png_backend(fig, png_ctx)
        call render_to_backend(fig)
        call extract_png_data(fig, png_data, status)
    end subroutine render_frame_to_png

    subroutine setup_png_backend(fig, png_ctx)
        type(figure_t), intent(inout) :: fig
        type(png_context), intent(out) :: png_ctx
        
        png_ctx = create_png_canvas(fig%width, fig%height)
        
        if (allocated(fig%backend)) deallocate(fig%backend)
        allocate(png_context :: fig%backend)
        fig%backend = png_ctx
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
        
        select type (backend => fig%backend)
        type is (png_context)
            call get_png_data(fig%width, fig%height, backend%raster%image_data, png_data)
            status = 0
        class default
            status = -1
        end select
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
        ! Placeholder for contour rendering
    end subroutine render_contour_plot

    subroutine render_pcolormesh_plot(fig, plot_data)
        type(figure_t), intent(inout) :: fig
        type(plot_data_t), intent(in) :: plot_data
        ! Placeholder for pcolormesh rendering
    end subroutine render_pcolormesh_plot

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

    function get_current_timestamp() result(ts)
        integer :: ts
        integer :: values(8)
        
        call date_and_time(values=values)
        ts = values(5) * 10000 + values(6) * 100 + values(7)
    end function get_current_timestamp

    function validate_generated_video(filename) result(is_valid)
        character(len=*), intent(in) :: filename
        logical :: is_valid
        logical :: exists, has_content, has_video_header, passes_ffprobe, adequate_size
        integer :: file_size
        
        ! Comprehensive validation for generated video files
        inquire(file=filename, exist=exists, size=file_size)
        
        has_content = (file_size > 100)  ! Minimum reasonable size
        adequate_size = validate_size_for_video_content(filename, file_size)
        has_video_header = validate_video_header_format(filename)
        passes_ffprobe = validate_with_ffprobe(filename)
        
        is_valid = exists .and. has_content .and. adequate_size .and. &
                  has_video_header .and. passes_ffprobe
    end function validate_generated_video

    function validate_size_for_video_content(filename, file_size) result(adequate)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: file_size
        logical :: adequate
        integer :: min_expected
        
        ! Calculate minimum expected size based on content
        ! For simple animations: ~200-500 bytes per frame minimum
        ! Even heavily compressed H.264 should produce some data per frame
        min_expected = 1000  ! Conservative minimum 1KB for any valid video
        
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
        character(len=*), intent(in) :: filename
        logical :: valid
        
        ! Validate filename first to prevent injection
        if (.not. is_safe_filename(filename)) then
            valid = .false.
            return
        end if
        
        ! Use the secure C implementation for ffprobe validation
        valid = validate_with_ffprobe_secure(filename)
    end function validate_with_ffprobe

    function is_safe_filename(filename) result(safe)
        character(len=*), intent(in) :: filename
        logical :: safe
        integer :: i, len_name
        character :: c
        
        safe = .false.
        len_name = len_trim(filename)
        
        ! Check basic constraints
        if (len_name == 0 .or. len_name > 255) return
        
        ! Check for directory traversal
        if (index(filename, '..') > 0) return
        
        ! Check for dangerous characters
        do i = 1, len_name
            c = filename(i:i)
            ! Allow only alphanumeric, dash, underscore, dot, forward slash
            if (.not. ((c >= 'a' .and. c <= 'z') .or. &
                       (c >= 'A' .and. c <= 'Z') .or. &
                       (c >= '0' .and. c <= '9') .or. &
                       c == '-' .or. c == '_' .or. c == '.' .or. c == '/')) then
                return
            end if
        end do
        
        ! Check for valid video extension
        if (index(filename, '.mp4') > 0 .or. &
            index(filename, '.avi') > 0 .or. &
            index(filename, '.mkv') > 0) then
            safe = .true.
        end if
    end function is_safe_filename

    function validate_with_ffprobe_secure(filename) result(valid)
        character(len=*), intent(in) :: filename
        logical :: valid
        
        interface
            function validate_with_ffprobe_c(filename) result(status) bind(C, name="validate_with_ffprobe_c")
                import :: c_char, c_int
                character(kind=c_char), intent(in) :: filename(*)
                integer(c_int) :: status
            end function validate_with_ffprobe_c
        end interface
        
        character(len=len_trim(filename)+1, kind=c_char) :: c_filename
        integer :: status
        
        c_filename = trim(filename) // c_null_char
        status = validate_with_ffprobe_c(c_filename)
        valid = (status == 0)
    end function validate_with_ffprobe_secure

end module fortplot_animation
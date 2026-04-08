module fortplot_imagemagick
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_system_runtime, only: check_command_available_runtime, is_windows, delete_file_runtime
    implicit none
    private

    public :: check_imagemagick_available
    public :: compare_images_rmse
    public :: compare_images_psnr
    public :: generate_reference_image
    public :: analyze_edge_smoothness

contains

    function check_imagemagick_available() result(available)
        logical :: available

        call check_command_available_runtime("magick", available)
        if (.not. available) call check_command_available_runtime("convert", available)
    end function check_imagemagick_available

    function compare_images_rmse(image1, image2) result(rmse)
        character(len=*), intent(in) :: image1, image2
        real(wp) :: rmse

        call read_compare_metric("RMSE", image1, image2, rmse)
    end function compare_images_rmse

    function compare_images_psnr(image1, image2) result(psnr)
        character(len=*), intent(in) :: image1, image2
        real(wp) :: psnr

        call read_compare_metric("PSNR", image1, image2, psnr)
    end function compare_images_psnr

    subroutine generate_reference_image(filename, width, height)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: width, height
        character(len=:), allocatable :: command, tool
        integer :: cmdstat, exitstat

        tool = imagemagick_tool()
        if (len(tool) == 0) return

        command = trim(tool) // ' -size ' // trim(int_to_str(width)) // 'x' // trim(int_to_str(height)) // &
                  ' xc:white -stroke black -strokewidth 2 -draw "line 10,10 ' // &
                  trim(int_to_str(width - 10)) // ',' // trim(int_to_str(height - 10)) // '" -blur 0x0.5 ' // &
                  quote_argument(filename)

        call execute_command_line(command, wait=.true., exitstat=exitstat, cmdstat=cmdstat)
    end subroutine generate_reference_image

    function analyze_edge_smoothness(image_file) result(smoothness_score)
        character(len=*), intent(in) :: image_file
        real(wp) :: smoothness_score
        character(len=:), allocatable :: tool, histogram_file, command
        integer :: cmdstat, exitstat
        logical :: exists
        integer :: foreground_pixels, antialias_pixels, unique_levels

        tool = imagemagick_tool()
        if (len(tool) == 0) then
            smoothness_score = -1.0_wp
            return
        end if

        histogram_file = temp_path(image_file, 'histogram.txt')
        command = trim(tool) // ' ' // quote_argument(image_file) // &
                  ' -colorspace Gray -format %c histogram:info:- > ' // quote_argument(histogram_file)
        call execute_command_line(command, wait=.true., exitstat=exitstat, cmdstat=cmdstat)
        if (cmdstat /= 0 .or. exitstat /= 0) then
            smoothness_score = -1.0_wp
            return
        end if

        inquire(file=trim(histogram_file), exist=exists)
        if (.not. exists) then
            smoothness_score = -1.0_wp
            return
        end if

        call read_histogram_stats(histogram_file, foreground_pixels, antialias_pixels, unique_levels)
        call delete_file_runtime(histogram_file, exists)
        if (foreground_pixels <= 0) then
            smoothness_score = -1.0_wp
            return
        end if

        smoothness_score = 100.0_wp * real(antialias_pixels, wp) / real(foreground_pixels, wp)
        smoothness_score = smoothness_score * min(1.0_wp, real(unique_levels, wp) / 64.0_wp)
        smoothness_score = min(100.0_wp, max(0.0_wp, smoothness_score))
    end function analyze_edge_smoothness

    subroutine read_compare_metric(metric, image1, image2, value)
        character(len=*), intent(in) :: metric, image1, image2
        real(wp), intent(out) :: value
        character(len=:), allocatable :: tool, output_file, command
        character(len=512) :: line
        logical :: exists
        integer :: unit_id, ios, cmdstat, exitstat

        value = -1.0_wp
        tool = imagemagick_tool()
        if (len(tool) == 0) return

        output_file = temp_path(image1, trim(metric) // '.txt')
        command = compare_tool(tool) // ' -metric ' // trim(metric) // ' ' // &
                  quote_argument(image1) // ' ' // quote_argument(image2) // ' ' // null_sink() // &
                  ' 2> ' // quote_argument(output_file)

        call execute_command_line(command, wait=.true., exitstat=exitstat, cmdstat=cmdstat)
        inquire(file=trim(output_file), exist=exists)
        if (.not. exists) return

        open(newunit=unit_id, file=trim(output_file), status='old', action='read', iostat=ios)
        if (ios /= 0) then
            call delete_file_runtime(output_file, exists)
            return
        end if

        read(unit_id, '(A)', iostat=ios) line
        close(unit_id)
        call delete_file_runtime(output_file, exists)
        if (ios /= 0) return

        call parse_metric_value(trim(line), value)
    end subroutine read_compare_metric

    subroutine read_histogram_stats(histogram_file, foreground_pixels, antialias_pixels, unique_levels)
        character(len=*), intent(in) :: histogram_file
        integer, intent(out) :: foreground_pixels, antialias_pixels, unique_levels
        logical :: seen_levels(0:255)
        character(len=512) :: line
        integer :: unit_id, ios, pixel_count, gray_value
        logical :: parsed

        foreground_pixels = 0
        antialias_pixels = 0
        unique_levels = 0
        seen_levels = .false.

        open(newunit=unit_id, file=trim(histogram_file), status='old', action='read', iostat=ios)
        if (ios /= 0) return

        do
            read(unit_id, '(A)', iostat=ios) line
            if (ios /= 0) exit
            call parse_histogram_line(trim(line), pixel_count, gray_value, parsed)
            if (.not. parsed) cycle
            if (gray_value >= 255) cycle

            foreground_pixels = foreground_pixels + pixel_count
            if (gray_value <= 0) cycle

            antialias_pixels = antialias_pixels + pixel_count
            if (.not. seen_levels(gray_value)) then
                seen_levels(gray_value) = .true.
                unique_levels = unique_levels + 1
            end if
        end do

        close(unit_id)
    end subroutine read_histogram_stats

    subroutine parse_histogram_line(line, pixel_count, gray_value, parsed)
        character(len=*), intent(in) :: line
        integer, intent(out) :: pixel_count, gray_value
        logical, intent(out) :: parsed
        integer :: colon_pos, gray_start, gray_end, percent_pos, ios
        character(len=:), allocatable :: count_token, gray_token
        real(wp) :: gray_level

        pixel_count = 0
        gray_value = 0
        parsed = .false.

        colon_pos = index(line, ':')
        gray_start = index(line, 'gray(')
        if (colon_pos <= 1 .or. gray_start <= 0) return

        count_token = adjustl(line(:colon_pos - 1))
        read(count_token, *, iostat=ios) pixel_count
        if (ios /= 0) return

        gray_start = gray_start + len('gray(')
        gray_end = index(line(gray_start:), ')')
        if (gray_end <= 0) return

        gray_token = adjustl(line(gray_start:gray_start + gray_end - 2))
        percent_pos = index(gray_token, '%')
        if (percent_pos > 0) then
            read(gray_token(:percent_pos - 1), *, iostat=ios) gray_level
            if (ios /= 0) return
            gray_value = nint(255.0_wp * gray_level / 100.0_wp)
        else
            read(gray_token, *, iostat=ios) gray_level
            if (ios /= 0) return
            gray_value = nint(gray_level)
        end if

        gray_value = max(0, min(255, gray_value))
        parsed = .true.
    end subroutine parse_histogram_line

    subroutine parse_metric_value(line, value)
        character(len=*), intent(in) :: line
        real(wp), intent(out) :: value
        character(len=:), allocatable :: token
        integer :: i, ios

        value = -1.0_wp
        if (index(line, 'inf') > 0 .or. index(line, 'INF') > 0) then
            value = 100.0_wp
            return
        end if

        token = ''
        do i = 1, len_trim(line)
            if (line(i:i) == ' ' .or. line(i:i) == '(') exit
            token = token // line(i:i)
        end do
        if (len(token) == 0) return

        read(token, *, iostat=ios) value
        if (ios /= 0) value = -1.0_wp
    end subroutine parse_metric_value

    function imagemagick_tool() result(tool)
        character(len=:), allocatable :: tool
        logical :: available

        call check_command_available_runtime("magick", available)
        if (available) then
            tool = "magick"
            return
        end if

        call check_command_available_runtime("convert", available)
        if (available) then
            tool = "convert"
        else
            tool = ""
        end if
    end function imagemagick_tool

    function compare_tool(tool) result(command)
        character(len=*), intent(in) :: tool
        character(len=:), allocatable :: command

        if (trim(tool) == 'magick') then
            command = 'magick compare'
        else
            command = 'compare'
        end if
    end function compare_tool

    function temp_path(seed, suffix) result(path)
        character(len=*), intent(in) :: seed, suffix
        character(len=:), allocatable :: path
        integer :: count, rate, max_count

        call system_clock(count, rate, max_count)
        path = trim(seed) // '.' // trim(int_to_str(count)) // '.' // trim(suffix)
    end function temp_path

    function null_sink() result(path)
        character(len=:), allocatable :: path

        if (is_windows()) then
            path = 'NUL'
        else
            path = '/dev/null'
        end if
    end function null_sink

    function quote_argument(text) result(quoted)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: quoted

        if (is_windows()) then
            quoted = '"' // replace_all(trim(text), '"', '""') // '"'
        else
            quoted = "'" // replace_all(trim(text), "'", "'\''") // "'"
        end if
    end function quote_argument

    function replace_all(text, old, new) result(updated)
        character(len=*), intent(in) :: text, old, new
        character(len=:), allocatable :: updated
        integer :: pos, old_len

        updated = text
        old_len = len(old)
        if (old_len == 0) return

        pos = index(updated, old)
        do while (pos > 0)
            if (pos == 1) then
                updated = new // updated(pos + old_len:)
            else if (pos + old_len > len(updated)) then
                updated = updated(:pos - 1) // new
            else
                updated = updated(:pos - 1) // new // updated(pos + old_len:)
            end if
            pos = index(updated, old)
        end do
    end function replace_all

    function int_to_str(n) result(str)
        integer, intent(in) :: n
        character(len=32) :: str

        write(str, '(I0)') n
    end function int_to_str

end module fortplot_imagemagick

program fortplot_truetype_benchmark
    use fortplot_truetype
    use fortplot_text, only: find_any_available_font
    use, intrinsic :: iso_fortran_env, only: wp => real64, int8, int64
    implicit none

    type(truetype_font_t) :: font
    character(len=256) :: font_path, arg
    integer :: iterations
    integer :: start_count, end_count, count_rate
    integer(int64) :: checksum, total_pixels
    real(wp) :: elapsed_ms

    iterations = 200
    if (.not. find_any_available_font(font_path)) stop 1

    call get_command_argument(1, arg)
    if (len_trim(arg) > 0) font_path = trim(arg)

    arg = ''
    call get_command_argument(2, arg)
    if (len_trim(arg) > 0) read(arg, *) iterations

    if (.not. font%init(trim(font_path))) stop 2

    call system_clock(start_count, count_rate)
    call run_workload(font, iterations, checksum, total_pixels)
    call system_clock(end_count)

    elapsed_ms = real(end_count - start_count, wp) * 1000.0_wp / real(count_rate, wp)

    print '(A)', 'impl=fortran'
    print '(A)', 'font=' // trim(font_path)
    print '(A,I0)', 'iterations=', iterations
    print '(A,I0)', 'glyphs=', iterations * 3 * (126 - 33 + 1)
    print '(A,I0)', 'pixels=', total_pixels
    print '(A,I0)', 'checksum=', checksum
    print '(A,F0.3)', 'elapsed_ms=', elapsed_ms

    call font%cleanup()

contains

    subroutine run_workload(font, iterations, checksum, total_pixels)
        type(truetype_font_t), intent(in) :: font
        integer, intent(in) :: iterations
        integer(int64), intent(out) :: checksum, total_pixels

        integer, parameter :: sizes(3) = [16, 24, 32]
        integer(int8), allocatable :: bitmap(:)
        real(wp) :: scale
        integer :: iter, si, cp, width, height, xoff, yoff

        checksum = 0_int64
        total_pixels = 0_int64

        do iter = 1, iterations
            do si = 1, size(sizes)
                scale = font%scale_for_pixel_height(real(sizes(si), wp))
                do cp = 33, 126
                    call font%get_codepoint_bitmap(scale, scale, cp, bitmap, &
                        width, height, xoff, yoff)
                    if (allocated(bitmap)) then
                        checksum = checksum + sum(int(iand(int(bitmap), 255), int64))
                        total_pixels = total_pixels + int(size(bitmap), int64)
                        deallocate(bitmap)
                    end if
                end do
            end do
        end do
    end subroutine run_workload

end program fortplot_truetype_benchmark

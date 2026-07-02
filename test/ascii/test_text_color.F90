program test_text_color
    !! ANSI color modes for the text backend (issue #2062).
    !!
    !! Proves that saved .txt output stays plain by default and under 'never',
    !! that 'ansi16' wraps colored plot glyphs in SGR foreground spans with a
    !! reset before every line/frame boundary, that 'auto' resolves
    !! conservatively for file output and honors NO_COLOR / TERM=dumb, and that
    !! CLICOLOR_FORCE forces color. Includes negative fixtures: a colored line
    !! missing its trailing reset must fail.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use, intrinsic :: iso_c_binding, only: c_int, c_char, c_null_char
    use fortplot_figure, only: figure_t
    use fortplot_text_color, only: resolve_text_color_mode_terminal, &
                                   resolve_text_color_mode_file, sgr_reset
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none

    type :: env_buffer_t
        character(kind=c_char), allocatable :: value(:)
    end type env_buffer_t

    character(len=1), parameter :: ESC = achar(27)
    logical :: dir_ok

    interface
        integer(c_int) function c_putenv(name_value) bind(c, name="putenv")
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: name_value(*)
        end function c_putenv
    end interface

    call create_directory_runtime('build/test/output', dir_ok)

    call test_default_is_plain()
    call test_never_is_plain()
    call test_ansi16_has_spans_and_resets()
    call test_unicode_ansi16_has_spans()
    call test_pcolormesh_ansi16_has_spans()
    call test_auto_file_is_plain()
    call test_resolver_env_policy()

    print *, 'PASS: test_text_color'

contains

    subroutine build_line_figure(fig)
        type(figure_t), intent(out) :: fig
        real(wp) :: x(60), sx(60)
        integer :: i

        x = [(real(i, wp), i=0, size(x) - 1)]/6.0_wp
        sx = sin(x)
        call fig%initialize(80, 24)
        call fig%set_title('Color Test')
        call fig%add_plot(x, sx, label='sin(x)')
    end subroutine build_line_figure

    subroutine test_default_is_plain()
        !! No color mode requested: saved .txt must contain zero ESC bytes.
        character(len=*), parameter :: outfile = &
                                       'build/test/output/color_default_2062.txt'
        type(figure_t) :: fig
        character(len=:), allocatable :: bytes

        call build_line_figure(fig)
        call fig%savefig(outfile)

        bytes = read_file_bytes(outfile)
        if (len(bytes) <= 0) then
            print *, 'FAIL: default color produced no output'
            stop 1
        end if
        if (index(bytes, ESC) > 0) then
            print *, 'FAIL: default .txt contains ESC bytes'
            stop 1
        end if
    end subroutine test_default_is_plain

    subroutine test_never_is_plain()
        !! Explicit 'never': saved .txt must contain zero ESC bytes.
        character(len=*), parameter :: outfile = &
                                       'build/test/output/color_never_2062.txt'
        type(figure_t) :: fig
        character(len=:), allocatable :: bytes

        call build_line_figure(fig)
        call fig%set_text_color_mode('never')
        call fig%savefig(outfile)

        bytes = read_file_bytes(outfile)
        if (index(bytes, ESC) > 0) then
            print *, "FAIL: color='never' .txt contains ESC bytes"
            stop 1
        end if
    end subroutine test_never_is_plain

    subroutine test_ansi16_has_spans_and_resets()
        !! Positive fixture: ansi16 emits SGR foreground escapes and reset
        !! sequences around colored glyphs. Negative fixture: every line that
        !! opens a color must reset before its trailing border.
        character(len=*), parameter :: outfile = &
                                       'build/test/output/color_ansi16_2062.txt'
        type(figure_t) :: fig
        character(len=:), allocatable :: bytes

        call build_line_figure(fig)
        call fig%set_text_color_mode('ansi16')
        call fig%savefig(outfile)

        bytes = read_file_bytes(outfile)
        if (index(bytes, ESC//'[') == 0) then
            print *, "FAIL: ansi16 emitted no SGR escape"
            stop 1
        end if
        if (index(bytes, sgr_reset()) == 0) then
            print *, "FAIL: ansi16 emitted no reset sequence"
            stop 1
        end if
        call assert_each_colored_line_resets(bytes)
    end subroutine test_ansi16_has_spans_and_resets

    subroutine test_unicode_ansi16_has_spans()
        character(len=*), parameter :: outfile = &
                                       'build/test/output/color_unicode_ansi16_2062.txt'
        type(figure_t) :: fig
        character(len=:), allocatable :: bytes

        call build_line_figure(fig)
        call fig%set_text_charset('unicode')
        call fig%set_text_color_mode('ansi16')
        call fig%savefig(outfile)

        bytes = read_file_bytes(outfile)
        if (index(bytes, '┌') == 0) then
            print *, "FAIL: unicode color fixture missing Unicode frame"
            stop 1
        end if
        if (index(bytes, ESC//'[') == 0) then
            print *, "FAIL: unicode ansi16 emitted no SGR escape"
            stop 1
        end if
        call assert_each_colored_line_resets(bytes)
    end subroutine test_unicode_ansi16_has_spans

    subroutine test_pcolormesh_ansi16_has_spans()
        character(len=*), parameter :: outfile = &
                                       'build/test/output/color_pcolormesh_ansi16.txt'
        type(figure_t) :: fig
        integer, parameter :: nx = 13, ny = 13
        real(wp) :: x(nx), y(ny), z(ny - 1, nx - 1)
        character(len=:), allocatable :: bytes
        integer :: i, j

        do i = 1, nx
            x(i) = -3.0_wp + 6.0_wp*real(i - 1, wp)/real(nx - 1, wp)
        end do
        do j = 1, ny
            y(j) = -2.0_wp + 4.0_wp*real(j - 1, wp)/real(ny - 1, wp)
        end do
        do i = 1, nx - 1
            do j = 1, ny - 1
                z(j, i) = sin(x(i))*cos(y(j))
            end do
        end do

        call fig%initialize(80, 24)
        call fig%set_text_charset('unicode')
        call fig%set_text_color_mode('ansi16')
        call fig%add_pcolormesh(x, y, z, cmap='plasma')
        call fig%savefig(outfile)

        bytes = read_file_bytes(outfile)
        if (index(bytes, '┌') == 0) then
            print *, 'FAIL: pcolormesh color fixture missing Unicode frame'
            stop 1
        end if
        if (index(bytes, ESC//'[') == 0) then
            print *, 'FAIL: pcolormesh ansi16 emitted no SGR escape'
            stop 1
        end if
        call assert_contains(bytes, '1.5', &
                             'pcolormesh color split positive tick label')
        call assert_contains(bytes, '-0.5', &
                             'pcolormesh color split negative tick label')
        call assert_each_colored_line_resets(bytes)
    end subroutine test_pcolormesh_ansi16_has_spans

    subroutine assert_contains(bytes, needle, message)
        character(len=*), intent(in) :: bytes, needle, message

        if (index(bytes, needle) == 0) then
            print *, 'FAIL: ', message
            stop 1
        end if
    end subroutine assert_contains

    subroutine assert_each_colored_line_resets(bytes)
        !! For every line that contains an ESC, the last escape on that line must
        !! be the reset ESC[0m. This fails if a colored span reaches the newline
        !! without a reset (negative fixture from the issue).
        character(len=*), intent(in) :: bytes
        integer :: start, nl, line_end
        character(len=:), allocatable :: reset

        reset = sgr_reset()
        start = 1
        do
            if (start > len(bytes)) exit
            nl = index(bytes(start:), achar(10))
            if (nl == 0) then
                line_end = len(bytes)
            else
                line_end = start + nl - 2
            end if
            if (line_end >= start) then
                call check_line_reset(bytes(start:line_end), reset)
            end if
            if (nl == 0) exit
            start = start + nl
        end do
    end subroutine assert_each_colored_line_resets

    subroutine check_line_reset(line, reset)
        character(len=*), intent(in) :: line, reset
        integer :: p, last_esc, i

        last_esc = 0
        do i = 1, len(line)
            if (line(i:i) == ESC) last_esc = i
        end do
        if (last_esc == 0) return
        p = last_esc + len(reset) - 1
        if (p > len(line)) then
            print *, 'FAIL: colored line ends mid-escape without reset'
            stop 1
        end if
        if (line(last_esc:p) /= reset) then
            print *, 'FAIL: colored line missing reset before newline/border'
            stop 1
        end if
    end subroutine check_line_reset

    subroutine test_auto_file_is_plain()
        !! 'auto' saved to a file resolves conservatively to plain: files are
        !! reproducible and never a TTY.
        character(len=*), parameter :: outfile = &
                                       'build/test/output/color_auto_2062.txt'
        type(figure_t) :: fig
        character(len=:), allocatable :: bytes

        call build_line_figure(fig)
        call fig%set_text_color_mode('auto')
        call fig%savefig(outfile)

        bytes = read_file_bytes(outfile)
        if (index(bytes, ESC) > 0) then
            print *, "FAIL: color='auto' file save contains ESC bytes"
            stop 1
        end if
        if (resolve_text_color_mode_file('auto') /= 'never') then
            print *, "FAIL: file resolver did not map auto to never"
            stop 1
        end if
    end subroutine test_auto_file_is_plain

    subroutine test_resolver_env_policy()
        !! Terminal 'auto' resolution honors the environment: CLICOLOR_FORCE
        !! forces color, NO_COLOR overrides it, TERM=dumb stays plain.
        call clear_color_env()

        call set_env('CLICOLOR_FORCE', '1')
        if (resolve_text_color_mode_terminal('auto') == 'never') then
            print *, 'FAIL: CLICOLOR_FORCE=1 did not force color'
            stop 1
        end if

        call set_env('NO_COLOR', '1')
        if (resolve_text_color_mode_terminal('auto') /= 'never') then
            print *, 'FAIL: NO_COLOR did not disable forced auto color'
            stop 1
        end if

        call clear_color_env()
        call set_env('FORCE_COLOR', '1')
        if (resolve_text_color_mode_terminal('auto') == 'never') then
            print *, 'FAIL: FORCE_COLOR=1 did not force color'
            stop 1
        end if

        call clear_color_env()
        call set_env('TERM', 'dumb')
        if (resolve_text_color_mode_terminal('auto') /= 'never') then
            print *, 'FAIL: TERM=dumb did not stay plain'
            stop 1
        end if

        ! Explicit modes are honored regardless of environment.
        if (resolve_text_color_mode_terminal('ansi256') /= 'ansi256') then
            print *, 'FAIL: explicit ansi256 not honored'
            stop 1
        end if

        call clear_color_env()
    end subroutine test_resolver_env_policy

    subroutine clear_color_env()
        call unset_env('NO_COLOR')
        call unset_env('CLICOLOR_FORCE')
        call unset_env('FORCE_COLOR')
        call unset_env('COLORTERM')
        call set_env('TERM', 'xterm-256color')
    end subroutine clear_color_env

    subroutine set_env(name, value)
        character(len=*), intent(in) :: name, value
        integer(c_int) :: rc

        rc = put_env(name//'='//value)
        if (rc /= 0) then
            print *, 'FAIL: could not set env ', name
            stop 1
        end if
    end subroutine set_env

    subroutine unset_env(name)
        !! Remove a variable so env_is_set() reports it absent.
        character(len=*), intent(in) :: name
        integer(c_int) :: rc

        rc = put_env(name//'=')
        rc = put_env(name)
    end subroutine unset_env

    integer function put_env(name_value) result(rc)
        character(len=*), intent(in) :: name_value
        type(env_buffer_t), save :: saved_env(64)
        integer, save :: saved_count = 0
        integer :: slot

        slot = mod(saved_count, size(saved_env)) + 1
        saved_count = saved_count + 1
        if (allocated(saved_env(slot)%value)) deallocate (saved_env(slot)%value)
        saved_env(slot)%value = c_string(name_value)
        rc = c_putenv(saved_env(slot)%value)
    end function put_env

    function c_string(text) result(buffer)
        character(len=*), intent(in) :: text
        character(kind=c_char), allocatable :: buffer(:)
        integer :: i

        allocate (buffer(len_trim(text) + 1))
        do i = 1, len_trim(text)
            buffer(i) = transfer(text(i:i), buffer(i))
        end do
        buffer(size(buffer)) = c_null_char
    end function c_string

    function read_file_bytes(fname) result(bytes)
        character(len=*), intent(in) :: fname
        character(len=:), allocatable :: bytes
        integer :: unit, nbytes, ios

        open (newunit=unit, file=fname, access='stream', form='unformatted', &
              status='old', iostat=ios)
        if (ios /= 0) then
            print *, 'FAIL: cannot open ', fname
            stop 1
        end if
        inquire (unit=unit, size=nbytes)
        if (nbytes < 0) nbytes = 0
        allocate (character(len=nbytes) :: bytes)
        if (nbytes > 0) read (unit) bytes
        close (unit)
    end function read_file_bytes

end program test_text_color

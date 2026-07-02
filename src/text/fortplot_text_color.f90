module fortplot_text_color
    !! ANSI color-mode policy and SGR escape assembly for the text backend
    !! (issue #2062).
    !!
    !! Color modes:
    !!   never     no ANSI escapes
    !!   ansi16    SGR 30-37/90-97 foreground colors
    !!   ansi256   SGR 38;5;<n> foreground colors
    !!   truecolor SGR 38;2;<r>;<g>;<b> foreground colors
    !!   auto      color only for an interactive terminal that appears to
    !!             support it; suppressed by NO_COLOR and TERM=dumb.
    !!
    !! Saved .txt artifacts default to 'never' so bytes stay reproducible. The
    !! 'auto' mode is resolved conservatively at output time: file output never
    !! resolves to color, and terminal output resolves through the environment
    !! and a stdout TTY probe. All escape bytes are built with achar() so this
    !! source stays pure ASCII.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use, intrinsic :: iso_c_binding, only: c_int
    implicit none

    private
    public :: is_valid_text_color_mode, normalize_text_color_mode
    public :: resolve_text_color_mode_file, resolve_text_color_mode_terminal
    public :: sgr_foreground, sgr_reset
    public :: pack_rgb, unpack_rgb
    public :: color_to_ansi16, color_to_ansi256
    public :: COLOR_NONE

    character(len=1), parameter :: ESC = achar(27)
    !! Sentinel stored in the per-cell color buffer for cells with no plot color.
    integer, parameter :: COLOR_NONE = -1

    interface
        function c_isatty(fd) bind(c, name="isatty") result(r)
            import :: c_int
            integer(c_int), value :: fd
            integer(c_int) :: r
        end function c_isatty
    end interface

contains

    logical function is_valid_text_color_mode(mode) result(valid)
        !! True when mode names one of the five canonical color modes.
        character(len=*), intent(in) :: mode

        select case (lower_trim(mode))
        case ('never', 'ansi16', 'ansi256', 'truecolor', 'auto')
            valid = .true.
        case default
            valid = .false.
        end select
    end function is_valid_text_color_mode

    function normalize_text_color_mode(name) result(mode)
        !! Canonicalize a user color-mode selection; unknown names fall back to
        !! 'never' so unexpected input can never leak escapes into output.
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: mode

        mode = lower_trim(name)
        if (.not. is_valid_text_color_mode(mode)) mode = 'never'
    end function normalize_text_color_mode

    function resolve_text_color_mode_file(mode) result(resolved)
        !! Resolve the color mode for file output. Saved files must stay
        !! reproducible, so 'auto' always resolves to 'never'; explicit modes
        !! are honored because the caller asked for them.
        character(len=*), intent(in) :: mode
        character(len=:), allocatable :: resolved

        resolved = normalize_text_color_mode(mode)
        if (resolved == 'auto') resolved = 'never'
    end function resolve_text_color_mode_file

    function resolve_text_color_mode_terminal(mode) result(resolved)
        !! Resolve the color mode for interactive terminal output. Explicit
        !! modes are honored; 'auto' is resolved from the environment and a
        !! stdout TTY probe.
        character(len=*), intent(in) :: mode
        character(len=:), allocatable :: resolved

        resolved = normalize_text_color_mode(mode)
        if (resolved == 'auto') resolved = resolve_auto_from_environment()
    end function resolve_text_color_mode_terminal

    function resolve_auto_from_environment() result(resolved)
        !! Conservative 'auto' resolution following common terminal conventions.
        character(len=:), allocatable :: resolved

        resolved = 'never'

        ! NO_COLOR (any value, including empty) disables color entirely.
        if (env_is_set('NO_COLOR')) return

        ! CLICOLOR_FORCE=1 or FORCE_COLOR (non-zero) force color output.
        if (env_equals('CLICOLOR_FORCE', '1')) then
            resolved = terminal_color_capability()
            return
        end if
        if (env_forces_color('FORCE_COLOR')) then
            resolved = terminal_color_capability()
            return
        end if

        ! A dumb or unset terminal cannot render color.
        if (env_equals('TERM', 'dumb')) return
        if (.not. env_is_set('TERM')) return

        ! Require an interactive stdout.
        if (c_isatty(1_c_int) == 0_c_int) return

        resolved = terminal_color_capability()
    end function resolve_auto_from_environment

    function terminal_color_capability() result(mode)
        !! Best-effort color depth from COLORTERM/TERM. Defaults to ansi16 which
        !! every color terminal supports.
        character(len=:), allocatable :: mode
        character(len=256) :: value
        integer :: length, stat

        mode = 'ansi16'

        call get_environment_variable('COLORTERM', value, length, stat)
        if (stat == 0 .and. length > 0) then
            if (index(lower_trim(value(1:length)), 'truecolor') > 0) mode = 'truecolor'
            if (index(lower_trim(value(1:length)), '24bit') > 0) mode = 'truecolor'
            if (mode == 'truecolor') return
        end if

        call get_environment_variable('TERM', value, length, stat)
        if (stat == 0 .and. length > 0) then
            if (index(value(1:length), '256color') > 0) mode = 'ansi256'
        end if
    end function terminal_color_capability

    function sgr_foreground(mode, r, g, b) result(seq)
        !! SGR foreground escape for the given color mode and RGB (0..1).
        !! Returns an empty string for 'never'/unknown modes.
        character(len=*), intent(in) :: mode
        real(wp), intent(in) :: r, g, b
        character(len=:), allocatable :: seq
        integer :: ir, ig, ib

        select case (normalize_text_color_mode(mode))
        case ('ansi16')
            seq = ESC//'['//itoa(color_to_ansi16(r, g, b))//'m'
        case ('ansi256')
            seq = ESC//'[38;5;'//itoa(color_to_ansi256(r, g, b))//'m'
        case ('truecolor')
            ir = channel_255(r)
            ig = channel_255(g)
            ib = channel_255(b)
            seq = ESC//'[38;2;'//itoa(ir)//';'//itoa(ig)//';'//itoa(ib)//'m'
        case default
            seq = ''
        end select
    end function sgr_foreground

    function sgr_reset() result(seq)
        !! The SGR reset escape emitted after a colored span and before every
        !! newline or frame boundary.
        character(len=:), allocatable :: seq
        seq = ESC//'[0m'
    end function sgr_reset

    pure integer function pack_rgb(r, g, b) result(packed)
        !! Pack an RGB triple (0..1) into a single 24-bit integer for the
        !! per-cell color buffer.
        real(wp), intent(in) :: r, g, b
        packed = channel_255(r)*65536 + channel_255(g)*256 + channel_255(b)
    end function pack_rgb

    pure subroutine unpack_rgb(packed, r, g, b)
        !! Inverse of pack_rgb; returns channels in 0..1.
        integer, intent(in) :: packed
        real(wp), intent(out) :: r, g, b

        r = real(iand(ishft(packed, -16), 255), wp)/255.0_wp
        g = real(iand(ishft(packed, -8), 255), wp)/255.0_wp
        b = real(iand(packed, 255), wp)/255.0_wp
    end subroutine unpack_rgb

    pure integer function color_to_ansi16(r, g, b) result(code)
        !! Map an RGB triple to a standard (30-37) or bright (90-97) SGR
        !! foreground code by nearest 3-bit color plus a luminance-based bright
        !! bit.
        real(wp), intent(in) :: r, g, b
        integer :: bit
        real(wp) :: luminance
        real(wp), parameter :: HALF = 0.5_wp

        bit = 0
        if (r >= HALF) bit = bit + 1
        if (g >= HALF) bit = bit + 2
        if (b >= HALF) bit = bit + 4

        luminance = 0.299_wp*r + 0.587_wp*g + 0.114_wp*b
        if (luminance >= 0.75_wp) then
            code = 90 + bit
        else
            code = 30 + bit
        end if
    end function color_to_ansi16

    pure integer function color_to_ansi256(r, g, b) result(idx)
        !! Map an RGB triple to a 6x6x6 color-cube index (16..231).
        real(wp), intent(in) :: r, g, b
        integer :: ri, gi, bi

        ri = cube_axis(r)
        gi = cube_axis(g)
        bi = cube_axis(b)
        idx = 16 + 36*ri + 6*gi + bi
    end function color_to_ansi256

    pure integer function cube_axis(v) result(i)
        !! Quantize a 0..1 channel to a 0..5 color-cube axis.
        real(wp), intent(in) :: v
        i = nint(max(0.0_wp, min(1.0_wp, v))*5.0_wp)
    end function cube_axis

    pure integer function channel_255(v) result(i)
        !! Clamp and scale a 0..1 channel to a 0..255 byte.
        real(wp), intent(in) :: v
        i = nint(max(0.0_wp, min(1.0_wp, v))*255.0_wp)
    end function channel_255

    function itoa(n) result(str)
        !! Minimal non-negative integer to decimal string.
        integer, intent(in) :: n
        character(len=:), allocatable :: str
        character(len=16) :: buf

        write (buf, '(I0)') n
        str = trim(buf)
    end function itoa

    function lower_trim(s) result(out)
        !! Trim and lowercase an ASCII string.
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: out
        integer :: i, c

        out = trim(adjustl(s))
        do i = 1, len(out)
            c = iachar(out(i:i))
            if (c >= iachar('A') .and. c <= iachar('Z')) out(i:i) = achar(c + 32)
        end do
    end function lower_trim

    logical function env_is_set(name) result(is_set)
        !! True when an environment variable is present (any value, including
        !! empty).
        character(len=*), intent(in) :: name
        character(len=1) :: value
        integer :: length, stat

        call get_environment_variable(name, value, length, stat)
        ! stat==0 present and fit, stat==-1 present but truncated. Both mean set.
        is_set = stat == 0 .or. stat == -1
    end function env_is_set

    logical function env_equals(name, expected) result(matches)
        !! True when an environment variable is set and equals expected
        !! (case-insensitive, trimmed).
        character(len=*), intent(in) :: name, expected
        character(len=256) :: value
        integer :: length, stat

        matches = .false.
        call get_environment_variable(name, value, length, stat)
        if (stat /= 0) return
        if (length <= 0) return
        matches = lower_trim(value(1:length)) == lower_trim(expected)
    end function env_equals

    logical function env_forces_color(name) result(forces)
        !! True when a FORCE_COLOR-style variable is set to anything other than
        !! '0'. An empty value counts as forcing per the common convention.
        character(len=*), intent(in) :: name
        character(len=256) :: value
        integer :: length, stat

        forces = .false.
        call get_environment_variable(name, value, length, stat)
        if (stat /= 0 .and. stat /= -1) return
        if (length <= 0) then
            forces = .true.
            return
        end if
        forces = lower_trim(value(1:length)) /= '0'
    end function env_forces_color

end module fortplot_text_color

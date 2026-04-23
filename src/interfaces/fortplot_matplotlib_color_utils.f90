module fortplot_matplotlib_color_utils
    !! Shared helpers for resolving matplotlib-style color kwargs.
    !!
    !! The matplotlib facade accepts color arguments that may be either a
    !! character string (named color, hex, single letter) or a real RGB triple.
    !! This module centralises the resolution and validation logic so each
    !! wrapper keeps a single, declarative call site.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_colors, only: parse_color
    use fortplot_logging, only: log_warning

    implicit none
    private

    public :: resolve_color_string_or_rgb
    public :: resolve_sequence_to_rgb

contains

    subroutine resolve_color_string_or_rgb(color_str, color_rgb, context, &
                                           rgb_out, has_color)
        !! Merge string/RGB color inputs into a single RGB triple.
        !!
        !! Precedence:
        !! - RGB triple wins if supplied (explicit numeric value from the user).
        !! - Otherwise the string is parsed via fortplot_colors::parse_color.
        !! - If parsing fails, emit a single warning tagged with `context` and
        !!   report has_color = .false. so the caller can fall back.
        character(len=*), intent(in), optional :: color_str
        real(wp), intent(in), optional :: color_rgb(3)
        character(len=*), intent(in) :: context
        real(wp), intent(out) :: rgb_out(3)
        logical, intent(out) :: has_color

        logical :: ok

        rgb_out = [0.0_wp, 0.0_wp, 0.0_wp]
        has_color = .false.

        if (present(color_rgb)) then
            rgb_out = color_rgb
            has_color = .true.
            return
        end if

        if (present(color_str)) then
            if (len_trim(color_str) == 0) return
            call parse_color(color_str, rgb_out, ok)
            if (ok) then
                has_color = .true.
            else
                call log_warning(trim(context) // &
                    ": unrecognised color '" // trim(color_str) // "'")
            end if
        end if
    end subroutine resolve_color_string_or_rgb

    subroutine resolve_sequence_to_rgb(color_strings, rgb_array, context, &
                                       success)
        !! Parse an array of color strings into a 3xN RGB matrix.
        !!
        !! Returns success = .false. without allocating the output when any
        !! entry fails to parse; callers should fall back to defaults in that
        !! case rather than rendering with partially-resolved colors.
        character(len=*), intent(in) :: color_strings(:)
        real(wp), allocatable, intent(out) :: rgb_array(:, :)
        character(len=*), intent(in) :: context
        logical, intent(out) :: success

        integer :: i, n
        real(wp) :: rgb(3)
        logical :: ok

        success = .true.
        n = size(color_strings)
        if (n == 0) then
            success = .false.
            return
        end if

        allocate (rgb_array(3, n))
        do i = 1, n
            call parse_color(color_strings(i), rgb, ok)
            if (.not. ok) then
                call log_warning(trim(context) // &
                    ": unrecognised color '" // trim(color_strings(i)) // "'")
                success = .false.
                deallocate (rgb_array)
                return
            end if
            rgb_array(:, i) = rgb
        end do
    end subroutine resolve_sequence_to_rgb

end module fortplot_matplotlib_color_utils

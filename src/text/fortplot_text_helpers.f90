module fortplot_text_helpers
    !! Small helpers for preparing text for backends
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_text_layout, only: has_mathtext
    implicit none

    private
    public :: prepare_mathtext_if_needed

contains

    pure subroutine prepare_mathtext_if_needed(input_text, output_text, out_len)
        !! If text contains ^ or _ but no $...$, wrap with $ delimiters
        !! so downstream mathtext-aware renderers parse superscripts/subscripts.
        character(len=*), intent(in) :: input_text
        character(len=*), intent(out) :: output_text
        integer, intent(out) :: out_len
        character(len=:), allocatable :: trimmed
        integer :: n

        trimmed = trim(input_text)
        n = len_trim(trimmed)

        if (n <= 0) then
            output_text = ''
            out_len = 0
            return
        end if

        if (has_mathtext(trimmed) .or. index(trimmed, '$') > 0) then
            ! Already mathtext-delimited
            output_text = trimmed
            out_len = n
        else if (index(trimmed, '^') > 0 .or. index(trimmed, '_') > 0) then
            ! Add $ delimiters to engage mathtext pipeline in raster layout/renderers
            if (len(output_text) >= n + 2) then
                output_text = '$' // trimmed(1:n) // '$'
                out_len = n + 2
            else
                ! Fallback: truncate safely if caller provided too-small buffer
                output_text = '$' // trimmed(1:min(n, max(0, len(output_text)-2))) // '$'
                out_len = min(n, max(0, len(output_text)-2)) + 2
            end if
        else
            output_text = trimmed
            out_len = n
        end if
    end subroutine prepare_mathtext_if_needed

end module fortplot_text_helpers

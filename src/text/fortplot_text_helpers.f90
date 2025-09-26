module fortplot_text_helpers
    !! Small helpers for preparing text for backends
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: prepare_mathtext_if_needed

contains

    pure subroutine prepare_mathtext_if_needed(input_text, output_text, out_len)
        !! Return trimmed text and leave math parsing decisions to explicit $...$ delimiters.
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

        output_text = ''

        if (len(output_text) >= n) then
            output_text = trimmed
            out_len = n
        else
            out_len = len(output_text)
            if (out_len > 0) then
                output_text(1:out_len) = trimmed(1:out_len)
            end if
        end if
    end subroutine prepare_mathtext_if_needed

end module fortplot_text_helpers

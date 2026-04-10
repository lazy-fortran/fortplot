module fortplot_text_helpers
    !! Small helpers for preparing text for backends
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_latex_parser, only: process_latex_in_text
    use fortplot_unicode, only: escape_unicode_for_raster
    implicit none

    private
    public :: prepare_mathtext_if_needed, prepare_text_for_raster

contains

    pure subroutine prepare_mathtext_if_needed(input_text, output_text, out_len)
        !! Return trimmed text and leave math parsing decisions to explicit $...$ delimiters.
        character(len=*), intent(in) :: input_text
        character(len=*), intent(out) :: output_text
        integer, intent(out) :: out_len
        character(len=:), allocatable :: trimmed
        integer :: n
        integer :: capacity
        integer :: copy_len

        trimmed = trim(input_text)
        n = len_trim(trimmed)

        if (n <= 0) then
            output_text = ''
            out_len = 0
            return
        end if

        capacity = len(output_text)
        output_text = ''

        if (capacity <= 0) then
            out_len = 0
            return
        end if

        copy_len = min(n, capacity)
        output_text(1:copy_len) = trimmed(1:copy_len)
        out_len = copy_len
    end subroutine prepare_mathtext_if_needed

    subroutine prepare_text_for_raster(input_text, escaped_text)
        !! Run the full latex -> mathtext -> unicode pipeline for raster output.
        character(len=*), intent(in) :: input_text
        character(len=*), intent(out) :: escaped_text
        character(len=500) :: processed_text
        character(len=600) :: math_ready
        integer :: processed_len, math_len

        call process_latex_in_text(trim(input_text), processed_text, &
                                   processed_len)
        call prepare_mathtext_if_needed(processed_text(1:processed_len), &
                                        math_ready, math_len)
        call escape_unicode_for_raster(math_ready(1:math_len), escaped_text)
    end subroutine prepare_text_for_raster

end module fortplot_text_helpers

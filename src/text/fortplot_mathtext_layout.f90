module fortplot_mathtext_layout
    !! Font-independent vertical boxes, measured in units of the base font size.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_mathtext, only: mathtext_element_t, parse_mathtext, &
        ELEMENT_NORMAL, ELEMENT_SQRT, ELEMENT_FRACTION
    implicit none
    private
    public :: mathtext_vertical_bounds, fraction_vertical_offsets

contains

    recursive subroutine mathtext_vertical_bounds(elements, above, below)
        type(mathtext_element_t), intent(in) :: elements(:)
        real(wp), intent(out) :: above, below
        real(wp) :: child_above, child_below, scale
        integer :: i

        above = 0.0_wp
        below = 0.0_wp
        do i = 1, size(elements)
            call element_vertical_bounds(elements(i), child_above, child_below)
            scale = elements(i)%font_size_ratio
            above = max(above, elements(i)%vertical_offset + scale*child_above)
            below = max(below, -elements(i)%vertical_offset + scale*child_below)
        end do
    end subroutine mathtext_vertical_bounds

    recursive subroutine element_vertical_bounds(element, above, below)
        type(mathtext_element_t), intent(in) :: element
        real(wp), intent(out) :: above, below
        type(mathtext_element_t), allocatable :: children(:)
        real(wp) :: other_above, other_below, numerator_y, denominator_y

        if (element%element_type == ELEMENT_NORMAL) then
            above = 0.72_wp
            below = 0.0_wp
            if (scan(element%text, 'gjpqy') > 0) below = 0.22_wp
            return
        end if
        children = parse_mathtext(element%text, element%italic)
        call mathtext_vertical_bounds(children, above, below)
        if (element%element_type == ELEMENT_SQRT) above = above + 0.08_wp
        if (element%element_type == ELEMENT_FRACTION) then
            call fraction_vertical_offsets(element, numerator_y, denominator_y)
            children = parse_mathtext(element%denominator, element%italic)
            call mathtext_vertical_bounds(children, other_above, other_below)
            above = numerator_y + 0.7_wp*above
            below = -denominator_y + 0.7_wp*other_below
        end if
    end subroutine element_vertical_bounds

    recursive subroutine fraction_vertical_offsets(element, numerator_y, denominator_y)
        type(mathtext_element_t), intent(in) :: element
        real(wp), intent(out) :: numerator_y, denominator_y
        type(mathtext_element_t), allocatable :: children(:)
        real(wp) :: above, below

        children = parse_mathtext(element%text, element%italic)
        call mathtext_vertical_bounds(children, above, below)
        numerator_y = 0.375_wp + 0.7_wp*below
        children = parse_mathtext(element%denominator, element%italic)
        call mathtext_vertical_bounds(children, above, below)
        denominator_y = 0.125_wp - 0.7_wp*above
    end subroutine fraction_vertical_offsets

end module fortplot_mathtext_layout

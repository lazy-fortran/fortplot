module fortplot_spec_rendering_utils
    !! Shared utilities for spec rendering modules.
    !!
    !! Contains functions used across spec_rendering, mark_handlers,
    !! and field_rendering without pulling in heavy dependencies.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_constants, only: APPROX_EQUAL_TOLERANCE
    use fortplot_colors, only: parse_color
    use fortplot_spec_types, only: encoding_t, data_t

    implicit none
    private

    public :: get_label_from_encoding
    public :: approx_equal
    public :: ends_with

contains

    function get_label_from_encoding(enc, data) result(label)
        type(encoding_t), intent(in) :: enc
        type(data_t), intent(in), optional :: data
        character(len=:), allocatable :: label
        integer :: value_length, i, j
        character(len=:), allocatable :: field_name

        if (.not. enc%color%defined) return

        ! A per-layer series label is carried on the colour channel as either a
        ! constant `datum` (current builder) or a legacy `value`; both hold the
        ! quoted label text.
        if (allocated(enc%color%datum)) then
            value_length = len(enc%color%datum)
            if (value_length >= 2 .and. enc%color%datum(1:1) == '"' .and. &
                enc%color%datum(value_length:value_length) == '"') then
                label = enc%color%datum(2:value_length - 1)
            else
                label = enc%color%datum
            end if
            return
        end if

        if (allocated(enc%color%value)) then
            value_length = len(enc%color%value)
            if (value_length >= 2 .and. enc%color%value(1:1) == '"' .and. &
                enc%color%value(value_length:value_length) == '"') then
                label = enc%color%value(2:value_length - 1)
            else
                label = enc%color%value
            end if
            return
        end if

        if (.not. present(data)) return
        if (.not. allocated(enc%color%field)) return
        if (.not. allocated(data%columns)) return

        field_name = enc%color%field
        do i = 1, size(data%columns)
            if (data%columns(i)%field /= field_name) cycle
            if (.not. data%columns(i)%is_string) return
            if (.not. allocated(data%columns(i)%string_values)) return
            if (size(data%columns(i)%string_values) == 0) return

            label = trim(data%columns(i)%string_values(1))
            if (len_trim(label) == 0) then
                return
            end if
            if (size(data%columns(i)%string_values) > 1) then
                do j = 2, size(data%columns(i)%string_values)
                    if (trim(data%columns(i)%string_values(j)) /= label) then
                        exit
                    end if
                end do
            end if
            return
        end do
    end function get_label_from_encoding

    logical function approx_equal(a, b) result(equal)
        real(wp), intent(in) :: a, b

        equal = abs(a - b) < APPROX_EQUAL_TOLERANCE
    end function approx_equal

    logical function ends_with(text, suffix) result(matches)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: suffix

        integer :: text_length
        integer :: suffix_length
        text_length = len_trim(text)
        suffix_length = len_trim(suffix)
        if (suffix_length == 0 .or. suffix_length > text_length) then
            matches = .false.
            return
        end if

        matches = text(text_length - suffix_length + 1:text_length) == &
                  suffix(1:suffix_length)
    end function ends_with

end module fortplot_spec_rendering_utils

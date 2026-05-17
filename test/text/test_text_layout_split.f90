program test_text_layout_split
    use fortplot_text_layout, only: calculate_text_width, calculate_text_height, &
        calculate_text_width_with_size, calculate_text_descent, has_mathtext, &
        preprocess_math_text, DEFAULT_FONT_SIZE
    use fortplot_mathtext, only: parse_mathtext, mathtext_element_t
    use fortplot_text_fonts, only: init_text_system
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    integer :: width_default, width_scaled
    integer :: height_default
    integer :: descent
    logical :: init_ok
    character(len=128) :: mixed_label
    character(len=256) :: processed_label
    integer :: processed_len
    type(mathtext_element_t), allocatable :: elements(:)
    logical :: found_literal
    integer :: i, subscripts

    init_ok = init_text_system()
    if (.not. init_ok) then
        error stop "text system init failed"
    end if

    width_default = calculate_text_width('Split Layout Test')
    if (width_default <= 0) then
        error stop "default width should be positive"
    end if

    width_scaled = calculate_text_width_with_size('Split Layout Test', &
        real(DEFAULT_FONT_SIZE, wp))
    if (abs(width_scaled - width_default) > 5) then
        error stop "scaled width should match default baseline"
    end if

    height_default = calculate_text_height('Split Layout Test')
    if (height_default <= 0) then
        error stop "height should be positive"
    end if

    descent = calculate_text_descent('Split Layout Test')
    if (descent <= 0) then
        error stop "descent should be positive"
    end if

    if (.not. has_mathtext('$x^2 + y_1$')) then
        error stop "mathtext markers should be detected"
    end if

    if (has_mathtext('plain text')) then
        error stop "non mathtext should not be marked"
    end if

    mixed_label = 'fill_between data $x_1$'
    call preprocess_math_text(mixed_label, processed_label, processed_len)
    elements = parse_mathtext(processed_label(1:processed_len))

    if (.not. allocated(elements)) then
        error stop "elements should be allocated"
    end if

    found_literal = .false.
    subscripts = 0
    do i = 1, size(elements)
        if (trim(elements(i)%text) == 'fill_between data') then
            if (abs(elements(i)%font_size_ratio - 1.0_wp) > 1.0e-6_wp) then
                error stop "literal text font ratio should remain base"
            end if
            if (abs(elements(i)%vertical_offset) > 1.0e-6_wp) then
                error stop "literal text vertical offset should stay zero"
            end if
            found_literal = .true.
        end if
        if (elements(i)%vertical_offset < 0.0_wp) then
            subscripts = subscripts + 1
        end if
    end do

    if (.not. found_literal) then
        error stop "literal underscore segment was misparsed"
    end if

    if (subscripts /= 1) then
        error stop "math segment subscript should remain intact"
    end if

end program test_text_layout_split

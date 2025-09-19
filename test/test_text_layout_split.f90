program test_text_layout_split
    use fortplot_text_layout, only: calculate_text_width, calculate_text_height, &
        calculate_text_width_with_size, calculate_text_descent, has_mathtext, &
        DEFAULT_FONT_SIZE
    use fortplot_text_fonts, only: init_text_system
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    integer :: width_default, width_scaled
    integer :: height_default
    integer :: descent
    logical :: init_ok

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

end program test_text_layout_split

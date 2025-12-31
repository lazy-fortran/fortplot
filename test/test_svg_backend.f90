program test_svg_backend
    !! Test SVG backend basic functionality
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_svg, only: svg_context, create_svg_canvas
    implicit none

    logical :: all_passed
    type(svg_context) :: ctx
    character(len=512) :: test_file
    logical :: file_exists
    integer :: unit, ios
    character(len=1024) :: line
    logical :: has_svg_header, has_svg_close, has_rect

    all_passed = .true.
    test_file = '/tmp/test_svg_backend_output.svg'

    print *, 'Testing SVG backend creation...'
    ctx = create_svg_canvas(800, 600)
    if (ctx%width /= 800 .or. ctx%height /= 600) then
        print *, 'FAIL: Canvas dimensions incorrect'
        all_passed = .false.
    else
        print *, 'PASS: Canvas dimensions correct'
    end if

    print *, 'Testing SVG coordinate setting...'
    call ctx%set_coordinates(0.0_wp, 10.0_wp, 0.0_wp, 5.0_wp)
    if (abs(ctx%x_min - 0.0_wp) > 1e-10_wp .or. &
        abs(ctx%x_max - 10.0_wp) > 1e-10_wp .or. &
        abs(ctx%y_min - 0.0_wp) > 1e-10_wp .or. &
        abs(ctx%y_max - 5.0_wp) > 1e-10_wp) then
        print *, 'FAIL: Coordinates not set correctly'
        all_passed = .false.
    else
        print *, 'PASS: Coordinates set correctly'
    end if

    print *, 'Testing SVG color setting...'
    call ctx%color(0.5_wp, 0.25_wp, 0.75_wp)
    if (abs(ctx%current_r - 0.5_wp) > 1e-10_wp .or. &
        abs(ctx%current_g - 0.25_wp) > 1e-10_wp .or. &
        abs(ctx%current_b - 0.75_wp) > 1e-10_wp) then
        print *, 'FAIL: Color not set correctly'
        all_passed = .false.
    else
        print *, 'PASS: Color set correctly'
    end if

    print *, 'Testing SVG line drawing...'
    call ctx%line(0.0_wp, 0.0_wp, 10.0_wp, 5.0_wp)
    if (.not. allocated(ctx%content_stream)) then
        print *, 'FAIL: Content stream not created'
        all_passed = .false.
    else if (index(ctx%content_stream, '<line') == 0) then
        print *, 'FAIL: Line element not found in stream'
        all_passed = .false.
    else
        print *, 'PASS: Line element created'
    end if

    print *, 'Testing SVG line width setting...'
    call ctx%set_line_width(2.5_wp)
    if (abs(ctx%current_line_width - 2.5_wp) > 1e-10_wp) then
        print *, 'FAIL: Line width not set correctly'
        all_passed = .false.
    else
        print *, 'PASS: Line width set correctly'
    end if

    print *, 'Testing SVG line style (dashed)...'
    call ctx%set_line_style('--')
    if (trim(ctx%current_dash_pattern) /= '6,3') then
        print *, 'FAIL: Dashed pattern not correct, got: ', &
            trim(ctx%current_dash_pattern)
        all_passed = .false.
    else
        print *, 'PASS: Dashed pattern correct'
    end if

    print *, 'Testing SVG line style (dotted)...'
    call ctx%set_line_style(':')
    if (trim(ctx%current_dash_pattern) /= '2,3') then
        print *, 'FAIL: Dotted pattern not correct'
        all_passed = .false.
    else
        print *, 'PASS: Dotted pattern correct'
    end if

    print *, 'Testing SVG marker drawing...'
    call ctx%draw_marker(5.0_wp, 2.5_wp, 'o')
    if (index(ctx%content_stream, '<circle') == 0) then
        print *, 'FAIL: Circle marker not found'
        all_passed = .false.
    else
        print *, 'PASS: Circle marker created'
    end if

    print *, 'Testing SVG file save...'
    call ctx%save(test_file)
    inquire (file=trim(test_file), exist=file_exists)
    if (.not. file_exists) then
        print *, 'FAIL: SVG file not created'
        all_passed = .false.
    else
        print *, 'PASS: SVG file created'

        has_svg_header = .false.
        has_svg_close = .false.
        has_rect = .false.
        open (newunit=unit, file=trim(test_file), status='old', action='read', &
              iostat=ios)
        if (ios == 0) then
            do while (ios == 0)
                read (unit, '(A)', iostat=ios) line
                if (ios /= 0) exit
                if (index(line, '<svg') > 0) has_svg_header = .true.
                if (index(line, '</svg>') > 0) has_svg_close = .true.
                if (index(line, '<rect') > 0) has_rect = .true.
            end do
            close (unit)
        end if

        if (.not. has_svg_header) then
            print *, 'FAIL: SVG header not found'
            all_passed = .false.
        else
            print *, 'PASS: SVG header present'
        end if

        if (.not. has_svg_close) then
            print *, 'FAIL: SVG closing tag not found'
            all_passed = .false.
        else
            print *, 'PASS: SVG closing tag present'
        end if

        if (.not. has_rect) then
            print *, 'FAIL: Background rect not found'
            all_passed = .false.
        else
            print *, 'PASS: Background rect present'
        end if
    end if

    print *, 'Testing get_ascii_output...'
    if (index(ctx%get_ascii_output(), 'SVG') == 0) then
        print *, 'FAIL: ASCII output does not mention SVG'
        all_passed = .false.
    else
        print *, 'PASS: ASCII output mentions SVG'
    end if

    if (all_passed) then
        print *, 'All SVG backend tests passed'
    else
        error stop 'SVG backend tests failed'
    end if

end program test_svg_backend

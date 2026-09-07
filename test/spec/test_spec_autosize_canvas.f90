program test_spec_autosize_canvas
    !! Independent Vega-Lite sizing contract:
    !! https://vega.github.io/vega-lite/docs/size.html#autosize
    !! With contains="padding", width/height include the explicit padding;
    !! with contains="content" (the default), padding adds to the canvas.
    use fortplot, only: spec_t, json_to_spec, spec_to_json
    use fortplot_spec_rendering, only: render_spec_to_file
    use fortplot_test_output_helpers, only: ensure_test_output_dir
    implicit none

    character(len=:), allocatable :: output_dir

    call ensure_test_output_dir('spec_autosize_canvas', output_dir)
    call check_canvas('padding', 640, 480)
    call check_canvas('content', 784, 591)
    print *, 'PASS: Vega-Lite autosize contains controls actual PNG canvas dimensions'

contains

    subroutine check_canvas(contains_value, expected_width, expected_height)
        character(len=*), intent(in) :: contains_value
        integer, intent(in) :: expected_width, expected_height
        type(spec_t) :: spec, roundtrip
        character(len=:), allocatable :: json, path
        character(len=24) :: header
        integer :: status, unit, width, height, i

        json = '{"width":640,"height":480,'// &
            '"autosize":{"type":"none","contains":"'// &
            contains_value//'"},'// &
            '"padding":{"left":80,"right":64,"top":58,"bottom":53},'// &
            '"mark":"line","data":{"values":[{"x":0,"y":0},'// &
            '{"x":1,"y":1}]},"encoding":{'// &
            '"x":{"field":"x","type":"quantitative"},'// &
            '"y":{"field":"y","type":"quantitative"}}}'
        call json_to_spec(json, spec, status)
        if (status /= 0) stop 1
        json = spec_to_json(spec)
        call json_to_spec(json, roundtrip, status)
        if (status /= 0) stop 1

        path = output_dir//contains_value//'.png'
        call render_spec_to_file(roundtrip, path, status)
        if (status /= 0) stop 2
        open (newunit=unit, file=path, access='stream', form='unformatted', &
            status='old', action='read', iostat=status)
        if (status /= 0) stop 3
        read (unit, iostat=status) header
        close (unit)
        if (status /= 0) stop 3
        width = 0
        height = 0
        do i = 17, 20
            width = 256*width + iachar(header(i:i))
            height = 256*height + iachar(header(i + 4:i + 4))
        end do
        if (width /= expected_width .or. height /= expected_height) then
            print *, 'FAIL: contains=', contains_value, ' canvas=', width, height, &
                ' expected=', expected_width, expected_height
            stop 4
        end if
    end subroutine check_canvas

end program test_spec_autosize_canvas

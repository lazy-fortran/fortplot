program test_spec_line_style
    !! CSS pixel stroke widths and opacity must survive the native JSON adapter.
    use fortplot, only: wp, spec_t, json_to_spec
    use fortplot_spec_rendering, only: render_spec_to_file
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_test_output_helpers, only: ensure_test_output_dir
    implicit none

    real(wp), parameter :: widths(2) = [1.0_wp, 2.083333333_wp]
    real(wp), parameter :: alphas(3) = [0.0_wp, 0.25_wp, 1.0_wp]
    character(len=32) :: name
    character(len=:), allocatable :: output_dir
    integer :: source, iw, ia

    call ensure_test_output_dir('spec_line_style', output_dir)
    do source = 1, 2
        do iw = 1, 2
            do ia = 1, 3
                write (name, '("source",i1,"_w",i1,"_a",i1)') source, iw, ia
                call check_line(trim(name), widths(iw), alphas(ia), source)
            end do
        end do
    end do
    call check_line('mark_overrides_config', 1.0_wp, 1.0_wp, 3)
    print *, 'PASS: native JSON line widths and opacity preserve CSS pixel coverage'

contains

    subroutine check_line(case_name, width, alpha, source)
        character(len=*), intent(in) :: case_name
        real(wp), intent(in) :: width, alpha
        integer, intent(in) :: source
        type(spec_t) :: spec
        type(figure_state_t) :: state
        real(wp), allocatable :: rgb(:, :, :)
        real(wp) :: coverage, expected
        character(len=:), allocatable :: json, path
        integer :: status, unit

        json = line_spec(width, alpha, source)
        open (newunit=unit, file=output_dir//case_name//'.json', status='replace')
        write (unit, '(a)') json
        close (unit)
        call json_to_spec(json, spec, status)
        if (status /= 0) error stop 'Cannot parse line-style fixture'
        path = output_dir//case_name//'.png'
        call render_spec_to_file(spec, path, status, rendered_state=state)
        if (status /= 0) error stop 'Cannot render line-style PNG'
        allocate (rgb(800, 600, 3))
        call state%backend%extract_rgb_data(800, 600, rgb)
        coverage = sum(max(rgb(350:450, 250:350, 1) - &
            rgb(350:450, 250:350, 2), 0.0_wp))/101
        expected = width*alpha
        if (abs(coverage - expected) > max(0.15_wp, 0.08_wp*expected)) then
            print *, 'FAIL: ', case_name, ' stroke coverage=', coverage, &
                ' expected CSS pixels times opacity=', expected
            error stop 'Line width or opacity was not preserved'
        end if
        if (alpha == 0.0_wp) then
            if (coverage > 1.0e-6_wp) error stop 'Transparent line is still visible'
        end if
        call render_spec_to_file(spec, output_dir//case_name//'.pdf', status)
        if (status /= 0) error stop 'Cannot render line-style PDF'
    end subroutine check_line

    function line_spec(width, alpha, source) result(json)
        real(wp), intent(in) :: width, alpha
        integer, intent(in) :: source
        character(len=:), allocatable :: json, mark_width, config

        mark_width = ',"strokeWidth":'//number(width)
        config = '"config":{"axis":{"grid":false}}'
        if (source == 2) then
            mark_width = ''
            config = '"config":{"axis":{"grid":false},"line":{"strokeWidth":'// &
                number(width)//'}}'
        else if (source == 3) then
            config = '"config":{"axis":{"grid":false},"line":{"strokeWidth":5}}'
        end if
        json = '{"width":800,"height":600,"autosize":{'// &
            '"type":"none","contains":"padding"},"padding":{'// &
            '"left":100,"right":80,"top":72,"bottom":66},'//config//','// &
            '"mark":{"type":"line","stroke":"red","opacity":'// &
            number(alpha)//mark_width//'},"data":{"values":['// &
            '{"x":-0.8,"y":0},{"x":0.8,"y":0}]},"encoding":{'// &
            '"x":{"field":"x","type":"quantitative",'// &
            '"scale":{"domain":[-1,1]}},"y":{"field":"y",'// &
            '"type":"quantitative","scale":{"domain":[-1,1]}}}}'
    end function line_spec

    function number(value) result(text)
        real(wp), intent(in) :: value
        character(len=:), allocatable :: text
        character(len=32) :: buffer
        write (buffer, '(f16.9)') value
        text = trim(adjustl(buffer))
    end function number

end program test_spec_line_style

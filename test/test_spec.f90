program test_spec
    !! Test suite for Vega-Lite spec_t types, JSON serialization,
    !! builder API, and rendering via figure_t bridge.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: spec_t, mark_t, encoding_t, channel_t, &
                        data_t, data_column_t, scale_t, axis_t, layer_t, &
                        vl_line, vl_point, vl_bar, vl_area, &
                        vl_layer_add, vl_channel, &
                        spec_savefig, spec_to_figure, &
                        spec_to_json, spec_to_json_file, &
                        json_to_spec, figure_t
    use fortplot_validation, only: validate_file_exists, &
                                   validate_png_format, validate_pdf_format, &
                                   validation_result_t
    use test_output_helpers, only: ensure_test_output_dir
    implicit none

    integer :: total_tests, passed_tests
    character(len=:), allocatable :: out_dir

    total_tests = 0
    passed_tests = 0

    call ensure_test_output_dir('spec', out_dir)

    call test_types_init()
    call test_channel_builder()
    call test_vl_line_builder()
    call test_vl_point_builder()
    call test_vl_bar_builder()
    call test_vl_area_builder()
    call test_json_single_view()
    call test_json_string_data()
    call test_json_mark_properties()
    call test_json_scale_axis()
    call test_json_file_output()
    call test_layered_spec()
    call test_render_line_png()
    call test_render_point_png()
    call test_render_bar_png()
    call test_savefig_vl_json()
    call test_savefig_png()
    call test_json_roundtrip_line()
    call test_json_roundtrip_mark_props()
    call test_json_roundtrip_scale_axis()
    call test_json_roundtrip_layered()
    call test_json_roundtrip_string_data()
    call test_json_roundtrip_render()
    call test_json_roundtrip_label_angle()
    call test_json_roundtrip_exponent()
    call test_json_roundtrip_filled()

    print *, ''
    print *, '=== Spec Test Summary ==='
    print *, 'Tests passed:', passed_tests, '/', total_tests

    if (passed_tests == total_tests) then
        print *, 'All spec tests PASSED!'
        stop 0
    else
        print *, 'FAIL: Some spec tests failed'
        stop 1
    end if

contains

    subroutine assert(condition, test_name)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: test_name

        total_tests = total_tests + 1
        if (condition) then
            passed_tests = passed_tests + 1
            print *, '  PASS: ', test_name
        else
            print *, '  FAIL: ', test_name
        end if
    end subroutine assert

    subroutine test_types_init()
        !! Verify default initialization of spec types
        type(spec_t) :: spec
        type(mark_t) :: m
        type(channel_t) :: ch
        type(scale_t) :: sc
        type(axis_t) :: ax

        print *, 'Test: types initialization'

        call assert(spec%width == 400, 'default width 400')
        call assert(spec%height == 300, 'default height 300')
        call assert(.not. spec%is_layered, 'not layered')
        call assert(spec%layer_count == 0, 'zero layers')

        call assert(m%opacity == 1.0_wp, 'mark opacity 1.0')
        call assert(m%size < 0.0_wp, 'mark size unset')
        call assert(m%filled, 'mark filled by default')

        call assert(.not. ch%defined, 'channel not defined')
        call assert(.not. sc%domain_set, 'scale domain unset')
        call assert(.not. ax%title_set, 'axis title unset')
        call assert(.not. ax%grid, 'axis grid off')
    end subroutine test_types_init

    subroutine test_channel_builder()
        !! Test vl_channel helper
        type(channel_t) :: ch

        print *, 'Test: channel builder'

        ch = vl_channel('temperature', 'quantitative')
        call assert(ch%defined, 'channel defined')
        call assert(ch%field == 'temperature', 'field name')
        call assert(ch%type == 'quantitative', 'field type')
    end subroutine test_channel_builder

    subroutine test_vl_line_builder()
        !! Test vl_line builder
        type(spec_t) :: spec
        real(wp) :: x(5), y(5)
        integer :: i

        print *, 'Test: vl_line builder'

        x = [(real(i, wp), i=1, 5)]
        y = [(real(i, wp)**2, i=1, 5)]

        spec = vl_line(x, y, title='Quadratic', &
                       xlabel='X', ylabel='Y^2', &
                       width=600, height=400)

        call assert(spec%mark%type == 'line', 'mark type line')
        call assert(spec%title == 'Quadratic', 'title set')
        call assert(spec%width == 600, 'width 600')
        call assert(spec%height == 400, 'height 400')
        call assert(spec%data%nrows == 5, 'data has 5 rows')
        call assert(size(spec%data%columns) == 2, '2 columns')
        call assert(spec%encoding%x%defined, 'x channel defined')
        call assert(spec%encoding%y%defined, 'y channel defined')
        call assert(spec%encoding%x%axis%title == 'X', &
                    'x axis title')
        call assert(spec%encoding%y%axis%title == 'Y^2', &
                    'y axis title')
        call assert(abs(spec%data%columns(1)%values(3) - 3.0_wp) &
                    < 1.0d-10, 'x data correct')
        call assert(abs(spec%data%columns(2)%values(3) - 9.0_wp) &
                    < 1.0d-10, 'y data correct')
    end subroutine test_vl_line_builder

    subroutine test_vl_point_builder()
        !! Test vl_point builder
        type(spec_t) :: spec
        real(wp) :: x(3), y(3)

        print *, 'Test: vl_point builder'

        x = [1.0_wp, 2.0_wp, 3.0_wp]
        y = [4.0_wp, 5.0_wp, 6.0_wp]
        spec = vl_point(x, y)

        call assert(spec%mark%type == 'point', 'mark type point')
        call assert(spec%data%nrows == 3, '3 rows')
        call assert(spec%width == 400, 'default width')
    end subroutine test_vl_point_builder

    subroutine test_vl_bar_builder()
        !! Test vl_bar builder
        type(spec_t) :: spec
        real(wp) :: x(4), y(4)

        print *, 'Test: vl_bar builder'

        x = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]
        y = [10.0_wp, 20.0_wp, 15.0_wp, 25.0_wp]
        spec = vl_bar(x, y, title='Sales')

        call assert(spec%mark%type == 'bar', 'mark type bar')
        call assert(spec%encoding%x%type == 'ordinal', &
                    'x type ordinal for bar')
        call assert(spec%title == 'Sales', 'title set')
    end subroutine test_vl_bar_builder

    subroutine test_vl_area_builder()
        !! Test vl_area builder
        type(spec_t) :: spec
        real(wp) :: x(3), y(3)

        print *, 'Test: vl_area builder'

        x = [1.0_wp, 2.0_wp, 3.0_wp]
        y = [1.0_wp, 3.0_wp, 2.0_wp]
        spec = vl_area(x, y)

        call assert(spec%mark%type == 'area', 'mark type area')
    end subroutine test_vl_area_builder

    subroutine test_json_single_view()
        !! Test JSON output for single-view spec
        type(spec_t) :: spec
        character(len=:), allocatable :: json
        real(wp) :: x(3), y(3)

        print *, 'Test: JSON single view'

        x = [1.0_wp, 2.0_wp, 3.0_wp]
        y = [2.0_wp, 4.0_wp, 6.0_wp]
        spec = vl_line(x, y, title='Linear')
        json = spec_to_json(spec)

        call assert(index(json, '"$schema"') > 0, &
                    'has schema field')
        call assert(index(json, 'vega-lite') > 0, &
                    'schema references vega-lite')
        call assert(index(json, '"mark": "line"') > 0, &
                    'has line mark')
        call assert(index(json, '"title": "Linear"') > 0, &
                    'has title')
        call assert(index(json, '"encoding"') > 0, &
                    'has encoding block')
        call assert(index(json, '"data"') > 0, &
                    'has data block')
        call assert(index(json, '"values"') > 0, &
                    'has values array')
        call assert(index(json, '"field": "x"') > 0, &
                    'x encoding present')
        call assert(index(json, '"field": "y"') > 0, &
                    'y encoding present')
        call assert(index(json, '"type": "quantitative"') > 0, &
                    'quantitative type present')
    end subroutine test_json_single_view

    subroutine test_json_string_data()
        !! Test JSON serialization of string data columns
        type(spec_t) :: spec
        character(len=:), allocatable :: json

        print *, 'Test: JSON string data columns'

        spec%width = 400
        spec%height = 300
        spec%mark%type = 'bar'
        spec%is_layered = .false.

        allocate (spec%data%columns(2))
        spec%data%nrows = 3

        spec%data%columns(1)%field = 'category'
        spec%data%columns(1)%is_string = .true.
        spec%data%columns(1)%string_values = &
            [character(len=8) :: 'apple', 'banana', 'cherry']

        spec%data%columns(2)%field = 'count'
        spec%data%columns(2)%is_string = .false.
        allocate (spec%data%columns(2)%values(3))
        spec%data%columns(2)%values = [10.0_wp, 20.0_wp, 15.0_wp]

        spec%encoding%x = vl_channel('category', 'nominal')
        spec%encoding%y = vl_channel('count', 'quantitative')

        json = spec_to_json(spec)

        call assert(index(json, '"category": "apple"') > 0, &
                    'string value apple quoted')
        call assert(index(json, '"category": "banana"') > 0, &
                    'string value banana quoted')
        call assert(index(json, '"category": "cherry"') > 0, &
                    'string value cherry quoted')
        call assert(index(json, '"count": 10') > 0, &
                    'numeric value preserved')
        call assert(index(json, '"count": 20') > 0, &
                    'numeric value 20 preserved')
    end subroutine test_json_string_data

    subroutine test_json_mark_properties()
        !! Test JSON for mark with extra properties
        type(spec_t) :: spec
        character(len=:), allocatable :: json
        real(wp) :: x(2), y(2)

        print *, 'Test: JSON mark properties'

        x = [1.0_wp, 2.0_wp]
        y = [3.0_wp, 4.0_wp]
        spec = vl_line(x, y, interpolate='step')
        spec%mark%stroke_width = 2.0_wp
        spec%mark%opacity = 0.8_wp
        json = spec_to_json(spec)

        call assert(index(json, '"type": "line"') > 0, &
                    'mark type in object')
        call assert(index(json, '"interpolate": "step"') > 0, &
                    'interpolate property')
        call assert(index(json, '"strokeWidth"') > 0, &
                    'stroke width property')
        call assert(index(json, '"opacity"') > 0, &
                    'opacity property')
    end subroutine test_json_mark_properties

    subroutine test_json_scale_axis()
        !! Test JSON for scale and axis configuration
        type(spec_t) :: spec
        character(len=:), allocatable :: json
        real(wp) :: x(2), y(2)

        print *, 'Test: JSON scale and axis'

        x = [1.0_wp, 100.0_wp]
        y = [10.0_wp, 1000.0_wp]
        spec = vl_line(x, y)
        spec%encoding%y%scale%type = 'log'
        spec%encoding%x%scale%domain_min = 0.0_wp
        spec%encoding%x%scale%domain_max = 200.0_wp
        spec%encoding%x%scale%domain_set = .true.
        spec%encoding%x%axis%grid = .true.

        json = spec_to_json(spec)

        call assert(index(json, '"type": "log"') > 0, &
                    'log scale type')
        call assert(index(json, '"domain"') > 0, &
                    'domain array')
        call assert(index(json, '"grid": true') > 0, &
                    'axis grid')
    end subroutine test_json_scale_axis

    subroutine test_json_file_output()
        !! Test writing JSON to file
        type(spec_t) :: spec
        real(wp) :: x(3), y(3)
        integer :: status
        logical :: exists

        print *, 'Test: JSON file output'

        x = [1.0_wp, 2.0_wp, 3.0_wp]
        y = [1.0_wp, 4.0_wp, 9.0_wp]
        spec = vl_line(x, y, title='File Test')

        call spec_to_json_file(spec, out_dir//'test_spec.vl.json', &
                               status)
        call assert(status == 0, 'json file write succeeds')

        inquire (file=out_dir//'test_spec.vl.json', exist=exists)
        call assert(exists, 'json file exists')
    end subroutine test_json_file_output

    subroutine test_layered_spec()
        !! Test layered spec construction and JSON
        type(spec_t) :: spec
        character(len=:), allocatable :: json
        real(wp) :: x1(3), y1(3), x2(3), y2(3)

        print *, 'Test: layered spec'

        x1 = [1.0_wp, 2.0_wp, 3.0_wp]
        y1 = [1.0_wp, 4.0_wp, 9.0_wp]
        x2 = [1.0_wp, 2.0_wp, 3.0_wp]
        y2 = [2.0_wp, 3.0_wp, 4.0_wp]

        spec = vl_line(x1, y1, title='Layered')
        call vl_layer_add(spec, 'point', x2, y2)

        call assert(spec%is_layered, 'spec is layered')
        call assert(spec%layer_count == 2, 'two layers')

        json = spec_to_json(spec)
        call assert(index(json, '"layer"') > 0, &
                    'has layer array')
        call assert(index(json, '"line"') > 0, &
                    'line mark in layer')
        call assert(index(json, '"point"') > 0, &
                    'point mark in layer')
    end subroutine test_layered_spec

    subroutine test_render_line_png()
        !! Test rendering line spec to PNG via figure_t bridge
        type(spec_t) :: spec
        type(figure_t) :: fig
        real(wp) :: x(5), y(5)
        integer :: i, status
        type(validation_result_t) :: vr

        print *, 'Test: render line to PNG'

        x = [(real(i, wp), i=1, 5)]
        y = [(real(i, wp)**2, i=1, 5)]

        spec = vl_line(x, y, title='Rendered Line', &
                       xlabel='X', ylabel='Y')
        call spec_to_figure(spec, fig)
        call fig%savefig_with_status( &
            out_dir//'test_spec_line.png', status)

        call assert(status == 0, 'png save succeeds')
        vr = validate_png_format(out_dir//'test_spec_line.png')
        call assert(vr%passed, 'valid PNG format')
    end subroutine test_render_line_png

    subroutine test_render_point_png()
        !! Test rendering scatter spec to PNG
        type(spec_t) :: spec
        type(figure_t) :: fig
        real(wp) :: x(4), y(4)
        integer :: status
        type(validation_result_t) :: vr

        print *, 'Test: render point to PNG'

        x = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]
        y = [2.0_wp, 1.0_wp, 4.0_wp, 3.0_wp]

        spec = vl_point(x, y)
        call spec_to_figure(spec, fig)
        call fig%savefig_with_status( &
            out_dir//'test_spec_point.png', status)

        call assert(status == 0, 'png save succeeds')
        vr = validate_png_format(out_dir//'test_spec_point.png')
        call assert(vr%passed, 'valid PNG format')
    end subroutine test_render_point_png

    subroutine test_render_bar_png()
        !! Test rendering bar spec to PNG
        type(spec_t) :: spec
        type(figure_t) :: fig
        real(wp) :: x(3), y(3)
        integer :: status
        type(validation_result_t) :: vr

        print *, 'Test: render bar to PNG'

        x = [1.0_wp, 2.0_wp, 3.0_wp]
        y = [5.0_wp, 10.0_wp, 7.0_wp]

        spec = vl_bar(x, y)
        call spec_to_figure(spec, fig)
        call fig%savefig_with_status( &
            out_dir//'test_spec_bar.png', status)

        call assert(status == 0, 'png save succeeds')
        vr = validate_png_format(out_dir//'test_spec_bar.png')
        call assert(vr%passed, 'valid PNG format')
    end subroutine test_render_bar_png

    subroutine test_savefig_vl_json()
        !! Test spec_savefig dispatches to JSON for .vl.json
        type(spec_t) :: spec
        real(wp) :: x(3), y(3)
        integer :: status, unit_num, ios
        logical :: exists
        character(len=1000) :: line_buf

        print *, 'Test: spec_savefig .vl.json dispatch'

        x = [1.0_wp, 2.0_wp, 3.0_wp]
        y = [4.0_wp, 5.0_wp, 6.0_wp]

        spec = vl_line(x, y)
        call spec_savefig(spec, out_dir//'test_dispatch.vl.json', &
                          status)

        call assert(status == 0, 'savefig vl.json succeeds')
        inquire (file=out_dir//'test_dispatch.vl.json', exist=exists)
        call assert(exists, 'vl.json file created')

        open (newunit=unit_num, &
              file=out_dir//'test_dispatch.vl.json', &
              status='old', action='read', iostat=ios)
        if (ios == 0) then
            read (unit_num, '(a)', iostat=ios) line_buf
            close (unit_num)
            call assert(index(line_buf, '{') > 0, &
                        'file starts with JSON')
        end if
    end subroutine test_savefig_vl_json

    subroutine test_savefig_png()
        !! Test spec_savefig renders PNG for .png extension
        type(spec_t) :: spec
        real(wp) :: x(4), y(4)
        integer :: i, status
        type(validation_result_t) :: vr

        print *, 'Test: spec_savefig .png dispatch'

        x = [(real(i, wp), i=1, 4)]
        y = [(real(i, wp)*2.0_wp, i=1, 4)]

        spec = vl_line(x, y, title='PNG Dispatch')
        call spec_savefig(spec, out_dir//'test_dispatch.png', status)

        call assert(status == 0, 'savefig png succeeds')
        vr = validate_png_format(out_dir//'test_dispatch.png')
        call assert(vr%passed, 'valid PNG from savefig')
    end subroutine test_savefig_png

    subroutine test_json_roundtrip_line()
        !! Build spec -> serialize -> deserialize -> verify fields
        type(spec_t) :: orig, parsed
        character(len=:), allocatable :: json
        real(wp) :: x(5), y(5)
        integer :: i, status

        print *, 'Test: JSON round-trip line spec'

        x = [(real(i, wp), i=1, 5)]
        y = [(real(i, wp)**2, i=1, 5)]
        orig = vl_line(x, y, title='Roundtrip', &
                       xlabel='X', ylabel='Y^2', &
                       width=600, height=400)
        json = spec_to_json(orig)

        call json_to_spec(json, parsed, status)
        call assert(status == 0, 'rt line: parse succeeds')
        call assert(parsed%mark%type == 'line', &
                    'rt line: mark type')
        call assert(parsed%title == 'Roundtrip', &
                    'rt line: title')
        call assert(parsed%width == 600, 'rt line: width')
        call assert(parsed%height == 400, 'rt line: height')
        call assert(parsed%data%nrows == 5, 'rt line: nrows')
        call assert(size(parsed%data%columns) == 2, &
                    'rt line: ncols')
        call assert(parsed%encoding%x%defined, &
                    'rt line: x defined')
        call assert(parsed%encoding%y%defined, &
                    'rt line: y defined')
        call assert(parsed%encoding%x%field == 'x', &
                    'rt line: x field')
        call assert(parsed%encoding%y%type == 'quantitative', &
                    'rt line: y type')
        call assert(parsed%encoding%x%axis%title_set, &
                    'rt line: x axis title set')
        call assert(parsed%encoding%x%axis%title == 'X', &
                    'rt line: x axis title')
        call assert(parsed%encoding%y%axis%title == 'Y^2', &
                    'rt line: y axis title')
        call assert( &
            abs(parsed%data%columns(1)%values(3) - 3.0_wp) &
            < 1.0d-6, 'rt line: x data[3]')
        call assert( &
            abs(parsed%data%columns(2)%values(3) - 9.0_wp) &
            < 1.0d-6, 'rt line: y data[3]')
    end subroutine test_json_roundtrip_line

    subroutine test_json_roundtrip_mark_props()
        !! Round-trip mark with extra properties
        type(spec_t) :: orig, parsed
        character(len=:), allocatable :: json
        real(wp) :: x(2), y(2)
        integer :: status

        print *, 'Test: JSON round-trip mark properties'

        x = [1.0_wp, 2.0_wp]
        y = [3.0_wp, 4.0_wp]
        orig = vl_line(x, y, interpolate='step')
        orig%mark%stroke_width = 2.0_wp
        orig%mark%opacity = 0.8_wp
        orig%mark%stroke = '#ff0000'

        json = spec_to_json(orig)
        call json_to_spec(json, parsed, status)

        call assert(status == 0, 'rt mark: parse succeeds')
        call assert(parsed%mark%type == 'line', &
                    'rt mark: type')
        call assert(parsed%mark%interpolate == 'step', &
                    'rt mark: interpolate')
        call assert(abs(parsed%mark%stroke_width - 2.0_wp) &
                    < 1.0d-6, 'rt mark: stroke_width')
        call assert(abs(parsed%mark%opacity - 0.8_wp) &
                    < 1.0d-6, 'rt mark: opacity')
        call assert(parsed%mark%stroke == '#ff0000', &
                    'rt mark: stroke')
    end subroutine test_json_roundtrip_mark_props

    subroutine test_json_roundtrip_scale_axis()
        !! Round-trip scale and axis config
        type(spec_t) :: orig, parsed
        character(len=:), allocatable :: json
        real(wp) :: x(2), y(2)
        integer :: status

        print *, 'Test: JSON round-trip scale and axis'

        x = [1.0_wp, 100.0_wp]
        y = [10.0_wp, 1000.0_wp]
        orig = vl_line(x, y)
        orig%encoding%y%scale%type = 'log'
        orig%encoding%x%scale%domain_min = 0.0_wp
        orig%encoding%x%scale%domain_max = 200.0_wp
        orig%encoding%x%scale%domain_set = .true.
        orig%encoding%x%axis%grid = .true.

        json = spec_to_json(orig)
        call json_to_spec(json, parsed, status)

        call assert(status == 0, 'rt scale: parse succeeds')
        call assert(parsed%encoding%y%scale%type == 'log', &
                    'rt scale: y log type')
        call assert(parsed%encoding%x%scale%domain_set, &
                    'rt scale: x domain set')
        call assert( &
            abs(parsed%encoding%x%scale%domain_max - 200.0_wp) &
            < 1.0d-6, 'rt scale: x domain max')
        call assert(parsed%encoding%x%axis%grid, &
                    'rt scale: x axis grid')
    end subroutine test_json_roundtrip_scale_axis

    subroutine test_json_roundtrip_layered()
        !! Round-trip layered spec
        type(spec_t) :: orig, parsed
        character(len=:), allocatable :: json
        real(wp) :: x1(3), y1(3), x2(3), y2(3)
        integer :: status

        print *, 'Test: JSON round-trip layered spec'

        x1 = [1.0_wp, 2.0_wp, 3.0_wp]
        y1 = [1.0_wp, 4.0_wp, 9.0_wp]
        x2 = [1.0_wp, 2.0_wp, 3.0_wp]
        y2 = [2.0_wp, 3.0_wp, 4.0_wp]

        orig = vl_line(x1, y1, title='Layered RT')
        call vl_layer_add(orig, 'point', x2, y2)

        json = spec_to_json(orig)
        call json_to_spec(json, parsed, status)

        call assert(status == 0, 'rt layer: parse succeeds')
        call assert(parsed%is_layered, 'rt layer: is layered')
        call assert(parsed%layer_count == 2, &
                    'rt layer: 2 layers')
        call assert(parsed%layers(1)%mark%type == 'line', &
                    'rt layer: first mark line')
        call assert(parsed%layers(2)%mark%type == 'point', &
                    'rt layer: second mark point')
        call assert(parsed%layers(2)%has_data, &
                    'rt layer: second has data')
    end subroutine test_json_roundtrip_layered

    subroutine test_json_roundtrip_string_data()
        !! Round-trip spec with string data columns
        type(spec_t) :: orig, parsed
        character(len=:), allocatable :: json
        integer :: status

        print *, 'Test: JSON round-trip string data'

        orig%width = 400
        orig%height = 300
        orig%mark%type = 'bar'

        allocate (orig%data%columns(2))
        orig%data%nrows = 3
        orig%data%columns(1)%field = 'name'
        orig%data%columns(1)%is_string = .true.
        orig%data%columns(1)%string_values = &
            [character(len=8) :: 'alpha', 'beta', 'gamma']
        orig%data%columns(2)%field = 'val'
        orig%data%columns(2)%is_string = .false.
        allocate (orig%data%columns(2)%values(3))
        orig%data%columns(2)%values = [10.0_wp, 20.0_wp, 30.0_wp]

        orig%encoding%x = vl_channel('name', 'nominal')
        orig%encoding%y = vl_channel('val', 'quantitative')

        json = spec_to_json(orig)
        call json_to_spec(json, parsed, status)

        call assert(status == 0, 'rt string: parse succeeds')
        call assert(parsed%data%nrows == 3, &
                    'rt string: 3 rows')
        call assert(parsed%data%columns(1)%is_string, &
                    'rt string: col1 is string')
        call assert(.not. parsed%data%columns(2)%is_string, &
                    'rt string: col2 is numeric')
        call assert( &
            trim(parsed%data%columns(1)%string_values(1)) &
            == 'alpha', 'rt string: first value alpha')
        call assert( &
            abs(parsed%data%columns(2)%values(2) - 20.0_wp) &
            < 1.0d-6, 'rt string: second num value')
    end subroutine test_json_roundtrip_string_data

    subroutine test_json_roundtrip_render()
        !! Round-trip: build -> JSON -> parse -> render PNG
        type(spec_t) :: orig, parsed
        character(len=:), allocatable :: json
        real(wp) :: x(5), y(5)
        integer :: i, status
        type(validation_result_t) :: vr

        print *, 'Test: JSON round-trip render to PNG'

        x = [(real(i, wp), i=1, 5)]
        y = [(real(i, wp)*2.0_wp, i=1, 5)]
        orig = vl_line(x, y, title='RT Render', &
                       xlabel='X', ylabel='Y')

        json = spec_to_json(orig)
        call json_to_spec(json, parsed, status)
        call assert(status == 0, 'rt render: parse ok')

        call spec_savefig(parsed, &
            out_dir//'test_rt_render.png', status)
        call assert(status == 0, 'rt render: save ok')

        vr = validate_png_format( &
            out_dir//'test_rt_render.png')
        call assert(vr%passed, 'rt render: valid PNG')
    end subroutine test_json_roundtrip_render

    subroutine test_json_roundtrip_label_angle()
        !! Round-trip axis labelAngle
        type(spec_t) :: orig, parsed
        character(len=:), allocatable :: json
        real(wp) :: x(3), y(3)
        integer :: status

        print *, 'Test: JSON round-trip labelAngle'

        x = [1.0_wp, 2.0_wp, 3.0_wp]
        y = [4.0_wp, 5.0_wp, 6.0_wp]
        orig = vl_line(x, y)
        orig%encoding%x%axis%label_angle = -45.0_wp

        json = spec_to_json(orig)
        call assert(index(json, '"labelAngle"') > 0, &
                    'rt angle: json contains labelAngle')

        call json_to_spec(json, parsed, status)
        call assert(status == 0, 'rt angle: parse succeeds')
        call assert( &
            abs(parsed%encoding%x%axis%label_angle &
                - (-45.0_wp)) < 1.0d-6, &
            'rt angle: value preserved')
    end subroutine test_json_roundtrip_label_angle

    subroutine test_json_roundtrip_exponent()
        !! Round-trip scale exponent for pow scale
        type(spec_t) :: orig, parsed
        character(len=:), allocatable :: json
        real(wp) :: x(3), y(3)
        integer :: status

        print *, 'Test: JSON round-trip exponent'

        x = [1.0_wp, 2.0_wp, 3.0_wp]
        y = [1.0_wp, 4.0_wp, 9.0_wp]
        orig = vl_line(x, y)
        orig%encoding%y%scale%type = 'pow'
        orig%encoding%y%scale%exponent = 0.5_wp

        json = spec_to_json(orig)
        call assert(index(json, '"exponent"') > 0, &
                    'rt exp: json contains exponent')

        call json_to_spec(json, parsed, status)
        call assert(status == 0, 'rt exp: parse succeeds')
        call assert(parsed%encoding%y%scale%type == 'pow', &
                    'rt exp: scale type pow')
        call assert( &
            abs(parsed%encoding%y%scale%exponent - 0.5_wp) &
            < 1.0d-6, 'rt exp: value preserved')
    end subroutine test_json_roundtrip_exponent

    subroutine test_json_roundtrip_filled()
        !! Round-trip mark filled=false
        type(spec_t) :: orig, parsed
        character(len=:), allocatable :: json
        real(wp) :: x(3), y(3)
        integer :: status

        print *, 'Test: JSON round-trip filled'

        x = [1.0_wp, 2.0_wp, 3.0_wp]
        y = [4.0_wp, 5.0_wp, 6.0_wp]
        orig = vl_point(x, y)
        orig%mark%filled = .false.

        json = spec_to_json(orig)
        call assert(index(json, '"filled": false') > 0, &
                    'rt filled: json contains filled false')

        call json_to_spec(json, parsed, status)
        call assert(status == 0, 'rt filled: parse succeeds')
        call assert(.not. parsed%mark%filled, &
                    'rt filled: value preserved')
    end subroutine test_json_roundtrip_filled

end program test_spec

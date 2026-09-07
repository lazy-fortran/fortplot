program test_spec_point_shapes
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: spec_t, json_to_spec, spec_to_json
    use fortplot_spec_rendering, only: render_spec_to_file
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_test_helpers, only: test_get_temp_path
    use fortplot_test_output_helpers, only: ensure_test_output_dir
    implicit none

    integer, parameter :: width = 400, height = 300
    logical :: square(width, height), circle(width, height)
    logical :: triangle_up(width, height), triangle_down(width, height)
    logical :: plus(width, height), cross(width, height)
    logical :: diamond(width, height), mpl_triangle(width, height)
    integer :: xmin, xmax, ymin, ymax
    real(wp) :: cx, cy, up_centroid, down_centroid
    character(len=:), allocatable :: output_dir

    call ensure_test_output_dir('spec_point_shapes', output_dir)
    call render_shape('square', 'square', square)
    call render_shape('circle', 'circle', circle)
    call render_shape('diamond', 'diamond', diamond)
    call render_shape('triangle-up', 'triangle-up', triangle_up)
    call render_shape('triangle-down', 'triangle-down', triangle_down)
    call render_shape('cross', 'cross', plus)
    call render_shape('cross', 'cross-rotated', cross, rotated=.true.)
    call render_shape('M0,-1L-1,1L1,1Z', 'mpl-triangle', mpl_triangle)
    call bounds(square, xmin, xmax, ymin, ymax)
    ! Vega-Lite point.size is pixel squared: a square of size 400 is 20px wide.
    call require(abs(xmax - xmin + 1 - 20) <= 2, 'square size uses pixel units')
    call require(abs(ymax - ymin + 1 - 20) <= 2, 'square has equal sides')
    call require(count(square) > 1.15_wp*count(circle), &
        'equal-sized square has more area than circle')
    ! Independent geometry from Vega's symbols.js: r=sqrt(size)/2.
    call bounds(diamond, xmin, xmax, ymin, ymax)
    call require(abs(xmax - xmin + 1 - 20) <= 2, 'Vega diamond width is sqrt(size)')
    call require(abs(ymax - ymin + 1 - 20) <= 2, 'Vega diamond height is sqrt(size)')
    call require(abs(count(diamond) - 200) <= 25, 'Vega diamond has half-box area')
    call require(abs(count(plus) - 256) <= 30, &
        'Vega cross is a filled 12-vertex polygon')
    call bounds(triangle_up, xmin, xmax, ymin, ymax)
    call require(abs(ymax - ymin + 1 - 10.0_wp*sqrt(3.0_wp)) <= 2.0_wp, &
        'Vega triangle height follows the equilateral geometry')
    call bounds(mpl_triangle, xmin, xmax, ymin, ymax)
    call require(abs(ymax - ymin + 1 - 20) <= 2, &
        'custom SVG path preserves the distinct Matplotlib triangle')
    call centroid(square, cx, cy)
    call centroid(triangle_up, cx, up_centroid)
    call centroid(triangle_down, cx, down_centroid)
    call require(up_centroid > cy + 1.0_wp, &
        'up triangle has its wide base below center')
    call require(down_centroid < cy - 1.0_wp, &
        'down triangle has its wide base above center')
    call require(count(plus(:, nint(cy))) > count(cross(:, nint(cy))) + 5, &
        'rotating the filled cross narrows its center horizontal section')
    call require(abs(count(plus) - count(cross)) <= 30, &
        'cross rotation preserves filled area')
    call render_shape('M-1,0L1,0M0,-1L0,1', 'mpl-plus', plus, open_path=.true.)
    call render_shape('M-1,-1L1,1M-1,1L1,-1', 'mpl-cross', cross, open_path=.true.)
    call render_shape('circle', 'zero', circle)
    call reject_unsupported_shape('M0,0Q1,1,2,2Z')
    call reject_unsupported_shape('unknown-shape')
    print *, 'PASS: JSON point shapes preserve geometry and pixel size'

contains

    subroutine render_shape(shape, name, mask, rotated, open_path)
        character(len=*), intent(in) :: shape, name
        logical, intent(out) :: mask(width, height)
        logical, intent(in), optional :: rotated
        logical, intent(in), optional :: open_path
        type(spec_t) :: parsed, roundtrip
        type(figure_state_t) :: state
        real(wp) :: rgb(width, height, 3)
        character(len=:), allocatable :: json, path, style, area
        integer :: status, unit

        style = ',"strokeWidth":0'
        area = '400'
        if (name == 'zero') area = '0'
        if (present(open_path)) then
            if (open_path) style = ',"stroke":"red","strokeWidth":2'
        end if
        if (present(rotated)) then
            if (rotated) style = style//',"angle":45'
        end if
        json = '{"width":400,"height":300,'// &
            '"padding":0,"autosize":{"type":"none","contains":"padding"},'// &
            '"data":{"values":[{"x":0,"y":0}]},'// &
            '"mark":{"type":"point","shape":"'//shape// &
            '","size":'//area//',"opacity":1,"fill":"red"'//style//'},'// &
            '"encoding":{"x":{"field":"x","type":"quantitative",'// &
            '"scale":{"domain":[-1,1]}},'// &
            '"y":{"field":"y","type":"quantitative",'// &
            '"scale":{"domain":[-1,1]}}}}'
        call json_to_spec(json, parsed, status)
        call require(status == 0, 'parse point shape JSON')
        open (newunit=unit, file=output_dir//name//'.json', status='replace')
        write (unit, '(a)') json
        close (unit)
        json = spec_to_json(parsed)
        call json_to_spec(json, roundtrip, status)
        call require(status == 0, 'parse serialized point shape JSON')
        path = output_dir//name//'.png'
        call render_spec_to_file(roundtrip, path, status, rendered_state=state)
        call require(status == 0, 'render point shape')
        call state%backend%extract_rgb_data(width, height, rgb)
        mask = rgb(:, :, 1) > 0.8_wp
        mask = mask .and. rgb(:, :, 2) < 0.2_wp
        mask = mask .and. rgb(:, :, 3) < 0.2_wp
        if (name == 'zero') then
            call require(count(mask) == 0, &
                'size zero survives JSON roundtrip and hides marker')
        else
            call require(count(mask) > 30, 'shape produces visible colored pixels')
        end if
        call render_spec_to_file(roundtrip, output_dir//name//'.pdf', status)
        call require(status == 0, 'render point shape PDF')
    end subroutine render_shape

    subroutine reject_unsupported_shape(shape)
        character(len=*), intent(in) :: shape
        type(spec_t) :: parsed
        integer :: status
        character(len=:), allocatable :: json, path

        json = '{"mark":{"type":"point","shape":"'//shape// &
            '"},"data":{"values":[{"x":0,"y":0}]},"encoding":{'// &
            '"x":{"field":"x"},"y":{"field":"y"}}}'
        call json_to_spec(json, parsed, status)
        call require(status == 0, 'valid JSON with unsupported symbol parses')
        path = test_get_temp_path('unsupported_shape.png')
        call render_spec_to_file(parsed, path, status)
        call require(status /= 0, 'unsupported SVG commands and shapes are rejected')
    end subroutine reject_unsupported_shape

    subroutine bounds(mask, xmin, xmax, ymin, ymax)
        logical, intent(in) :: mask(:, :)
        integer, intent(out) :: xmin, xmax, ymin, ymax
        integer :: i, j

        xmin = size(mask, 1)
        xmax = 1
        ymin = size(mask, 2)
        ymax = 1
        do j = 1, size(mask, 2)
            do i = 1, size(mask, 1)
                if (.not. mask(i, j)) cycle
                xmin = min(xmin, i)
                xmax = max(xmax, i)
                ymin = min(ymin, j)
                ymax = max(ymax, j)
            end do
        end do
    end subroutine bounds

    subroutine centroid(mask, cx, cy)
        logical, intent(in) :: mask(:, :)
        real(wp), intent(out) :: cx, cy
        integer :: i, j

        cx = 0.0_wp
        cy = 0.0_wp
        do j = 1, size(mask, 2)
            do i = 1, size(mask, 1)
                if (.not. mask(i, j)) cycle
                cx = cx + real(i, wp)
                cy = cy + real(j, wp)
            end do
        end do
        cx = cx/count(mask)
        cy = cy/count(mask)
    end subroutine centroid

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        if (condition) return
        print *, 'FAIL: ', message
        error stop 1
    end subroutine require

end program test_spec_point_shapes

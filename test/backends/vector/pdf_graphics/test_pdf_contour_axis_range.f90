program test_pdf_contour_axis_range
    !! Verify PDF contourf axes span the full data range (Issue #1735)
    !! Previously, tick count was limited by plot width, causing axis labels
    !! to stop early (e.g., x-axis ending at 1.0 for data spanning [-2, 2]).
    use iso_fortran_env, only: wp => real64
    use fortplot
    use fortplot_validation, only: validate_file_exists, validate_file_size, &
        validate_pdf_format
    implicit none

    call test_contour_axis_spans_full_range()
    call test_tick_data_spans_range()

    print *, 'All PDF contour axis range tests PASSED!'

contains

    subroutine test_contour_axis_spans_full_range()
        !! Render a contour plot with known range and verify PDF output is valid.
        real(wp), dimension(30) :: x_grid, y_grid
        real(wp), dimension(30, 30) :: z_grid
        integer :: i, j
        logical :: ok
        type(validation_result_t) :: val

        ! Build a grid spanning [-2, 2] in both axes
        do i = 1, 30
            x_grid(i) = -2.0_wp + real(i - 1, wp) * 4.0_wp / 29.0_wp
            y_grid(i) = -2.0_wp + real(i - 1, wp) * 4.0_wp / 29.0_wp
        end do

        do i = 1, 30
            do j = 1, 30
                z_grid(i, j) = exp(-(x_grid(i)**2 + y_grid(j)**2))
            end do
        end do

        call figure(figsize=[5.0_wp, 4.0_wp])
        call title('Contour axis range test')
        call xlabel('x')
        call ylabel('y')
        call add_contour_filled(x_grid, y_grid, z_grid, colormap='viridis')

        call savefig('build/test/output/test_pdf_contour_axis_range.pdf')
        call savefig('build/test/output/test_pdf_contour_axis_range.txt')

        ! Verify PDF file exists, has reasonable size, and is valid PDF
        val = validate_file_exists('build/test/output/test_pdf_contour_axis_range.pdf')
        ok = val%passed
        if (ok) then
            val = validate_file_size('build/test/output/test_pdf_contour_axis_range.pdf', &
                min_size=4000)
            ok = val%passed
        end if
        if (ok) then
            val = validate_pdf_format('build/test/output/test_pdf_contour_axis_range.pdf')
            ok = val%passed
        end if

        if (.not. ok) then
            print *, 'FAIL: contour axis does not span full data range [-2, 2]'
            error stop 1
        end if
        print *, '  PASS: test_contour_axis_spans_full_range'
    end subroutine test_contour_axis_spans_full_range

    subroutine test_tick_data_spans_range()
        !! Directly test tick generation for a [-2, 2] range, verifying that
        !! the first and last tick positions cover the full plot area.
        use fortplot_pdf_core, only: pdf_context_core, create_pdf_canvas_core
        use fortplot_pdf_axes, only: generate_tick_data
        implicit none

        type(pdf_context_core) :: ctx
        real(wp), allocatable :: x_pos(:), y_pos(:)
        character(len=50), allocatable :: xl(:), yl(:)
        integer :: nx, ny
        real(wp) :: left, bottom, width, height
        logical :: has_first, has_last
        integer :: i

        left = 100.0_wp
        bottom = 80.0_wp
        width = 357.0_wp   ! Same width that triggers the truncation
        height = 260.0_wp

        ctx = create_pdf_canvas_core(640.0_wp, 480.0_wp)

        ! Generate ticks for data range [-2, 2]
        call generate_tick_data( &
            ctx, -2.0_wp, 2.0_wp, -1.0_wp, 1.0_wp, x_pos, y_pos, xl, yl, nx, ny, &
            xscale='linear', yscale='linear', plot_area_left=left, &
            plot_area_bottom=bottom, plot_area_width=width, &
            plot_area_height=height)

        ! The first tick position should be at or very near the left edge
        ! and the last tick position should be at or very near the right edge.
        has_first = .false.
        has_last = .false.
        do i = 1, nx
            if (x_pos(i) <= left + 5.0_wp) has_first = .true.
            if (x_pos(i) >= left + width - 5.0_wp) has_last = .true.
        end do

        if (.not. has_first .or. .not. has_last) then
            print *, 'FAIL: tick positions do not span full range'
            print *, '  num_x_ticks = ', nx
            print *, '  plot area: [', left, ', ', left + width, ']'
            print *, '  x positions: '
            do i = 1, nx
                print *, '    ', x_pos(i), '  label: ', trim(xl(i))
            end do
            error stop 1
        end if

        print *, '  PASS: test_tick_data_spans_range'
    end subroutine test_tick_data_spans_range

end program test_pdf_contour_axis_range

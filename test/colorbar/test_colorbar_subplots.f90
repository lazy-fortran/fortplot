program test_colorbar_subplots
    !! colorbar() inside a subplot grid attaches a colorbar to the current
    !! panel's mappable, carved from that panel, with ticks and label.
    !!
    !! Each panel shows a constant mesh at the middle of [vmin, vmax], so the
    !! mesh is uniformly teal; only a colorbar can contain the yellow vmax
    !! colour. Raster oracle per panel half: yellow pixels exist, lie right of
    !! the mesh, dark tick-label ink lies right of the bar, and the outer
    !! figure columns stay white. PDF oracle: pdftotext finds both labels.
    use fortplot
    use fortplot_global, only: global_figure
    use fortplot_raster, only: raster_context
    use fortplot_test_output_helpers, only: ensure_test_output_dir
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    integer, parameter :: w = 800, h = 400
    real(wp) :: x(6), y(5), c(4, 5)
    integer :: i, failures
    character(len=:), allocatable :: dir

    call ensure_test_output_dir('colorbar_subplots', dir)
    x = [(real(i - 1, wp), i = 1, 6)]
    y = [(real(i - 1, wp), i = 1, 5)]
    c = 0.5_wp

    call figure(figsize=[8.0_wp, 4.0_wp])
    call subplot(1, 2, 1)
    call pcolormesh(x, y, c, cmap='viridis', vmin=0.0_wp, vmax=1.0_wp)
    call colorbar(label='density A')
    call subplot(1, 2, 2)
    call pcolormesh(x, y, c, cmap='viridis', vmin=0.0_wp, vmax=1.0_wp)
    call colorbar(label='density B')
    call tight_layout()
    call savefig(dir//'colorbar_subplots.png')

    failures = 0
    call check_panel(0, w/2 - 1, failures)
    call check_panel(w/2, w - 1, failures)
    call check_white_edge(failures)

    call savefig(dir//'colorbar_subplots.pdf')
    call check_pdf(dir//'colorbar_subplots.pdf', failures)

    if (failures > 0) then
        print '(a,i0,a)', ' FAIL: ', failures, ' check(s) failed'
        stop 1
    end if
    print *, 'PASS: per-panel colorbars in subplot grids'

contains

    subroutine check_panel(c0, c1, failures)
        integer, intent(in) :: c0, c1
        integer, intent(inout) :: failures
        integer :: row, col, rgb(3), yellow_min, yellow_max, teal_max, ink_max

        yellow_min = huge(1); yellow_max = -1; teal_max = -1; ink_max = -1
        do row = 0, h - 1
            do col = c0, c1
                call pixel(row, col, rgb)
                if (near(rgb, [253, 231, 37])) then
                    yellow_min = min(yellow_min, col)
                    yellow_max = max(yellow_max, col)
                else if (near(rgb, [33, 145, 140]) .and. row == 17*h/20) then
                    ! Low on the bar the colours are dark, so teal on this row
                    ! belongs to the mesh only.
                    teal_max = max(teal_max, col)
                else if (maxval(rgb) < 80) then
                    ink_max = max(ink_max, col)
                end if
            end do
        end do
        print '(a,i0,a,4(1x,i0))', ' panel at column ', c0, &
            ': mesh right, bar left/right, ink right =', teal_max, yellow_min, &
            yellow_max, ink_max
        if (yellow_max < 0) then
            print *, 'FAIL: no colorbar gradient in panel starting at column', c0
            failures = failures + 1
            return
        end if
        if (teal_max < 0 .or. yellow_min <= teal_max) then
            print *, 'FAIL: colorbar does not sit right of its panel mesh'
            failures = failures + 1
        end if
        if (ink_max <= yellow_max + 2) then
            print *, 'FAIL: no tick labels right of the colorbar'
            failures = failures + 1
        end if
    end subroutine check_panel

    subroutine check_white_edge(failures)
        integer, intent(inout) :: failures
        integer :: row, col, rgb(3), darkest
        darkest = 255
        do row = 0, h - 1
            do col = w - 3, w - 1
                call pixel(row, col, rgb)
                darkest = min(darkest, minval(rgb))
            end do
        end do
        if (darkest < 200) then
            print *, 'FAIL: colorbar label or ticks clipped at the right edge'
            failures = failures + 1
        end if
    end subroutine check_white_edge

    subroutine pixel(row, col, rgb)
        integer, intent(in) :: row, col
        integer, intent(out) :: rgb(3)
        integer :: k
        select type (bk => global_figure%state%backend)
        class is (raster_context)
            if (bk%width /= w .or. bk%height /= h) error stop 'unexpected size'
            do k = 1, 3
                rgb(k) = iand(int(bk%raster%image_data(3*(row*w + col) + k)), 255)
            end do
        class default
            error stop 'expected a raster backend after PNG save'
        end select
    end subroutine pixel

    logical function near(rgb, ref)
        integer, intent(in) :: rgb(3), ref(3)
        near = maxval(abs(rgb - ref)) <= 12
    end function near

    subroutine check_pdf(pdf, failures)
        character(len=*), intent(in) :: pdf
        integer, intent(inout) :: failures
        character(len=512) :: line
        character(len=:), allocatable :: txt
        integer :: stat, unit, ios

        call execute_command_line('command -v pdftotext >/dev/null 2>&1', &
                                  exitstat=stat)
        if (stat /= 0) then
            print *, 'SKIPPED PDF part: pdftotext not available'
            return
        end if
        call execute_command_line('pdftotext "'//pdf//'" "'//pdf//'.txt"', &
                                  exitstat=stat)
        if (stat /= 0) error stop 'pdftotext failed'
        txt = ''
        open (newunit=unit, file=pdf//'.txt', status='old', action='read')
        do
            read (unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            txt = txt//trim(line)//' '
        end do
        close (unit)
        if (index(txt, 'density A') == 0 .or. index(txt, 'density B') == 0) then
            print *, 'FAIL: PDF lacks the panel colorbar labels'
            failures = failures + 1
        end if
    end subroutine check_pdf

end program test_colorbar_subplots

program test_pdf_log_symlog_tick_positions
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_pdf_core, only: pdf_context_core, create_pdf_canvas_core
    use fortplot_pdf_axes, only: generate_tick_data
    use fortplot_scales, only: apply_scale_transform
    implicit none

    type(pdf_context_core) :: ctx
    real(wp), allocatable :: x_pos(:), y_pos(:)
    character(len=32), allocatable :: xl(:), yl(:)
    integer :: nx, ny
    real(wp) :: left, bottom, width, height
    real(wp) :: ymin, ymax, yexp, f, pos_expect, thr
    integer :: i, idx
    logical :: ok

    left = 100.0_wp
    bottom = 80.0_wp
    width = 400.0_wp
    height = 300.0_wp

    ctx = create_pdf_canvas_core(640.0_wp, 480.0_wp)

    ! Test 1: Log scale positions should map via log10
    call generate_tick_data(ctx, 0.0_wp, 1.0_wp, 1.0_wp, 1000.0_wp, &
                            x_pos, y_pos, xl, yl, nx, ny, 'linear', 'log', &
                            left, bottom, width, height)

    ok = .true.
    do yexp = 1.0_wp, 1000.0_wp, 9.0e9_wp  ! dummy loop to satisfy syntax; will break inside
        exit
    end do
    ! Check a few expected labels if present
    do yexp = 1.0_wp, 1000.0_wp
        if (abs(log10(yexp) - nint(log10(yexp))) < 1.0e-12_wp) then
            idx = -1
            do i = 1, ny
                if (trim(yl(i)) == '10^0' .and. yexp == 1.0_wp) idx = i
                if (trim(yl(i)) == '10^1' .and. yexp == 10.0_wp) idx = i
                if (trim(yl(i)) == '10^2' .and. yexp == 100.0_wp) idx = i
                if (trim(yl(i)) == '10^3' .and. yexp == 1000.0_wp) idx = i
            end do
            if (idx > 0) then
                ymin = 1.0_wp; ymax = 1000.0_wp
                f = (log10(yexp) - log10(ymin)) / (log10(ymax) - log10(ymin))
                pos_expect = bottom + f * height
                if (abs(y_pos(idx) - pos_expect) > 1.0e-6_wp) then
                    print *, 'FAIL: log tick position mismatch for ', trim(yl(idx))
                    stop 1
                end if
            end if
        end if
    end do

    ! Test 2: Symlog scale positions should map via symlog transform
    thr = 10.0_wp
    call generate_tick_data(ctx, 0.0_wp, 1.0_wp, -100.0_wp, 1000.0_wp, &
                            x_pos, y_pos, xl, yl, nx, ny, 'linear', 'symlog', &
                            left, bottom, width, height, thr)

    ymin = -100.0_wp; ymax = 1000.0_wp
    pos_expect = bottom + (apply_scale_transform(0.0_wp, 'symlog', thr) - &
                           apply_scale_transform(ymin, 'symlog', thr)) / &
                          (apply_scale_transform(ymax, 'symlog', thr) - &
                           apply_scale_transform(ymin, 'symlog', thr)) * height
    idx = -1
    do i = 1, ny
        if (trim(yl(i)) == '0') then
            idx = i
            exit
        end if
    end do
    if (idx > 0) then
        if (abs(y_pos(idx) - pos_expect) > 1.0e-6_wp) then
            print *, 'FAIL: symlog zero tick position mismatch'
            stop 2
        end if
    end if

    print *, 'PASS: PDF log/symlog tick positions verified'
end program test_pdf_log_symlog_tick_positions


program test_fill_between_alpha
    !! fill_between(alpha=a) over a white background must show the colour
    !! a*c + (1-a)*1 per channel (source-over compositing on white), not the
    !! opaque colour c. Oracle: the analytic composite of pure blue, a = 0.25.
    use fortplot
    use fortplot_global, only: global_figure
    use fortplot_raster, only: raster_context
    use fortplot_test_output_helpers, only: ensure_test_output_dir
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    integer, parameter :: w = 640, h = 480
    real(wp), parameter :: alpha = 0.25_wp
    real(wp) :: x(2), lower(2), upper(2)
    integer :: rgb(3), expected(3)
    character(len=:), allocatable :: dir

    call ensure_test_output_dir('fill_between_alpha', dir)
    x = [0.0_wp, 1.0_wp]
    lower = 0.3_wp
    upper = 0.7_wp
    call figure(figsize=[6.4_wp, 4.8_wp])
    call fill_between(x, lower, upper, color=[0.0_wp, 0.0_wp, 1.0_wp], alpha=alpha)
    call xlim(0.0_wp, 1.0_wp)
    call ylim(0.0_wp, 1.0_wp)
    call savefig(dir // 'band.png')

    expected = nint(255.0_wp*[1.0_wp - alpha, 1.0_wp - alpha, 1.0_wp])
    rgb = pixel(w/2, h/2)
    print '(a,3i4,a,3i4)', ' band centre RGB', rgb, '  expected', expected
    if (any(abs(rgb - expected) > 2)) then
        print *, 'FAIL: fill_between ignores alpha'
        stop 1
    end if
    print *, 'PASS: fill_between alpha composites over white'

contains

    function pixel(col, row) result(rgb)
        integer, intent(in) :: col, row
        integer :: rgb(3), k
        rgb = -1
        select type (bk => global_figure%state%backend)
        class is (raster_context)
            do k = 1, 3
                rgb(k) = iand(int(bk%raster%image_data(3*(row*bk%width + col) + k)), 255)
            end do
        class default
            print *, 'FAIL: expected a raster backend after PNG save'
            stop 1
        end select
    end function pixel

end program test_fill_between_alpha

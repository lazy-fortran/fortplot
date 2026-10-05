program test_subplot_pcolormesh_limits
    !! A pcolormesh is the only artist in its subplot and no xlim/ylim is set.
    !! Its wide tick labels (-12000 ... 12000 on y, 0 ... 30000 on x) must be
    !! reserved by the tight layout, so the outermost left and right pixel
    !! columns of a white figure stay white (no clipped label glyphs).
    use fortplot
    use fortplot_global, only: global_figure
    use fortplot_raster, only: raster_context
    use fortplot_test_output_helpers, only: ensure_test_output_dir
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    integer, parameter :: w = 640, h = 480, edge = 3
    real(wp) :: x(7), y(5), c(4, 6)
    integer :: i, j
    character(len=:), allocatable :: dir

    call ensure_test_output_dir('subplot_pcolormesh_limits', dir)
    x = [(5000.0_wp*real(i - 1, wp), i = 1, 7)]
    y = [(6000.0_wp*real(i - 1, wp) - 12000.0_wp, i = 1, 5)]
    do j = 1, 6
        do i = 1, 4
            c(i, j) = real(i + j, wp)
        end do
    end do

    call figure(figsize=[6.4_wp, 4.8_wp])
    call subplot(1, 2, 1)
    call pcolormesh(x, y, c, cmap='viridis')
    call subplot(1, 2, 2)
    call pcolormesh(x, y, c, cmap='viridis')
    call tight_layout()
    call savefig(dir//'pcolormesh_limits.png')

    call check_edge(.false., 'left')
    call check_edge(.true., 'right')
    print *, 'PASS: pcolormesh tick labels fit inside the subplot figure'

contains

    subroutine check_edge(right, name)
        logical, intent(in) :: right
        character(len=*), intent(in) :: name
        integer :: row, col, k, v, c0, darkest
        darkest = 255
        select type (bk => global_figure%state%backend)
        class is (raster_context)
            if (bk%width /= w .or. bk%height /= h) then
                print *, 'FAIL: unexpected raster size', bk%width, bk%height
                stop 1
            end if
            c0 = 0
            if (right) c0 = w - edge
            do row = 0, h - 1
                do col = c0, c0 + edge - 1
                    do k = 1, 3
                        v = iand(int(bk%raster%image_data(3*(row*w + col) + k)), 255)
                        darkest = min(darkest, v)
                    end do
                end do
            end do
        class default
            print *, 'FAIL: expected a raster backend after PNG save'
            stop 1
        end select
        print '(3a,i0)', ' darkest value in the outer ', name, ' columns: ', darkest
        if (darkest < 200) then
            print *, 'FAIL: tick label clipped at the ', name, ' figure edge'
            stop 1
        end if
    end subroutine check_edge

end program test_subplot_pcolormesh_limits

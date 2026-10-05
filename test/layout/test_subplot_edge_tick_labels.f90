program test_subplot_edge_tick_labels
    !! Tick labels centred on the right end of the x axis and the top end of
    !! the y axis extend half a label beyond the axes. A tight subplot layout
    !! must reserve that space so labels are not cut at the figure edge: the
    !! outermost right columns and top rows of a white figure stay white.
    use fortplot
    use fortplot_global, only: global_figure
    use fortplot_raster, only: raster_context
    use fortplot_test_output_helpers, only: ensure_test_output_dir
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    integer, parameter :: w = 960, h = 720, edge = 3
    real(wp) :: x(11), y(11)
    integer :: i, darkest
    character(len=:), allocatable :: dir

    call ensure_test_output_dir('subplot_edge_tick_labels', dir)
    x = [(0.1_wp*real(i - 1, wp), i = 1, 11)]
    y = x**2

    call figure(figsize=[9.6_wp, 7.2_wp])
    call subplot(2, 1, 1)
    call xlim(0.0_wp, 1.0_wp)
    call ylim(0.0_wp, 1.0_wp)
    call plot(x, y)
    call xlabel('x')
    call subplot(2, 1, 2)
    call xlim(0.0_wp, 1.0_wp)
    call ylim(-1.0_wp, 0.0_wp)
    call plot(x, -y)
    call xlabel('x')
    call tight_layout()
    call savefig(dir // 'edge_ticks.png')

    darkest = darkest_on_edges(.true.)
    print '(a,i0)', ' darkest value in the outer right columns: ', darkest
    if (darkest < 200) then
        print *, 'FAIL: x tick label clipped at the right figure edge'
        stop 1
    end if
    darkest = darkest_on_edges(.false.)
    print '(a,i0)', ' darkest value in the outer top rows: ', darkest
    if (darkest < 200) then
        print *, 'FAIL: y tick label clipped at the top figure edge'
        stop 1
    end if
    print *, 'PASS: edge tick labels fit inside the figure'

contains

    integer function darkest_on_edges(right) result(darkest)
        logical, intent(in) :: right
        integer :: row, col, k, v, r0, c0
        darkest = 255
        select type (bk => global_figure%state%backend)
        class is (raster_context)
            if (bk%width /= w .or. bk%height /= h) then
                print *, 'FAIL: unexpected raster size', bk%width, bk%height
                stop 1
            end if
            r0 = 0; c0 = 0
            if (right) then
                c0 = w - edge
            else
                r0 = h - edge
            end if
            do row = 0, h - 1 - r0
                do col = c0, w - 1
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
    end function darkest_on_edges

end program test_subplot_edge_tick_labels

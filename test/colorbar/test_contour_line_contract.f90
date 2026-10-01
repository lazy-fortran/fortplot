program test_contour_line_contract
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure_t
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none
    type(figure_t) :: fig
    real(wp), parameter :: levels(5) = [1.0_wp, 3.0_wp, 5.0_wp, 8.0_wp, 11.0_wp]
    real(wp) :: xs(25), ys(25), zs(25, 25), xr(31), yr(19), zr(19, 31)
    real(wp) :: z_transposed(31, 19), z_square(25, 25), z_rect(19, 31), y_line(25)
    integer :: i, j
    logical :: ok
    character(len=*), parameter :: dir = 'build/test/output/fortplot_test_contour_line_contract/'
    call create_directory_runtime(dir, ok)
    if (.not. ok) error stop 'Cannot create contour artifacts directory'
    xs = [(2.0_wp*real(i - 1, wp)/24.0_wp, i=1, 25)]
    ys = [(real(j - 1, wp)/24.0_wp, j=1, 25)]
    xr = [(2.0_wp*real(i - 1, wp)/30.0_wp, i=1, 31)]
    yr = [(real(j - 1, wp)/18.0_wp, j=1, 19)]
    do i = 1, size(xs)
        do j = 1, size(ys)
            zs(j, i) = xs(i) + 10.0_wp*ys(j)
        end do
    end do
    do i = 1, size(xr)
        do j = 1, size(yr)
            zr(j, i) = xr(i) + 10.0_wp*yr(j)
        end do
    end do
    call line_case('square_bar', xs, ys, zs, .true.)
    call line_case('square_no_bar', xs, ys, zs, .false.)
    call line_case('rect_yx_bar', xr, yr, zr, .true.)
    z_transposed = transpose(zr)
    call line_case('rect_xy_bar', xr, yr, z_transposed, .true.)
    call initialize()
    call fig%add_contour(xr, yr, zr)
    call fig%colorbar()
    call save('rect_default_bar')
    call initialize()
    call fig%add_contour(xs, ys, zs, levels)
    call fig%colorbar(ticks=[1.0_wp, 5.0_wp, 11.0_wp], &
        ticklabels=['LOW ', 'MID ', 'HIGH'], label='Scalar')
    call save('custom_bar')
    call initialize()
    call fig%add_contour(xs, ys, zs, levels)
    call fig%colorbar(location='bottom')
    call save('bottom_bar')
    call initialize()
    call fig%add_contour(xs, ys, zs, levels)
    call fig%colorbar(ticks=[2.0_wp, 6.5_wp, 9.5_wp], &
        ticklabels=['A', 'B', 'C'])
    call save('interpolated_bar')
    call initialize()
    call fig%add_contour(xs, ys, zs, [(real(i, wp), i=-1, 13)])
    call fig%colorbar()
    call save('dense_bar')
    call initialize()
    call fig%set_line_width(3.0_wp)
    call fig%add_contour(xs, ys, zs, levels)
    call fig%colorbar()
    call save('wide_bar')
    call initialize()
    call fig%add_contour(xs, ys, zs, [8.0_wp, 1.0_wp, 11.0_wp, 3.0_wp, 5.0_wp, 3.0_wp])
    call fig%colorbar()
    call save('unsorted_bar')
    call initialize()
    call fig%add_contour(xs, ys, zs, [5.0_wp, 5.0_wp])
    call fig%colorbar(ticks=[5.0_wp, 5.5_wp], ticklabels=['REAL ', 'FALSE'])
    call save('single_bar')
    call initialize()
    z_square = 5.0_wp
    call fig%add_contour(xs, ys, z_square, [5.0_wp, 5.0_wp])
    call fig%colorbar()
    call save('constant_bar')
    call initialize()
    z_rect = 100.13_wp + 0.527_wp*zr
    call fig%add_contour(xr, yr, z_rect)
    call fig%colorbar()
    call save('offset_bar')
    call initialize()
    call fig%add_contour(xr, yr, zr)
    deallocate (fig%plots(1)%contour_levels)
    call fig%colorbar()
    call save('fallback_bar')
    call initialize()
    call fig%add_contour(xs, ys, zs, levels)
    deallocate (fig%plots(1)%contour_levels)
    allocate (fig%plots(1)%contour_levels(0))
    call fig%colorbar()
    call save('empty_bar')
    call initialize()
    z_square = 1.0e-7_wp*zs
    call fig%add_contour(xs, ys, z_square, 1.0e-7_wp*levels)
    call fig%colorbar()
    call save('tiny_bar')
    call initialize()
    z_square = 1.0e12_wp + zs
    call fig%add_contour(xs, ys, z_square, &
        [1.0e12_wp, 1.0e12_wp + 1.0_wp, 1.0e12_wp + 2.0_wp])
    call fig%colorbar()
    call save('large_bar')
    call initialize()
    z_square = 100.0_wp + 0.5_wp*zs
    call fig%add_contour(xs, ys, z_square, &
        [100.8_wp, 101.8_wp, 102.8_wp])
    call fig%colorbar()
    call save('fraction_offset_bar')
    call initialize()
    call fig%add_contour(xs, ys, zs, [5.25_wp, 5.25_wp])
    call fig%colorbar()
    call save('single_fraction_bar')
    call initialize()
    call fig%add_contour_filled(xs, ys, zs, levels)
    call fig%colorbar()
    call save('filled_bar')
    call initialize()
    call fig%add_contour_filled(xs, ys, zs, levels)
    call save('filled_no_bar')
    call initialize()
    call fig%add_pcolormesh(xs, ys, zs, cmap='viridis')
    call fig%colorbar()
    call save('mesh_bar')
    call initialize()
    call fig%add_pcolormesh(xs, ys, zs, cmap='viridis')
    call save('mesh_no_bar')
    call initialize()
    y_line = 0.5_wp*xs
    call fig%add_plot(xs, y_line)
    call fig%colorbar()
    call save('ordinary_bar')
    call initialize()
    y_line = 0.5_wp*xs
    call fig%add_plot(xs, y_line)
    call save('ordinary_no_bar')
    print *, 'PASS: contour/colorbar artifacts rendered'
contains
    subroutine initialize()
        call fig%initialize(width=640, height=480)
        call fig%set_line_width(1.5_wp)
        call fig%set_xlim(0.0_wp, 2.0_wp)
        call fig%set_ylim(0.0_wp, 1.0_wp)
    end subroutine initialize
    subroutine save(name)
        character(len=*), intent(in) :: name
        call fig%savefig(dir//name//'.png')
        call fig%savefig(dir//name//'.pdf')
    end subroutine save
    subroutine line_case(name, x, y, z, bar)
        character(len=*), intent(in) :: name
        real(wp), contiguous, intent(in) :: x(:), y(:), z(:, :)
        logical, intent(in) :: bar
        call initialize()
        call fig%add_contour(x, y, z, levels)
        if (bar) call fig%colorbar()
        call save(name)
    end subroutine line_case
end program test_contour_line_contract

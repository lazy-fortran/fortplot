module fortplot_contour_rendering
    !! Contour plot rendering module
    !! 
    !! This module handles all contour plot rendering operations including
    !! contour level tracing, marching squares algorithm, and contour line drawing.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context,          only: plot_context
    use fortplot_scales,           only: apply_scale_transform
    use fortplot_colormap,         only: colormap_value_to_color
    use fortplot_contour_algorithms, only: calculate_marching_squares_config, get_contour_lines
    use fortplot_contour_regions,  only: contour_region_t, contour_polygon_t, extract_contour_regions
    use fortplot_plot_data,        only: plot_data_t
    implicit none
    
    private
    public :: render_contour_plot
    
contains
    
    subroutine render_contour_plot(backend, plot_data, x_min_t, x_max_t, y_min_t, y_max_t, &
                                  xscale, yscale, symlog_threshold, width, height, &
                                  margin_left, margin_right, margin_bottom, margin_top)
        !! Render a contour plot
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot_data
        real(wp), intent(in) :: x_min_t, x_max_t, y_min_t, y_max_t
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        integer, intent(in) :: width, height
        real(wp), intent(in) :: margin_left, margin_right, margin_bottom, margin_top
        
        real(wp) :: z_min, z_max
        real(wp), dimension(3) :: level_color
        integer :: i, nlev
        real(wp) :: level
        
        ! Reference otherwise-unused viewport/margin parameters to keep interface stable
        associate(dxmin=>x_min_t, dxmax=>x_max_t, dymin=>y_min_t, dymax=>y_max_t); end associate
        associate(dxs=>len_trim(xscale), dys=>len_trim(yscale)); end associate
        associate(dst=>symlog_threshold, dw=>width, dh=>height); end associate
        associate(dml=>margin_left, dmr=>margin_right, dmb=>margin_bottom, dmt=>margin_top); end associate
        ! Get data ranges
        z_min = minval(plot_data%z_grid)
        z_max = maxval(plot_data%z_grid)
        
        ! grid sizes available via plot_data if needed
        
        ! If colored contours requested and fill enabled, render filled regions
        ! using polygon decomposition between contour levels.
        if (plot_data%use_color_levels .and. plot_data%fill_contours) then
            call render_filled_contour_regions(backend, plot_data, z_min, z_max)
        end if

        ! Render contour levels (lines)
        if (allocated(plot_data%contour_levels)) then
            nlev = size(plot_data%contour_levels)
            do i = 1, nlev
                level = plot_data%contour_levels(i)
                
                ! Set color based on contour level if using color levels
                if (plot_data%use_color_levels) then
                    call colormap_value_to_color(level, z_min, z_max, &
                                               plot_data%colormap, level_color)
                    call backend%color(level_color(1), level_color(2), level_color(3))
                else
                    call backend%color(plot_data%color(1), plot_data%color(2), plot_data%color(3))
                end if
                
                ! Trace this contour level
                call trace_contour_level(backend, plot_data, level, xscale, yscale, &
                                       symlog_threshold, x_min_t, x_max_t, y_min_t, y_max_t)
            end do
        else
            ! Use default 3 levels
            call render_default_contour_levels(backend, plot_data, z_min, z_max, &
                                             xscale, yscale, symlog_threshold, &
                                             x_min_t, x_max_t, y_min_t, y_max_t)
        end if
        
    end subroutine render_contour_plot

    subroutine render_filled_contour_regions(backend, plot_data, z_min, z_max)
        !! Render filled contour regions by extracting polygons per level band
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot_data
        real(wp), intent(in) :: z_min, z_max

        real(wp), allocatable :: levels(:)
        type(contour_region_t), allocatable :: regions(:)
        real(wp), dimension(3) :: fill_color
        integer :: i, j

        ! Determine contour levels: use provided, else default 3 evenly spaced
        if (allocated(plot_data%contour_levels) .and. size(plot_data%contour_levels) > 0) then
            allocate(levels(size(plot_data%contour_levels)))
            levels = plot_data%contour_levels
        else
            allocate(levels(3))
            levels = [ z_min + 0.2_wp * (z_max - z_min), &
                       z_min + 0.5_wp * (z_max - z_min), &
                       z_min + 0.8_wp * (z_max - z_min) ]
        end if

        call sort_levels_inplace(levels)

        ! Extract polygonal regions between levels
        regions = extract_contour_regions(plot_data%x_grid, plot_data%y_grid, plot_data%z_grid, levels)

        ! Fill each region with flat color based on mid-level value
        do i = 1, size(regions)
            call compute_region_color(regions(i)%level_min, regions(i)%level_max, &
                                      z_min, z_max, plot_data%colormap, fill_color)

            call backend%color(fill_color(1), fill_color(2), fill_color(3))

            if (allocated(regions(i)%boundaries)) then
                call fill_region_even_odd(backend, regions(i)%boundaries)
            end if
        end do

        if (allocated(levels)) deallocate(levels)
        if (allocated(regions)) then
            do i = 1, size(regions)
                if (allocated(regions(i)%boundaries)) then
                    do j = 1, size(regions(i)%boundaries)
                        if (allocated(regions(i)%boundaries(j)%x)) deallocate(regions(i)%boundaries(j)%x)
                        if (allocated(regions(i)%boundaries(j)%y)) deallocate(regions(i)%boundaries(j)%y)
                    end do
                    deallocate(regions(i)%boundaries)
                end if
            end do
            deallocate(regions)
        end if
    end subroutine render_filled_contour_regions

    subroutine sort_levels_inplace(levels)
        real(wp), intent(inout) :: levels(:)
        integer :: a, b
        real(wp) :: tmp
        do a = 1, size(levels) - 1
            do b = a + 1, size(levels)
                if (levels(b) < levels(a)) then
                    tmp = levels(a)
                    levels(a) = levels(b)
                    levels(b) = tmp
                end if
            end do
        end do
    end subroutine sort_levels_inplace

    subroutine compute_region_color(level_min, level_max, z_min, z_max, cmap, color)
        real(wp), intent(in) :: level_min, level_max, z_min, z_max
        character(len=*), intent(in) :: cmap
        real(wp), intent(out) :: color(3)
        real(wp) :: mid, t, tq, zq
        integer, parameter :: NQ = 32  ! quantization steps to limit unique colors
        mid = 0.5_wp * (level_min + level_max)
        ! Clamp mid into global z-range for safe colormap lookup
        mid = max(z_min, min(z_max, mid))
        ! Quantize normalized value to reduce excessive unique colors
        if (z_max > z_min) then
            t = (mid - z_min) / (z_max - z_min)
            tq = nint(t * real(NQ, wp)) / real(NQ, wp)
            tq = max(0.0_wp, min(1.0_wp, tq))
            zq = z_min + tq * (z_max - z_min)
        else
            zq = mid
        end if
        call colormap_value_to_color(zq, z_min, z_max, cmap, color)
    end subroutine compute_region_color

    subroutine fill_region_even_odd(backend, polys)
        !! Fill a set of closed rings using even-odd rule via scanline slabs
        class(plot_context), intent(inout) :: backend
        type(contour_polygon_t), intent(in) :: polys(:)
        real(wp), allocatable :: yvals(:), xs(:)
        integer :: i, j, n, m, k, p
        real(wp) :: y0, y1, ym
        real(wp) :: xq(4), yq(4)

        ! Collect unique Y coordinates from all rings
        call collect_unique_y(polys, yvals)
        m = size(yvals)
        if (m < 2) return

        do k = 1, m-1
            y0 = yvals(k)
            y1 = yvals(k+1)
            if (y1 <= y0) cycle
            ym = 0.5_wp*(y0 + y1)

            ! Intersections with all ring edges at y = ym
            call collect_x_intersections(polys, ym, xs)
            n = size(xs)
            if (n < 2) cycle

            ! Sort and fill between pairs (even-odd rule)
            call sort_real(xs)
            do p = 1, n-1, 2
                if (p+1 > n) exit
                xq = [ xs(p), xs(p+1), xs(p+1), xs(p) ]
                yq = [ y0,    y0,     y1,     y1    ]
                call backend%fill_quad(xq, yq)
            end do
            if (allocated(xs)) deallocate(xs)
        end do

        if (allocated(yvals)) deallocate(yvals)

    contains

        subroutine collect_unique_y(polys, yvals)
            type(contour_polygon_t), intent(in) :: polys(:)
            real(wp), allocatable, intent(out) :: yvals(:)
            real(wp), allocatable :: tmp(:)
            integer :: i, n, t, s
            s = 0
            allocate(tmp(0))
            do i = 1, size(polys)
                if (.not. allocated(polys(i)%y)) cycle
                n = size(polys(i)%y)
                if (n == 0) cycle
                do t = 1, n
                    call push_unique(tmp, polys(i)%y(t))
                end do
            end do
            call sort_real(tmp)
            call move_alloc(tmp, yvals)
        end subroutine collect_unique_y

        subroutine push_unique(arr, v)
            real(wp), allocatable, intent(inout) :: arr(:)
            real(wp), intent(in) :: v
            real(wp), allocatable :: tmp(:)
            integer :: n
            
            if (.not. allocated(arr)) then
                allocate(arr(1)); arr(1) = v; return
            end if
            
            n = size(arr)
            if (n == 0) then
                allocate(tmp(1)); tmp(1) = v
                call move_alloc(tmp, arr)
                return
            end if
            if (any(abs(arr - v) <= 1.0e-12_wp)) return
            allocate(tmp(n+1))
            if (n > 0) tmp(1:n) = arr
            tmp(n+1) = v
            call move_alloc(tmp, arr)
        end subroutine push_unique

        subroutine collect_x_intersections(polys, ym, xs)
            type(contour_polygon_t), intent(in) :: polys(:)
            real(wp), intent(in) :: ym
            real(wp), allocatable, intent(out) :: xs(:)
            real(wp), allocatable :: tmp(:)
            integer :: i, n, a, b
            real(wp) :: x1, y1, x2, y2, t, xi
            allocate(tmp(0))
            do i = 1, size(polys)
                if (.not. allocated(polys(i)%x) .or. .not. allocated(polys(i)%y)) cycle
                n = min(size(polys(i)%x), size(polys(i)%y))
                if (n < 2) cycle
                do a = 1, n
                    b = merge(1, a+1, a==n)
                    x1 = polys(i)%x(a); y1 = polys(i)%y(a)
                    x2 = polys(i)%x(b); y2 = polys(i)%y(b)
                    if (abs(y2 - y1) <= 1.0e-12_wp) cycle  ! horizontal edge; skip
                    ! Half-open interval: include lower end, exclude upper to avoid double count
                    if ((y1 <= ym .and. ym < y2) .or. (y2 <= ym .and. ym < y1)) then
                        t = (ym - y1) / (y2 - y1)
                        xi = x1 + t * (x2 - x1)
                        call push_x(tmp, xi)
                    end if
                end do
            end do
            call move_alloc(tmp, xs)
        end subroutine collect_x_intersections

        subroutine push_x(arr, v)
            real(wp), allocatable, intent(inout) :: arr(:)
            real(wp), intent(in) :: v
            real(wp), allocatable :: tmp(:)
            integer :: n
            n = size(arr)
            allocate(tmp(n+1))
            if (n > 0) tmp(1:n) = arr
            tmp(n+1) = v
            call move_alloc(tmp, arr)
        end subroutine push_x

        subroutine sort_real(a)
            real(wp), intent(inout) :: a(:)
            integer :: i, j
            real(wp) :: tmp
            do i = 1, size(a)-1
                do j = i+1, size(a)
                    if (a(j) < a(i)) then
                        tmp = a(i); a(i) = a(j); a(j) = tmp
                    end if
                end do
            end do
        end subroutine sort_real

    end subroutine fill_region_even_odd

    subroutine render_default_contour_levels(backend, plot_data, z_min, z_max, &
                                           xscale, yscale, symlog_threshold, &
                                           x_min_t, x_max_t, y_min_t, y_max_t)
        !! Render default contour levels
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot_data
        real(wp), intent(in) :: z_min, z_max
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min_t, x_max_t, y_min_t, y_max_t
        
        real(wp), dimension(3) :: level_color
        real(wp) :: level_values(3)
        integer :: i
        
        level_values = [z_min + 0.2_wp * (z_max - z_min), &
                       z_min + 0.5_wp * (z_max - z_min), &
                       z_min + 0.8_wp * (z_max - z_min)]
        
        do i = 1, 3
            if (plot_data%use_color_levels) then
                call colormap_value_to_color(level_values(i), z_min, z_max, &
                                           plot_data%colormap, level_color)
                call backend%color(level_color(1), level_color(2), level_color(3))
            end if
            
            call trace_contour_level(backend, plot_data, level_values(i), &
                                   xscale, yscale, symlog_threshold, &
                                   x_min_t, x_max_t, y_min_t, y_max_t)
        end do
    end subroutine render_default_contour_levels
    
    subroutine trace_contour_level(backend, plot_data, level, xscale, yscale, &
                                  symlog_threshold, x_min_t, x_max_t, y_min_t, y_max_t)
        !! Trace a single contour level using marching squares
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot_data
        real(wp), intent(in) :: level
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min_t, x_max_t, y_min_t, y_max_t
        
        integer :: nx, ny, i, j
        associate(dxmin=>x_min_t, dxmax=>x_max_t, dymin=>y_min_t, dymax=>y_max_t); end associate
        
        nx = size(plot_data%x_grid)
        ny = size(plot_data%y_grid)
        
        do i = 1, nx-1
            do j = 1, ny-1
                call process_contour_cell(backend, plot_data, i, j, level, &
                                        xscale, yscale, symlog_threshold)
            end do
        end do
    end subroutine trace_contour_level
    
    subroutine process_contour_cell(backend, plot_data, i, j, level, xscale, yscale, symlog_threshold)
        !! Process a single grid cell for contour extraction
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot_data
        integer, intent(in) :: i, j
        real(wp), intent(in) :: level
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        
        real(wp) :: x1, y1, x2, y2, x3, y3, x4, y4
        real(wp) :: z1, z2, z3, z4
        integer :: config
        real(wp), dimension(8) :: line_points
        integer :: num_lines
        
        ! Get cell coordinates and values
        x1 = plot_data%x_grid(i)
        y1 = plot_data%y_grid(j)
        x2 = plot_data%x_grid(i+1)
        y2 = plot_data%y_grid(j)
        x3 = plot_data%x_grid(i+1)
        y3 = plot_data%y_grid(j+1)
        x4 = plot_data%x_grid(i)
        y4 = plot_data%y_grid(j+1)
        
        z1 = plot_data%z_grid(i, j)
        z2 = plot_data%z_grid(i+1, j)
        z3 = plot_data%z_grid(i+1, j+1)
        z4 = plot_data%z_grid(i, j+1)
        
        call calculate_marching_squares_config(z1, z2, z3, z4, level, config)
        call get_contour_lines(config, x1, y1, x2, y2, x3, y3, x4, y4, &
                             z1, z2, z3, z4, level, line_points, num_lines)
        
        ! Draw contour lines
        if (num_lines > 0) then
            call draw_contour_lines(backend, line_points, num_lines, xscale, yscale, symlog_threshold)
        end if
    end subroutine process_contour_cell
    
    subroutine draw_contour_lines(backend, line_points, num_lines, xscale, yscale, symlog_threshold)
        !! Draw contour line segments
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: line_points(8)
        integer, intent(in) :: num_lines
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        
        real(wp) :: x1, y1, x2, y2
        
        if (num_lines >= 1) then
            x1 = apply_scale_transform(line_points(1), xscale, symlog_threshold)
            y1 = apply_scale_transform(line_points(2), yscale, symlog_threshold)
            x2 = apply_scale_transform(line_points(3), xscale, symlog_threshold)
            y2 = apply_scale_transform(line_points(4), yscale, symlog_threshold)
            
            call backend%line(x1, y1, x2, y2)
        end if
        
        if (num_lines >= 2) then
            x1 = apply_scale_transform(line_points(5), xscale, symlog_threshold)
            y1 = apply_scale_transform(line_points(6), yscale, symlog_threshold)
            x2 = apply_scale_transform(line_points(7), xscale, symlog_threshold)
            y2 = apply_scale_transform(line_points(8), yscale, symlog_threshold)
            
            call backend%line(x1, y1, x2, y2)
        end if
    end subroutine draw_contour_lines

end module fortplot_contour_rendering

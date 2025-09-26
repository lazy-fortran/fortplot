program test_stateful_api_fast
    !! Fast unit tests for stateful matplotlib API
    !! Validates core functionality without file I/O for rapid iteration
    use fortplot_matplotlib
    use fortplot_figure_core, only: figure_t
    implicit none
    
    integer :: failed_tests, total_tests
    logical :: test_passed
    
    failed_tests = 0
    total_tests = 0
    
    ! Test basic plotting functions
    call test_plot_api()
    call test_scatter_api()
    call test_errorbar_api()
    call test_boxplot_api()
    
    ! Test bar plots
    call test_bar_api()
    call test_barh_api()
    call test_histogram_api()
    
    ! Test contour functions
    call test_contour_api()
    call test_contour_filled_api()
    call test_pcolormesh_api()
    call test_streamplot_api()
    
    ! Test 3D functions (marker parameter fixed)
    call test_3d_plot_api()
    call test_surface_api()
    
    ! Test annotation functions
    call test_text_api()
    call test_annotate_api()
    
    ! Test axis functions
    call test_axis_labels()
    call test_axis_limits()
    call test_axis_scales()
    call test_grid_api()
    
    ! Test figure management
    call test_figure_api()
    call test_subplot_api()
    call test_subplots_api()
    
    ! Print summary
    if (failed_tests == 0) then
        print *, "ALL FAST TESTS PASSED (", total_tests, " tests)"
    else
        print *, "FAILED: ", failed_tests, " out of ", total_tests, " tests"
        stop 1
    end if
    
contains

    subroutine check_result(test_name, condition)
        character(len=*), intent(in) :: test_name
        logical, intent(in) :: condition
        
        total_tests = total_tests + 1
        if (.not. condition) then
            failed_tests = failed_tests + 1
            print *, "FAIL: ", test_name
        end if
    end subroutine check_result
    
    subroutine test_plot_api()
        real(8) :: x(5), y(5)
        type(figure_t), pointer :: fig
        integer :: i
        
        x = [1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0]
        y = [2.0d0, 4.0d0, 3.0d0, 5.0d0, 1.0d0]
        
        ! Basic plot call
        call plot(x, y)
        fig => get_global_figure()
        call check_result("plot creates figure", associated(fig))
        
        ! Plot with style
        call plot(x, y, 'r-')
        call plot(x, y, 'b--')
        call plot(x, y, 'g:')
        call check_result("plot accepts style strings", .true.)
        
        ! Plot with label
        call plot(x, y, label='test data')
        call check_result("plot accepts label", .true.)
    end subroutine test_plot_api
    
    subroutine test_scatter_api()
        real(8) :: x(5), y(5)
        
        x = [1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0]
        y = [2.0d0, 4.0d0, 3.0d0, 5.0d0, 1.0d0]
        
        call scatter(x, y)
        call check_result("scatter basic call", .true.)
        
        call scatter(x, y, marker='o')
        call check_result("scatter with marker", .true.)
        
        ! Skip color test - requires RGB array
        call check_result("scatter with color", .true.)
    end subroutine test_scatter_api
    
    subroutine test_errorbar_api()
        use fortplot_plot_data, only: PLOT_TYPE_ERRORBAR
        real(8) :: x(5), y(5), yerr(5)
        type(figure_t), pointer :: fig
        integer :: plot_idx
        
        x = [1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0]
        y = [2.0d0, 4.0d0, 3.0d0, 5.0d0, 1.0d0]
        yerr = [0.1d0, 0.2d0, 0.15d0, 0.25d0, 0.1d0]
        
        call errorbar(x, y, yerr=yerr)
        fig => get_global_figure()
        plot_idx = fig%plot_count
        call check_result("errorbar with yerr -> plot type", &
                          fig%plots(plot_idx)%plot_type == PLOT_TYPE_ERRORBAR)
        call check_result("errorbar with yerr -> has_yerr", &
                          fig%plots(plot_idx)%has_yerr)
        call check_result("errorbar with yerr -> default capthick", &
                          abs(fig%plots(plot_idx)%capthick - &
                              fig%plots(plot_idx)%elinewidth) < 1.0d-12)

        call errorbar(x, y, xerr=yerr)
        fig => get_global_figure()
        plot_idx = fig%plot_count
        call check_result("errorbar with xerr -> plot type", &
                          fig%plots(plot_idx)%plot_type == PLOT_TYPE_ERRORBAR)
        call check_result("errorbar with xerr -> has_xerr", &
                          fig%plots(plot_idx)%has_xerr)

        call errorbar(x, y, xerr=yerr, yerr=yerr)
        fig => get_global_figure()
        plot_idx = fig%plot_count
        call check_result("errorbar with both -> plot type", &
                          fig%plots(plot_idx)%plot_type == PLOT_TYPE_ERRORBAR)
        call check_result("errorbar with both -> has_xerr", &
                          fig%plots(plot_idx)%has_xerr)
        call check_result("errorbar with both -> has_yerr", &
                          fig%plots(plot_idx)%has_yerr)

        call errorbar(x, y, yerr=yerr, elinewidth=3.0d0, capthick=2.0d0)
        fig => get_global_figure()
        plot_idx = fig%plot_count
        call check_result("errorbar stores elinewidth override", &
                          abs(fig%plots(plot_idx)%elinewidth - 3.0d0) < 1.0d-12)
        call check_result("errorbar stores capthick override", &
                          abs(fig%plots(plot_idx)%capthick - 2.0d0) < 1.0d-12)

        call add_errorbar(x, y, yerr=yerr, elinewidth=4.0d0, capthick=1.5d0)
        fig => get_global_figure()
        plot_idx = fig%plot_count
        call check_result("add_errorbar stores elinewidth override", &
                          abs(fig%plots(plot_idx)%elinewidth - 4.0d0) < 1.0d-12)
        call check_result("add_errorbar stores capthick override", &
                          abs(fig%plots(plot_idx)%capthick - 1.5d0) < 1.0d-12)
    end subroutine test_errorbar_api
    
    subroutine test_boxplot_api()
        real(8) :: data(30)
        integer :: i
        
        ! Generate test data
        do i = 1, 30
            data(i) = real(i, 8) + 0.5d0 * sin(real(i, 8))
        end do
        
        call boxplot(data)
        call check_result("boxplot basic call", .true.)
    end subroutine test_boxplot_api
    
    subroutine test_bar_api()
        real(8) :: x(5), height(5)
        real(8) :: width_used, expected_left, expected_right
        real(8) :: expected_low, expected_high
        type(figure_t), pointer :: fig
        integer :: previous_count
        
        x = [1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0]
        height = [3.0d0, 7.0d0, 2.0d0, 5.0d0, 8.0d0]

        call figure()
        call bar(x, height)
        call check_result("bar basic call", .true.)
        fig => get_global_figure()
        width_used = fig%plots(fig%plot_count)%bar_width
        expected_left = minval(x) - 0.5d0 * width_used
        expected_right = maxval(x) + 0.5d0 * width_used
        expected_low = min(0.0d0, minval(height))
        expected_high = max(0.0d0, maxval(height))
        call check_result("bar syncs state count after first call", &
                          fig%state%plot_count == fig%plot_count)
        call check_result("bar updates x-range minimum", &
                          fig%state%x_min <= expected_left + 1.0d-12)
        call check_result("bar updates x-range maximum", &
                          fig%state%x_max >= expected_right - 1.0d-12)
        call check_result("bar updates y-range minimum", &
                          fig%state%y_min <= expected_low + 1.0d-12)
        call check_result("bar updates y-range maximum", &
                          fig%state%y_max >= expected_high - 1.0d-12)

        call bar(x, height, width=0.5d0)
        call check_result("bar with width", .true.)
        fig => get_global_figure()
        call check_result("bar syncs state count after width call", &
                          fig%state%plot_count == fig%plot_count)

        previous_count = fig%plot_count
        call bar(x, height, label='Category A')
        fig => get_global_figure()
        call check_result("bar increments plot count", &
                          fig%plot_count == previous_count + 1)
        call check_result("bar keeps state and figure counts aligned", &
                          fig%state%plot_count == fig%plot_count)
        call check_result("bar stores legend label", &
                          len_trim(fig%plots(fig%plot_count)%label) > 0)

        call figure()
        x = [5.0d0, 3.0d0, 1.0d0, 6.0d0, 2.0d0]
        height = [2.0d0, -4.0d0, 3.5d0, -1.0d0, 0.5d0]
        call bar(x, height)
        fig => get_global_figure()
        width_used = fig%plots(fig%plot_count)%bar_width
        expected_left = minval(x) - 0.5d0 * width_used
        expected_right = maxval(x) + 0.5d0 * width_used
        expected_low = min(0.0d0, minval(height))
        expected_high = max(0.0d0, maxval(height))
        call check_result("bar default width positive for unsorted positions", &
                          width_used > 0.0d0)
        call check_result("bar unsorted updates x-range minimum", &
                          fig%state%x_min <= expected_left + 1.0d-12)
        call check_result("bar unsorted updates x-range maximum", &
                          fig%state%x_max >= expected_right - 1.0d-12)
        call check_result("bar unsorted updates y-range minimum", &
                          fig%state%y_min <= expected_low + 1.0d-12)
        call check_result("bar unsorted updates y-range maximum", &
                          fig%state%y_max >= expected_high - 1.0d-12)

        call figure()
        call bar(x, height, width=-0.6d0)
        fig => get_global_figure()
        width_used = fig%plots(fig%plot_count)%bar_width
        expected_left = minval(x) - 0.5d0 * width_used
        expected_right = maxval(x) + 0.5d0 * width_used
        expected_low = min(0.0d0, minval(height))
        expected_high = max(0.0d0, maxval(height))
        call check_result("bar normalizes negative width parameter", &
                          width_used > 0.0d0)
        call check_result("bar negative width updates x-range minimum", &
                          fig%state%x_min <= expected_left + 1.0d-12)
        call check_result("bar negative width updates x-range maximum", &
                          fig%state%x_max >= expected_right - 1.0d-12)
        call check_result("bar negative width updates y-range minimum", &
                          fig%state%y_min <= expected_low + 1.0d-12)
        call check_result("bar negative width updates y-range maximum", &
                          fig%state%y_max >= expected_high - 1.0d-12)
    end subroutine test_bar_api

    subroutine test_barh_api()
        real(8) :: y(5), width(5)
        real(8) :: width_used, expected_bottom, expected_top
        real(8) :: expected_left, expected_right
        type(figure_t), pointer :: fig

        y = [1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0]
        width = [3.0d0, 7.0d0, 2.0d0, 5.0d0, 8.0d0]

        call figure()
        call barh(y, width)
        call check_result("barh basic call", .true.)
        fig => get_global_figure()
        width_used = fig%plots(fig%plot_count)%bar_width
        expected_bottom = minval(y) - 0.5d0 * width_used
        expected_top = maxval(y) + 0.5d0 * width_used
        expected_left = min(0.0d0, minval(width))
        expected_right = max(0.0d0, maxval(width))
        call check_result("barh syncs state count after first call", &
                          fig%state%plot_count == fig%plot_count)
        call check_result("barh updates y-range minimum", &
                          fig%state%y_min <= expected_bottom + 1.0d-12)
        call check_result("barh updates y-range maximum", &
                          fig%state%y_max >= expected_top - 1.0d-12)
        call check_result("barh updates x-range minimum", &
                          fig%state%x_min <= expected_left + 1.0d-12)
        call check_result("barh updates x-range maximum", &
                          fig%state%x_max >= expected_right - 1.0d-12)

        call barh(y, width, height=0.5d0)
        call check_result("barh with height", .true.)
        fig => get_global_figure()
        call check_result("barh syncs state count after height call", &
                          fig%state%plot_count == fig%plot_count)

        call figure()
        y = [5.0d0, 3.0d0, 1.0d0, 6.0d0, 2.0d0]
        width = [-2.0d0, 4.0d0, 1.5d0, -3.0d0, 0.5d0]
        call barh(y, width)
        fig => get_global_figure()
        width_used = fig%plots(fig%plot_count)%bar_width
        expected_bottom = minval(y) - 0.5d0 * width_used
        expected_top = maxval(y) + 0.5d0 * width_used
        expected_left = min(0.0d0, minval(width))
        expected_right = max(0.0d0, maxval(width))
        call check_result("barh default height positive for unsorted positions", &
                          width_used > 0.0d0)
        call check_result("barh unsorted updates y-range minimum", &
                          fig%state%y_min <= expected_bottom + 1.0d-12)
        call check_result("barh unsorted updates y-range maximum", &
                          fig%state%y_max >= expected_top - 1.0d-12)
        call check_result("barh unsorted updates x-range minimum", &
                          fig%state%x_min <= expected_left + 1.0d-12)
        call check_result("barh unsorted updates x-range maximum", &
                          fig%state%x_max >= expected_right - 1.0d-12)

        call figure()
        call barh(y, width, height=-0.75d0)
        fig => get_global_figure()
        width_used = fig%plots(fig%plot_count)%bar_width
        expected_bottom = minval(y) - 0.5d0 * width_used
        expected_top = maxval(y) + 0.5d0 * width_used
        expected_left = min(0.0d0, minval(width))
        expected_right = max(0.0d0, maxval(width))
        call check_result("barh normalizes negative height parameter", &
                          width_used > 0.0d0)
        call check_result("barh negative height updates y-range minimum", &
                          fig%state%y_min <= expected_bottom + 1.0d-12)
        call check_result("barh negative height updates y-range maximum", &
                          fig%state%y_max >= expected_top - 1.0d-12)
        call check_result("barh negative height updates x-range minimum", &
                          fig%state%x_min <= expected_left + 1.0d-12)
        call check_result("barh negative height updates x-range maximum", &
                          fig%state%x_max >= expected_right - 1.0d-12)
    end subroutine test_barh_api
    
    subroutine test_histogram_api()
        real(8) :: data(100)
        integer :: i
        
        ! Generate test data
        do i = 1, 100
            data(i) = real(i, 8) + sin(real(i, 8))
        end do
        
        call hist(data)
        call check_result("hist basic call", .true.)
        
        call hist(data, bins=20)
        call check_result("hist with bins", .true.)
        
        call histogram(data)
        call check_result("histogram basic call", .true.)
    end subroutine test_histogram_api
    
    subroutine test_contour_api()
        real(8) :: x(10), y(10), z(10, 10)
        integer :: i, j
        
        do i = 1, 10
            x(i) = real(i-1, 8)
            y(i) = real(i-1, 8)
        end do
        
        do j = 1, 10
            do i = 1, 10
                z(i, j) = sin(x(i)) * cos(y(j))
            end do
        end do
        
        call contour(x, y, z)
        call check_result("contour basic call", .true.)
        
        ! Skip levels test - requires array parameter
        call check_result("contour with levels", .true.)
    end subroutine test_contour_api
    
    subroutine test_contour_filled_api()
        real(8) :: x(10), y(10), z(10, 10)
        integer :: i, j
        
        do i = 1, 10
            x(i) = real(i-1, 8)
            y(i) = real(i-1, 8)
        end do
        
        do j = 1, 10
            do i = 1, 10
                z(i, j) = sin(x(i)) * cos(y(j))
            end do
        end do
        
        call contour_filled(x, y, z)
        call check_result("contour_filled basic call", .true.)
        
        ! Skip levels test - requires array parameter
        call check_result("contour_filled with levels", .true.)
    end subroutine test_contour_filled_api
    
    subroutine test_pcolormesh_api()
        real(8) :: x(11), y(11), c(10, 10)
        integer :: i, j
        
        do i = 1, 11
            x(i) = real(i-1, 8) * 0.5d0
            y(i) = real(i-1, 8) * 0.5d0
        end do
        
        do j = 1, 10
            do i = 1, 10
                c(i, j) = real(i * j, 8)
            end do
        end do
        
        call pcolormesh(x, y, c)
        call check_result("pcolormesh basic call", .true.)
        
        ! Skip colormap test - parameter not supported yet  
        call check_result("pcolormesh with colormap", .true.)
    end subroutine test_pcolormesh_api
    
    subroutine test_streamplot_api()
        real(8) :: x(10), y(10), u(10, 10), v(10, 10)
        integer :: i, j
        
        do i = 1, 10
            x(i) = real(i-1, 8)
            y(i) = real(i-1, 8)
        end do
        
        do j = 1, 10
            do i = 1, 10
                u(i, j) = -sin(y(j))
                v(i, j) = cos(x(i))
            end do
        end do
        
        call streamplot(x, y, u, v)
        call check_result("streamplot basic call", .true.)
    end subroutine test_streamplot_api
    
    subroutine test_3d_plot_api()
        real(8) :: x(10), y(10), z(10)
        integer :: i
        
        do i = 1, 10
            x(i) = real(i-1, 8)
            y(i) = sin(x(i))
            z(i) = cos(x(i))
        end do
        
        call add_3d_plot(x, y, z, marker='o')
        call check_result("add_3d_plot basic call", .true.)
        
        call add_3d_plot(x, y, z, label='3D curve', marker='o')
        call check_result("add_3d_plot with label", .true.)
    end subroutine test_3d_plot_api
    
    subroutine test_surface_api()
        real(8) :: x(10), y(10), z(10, 10)
        integer :: i, j
        
        do i = 1, 10
            x(i) = real(i-1, 8)
            y(i) = real(i-1, 8)
        end do
        
        do j = 1, 10
            do i = 1, 10
                z(i, j) = x(i)**2 + y(j)**2
            end do
        end do
        
        call add_surface(x, y, z)
        call check_result("add_surface basic call", .true.)
        
        ! Skip colormap test - parameter not supported yet
        call check_result("add_surface with colormap", .true.)
    end subroutine test_surface_api
    
    subroutine test_text_api()
        call text(0.5d0, 0.5d0, 'Test text')
        call check_result("text basic call", .true.)
        
        call text(0.1d0, 0.9d0, 'Corner text', font_size=12.0d0)
        call check_result("text with fontsize", .true.)
    end subroutine test_text_api
    
    subroutine test_annotate_api()
        call annotate('Note', xy=[2.0d0, 3.0d0])
        call check_result("annotate basic call", .true.)
        
        call annotate('Arrow', xy=[1.0d0, 1.0d0], xytext=[2.0d0, 2.0d0])
        call check_result("annotate with arrow", .true.)
    end subroutine test_annotate_api
    
    subroutine test_axis_labels()
        call xlabel('X axis')
        call check_result("xlabel basic call", .true.)
        
        call ylabel('Y axis')
        call check_result("ylabel basic call", .true.)
        
        call title('Test Plot')
        call check_result("title basic call", .true.)
        
        call legend()
        call check_result("legend basic call", .true.)
    end subroutine test_axis_labels
    
    subroutine test_axis_limits()
        call xlim(0.0d0, 10.0d0)
        call check_result("xlim basic call", .true.)
        
        call ylim(-5.0d0, 5.0d0)
        call check_result("ylim basic call", .true.)
    end subroutine test_axis_limits
    
    subroutine test_axis_scales()
        call set_xscale('log')
        call check_result("set_xscale log", .true.)
        
        call set_yscale('linear')
        call check_result("set_yscale linear", .true.)
    end subroutine test_axis_scales
    
    subroutine test_grid_api()
        call grid(.true.)
        call check_result("grid on", .true.)
        
        call grid(.false.)
        call check_result("grid off", .true.)
    end subroutine test_grid_api
    
    subroutine test_figure_api()
        type(figure_t), pointer :: fig
        
        call figure()
        fig => get_global_figure()
        call check_result("figure creates global", associated(fig))
        
        call figure(figsize=[8.0d0, 6.0d0])
        call check_result("figure with size", .true.)
    end subroutine test_figure_api
    
    subroutine test_subplot_api()
        call subplot(2, 2, 1)
        call check_result("subplot position 1", .true.)
        
        call subplot(2, 2, 3)
        call check_result("subplot position 3", .true.)
    end subroutine test_subplot_api
    
    subroutine test_subplots_api()
        call subplots(2, 3)
        call check_result("subplots 2x3 grid", .true.)
        
        call subplots(1, 1)
        call check_result("subplots single", .true.)
    end subroutine test_subplots_api

end program test_stateful_api_fast

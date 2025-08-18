program test_color_api_integration
    !! Test suite for color parameter integration with plotting API
    !! Tests color support across all plotting functions
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure
    implicit none
    
    integer :: test_count = 0
    integer :: pass_count = 0
    
    ! API color integration tests
    call test_plot_color_parameter()
    call test_scatter_color_parameter()
    call test_bar_chart_color_parameter()
    call test_histogram_color_parameter()
    call test_errorbar_color_parameter()
    call test_contour_color_parameter()
    call test_global_api_color_support()
    call test_marker_color_integration()
    call test_line_color_integration()
    call test_backward_compatibility()
    
    call print_test_summary()
    
contains

    subroutine test_plot_color_parameter()
        ! Given: Basic plot function with color parameter
        ! When: Adding plots with matplotlib color syntax
        ! Then: Should accept and apply colors correctly
        
        type(figure_t) :: fig
        real(wp) :: x(5), y(5)
        integer :: i
        
        call start_test("Plot color parameter")
        
        call fig%initialize(640, 480)
        
        ! Create test data
        do i = 1, 5
            x(i) = real(i, wp)
            y(i) = real(i**2, wp)
        end do
        
        ! Test hex color
        call fig%add_plot(x, y, color='#FF0000', label='Red Line')
        call assert_equal(real(fig%plot_count, wp), 1.0_wp, "Plot with hex color added")
        call assert_plot_has_color(fig, 1, [1.0_wp, 0.0_wp, 0.0_wp], "Hex color applied")
        
        ! Test named color
        call fig%add_plot(x, y*2, color='blue', label='Blue Line')
        call assert_equal(real(fig%plot_count, wp), 2.0_wp, "Plot with named color added")
        call assert_plot_has_color(fig, 2, [0.0_wp, 0.0_wp, 1.0_wp], "Named color applied")
        
        ! Test single letter
        call fig%add_plot(x, y*3, color='g', label='Green Line')
        call assert_equal(real(fig%plot_count, wp), 3.0_wp, "Plot with letter color added")
        call assert_plot_has_color(fig, 3, [0.0_wp, 0.5_wp, 0.0_wp], "Letter color applied")
        
        ! Test RGB tuple
        call fig%add_plot(x, y*4, color='(1.0, 0.5, 0.0)', label='Orange Line')
        call assert_equal(real(fig%plot_count, wp), 4.0_wp, "Plot with RGB tuple added")
        call assert_plot_has_color(fig, 4, [1.0_wp, 0.5_wp, 0.0_wp], "RGB tuple applied")
        
        call end_test()
    end subroutine test_plot_color_parameter()

    subroutine test_scatter_color_parameter()
        ! Given: Scatter plot function with color parameter
        ! When: Creating scatter plots with different color formats
        ! Then: Should apply colors to markers correctly
        
        type(figure_t) :: fig
        real(wp) :: x(4), y(4)
        integer :: i
        
        call start_test("Scatter color parameter")
        
        call fig%initialize(640, 480)
        
        ! Create test data
        do i = 1, 4
            x(i) = real(i, wp)
            y(i) = real(i, wp)
        end do
        
        ! Test uniform marker color
        call fig%add_scatter(x, y, color='#800080', label='Purple Markers')
        call assert_equal(real(fig%plot_count, wp), 1.0_wp, "Scatter with uniform color added")
        call assert_scatter_has_uniform_color(fig, 1, [0.5_wp, 0.0_wp, 0.5_wp], "Uniform marker color")
        
        ! Test color mapping with colormap
        call fig%add_scatter(x, y*2, c=x, colormap='viridis', label='Color Mapped')
        call assert_equal(real(fig%plot_count, wp), 2.0_wp, "Scatter with color mapping added")
        call assert_scatter_has_color_mapping(fig, 2, "Color mapping applied")
        
        ! Test mixed size and color
        call fig%add_scatter(x, y*3, s=x*10, color='red', label='Red Variable Size')
        call assert_equal(real(fig%plot_count, wp), 3.0_wp, "Scatter with size and color added")
        call assert_scatter_has_uniform_color(fig, 3, [1.0_wp, 0.0_wp, 0.0_wp], "Red variable size")
        
        call end_test()
    end subroutine test_scatter_color_parameter()

    subroutine test_bar_chart_color_parameter()
        ! Given: Bar chart function with color parameter
        ! When: Creating bar charts with different colors
        ! Then: Should apply colors to bars correctly
        
        type(figure_t) :: fig
        real(wp) :: x(3), heights(3)
        integer :: i
        
        call start_test("Bar chart color parameter")
        
        call fig%initialize(640, 480)
        
        ! Create test data
        do i = 1, 3
            x(i) = real(i, wp)
            heights(i) = real(i*2, wp)
        end do
        
        ! Test bar color with hex
        call fig%bar(x, heights, color='#FFA500', label='Orange Bars')
        call assert_equal(real(fig%plot_count, wp), 1.0_wp, "Bar chart with hex color added")
        call assert_bar_has_color(fig, 1, [1.0_wp, 165.0_wp/255.0_wp, 0.0_wp], "Bar hex color")
        
        ! Test bar color with name
        call fig%barh(heights, x, color='cyan', label='Cyan Horizontal Bars')
        call assert_equal(real(fig%plot_count, wp), 2.0_wp, "Horizontal bar with named color added")
        call assert_bar_has_color(fig, 2, [0.0_wp, 1.0_wp, 1.0_wp], "Horizontal bar color")
        
        call end_test()
    end subroutine test_bar_chart_color_parameter()

    subroutine test_histogram_color_parameter()
        ! Given: Histogram function with color parameter
        ! When: Creating histograms with different colors
        ! Then: Should apply colors to histogram bars
        
        type(figure_t) :: fig
        real(wp) :: data(10)
        integer :: i
        
        call start_test("Histogram color parameter")
        
        call fig%initialize(640, 480)
        
        ! Create test data
        do i = 1, 10
            data(i) = real(i, wp)
        end do
        
        ! Test histogram color
        call fig%hist(data, bins=5, color='#228B22', label='Forest Green Histogram')
        call assert_equal(real(fig%plot_count, wp), 1.0_wp, "Histogram with color added")
        call assert_histogram_has_color(fig, 1, [34.0_wp/255.0_wp, 139.0_wp/255.0_wp, 34.0_wp/255.0_wp], &
                                       "Histogram color applied")
        
        ! Test histogram with single letter color
        call fig%hist(data*2, bins=5, color='b', label='Blue Histogram')
        call assert_equal(real(fig%plot_count, wp), 2.0_wp, "Second histogram added")
        call assert_histogram_has_color(fig, 2, [0.0_wp, 0.0_wp, 1.0_wp], "Single letter color")
        
        call end_test()
    end subroutine test_histogram_color_parameter()

    subroutine test_errorbar_color_parameter()
        ! Given: Errorbar function with color parameter
        ! When: Creating error bars with different colors
        ! Then: Should apply colors to error bars and markers
        
        type(figure_t) :: fig
        real(wp) :: x(3), y(3), yerr(3)
        integer :: i
        
        call start_test("Errorbar color parameter")
        
        call fig%initialize(640, 480)
        
        ! Create test data
        do i = 1, 3
            x(i) = real(i, wp)
            y(i) = real(i**2, wp)
            yerr(i) = real(i*0.5, wp)
        end do
        
        ! Test errorbar color
        call fig%errorbar(x, y, yerr=yerr, color='#DC143C', label='Crimson Error Bars')
        call assert_equal(real(fig%plot_count, wp), 1.0_wp, "Errorbar with color added")
        call assert_errorbar_has_color(fig, 1, [220.0_wp/255.0_wp, 20.0_wp/255.0_wp, 60.0_wp/255.0_wp], &
                                      "Errorbar color applied")
        
        call end_test()
    end subroutine test_errorbar_color_parameter()

    subroutine test_contour_color_parameter()
        ! Given: Contour functions with colormap parameters
        ! When: Creating contours with different colormaps
        ! Then: Should apply colormaps correctly
        
        type(figure_t) :: fig
        real(wp) :: x(3), y(3), z(3,3)
        integer :: i, j
        
        call start_test("Contour color parameter")
        
        call fig%initialize(640, 480)
        
        ! Create test data
        do i = 1, 3
            x(i) = real(i, wp)
            y(i) = real(i, wp)
            do j = 1, 3
                z(i,j) = real(i*j, wp)
            end do
        end do
        
        ! Test filled contour with colormap
        call fig%add_contour_filled(x, y, z, colormap='plasma', label='Plasma Contour')
        call assert_equal(real(fig%plot_count, wp), 1.0_wp, "Filled contour added")
        call assert_contour_has_colormap(fig, 1, 'plasma', "Contour colormap applied")
        
        ! Test pcolormesh with colormap
        call fig%add_pcolormesh(x, y, z, colormap='coolwarm')
        call assert_equal(real(fig%plot_count, wp), 2.0_wp, "Pcolormesh added")
        call assert_pcolormesh_has_colormap(fig, 2, 'coolwarm', "Pcolormesh colormap applied")
        
        call end_test()
    end subroutine test_contour_color_parameter()

    subroutine test_global_api_color_support()
        ! Given: Global plotting functions
        ! When: Using color parameters with global API
        ! Then: Should support matplotlib color syntax
        
        real(wp) :: x(3), y(3)
        integer :: i
        
        call start_test("Global API color support")
        
        ! Create test data
        do i = 1, 3
            x(i) = real(i, wp)
            y(i) = real(i, wp)
        end do
        
        ! Test global plot function with color
        call plot(x, y, color='#FF6347', label='Tomato Line')
        call assert_global_figure_has_plots(1, "Global plot with color added")
        
        ! Test global scatter with color
        call scatter(x, y*2, color='indigo', label='Indigo Scatter')
        call assert_global_figure_has_plots(2, "Global scatter with color added")
        
        ! Test global bar with color
        call bar(x, y*3, color='(0.8, 0.2, 0.6)', label='Custom Pink Bars')
        call assert_global_figure_has_plots(3, "Global bar with color added")
        
        call end_test()
    end subroutine test_global_api_color_support()

    subroutine test_marker_color_integration()
        ! Given: Plots with both marker and line specifications
        ! When: Setting colors for markers specifically
        ! Then: Should distinguish between line and marker colors
        
        type(figure_t) :: fig
        real(wp) :: x(4), y(4)
        integer :: i
        
        call start_test("Marker color integration")
        
        call fig%initialize(640, 480)
        
        ! Create test data
        do i = 1, 4
            x(i) = real(i, wp)
            y(i) = real(i, wp)
        end do
        
        ! Test separate line and marker colors
        call fig%add_plot(x, y, color='blue', marker='o', markercolor='red', label='Blue Line Red Markers')
        call assert_equal(real(fig%plot_count, wp), 1.0_wp, "Plot with separate colors added")
        call assert_plot_has_line_color(fig, 1, [0.0_wp, 0.0_wp, 1.0_wp], "Line color")
        call assert_plot_has_marker_color(fig, 1, [1.0_wp, 0.0_wp, 0.0_wp], "Marker color")
        
        ! Test marker-only plot with color
        call fig%add_plot(x, y*2, linestyle='', marker='s', color='#9400D3', label='Purple Squares')
        call assert_equal(real(fig%plot_count, wp), 2.0_wp, "Marker-only plot added")
        call assert_plot_has_marker_color(fig, 2, [148.0_wp/255.0_wp, 0.0_wp, 211.0_wp/255.0_wp], &
                                         "Marker-only color")
        
        call end_test()
    end subroutine test_marker_color_integration()

    subroutine test_line_color_integration()
        ! Given: Different line style specifications
        ! When: Setting colors for different line styles
        ! Then: Should apply colors correctly to all line types
        
        type(figure_t) :: fig
        real(wp) :: x(3), y(3)
        integer :: i
        
        call start_test("Line color integration")
        
        call fig%initialize(640, 480)
        
        ! Create test data
        do i = 1, 3
            x(i) = real(i, wp)
            y(i) = real(i**2, wp)
        end do
        
        ! Test solid line color
        call fig%add_plot(x, y, color='green', linestyle='-', label='Solid Green')
        call assert_plot_has_line_style(fig, 1, '-', "Solid line style")
        call assert_plot_has_line_color(fig, 1, [0.0_wp, 0.5_wp, 0.0_wp], "Solid line color")
        
        ! Test dashed line color
        call fig%add_plot(x, y*2, color='(1.0, 0.0, 1.0)', linestyle='--', label='Dashed Magenta')
        call assert_plot_has_line_style(fig, 2, '--', "Dashed line style")
        call assert_plot_has_line_color(fig, 2, [1.0_wp, 0.0_wp, 1.0_wp], "Dashed line color")
        
        ! Test dotted line color
        call fig%add_plot(x, y*3, color='c', linestyle=':', label='Dotted Cyan')
        call assert_plot_has_line_style(fig, 3, ':', "Dotted line style")
        call assert_plot_has_line_color(fig, 3, [0.0_wp, 1.0_wp, 1.0_wp], "Dotted line color")
        
        call end_test()
    end subroutine test_line_color_integration()

    subroutine test_backward_compatibility()
        ! Given: Existing RGB array color specifications
        ! When: Using both old and new color syntax
        ! Then: Should maintain backward compatibility
        
        type(figure_t) :: fig
        real(wp) :: x(3), y(3)
        real(wp) :: rgb_color(3)
        integer :: i
        
        call start_test("Backward compatibility")
        
        call fig%initialize(640, 480)
        
        ! Create test data
        do i = 1, 3
            x(i) = real(i, wp)
            y(i) = real(i, wp)
        end do
        
        ! Test old RGB array method still works
        rgb_color = [0.5_wp, 0.0_wp, 0.5_wp]
        call fig%add_plot(x, y, color_rgb=rgb_color, label='Old RGB Array')
        call assert_equal(real(fig%plot_count, wp), 1.0_wp, "Old RGB array method works")
        call assert_plot_has_color(fig, 1, rgb_color, "Old RGB array color")
        
        ! Test new string method works alongside old
        call fig%add_plot(x, y*2, color='purple', label='New String Method')
        call assert_equal(real(fig%plot_count, wp), 2.0_wp, "New string method works")
        call assert_plot_has_color(fig, 2, [0.5_wp, 0.0_wp, 0.5_wp], "New string method color")
        
        ! Test preference when both specified (new should take precedence)
        call fig%add_plot(x, y*3, color_rgb=rgb_color, color='red', label='Both Specified')
        call assert_equal(real(fig%plot_count, wp), 3.0_wp, "Both methods specified")
        call assert_plot_has_color(fig, 3, [1.0_wp, 0.0_wp, 0.0_wp], "String color takes precedence")
        
        call end_test()
    end subroutine test_backward_compatibility()

    ! API integration test functions that must fail until implementation
    subroutine add_plot_with_color(fig, x, y, color, label)
        type(figure_t), intent(inout) :: fig
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in) :: color, label
        
        ! This will fail until color parameter support is implemented
        error stop "add_plot with color parameter not implemented - RED phase test"
    end subroutine add_plot_with_color

    subroutine add_scatter_with_color(fig, x, y, color, label)
        type(figure_t), intent(inout) :: fig
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in) :: color, label
        
        ! This will fail until scatter color support is implemented
        error stop "add_scatter with color parameter not implemented - RED phase test"
    end subroutine add_scatter_with_color

    subroutine global_plot_with_color(x, y, color, label)
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in) :: color, label
        
        ! This will fail until global API color support is implemented
        error stop "global plot with color not implemented - RED phase test"
    end subroutine global_plot_with_color

    ! Assertion functions that must fail until implementation
    subroutine assert_plot_has_color(fig, plot_idx, expected_rgb, message)
        type(figure_t), intent(in) :: fig
        integer, intent(in) :: plot_idx
        real(wp), intent(in) :: expected_rgb(3)
        character(len=*), intent(in) :: message
        
        ! This will fail until plot color storage is implemented
        error stop "Plot color assertion not implemented - RED phase test"
    end subroutine assert_plot_has_color

    subroutine assert_scatter_has_uniform_color(fig, plot_idx, expected_rgb, message)
        type(figure_t), intent(in) :: fig
        integer, intent(in) :: plot_idx
        real(wp), intent(in) :: expected_rgb(3)
        character(len=*), intent(in) :: message
        
        ! This will fail until scatter color storage is implemented
        error stop "Scatter color assertion not implemented - RED phase test"
    end subroutine assert_scatter_has_uniform_color

    subroutine assert_scatter_has_color_mapping(fig, plot_idx, message)
        type(figure_t), intent(in) :: fig
        integer, intent(in) :: plot_idx
        character(len=*), intent(in) :: message
        
        ! This will fail until scatter color mapping is implemented
        error stop "Scatter color mapping assertion not implemented - RED phase test"
    end subroutine assert_scatter_has_color_mapping

    subroutine assert_bar_has_color(fig, plot_idx, expected_rgb, message)
        type(figure_t), intent(in) :: fig
        integer, intent(in) :: plot_idx
        real(wp), intent(in) :: expected_rgb(3)
        character(len=*), intent(in) :: message
        
        ! This will fail until bar color storage is implemented
        error stop "Bar color assertion not implemented - RED phase test"
    end subroutine assert_bar_has_color

    subroutine assert_histogram_has_color(fig, plot_idx, expected_rgb, message)
        type(figure_t), intent(in) :: fig
        integer, intent(in) :: plot_idx
        real(wp), intent(in) :: expected_rgb(3)
        character(len=*), intent(in) :: message
        
        ! This will fail until histogram color storage is implemented
        error stop "Histogram color assertion not implemented - RED phase test"
    end subroutine assert_histogram_has_color

    subroutine assert_errorbar_has_color(fig, plot_idx, expected_rgb, message)
        type(figure_t), intent(in) :: fig
        integer, intent(in) :: plot_idx
        real(wp), intent(in) :: expected_rgb(3)
        character(len=*), intent(in) :: message
        
        ! This will fail until errorbar color storage is implemented
        error stop "Errorbar color assertion not implemented - RED phase test"
    end subroutine assert_errorbar_has_color

    subroutine assert_contour_has_colormap(fig, plot_idx, expected_colormap, message)
        type(figure_t), intent(in) :: fig
        integer, intent(in) :: plot_idx
        character(len=*), intent(in) :: expected_colormap, message
        
        ! This will fail until contour colormap validation is implemented
        error stop "Contour colormap assertion not implemented - RED phase test"
    end subroutine assert_contour_has_colormap

    subroutine assert_pcolormesh_has_colormap(fig, plot_idx, expected_colormap, message)
        type(figure_t), intent(in) :: fig
        integer, intent(in) :: plot_idx
        character(len=*), intent(in) :: expected_colormap, message
        
        ! This will fail until pcolormesh colormap validation is implemented
        error stop "Pcolormesh colormap assertion not implemented - RED phase test"
    end subroutine assert_pcolormesh_has_colormap

    subroutine assert_global_figure_has_plots(expected_count, message)
        integer, intent(in) :: expected_count
        character(len=*), intent(in) :: message
        
        ! This will fail until global figure tracking is implemented
        error stop "Global figure assertion not implemented - RED phase test"
    end subroutine assert_global_figure_has_plots

    subroutine assert_plot_has_line_color(fig, plot_idx, expected_rgb, message)
        type(figure_t), intent(in) :: fig
        integer, intent(in) :: plot_idx
        real(wp), intent(in) :: expected_rgb(3)
        character(len=*), intent(in) :: message
        
        ! This will fail until line color separation is implemented
        error stop "Line color assertion not implemented - RED phase test"
    end subroutine assert_plot_has_line_color

    subroutine assert_plot_has_marker_color(fig, plot_idx, expected_rgb, message)
        type(figure_t), intent(in) :: fig
        integer, intent(in) :: plot_idx
        real(wp), intent(in) :: expected_rgb(3)
        character(len=*), intent(in) :: message
        
        ! This will fail until marker color separation is implemented
        error stop "Marker color assertion not implemented - RED phase test"
    end subroutine assert_plot_has_marker_color

    subroutine assert_plot_has_line_style(fig, plot_idx, expected_style, message)
        type(figure_t), intent(in) :: fig
        integer, intent(in) :: plot_idx
        character(len=*), intent(in) :: expected_style, message
        
        ! This will fail until line style validation is implemented
        error stop "Line style assertion not implemented - RED phase test"
    end subroutine assert_plot_has_line_style

    ! Testing utilities
    subroutine start_test(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A,A)') "Running test: ", trim(test_name)
    end subroutine start_test

    subroutine end_test()
        pass_count = pass_count + 1
        write(*, '(A)') "  PASS"
    end subroutine end_test

    subroutine assert_true(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        if (.not. condition) then
            write(*, '(A,A)') "  FAIL: ", trim(message)
            error stop "Test assertion failed"
        end if
    end subroutine assert_true

    subroutine assert_equal(actual, expected, message)
        real(wp), intent(in) :: actual, expected
        character(len=*), intent(in) :: message
        real(wp), parameter :: tolerance = 1e-10_wp
        if (abs(actual - expected) > tolerance) then
            write(*, '(A,A,F10.6,A,F10.6)') "  FAIL: ", trim(message), &
                  actual, " /= ", expected
            error stop "Test assertion failed"
        end if
    end subroutine assert_equal

    subroutine print_test_summary()
        write(*, '(A,I0,A,I0,A)') "Tests completed: ", pass_count, "/", test_count, " passed"
        if (pass_count /= test_count) error stop "Some tests failed"
    end subroutine print_test_summary

end program test_color_api_integration
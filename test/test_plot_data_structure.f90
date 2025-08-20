program test_plot_data_structure
    !! Test suite for plot_data_t type functionality
    !! 
    !! GIVEN: plot_data_t type contains multiple plot type support
    !! WHEN: Module is refactored into focused modules
    !! THEN: plot_data_t functionality must be preserved
    !!
    !! Tests the data container structure that supports all plot types
    !! and ensures proper memory management and type safety.
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_core, only: plot_data_t, PLOT_TYPE_LINE, PLOT_TYPE_CONTOUR, &
                                   PLOT_TYPE_PCOLORMESH, PLOT_TYPE_SCATTER, &
                                   PLOT_TYPE_BAR, PLOT_TYPE_HISTOGRAM, PLOT_TYPE_BOXPLOT
    implicit none
    
    integer :: test_count = 0
    integer :: pass_count = 0
    
    ! Test plot_data_t structure and functionality
    call test_plot_data_initialization()
    call test_plot_data_line_storage()
    call test_plot_data_3d_detection()
    call test_plot_data_contour_storage()
    call test_plot_data_scatter_properties()
    call test_plot_data_bar_properties()
    call test_plot_data_histogram_properties()
    call test_plot_data_boxplot_properties()
    call test_plot_data_color_management()
    call test_plot_data_memory_efficiency()
    
    call print_test_summary()
    
contains

    subroutine test_plot_data_initialization()
        type(plot_data_t) :: plot_data
        
        call start_test("Plot data initialization")
        
        ! GIVEN: A new plot_data_t instance
        ! WHEN: Default initialization is checked
        ! THEN: Default values should be correct
        call assert_equal(real(plot_data%plot_type, wp), real(PLOT_TYPE_LINE, wp), "Default plot type")
        call assert_false(plot_data%use_color_levels, "Default color levels")
        call assert_true(plot_data%show_colorbar, "Default colorbar")
        call assert_equal(plot_data%bar_width, 0.8_wp, "Default bar width")
        call assert_false(plot_data%bar_horizontal, "Default bar orientation")
        call assert_false(plot_data%hist_density, "Default histogram density")
        call assert_equal(plot_data%position, 1.0_wp, "Default position")
        call assert_equal(plot_data%width, 0.6_wp, "Default width")
        call assert_true(plot_data%show_outliers, "Default show outliers")
        call assert_false(plot_data%horizontal, "Default horizontal")
        
        call end_test()
    end subroutine test_plot_data_initialization

    subroutine test_plot_data_line_storage()
        type(plot_data_t) :: plot_data
        real(wp) :: x_test(3), y_test(3)
        
        call start_test("Plot data line storage")
        
        ! Create test data
        x_test = [1.0_wp, 2.0_wp, 3.0_wp]
        y_test = [2.0_wp, 4.0_wp, 6.0_wp]
        
        ! GIVEN: Line plot data
        ! WHEN: Data is allocated and stored
        allocate(plot_data%x, source=x_test)
        allocate(plot_data%y, source=y_test)
        plot_data%plot_type = PLOT_TYPE_LINE
        
        ! THEN: Data should be properly stored
        call assert_true(allocated(plot_data%x), "X data allocated")
        call assert_true(allocated(plot_data%y), "Y data allocated")
        call assert_equal(real(size(plot_data%x), wp), 3.0_wp, "X data size")
        call assert_equal(plot_data%x(2), 2.0_wp, "X data value")
        call assert_equal(plot_data%y(2), 4.0_wp, "Y data value")
        
        call end_test()
    end subroutine test_plot_data_line_storage

    subroutine test_plot_data_3d_detection()
        type(plot_data_t) :: plot_2d, plot_3d
        real(wp) :: test_data(3)
        
        call start_test("Plot data 3D detection")
        
        test_data = [1.0_wp, 2.0_wp, 3.0_wp]
        
        ! Setup 2D plot
        allocate(plot_2d%x, source=test_data)
        allocate(plot_2d%y, source=test_data)
        
        ! Setup 3D plot
        allocate(plot_3d%x, source=test_data)
        allocate(plot_3d%y, source=test_data)
        allocate(plot_3d%z, source=test_data)
        
        ! GIVEN: 2D and 3D plot data
        ! WHEN: 3D detection is checked
        ! THEN: Should correctly identify 3D plots
        call assert_false(plot_2d%is_3d(), "2D plot detection")
        call assert_true(plot_3d%is_3d(), "3D plot detection")
        
        call end_test()
    end subroutine test_plot_data_3d_detection

    subroutine test_plot_data_contour_storage()
        type(plot_data_t) :: plot_data
        real(wp) :: x_grid(2), y_grid(2), z_grid(2,2), levels(3)
        
        call start_test("Plot data contour storage")
        
        ! Create grid data
        x_grid = [0.0_wp, 1.0_wp]
        y_grid = [0.0_wp, 1.0_wp]
        z_grid = reshape([1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp], [2,2])
        levels = [1.5_wp, 2.5_wp, 3.5_wp]
        
        ! GIVEN: Contour plot data
        ! WHEN: Grid data is stored
        allocate(plot_data%x_grid, source=x_grid)
        allocate(plot_data%y_grid, source=y_grid)
        allocate(plot_data%z_grid, source=z_grid)
        allocate(plot_data%contour_levels, source=levels)
        plot_data%plot_type = PLOT_TYPE_CONTOUR
        
        ! THEN: Grid data should be properly stored
        call assert_true(allocated(plot_data%x_grid), "X grid allocated")
        call assert_true(allocated(plot_data%y_grid), "Y grid allocated")
        call assert_true(allocated(plot_data%z_grid), "Z grid allocated")
        call assert_true(allocated(plot_data%contour_levels), "Contour levels allocated")
        call assert_equal(plot_data%z_grid(1,2), 3.0_wp, "Z grid value")
        call assert_equal(plot_data%contour_levels(2), 2.5_wp, "Contour level value")
        
        call end_test()
    end subroutine test_plot_data_contour_storage

    subroutine test_plot_data_scatter_properties()
        type(plot_data_t) :: plot_data
        real(wp) :: sizes(3), colors(3)
        
        call start_test("Plot data scatter properties")
        
        sizes = [10.0_wp, 20.0_wp, 30.0_wp]
        colors = [0.1_wp, 0.5_wp, 0.9_wp]
        
        ! GIVEN: Scatter plot with variable properties
        ! WHEN: Variable sizes and colors are set
        allocate(plot_data%scatter_sizes, source=sizes)
        allocate(plot_data%scatter_colors, source=colors)
        plot_data%plot_type = PLOT_TYPE_SCATTER
        plot_data%scatter_size_default = 25.0_wp
        plot_data%scatter_colormap = "plasma"
        plot_data%scatter_colorbar = .true.
        plot_data%scatter_vmin = 0.0_wp
        plot_data%scatter_vmax = 1.0_wp
        plot_data%scatter_vrange_set = .true.
        
        ! THEN: Scatter properties should be stored correctly
        call assert_true(allocated(plot_data%scatter_sizes), "Scatter sizes allocated")
        call assert_true(allocated(plot_data%scatter_colors), "Scatter colors allocated")
        call assert_equal(plot_data%scatter_size_default, 25.0_wp, "Default scatter size")
        call assert_true(plot_data%scatter_colorbar, "Scatter colorbar enabled")
        call assert_equal(plot_data%scatter_vmin, 0.0_wp, "Scatter vmin")
        call assert_equal(plot_data%scatter_vmax, 1.0_wp, "Scatter vmax")
        call assert_true(plot_data%scatter_vrange_set, "Scatter vrange set")
        
        call end_test()
    end subroutine test_plot_data_scatter_properties

    subroutine test_plot_data_bar_properties()
        type(plot_data_t) :: plot_data
        real(wp) :: bar_x(3), bar_heights(3)
        
        call start_test("Plot data bar properties")
        
        bar_x = [1.0_wp, 2.0_wp, 3.0_wp]
        bar_heights = [5.0_wp, 8.0_wp, 3.0_wp]
        
        ! GIVEN: Bar chart data
        ! WHEN: Bar properties are set
        allocate(plot_data%bar_x, source=bar_x)
        allocate(plot_data%bar_heights, source=bar_heights)
        plot_data%plot_type = PLOT_TYPE_BAR
        plot_data%bar_width = 0.6_wp
        plot_data%bar_horizontal = .true.
        
        ! THEN: Bar properties should be stored correctly
        call assert_true(allocated(plot_data%bar_x), "Bar x allocated")
        call assert_true(allocated(plot_data%bar_heights), "Bar heights allocated")
        call assert_equal(plot_data%bar_width, 0.6_wp, "Bar width")
        call assert_true(plot_data%bar_horizontal, "Bar horizontal orientation")
        call assert_equal(plot_data%bar_heights(2), 8.0_wp, "Bar height value")
        
        call end_test()
    end subroutine test_plot_data_bar_properties

    subroutine test_plot_data_histogram_properties()
        type(plot_data_t) :: plot_data
        real(wp) :: bin_edges(4), counts(3)
        
        call start_test("Plot data histogram properties")
        
        bin_edges = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
        counts = [5.0_wp, 10.0_wp, 3.0_wp]
        
        ! GIVEN: Histogram data
        ! WHEN: Histogram properties are set
        allocate(plot_data%hist_bin_edges, source=bin_edges)
        allocate(plot_data%hist_counts, source=counts)
        plot_data%plot_type = PLOT_TYPE_HISTOGRAM
        plot_data%hist_density = .true.
        
        ! THEN: Histogram properties should be stored correctly
        call assert_true(allocated(plot_data%hist_bin_edges), "Histogram bin edges allocated")
        call assert_true(allocated(plot_data%hist_counts), "Histogram counts allocated")
        call assert_true(plot_data%hist_density, "Histogram density mode")
        call assert_equal(plot_data%hist_bin_edges(3), 2.0_wp, "Bin edge value")
        call assert_equal(plot_data%hist_counts(2), 10.0_wp, "Count value")
        
        call end_test()
    end subroutine test_plot_data_histogram_properties

    subroutine test_plot_data_boxplot_properties()
        type(plot_data_t) :: plot_data
        real(wp) :: box_data(5), outliers(2)
        
        call start_test("Plot data boxplot properties")
        
        box_data = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        outliers = [0.1_wp, 5.9_wp]
        
        ! GIVEN: Boxplot data
        ! WHEN: Boxplot properties are set
        allocate(plot_data%box_data, source=box_data)
        allocate(plot_data%outliers, source=outliers)
        plot_data%plot_type = PLOT_TYPE_BOXPLOT
        plot_data%position = 2.0_wp
        plot_data%width = 0.8_wp
        plot_data%show_outliers = .false.
        plot_data%horizontal = .true.
        plot_data%q1 = 2.0_wp
        plot_data%q2 = 3.0_wp
        plot_data%q3 = 4.0_wp
        plot_data%whisker_low = 1.0_wp
        plot_data%whisker_high = 5.0_wp
        
        ! THEN: Boxplot properties should be stored correctly
        call assert_true(allocated(plot_data%box_data), "Box data allocated")
        call assert_true(allocated(plot_data%outliers), "Outliers allocated")
        call assert_equal(plot_data%position, 2.0_wp, "Boxplot position")
        call assert_equal(plot_data%width, 0.8_wp, "Boxplot width")
        call assert_false(plot_data%show_outliers, "Show outliers disabled")
        call assert_true(plot_data%horizontal, "Horizontal orientation")
        call assert_equal(plot_data%q2, 3.0_wp, "Median value")
        call assert_equal(plot_data%whisker_low, 1.0_wp, "Lower whisker")
        
        call end_test()
    end subroutine test_plot_data_boxplot_properties

    subroutine test_plot_data_color_management()
        type(plot_data_t) :: plot_data
        
        call start_test("Plot data color management")
        
        ! GIVEN: Plot data with color properties
        ! WHEN: Color properties are set
        plot_data%color = [0.5_wp, 0.7_wp, 0.9_wp]
        allocate(character(len=10) :: plot_data%label)
        plot_data%label = "test_plot"
        allocate(character(len=5) :: plot_data%linestyle)
        plot_data%linestyle = "solid"
        allocate(character(len=6) :: plot_data%marker)
        plot_data%marker = "circle"
        
        ! THEN: Color and style properties should be stored
        call assert_equal(plot_data%color(1), 0.5_wp, "Red color component")
        call assert_equal(plot_data%color(2), 0.7_wp, "Green color component")
        call assert_equal(plot_data%color(3), 0.9_wp, "Blue color component")
        call assert_true(allocated(plot_data%label), "Label allocated")
        call assert_true(allocated(plot_data%linestyle), "Linestyle allocated")
        call assert_true(allocated(plot_data%marker), "Marker allocated")
        
        call end_test()
    end subroutine test_plot_data_color_management

    subroutine test_plot_data_memory_efficiency()
        type(plot_data_t) :: plot_data_small, plot_data_large
        real(wp) :: small_data(3), large_data(1000)
        integer :: i
        
        call start_test("Plot data memory efficiency")
        
        ! Create test data of different sizes
        small_data = [1.0_wp, 2.0_wp, 3.0_wp]
        do i = 1, 1000
            large_data(i) = sin(real(i, wp) * 0.01_wp)
        end do
        
        ! GIVEN: Different sized datasets
        ! WHEN: Data is stored in plot_data_t
        allocate(plot_data_small%x, source=small_data)
        allocate(plot_data_small%y, source=small_data)
        allocate(plot_data_large%x, source=large_data)
        allocate(plot_data_large%y, source=large_data)
        
        ! THEN: Memory should be allocated efficiently
        call assert_equal(real(size(plot_data_small%x), wp), 3.0_wp, "Small data size")
        call assert_equal(real(size(plot_data_large%x), wp), 1000.0_wp, "Large data size")
        call assert_true(allocated(plot_data_small%x), "Small data allocated")
        call assert_true(allocated(plot_data_large%x), "Large data allocated")
        
        call end_test()
    end subroutine test_plot_data_memory_efficiency

    ! Test helper subroutines
    subroutine start_test(test_name)
        character(len=*), intent(in) :: test_name
        write(*, '(A, A)') 'Running test: ', test_name
    end subroutine start_test

    subroutine end_test()
        write(*, '(A)') 'Test completed'
        write(*, *)
    end subroutine end_test

    subroutine assert_equal(actual, expected, description)
        real(wp), intent(in) :: actual, expected
        character(len=*), intent(in) :: description
        real(wp), parameter :: tolerance = 1.0e-10_wp
        
        test_count = test_count + 1
        if (abs(actual - expected) < tolerance) then
            write(*, '(A, A)') '  PASS: ', description
            pass_count = pass_count + 1
        else
            write(*, '(A, A, F12.6, A, F12.6)') '  FAIL: ', description, actual, ' != ', expected
        end if
    end subroutine assert_equal

    subroutine assert_true(condition, description)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: description
        
        test_count = test_count + 1
        if (condition) then
            write(*, '(A, A)') '  PASS: ', description
            pass_count = pass_count + 1
        else
            write(*, '(A, A)') '  FAIL: ', description
        end if
    end subroutine assert_true

    subroutine assert_false(condition, description)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: description
        
        test_count = test_count + 1
        if (.not. condition) then
            write(*, '(A, A)') '  PASS: ', description
            pass_count = pass_count + 1
        else
            write(*, '(A, A)') '  FAIL: ', description
        end if
    end subroutine assert_false

    subroutine print_test_summary()
        write(*, '(A)') '============================================'
        write(*, '(A, I0, A, I0, A)') 'Test Summary: ', pass_count, ' of ', test_count, ' tests passed'
        if (pass_count == test_count) then
            write(*, '(A)') 'All tests PASSED!'
        else
            write(*, '(A)') 'Some tests FAILED!'
        end if
    end subroutine print_test_summary

end program test_plot_data_structure
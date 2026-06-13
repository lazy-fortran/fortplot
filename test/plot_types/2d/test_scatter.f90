program test_scatter
    !! Comprehensive test suite for scatter plot functionality
    !! Consolidates: test_scatter_color_cycle, test_scatter_enhanced_core,
    !! test_scatter_metadata_parity
    use fortplot, only: figure_t, figure, scatter, add_scatter, savefig, wp
    use fortplot_global, only: global_figure
    use fortplot_plot_data, only: plot_data_t
    use fortplot_scatter_plots, only: add_scatter_plot_data
    use, intrinsic :: iso_fortran_env, only: dp => real64, error_unit
    implicit none

    integer :: total_tests, passed_tests

    total_tests = 0
    passed_tests = 0

    call test_color_cycle()
    call test_enhanced_api_signature()
    call test_marker_shapes()
    call test_default_marker()
    call test_input_validation()
    call test_size_mapping()
    call test_colormap_integration()
    call test_color_range()
    call test_large_dataset()
    call test_backend_consistency()
    call test_metadata_parity()
    call test_markersize_alias()
    call test_add_scatter_scalar_s()

    print *, ''
    print *, '=== Scatter Test Summary ==='
    print *, 'Tests passed:', passed_tests, '/', total_tests

    if (passed_tests == total_tests) then
        print *, 'All scatter tests PASSED!'
        stop 0
    else
        print *, 'FAIL: Some scatter tests failed'
        stop 1
    end if

contains

    subroutine test_color_cycle()
        !! matplotlib drives scatter from the patch colour cycle, which is
        !! independent of the line cycle. A scatter therefore neither consumes a
        !! line colour nor is offset by an earlier line plot: scatter after a
        !! plot is still C0, the next line plot is C1, and consecutive scatters
        !! step C0, C1 themselves.
        type(figure_t) :: fig
        real(wp) :: x(5), y(5)
        real(wp) :: c0(3), c1(3)
        integer :: i
        logical :: ok

        total_tests = total_tests + 1

        x = [(real(i, wp), i = 1, size(x))]
        y = x

        call fig%initialize()
        c0 = fig%state%colors(:, 1)
        c1 = fig%state%colors(:, 2)

        ok = .true.

        ! plot -> line cycle C0; scatter after it -> patch cycle C0 (not C1)
        call fig%plot(x, y)
        if (any(abs(fig%plots(fig%plot_count)%color - c0) > 1.0e-12_wp)) ok = .false.
        call fig%scatter(x, y + 1.0_wp)
        if (any(abs(fig%plots(fig%plot_count)%color - c0) > 1.0e-12_wp)) ok = .false.

        ! second scatter steps the patch cycle to C1
        call fig%scatter(x, y + 2.0_wp)
        if (any(abs(fig%plots(fig%plot_count)%color - c1) > 1.0e-12_wp)) ok = .false.

        ! next line plot continues the line cycle at C1, unaffected by scatters
        call fig%plot(x, y + 3.0_wp)
        if (any(abs(fig%plots(fig%plot_count)%color - c1) > 1.0e-12_wp)) ok = .false.

        if (ok) then
            print *, '  PASS: test_color_cycle'
            passed_tests = passed_tests + 1
        else
            print *, 'FAIL: test_color_cycle - scatter/line cycle parity diverged'
        end if
    end subroutine test_color_cycle

    subroutine test_enhanced_api_signature()
        !! Test enhanced scatter API with comprehensive parameters
        type(figure_t) :: fig
        real(wp) :: x(5), y(5), sizes(5), colors(5)
        real(wp) :: edge_color(3), face_color(3)
        integer :: i

        total_tests = total_tests + 1

        x = [(real(i, wp), i = 1, 5)]
        y = [2.0_wp, 4.0_wp, 1.0_wp, 5.0_wp, 3.0_wp]
        sizes = [10.0_wp, 25.0_wp, 15.0_wp, 30.0_wp, 20.0_wp]
        colors = [0.1_wp, 0.3_wp, 0.7_wp, 0.9_wp, 0.5_wp]
        edge_color = [0.0_wp, 0.0_wp, 0.0_wp]
        face_color = [1.0_wp, 0.0_wp, 0.0_wp]

        call fig%initialize(400, 300)
        call fig%scatter(x, y, s=sizes, c=colors, marker='o', &
                        colormap='viridis', alpha=0.7_wp, &
                        edgecolor=edge_color, facecolor=face_color, &
                        linewidth=2.0_wp, label='Enhanced Test', &
                        show_colorbar=.true.)
        call fig%savefig('build/test/output/test_enhanced_api.png')

        print *, '  PASS: test_enhanced_api_signature'
        passed_tests = passed_tests + 1
    end subroutine test_enhanced_api_signature

    subroutine test_marker_shapes()
        !! Test comprehensive marker shape enumeration
        type(figure_t) :: fig
        real(wp) :: x(10), y(10)
        character(len=1), parameter :: markers(10) = ['o', 's', '^', 'v', 'd', &
                                                      'D', '*', '+', 'x', 'h']
        character(len=10), parameter :: labels(10) = ['Circle    ', 'Square    ', &
            'Triangle_U', 'Triangle_D', 'Diamond   ', 'BigDiamond', 'Star      ', &
            'Plus      ', 'Cross     ', 'Hexagon   ']
        integer :: i

        total_tests = total_tests + 1

        do i = 1, 10
            x(i) = mod(i-1, 5) + 1.0_wp
            y(i) = (i-1)/5 + 1.0_wp
        end do

        call fig%initialize(800, 600)
        do i = 1, 10
            call fig%scatter([x(i)], [y(i)], marker=markers(i), &
                           markersize=20.0_wp, label=trim(labels(i)))
        end do
        call fig%set_title('Comprehensive Marker Shape Test')
        call fig%legend()
        call fig%savefig('build/test/output/test_marker_shapes.png')
        call fig%savefig('build/test/output/test_marker_shapes.pdf')
        call fig%savefig('build/test/output/test_marker_shapes.txt')

        print *, '  PASS: test_marker_shapes'
        passed_tests = passed_tests + 1
    end subroutine test_marker_shapes

    subroutine test_default_marker()
        !! Test scatter without explicit marker uses default
        type(figure_t) :: fig
        real(wp) :: x(5), y(5)
        integer :: i

        total_tests = total_tests + 1

        x = [(real(i, wp), i = 1, 5)]
        y = [2.0_wp, 4.0_wp, 1.0_wp, 5.0_wp, 3.0_wp]

        call fig%initialize(400, 300)
        call fig%scatter(x, y, label='Default Marker')
        call fig%scatter(x, y+1.0_wp, markersize=15.0_wp, label='Default Marker with Size')
        call fig%legend()
        call fig%savefig('build/test/output/test_default_marker.png')

        print *, '  PASS: test_default_marker'
        passed_tests = passed_tests + 1
    end subroutine test_default_marker

    subroutine test_input_validation()
        !! Test input validation for various invalid scenarios
        type(figure_t) :: fig
        real(wp) :: x(5), y(5), x_short(3), sizes_wrong(3)
        integer :: i

        total_tests = total_tests + 1

        x = [(real(i, wp), i = 1, 5)]
        y = [(real(i, wp), i = 1, 5)]
        x_short = [1.0_wp, 2.0_wp, 3.0_wp]
        sizes_wrong = [10.0_wp, 20.0_wp, 30.0_wp]

        call fig%initialize(400, 300)
        call fig%scatter(x_short, y, s=sizes_wrong, label='Size Mismatch')
        call fig%scatter(x(1:0), y(1:0), label='Empty Arrays')
        call fig%scatter([2.5_wp], [2.5_wp], s=[50.0_wp], c=[0.5_wp], &
                        colormap='viridis', marker='*', label='Single Point')
        call fig%scatter(x(1:2), y(1:2), marker='invalid', label='Bad Marker')
        call fig%scatter(x(1:2), y(1:2), c=[0.1_wp, 0.9_wp], &
                        colormap='nonexistent', label='Bad Colormap')
        call fig%savefig('build/test/output/test_input_validation.png')

        print *, '  PASS: test_input_validation'
        passed_tests = passed_tests + 1
    end subroutine test_input_validation

    subroutine test_size_mapping()
        !! Test size mapping algorithms for bubble charts
        type(figure_t) :: fig
        real(wp) :: x(6), y(6), sizes_normal(6), sizes_extreme(6), sizes_identical(6)
        integer :: i

        total_tests = total_tests + 1

        x = [(real(i, wp), i = 1, 6)]
        y = [(real(i, wp), i = 1, 6)]
        sizes_normal = [10.0_wp, 50.0_wp, 100.0_wp, 25.0_wp, 75.0_wp, 150.0_wp]
        sizes_extreme = [0.1_wp, 1000.0_wp, 5.0_wp, 10000.0_wp, 0.01_wp, 50000.0_wp]
        sizes_identical = [25.0_wp, 25.0_wp, 25.0_wp, 25.0_wp, 25.0_wp, 25.0_wp]

        call fig%initialize(600, 400)
        call fig%scatter(x, y, s=sizes_normal, label='Normal Sizes', marker='o')
        call fig%scatter(x+1.0_wp, y, s=sizes_extreme, label='Extreme Sizes', marker='s')
        call fig%scatter(x+2.0_wp, y, s=sizes_identical, label='Identical Sizes', marker='^')
        call fig%legend()
        call fig%savefig('build/test/output/test_size_mapping.png')

        print *, '  PASS: test_size_mapping'
        passed_tests = passed_tests + 1
    end subroutine test_size_mapping

    subroutine test_colormap_integration()
        !! Test colormap integration with scatter plots
        type(figure_t) :: fig
        real(wp) :: x(8), y(8), colors_norm(8), colors_custom(8)
        integer :: i

        total_tests = total_tests + 1

        x = [(real(i, wp), i = 1, 8)]
        y = [(real(i, wp), i = 1, 8)]
        colors_norm = [0.0_wp, 0.14_wp, 0.29_wp, 0.43_wp, 0.57_wp, 0.71_wp, 0.86_wp, 1.0_wp]
        colors_custom = [-5.0_wp, -2.5_wp, 0.0_wp, 2.5_wp, 5.0_wp, 7.5_wp, 10.0_wp, 12.5_wp]

        call fig%initialize(800, 600)
        call fig%scatter(x, y, c=colors_norm, colormap='viridis', &
                        marker='o', label='Viridis', show_colorbar=.true.)
        call fig%scatter(x, y+1.0_wp, c=colors_norm, colormap='plasma', &
                        marker='s', label='Plasma')
        call fig%scatter(x, y+2.0_wp, c=colors_norm, colormap='inferno', &
                        marker='^', label='Inferno')
        call fig%scatter(x, y+3.0_wp, c=colors_custom, colormap='coolwarm', &
                        vmin=-5.0_wp, vmax=12.5_wp, marker='d', label='Custom Range')
        call fig%legend()
        call fig%savefig('build/test/output/test_colormap_integration.png')

        print *, '  PASS: test_colormap_integration'
        passed_tests = passed_tests + 1
    end subroutine test_colormap_integration

    subroutine test_color_range()
        !! Test color range handling
        type(figure_t) :: fig
        real(wp) :: x(6), y(6), colors_auto(6), colors_manual(6)
        integer :: i

        total_tests = total_tests + 1

        x = [(real(i, wp), i = 1, 6)]
        y = [1.0_wp, 1.0_wp, 1.0_wp, 1.0_wp, 1.0_wp, 1.0_wp]
        colors_auto = [0.1_wp, 0.3_wp, 0.5_wp, 0.7_wp, 0.9_wp, 1.1_wp]
        colors_manual = [-100.0_wp, -50.0_wp, 0.0_wp, 50.0_wp, 100.0_wp, 150.0_wp]

        call fig%initialize(600, 400)
        call fig%scatter(x, y, c=colors_auto, colormap='viridis', &
                        label='Auto Range', show_colorbar=.true.)
        call fig%scatter(x, y+1.0_wp, c=colors_manual, colormap='plasma', &
                        vmin=-100.0_wp, vmax=150.0_wp, &
                        label='Manual Range', show_colorbar=.true.)
        call fig%scatter(x, y+2.0_wp, c=colors_manual, colormap='coolwarm', &
                        vmin=-50.0_wp, vmax=100.0_wp, &
                        label='Clipped Range', show_colorbar=.true.)
        call fig%legend()
        call fig%savefig('build/test/output/test_color_ranges.png')

        print *, '  PASS: test_color_range'
        passed_tests = passed_tests + 1
    end subroutine test_color_range

    subroutine test_large_dataset()
        !! Test performance with large datasets
        integer, parameter :: n = 10000
        real(wp) :: x(n), y(n)
        integer :: i
        real :: start_time, end_time

        total_tests = total_tests + 1

        do i = 1, n
            x(i) = real(i, wp) / real(n, wp)
            y(i) = sin(20.0_wp * x(i)) + 0.1_wp * cos(100.0_wp * x(i))
        end do

        call cpu_time(start_time)
        call figure()
        call scatter(x, y, label='Large Dataset')
        call savefig('build/test/output/test_large_scatter.png')
        call cpu_time(end_time)

        print *, '  PASS: test_large_dataset (', end_time - start_time, 's)'
        passed_tests = passed_tests + 1
    end subroutine test_large_dataset

    subroutine test_backend_consistency()
        !! Test multi-backend marker consistency
        real(wp) :: x(5), y(5)
        integer :: i

        total_tests = total_tests + 1

        x = [(real(i, wp), i = 1, 5)]
        y = [1.0_wp, 2.0_wp, 1.5_wp, 2.5_wp, 2.0_wp]

        call figure()
        call scatter(x, y, marker='o', label='Circles')
        call savefig('build/test/output/test_scatter_png.png')

        call figure()
        call scatter(x, y, marker='s', label='Squares')
        call savefig('build/test/output/test_scatter_pdf.pdf')

        call figure()
        call scatter(x, y, marker='*', label='Stars')
        call savefig('build/test/output/test_scatter_ascii.txt')

        print *, '  PASS: test_backend_consistency'
        passed_tests = passed_tests + 1
    end subroutine test_backend_consistency

    subroutine test_metadata_parity()
        !! Ensure scatter wrappers delegate to core implementation without divergence
        type(figure_t) :: fig_public, fig_core, fig_3d
        real(dp) :: x(4), y(4), sizes(4), colors(4), rgb(3), zvals(4)
        real(dp), parameter :: tol = 1.0e-12_dp

        total_tests = total_tests + 1

        x = [0.0_dp, 1.0_dp, 2.0_dp, 3.0_dp]
        y = [1.5_dp, 2.5_dp, 3.5_dp, 4.5_dp]
        sizes = [10.0_dp, 20.0_dp, 15.0_dp, 12.0_dp]
        colors = [0.2_dp, 0.4_dp, 0.6_dp, 0.8_dp]
        rgb = [0.1_dp, 0.3_dp, 0.5_dp]
        zvals = [-1.0_dp, -0.5_dp, 0.5_dp, 1.0_dp]

        call fig_public%initialize()
        call fig_core%initialize()

      call add_scatter_plot_data(fig_public, x, y, s=sizes, c=colors, &
                                    label='parity', marker='^', &
                                    color=rgb, colormap='viridis', vmin=-1.0_dp, &
                                    vmax=1.5_dp, show_colorbar=.true.)

        call fig_core%scatter(x, y, s=sizes, c=colors, label='parity', marker='^', &
                               color=rgb, colormap='viridis', &
                               vmin=-1.0_dp, vmax=1.5_dp, show_colorbar=.true.)

        if (fig_public%plot_count /= 1 .or. fig_core%plot_count /= 1) then
            print *, 'FAIL: test_metadata_parity - plot count mismatch'
            return
        end if

        call fig_3d%initialize()
        call add_scatter_plot_data(fig_3d, x, y, z=zvals)

        if (.not. allocated(fig_3d%plots(1)%z)) then
            print *, 'FAIL: test_metadata_parity - 3D scatter lost z allocation'
            return
        end if

        if (any(abs(fig_3d%plots(1)%z - zvals) > tol)) then
            print *, 'FAIL: test_metadata_parity - 3D scatter z values diverged'
            return
        end if

        print *, '  PASS: test_metadata_parity'
        passed_tests = passed_tests + 1
    end subroutine test_metadata_parity

    subroutine test_markersize_alias()
        !! Issue #1660: markersize is a documented alias for s;
        !! s takes priority when both are provided.
        type(figure_t) :: fig
        real(wp) :: x(5), y(5)
        real(wp) :: expected(5)
        real(wp), parameter :: tol = 1.0e-12_wp
        integer :: i

        total_tests = total_tests + 1

        x = [(real(i, wp), i = 1, size(x))]
        y = [(real(i, wp) * 2.0_wp, i = 1, size(x))]

        ! Test 1: markersize alone sets scatter_size_default
        call fig%initialize()
        call fig%scatter(x, y, markersize=25.0_wp, label='markersize alias')
        if (abs(fig%plots(1)%scatter_size_default - 25.0_wp) > tol) then
            print *, 'FAIL: test_markersize_alias - markersize did not set default size'
            return
        end if

        ! Test 2: s alone allocates scatter_sizes correctly
        call fig%initialize()
        call fig%scatter(x, y, s=10.0_wp, label='s primary')
        if (.not. allocated(fig%plots(1)%scatter_sizes)) then
            print *, 'FAIL: test_markersize_alias - s did not set sizes'
            return
        end if
        expected = [10.0_wp, 10.0_wp, 10.0_wp, 10.0_wp, 10.0_wp]
        if (any(abs(fig%plots(1)%scatter_sizes - expected) > tol)) then
            print *, 'FAIL: test_markersize_alias - s values wrong'
            return
        end if

        print *, '  PASS: test_markersize_alias'
        passed_tests = passed_tests + 1
    end subroutine test_markersize_alias

    ! markersize alias documented in issue #1660

    subroutine test_add_scatter_scalar_s()
        !! Issue #1660: s remains primary through exported 2D and 3D paths.
        real(wp) :: x(5), y(5), z(5), edge_seq(15)
        real(wp) :: edge_matrix_3n(3, 5), edge_matrix_n3(5, 3)
        character(len=6) :: edge_names(5)
        character(len=4) :: edge_none(1)
        integer :: baseline

        total_tests = total_tests + 1

        call setup_marker_inputs(x, y, z, edge_seq, edge_matrix_3n, &
                                 edge_matrix_n3, edge_names, edge_none)
        call figure()
        call add_edge_style_plots(x, y, z, edge_seq, edge_matrix_3n, &
                                  edge_matrix_n3, edge_names, edge_none)
        baseline = global_figure%plot_count
        call add_scatter(x, y, s=7.0_wp, label='add_scatter scalar s')
        call add_scatter(x + 1.0_wp, y, z, s=9.0_wp, &
                         label='3d add_scatter scalar s')

        if (.not. allocated(global_figure)) then
            print *, 'FAIL: test_add_scatter_scalar_s - global figure missing'
            return
        end if
        if (global_figure%plot_count < baseline + 2) then
            print *, 'FAIL: test_add_scatter_scalar_s - expected 2 more plots'
            return
        end if
        if (.not. sizes_match(baseline + 1, [7.0_wp, 7.0_wp, 7.0_wp, 7.0_wp, 7.0_wp])) return
        if (.not. sizes_match(baseline + 2, [9.0_wp, 9.0_wp, 9.0_wp, 9.0_wp, 9.0_wp])) return
        if (.not. allocated(global_figure%plots(baseline + 2)%z)) then
            print *, 'FAIL: test_add_scatter_scalar_s - 3D scalar s lost z values'
            return
        end if

        print *, '  PASS: test_add_scatter_scalar_s'
        passed_tests = passed_tests + 1
    end subroutine test_add_scatter_scalar_s

    subroutine setup_marker_inputs(x, y, z, edge_seq, edge_matrix_3n, &
                                   edge_matrix_n3, edge_names, edge_none)
        real(wp), intent(out) :: x(5), y(5), z(5), edge_seq(15)
        real(wp), intent(out) :: edge_matrix_3n(3, 5), edge_matrix_n3(5, 3)
        character(len=6), intent(out) :: edge_names(5)
        character(len=4), intent(out) :: edge_none(1)
        integer :: i

        x = [(real(i, wp), i = 1, size(x))]
        y = [(real(i, wp) * 2.0_wp, i = 1, size(x))]
        z = [(real(i, wp) * 0.5_wp, i = 1, size(x))]
        edge_seq = [1.0_wp, 0.0_wp, 0.0_wp, &
                    0.0_wp, 1.0_wp, 0.0_wp, &
                    0.0_wp, 0.0_wp, 1.0_wp, &
                    0.5_wp, 0.5_wp, 0.0_wp, &
                    0.0_wp, 0.5_wp, 0.5_wp]
        do i = 1, size(x)
            edge_matrix_3n(:, i) = edge_seq(3*i - 2:3*i)
            edge_matrix_n3(i, :) = edge_seq(3*i - 2:3*i)
        end do
        edge_names = ['red   ', 'green ', 'blue  ', 'orange', 'purple']
        edge_none = ['none']
    end subroutine setup_marker_inputs

   subroutine add_edge_style_plots(x, y, z, edge_seq, edge_matrix_3n, &
                                     edge_matrix_n3, edge_names, edge_none)
        real(wp), intent(in) :: x(:), y(:), z(:), edge_seq(:)
        real(wp), intent(in) :: edge_matrix_3n(:, :), edge_matrix_n3(:, :)
        character(len=*), intent(in) :: edge_names(:), edge_none(:)

        call scatter(x + 5.0_wp, y, color='blue', edgecolors='none', &
                     label='edge none')
        call scatter(x + 6.0_wp, y, color=[0.0_wp, 0.0_wp, 1.0_wp], &
                     edgecolors=edge_seq, &
                     linewidths=[0.5_wp, 1.0_wp, 1.5_wp, 2.0_wp, 2.5_wp], &
                     label='edge sequence')
        call scatter(x + 7.0_wp, y, edgecolors='red', label='string edge')
        call scatter(x + 8.0_wp, y, edgecolors=edge_names, &
                     label='string edge sequence')
        call scatter(x + 9.0_wp, y, linewidths=1.75_wp, &
                     label='scalar linewidths')
        call add_scatter(x + 10.0_wp, y, z, linewidths=2.25_wp, &
                         label='3d scalar linewidths')
        call scatter(x + 11.0_wp, y, edgecolors=edge_matrix_3n, &
                     label='edge matrix 3xn')
        call scatter(x + 12.0_wp, y, edgecolors=edge_matrix_n3, &
                     label='edge matrix nx3')
        call scatter(x + 13.0_wp, y, edgecolors=edge_none, &
                     label='edge none sequence')
    end subroutine add_edge_style_plots

    logical function sizes_match(plot_idx, expected)
        integer, intent(in) :: plot_idx
        real(wp), intent(in) :: expected(:)

        real(wp), parameter :: tol = 1.0e-12_wp

     sizes_match = .false.
        if (.not. allocated(global_figure%plots(plot_idx)%scatter_sizes)) then
            print *, 'FAIL: size not stored for plot', plot_idx
            return
        end if
        if (size(global_figure%plots(plot_idx)%scatter_sizes) /= size(expected)) then
            print *, 'FAIL: size length mismatch for plot', plot_idx
            return
        end if
        if (any(abs(global_figure%plots(plot_idx)%scatter_sizes - expected) > tol)) then
            print *, 'FAIL: size values changed for plot', plot_idx
            return
        end if
        sizes_match = .true.
    end function sizes_match

end program test_scatter

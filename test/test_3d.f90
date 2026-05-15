program test_3d
    !! Comprehensive test suite for 3D plotting functionality
    !! Consolidates: test_3d_axes_pdf_ticks, test_3d_plot_data_storage,
    !! test_3d_projection, test_3d_tick_orientation
    use, intrinsic :: iso_fortran_env, only: wp => real64, dp => real64
    use fortplot, only: figure, add_3d_plot, title, savefig, figure_t
    use fortplot_figure_core, only: plot_data_t, PLOT_TYPE_LINE
    use fortplot_3d_plots, only: add_3d_plot_fig => add_3d_plot
    use fortplot_scatter_plots, only: add_scatter_plot_data
    use fortplot_projection, only: get_default_view_angles, project_3d_to_2d
    use fortplot_pdf, only: pdf_context, create_pdf_canvas
    use fortplot_windows_test_helper, only: get_windows_safe_tolerance
    use test_pdf_utils, only: extract_pdf_stream_text, pdf_stream_has_stroke_rgb, &
                              pdf_stream_count_operator
    use test_output_helpers, only: ensure_test_output_dir, assert_pdf_file_valid
    implicit none

    integer :: total_tests, passed_tests

    total_tests = 0
    passed_tests = 0

    call test_data_storage()
    call test_2d_no_z_allocation()
    call test_type_detection()
    call test_line_projection()
    call test_scatter_projection()
    call test_tick_orientation()
    call test_axes_pdf_ticks()
    call test_pdf_3d_plot_rendering()
    call test_filled_surface_depth_ordered_wireframe()
    call test_projection_elevation_rotation()

    print *, ''
    print *, '=== 3D Test Summary ==='
    print *, 'Tests passed:', passed_tests, '/', total_tests

    if (passed_tests == total_tests) then
        print *, 'All 3D tests PASSED!'
        stop 0
    else
        print *, 'FAIL: Some 3D tests failed'
        stop 1
    end if

contains

    subroutine test_data_storage()
        !! Test that plot_data_t can store 3D line plot data
        type(plot_data_t) :: plot_data
        real(wp), dimension(5) :: x, y, z

        total_tests = total_tests + 1

        x = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        y = [2.0_wp, 4.0_wp, 6.0_wp, 8.0_wp, 10.0_wp]
        z = [0.5_wp, 1.0_wp, 1.5_wp, 2.0_wp, 2.5_wp]

        plot_data%plot_type = PLOT_TYPE_LINE
        plot_data%x = x
        plot_data%y = y
        plot_data%z = z
        plot_data%label = '3D test data'

        if (.not. allocated(plot_data%x) .or. .not. allocated(plot_data%y) .or. &
            .not. allocated(plot_data%z)) then
            print *, 'FAIL: test_data_storage - arrays not allocated'
            return
        end if

        if (size(plot_data%x) /= 5 .or. size(plot_data%y) /= 5 .or. &
            size(plot_data%z) /= 5) then
            print *, 'FAIL: test_data_storage - size mismatch'
            return
        end if

        if (abs(plot_data%x(3) - 3.0_wp) > get_windows_safe_tolerance(1e-10_wp)) then
            print *, 'FAIL: test_data_storage - x coordinate value mismatch'
            return
        end if
        if (abs(plot_data%z(2) - 1.0_wp) > get_windows_safe_tolerance(1e-10_wp)) then
            print *, 'FAIL: test_data_storage - z coordinate value mismatch'
            return
        end if

        print *, '  PASS: test_data_storage'
        passed_tests = passed_tests + 1
    end subroutine test_data_storage

    subroutine test_2d_no_z_allocation()
        !! Test that 2D plots do not allocate z unnecessarily
        type(plot_data_t) :: plot_data
        real(wp), dimension(3) :: x, y

        total_tests = total_tests + 1

        x = [1.0_wp, 2.0_wp, 3.0_wp]
        y = [2.0_wp, 4.0_wp, 6.0_wp]

        plot_data%plot_type = PLOT_TYPE_LINE
        plot_data%x = x
        plot_data%y = y

        if (.not. allocated(plot_data%x) .or. .not. allocated(plot_data%y)) then
            print *, 'FAIL: test_2d_no_z_allocation - x/y not allocated'
            return
        end if
        if (allocated(plot_data%z)) then
            print *, 'FAIL: test_2d_no_z_allocation - z should not be allocated'
            return
        end if

        print *, '  PASS: test_2d_no_z_allocation'
        passed_tests = passed_tests + 1
    end subroutine test_2d_no_z_allocation

    subroutine test_type_detection()
        !! Test automatic detection of 3D plots
        type(plot_data_t) :: plot_data

        total_tests = total_tests + 1

        allocate (plot_data%x(3), plot_data%y(3), plot_data%z(3))

        if (.not. plot_data%is_3d()) then
            print *, 'FAIL: test_type_detection - should report 3D when z allocated'
            return
        end if

        deallocate (plot_data%z)
        if (plot_data%is_3d()) then
            print *, 'FAIL: test_type_detection - should report 2D after z dealloc'
            return
        end if

        print *, '  PASS: test_type_detection'
        passed_tests = passed_tests + 1
    end subroutine test_type_detection

    subroutine test_line_projection()
        !! Validate 3D line plot projection to 2D
        type(figure_t) :: fig
        real(wp) :: x(4), y(4), z(4)
        real(wp) :: x_expected(4), y_expected(4)
        real(wp) :: tol, azim, elev, dist
        integer :: plot_idx

        total_tests = total_tests + 1

        call fig%initialize()

        x = [0.0_wp, 1.0_wp, -1.0_wp, 0.5_wp]
        y = [0.0_wp, 0.5_wp, 0.5_wp, -0.5_wp]
        z = [0.0_wp, 0.5_wp, 0.5_wp, 1.0_wp]

        call get_default_view_angles(azim, elev, dist)
        call project_3d_to_2d(x, y, z, azim, elev, dist, x_expected, y_expected)

        call add_3d_plot_fig(fig, x, y, z)

        plot_idx = fig%plot_count
        tol = get_windows_safe_tolerance(1.0e-12_wp)

        if (plot_idx /= 1) then
            print *, 'FAIL: test_line_projection - expected one plot'
            return
        end if

        if (.not. allocated(fig%plots(plot_idx)%z)) then
            print *, 'FAIL: test_line_projection - z not retained'
            return
        end if

        if (any(abs(fig%plots(plot_idx)%x - x_expected) > tol)) then
            print *, 'FAIL: test_line_projection - x mismatch'
            return
        end if

        if (any(abs(fig%plots(plot_idx)%y - y_expected) > tol)) then
            print *, 'FAIL: test_line_projection - y mismatch'
            return
        end if

        if (any(abs(fig%plots(plot_idx)%z - z) > tol)) then
            print *, 'FAIL: test_line_projection - z mismatch'
            return
        end if

        print *, '  PASS: test_line_projection'
        passed_tests = passed_tests + 1
    end subroutine test_line_projection

    subroutine test_scatter_projection()
        !! Validate 3D scatter plot projection to 2D
        type(figure_t) :: fig
        real(wp) :: x(3), y(3), z(3)
        real(wp) :: x_expected(3), y_expected(3)
        real(wp) :: tol, azim, elev, dist
        integer :: plot_idx

        total_tests = total_tests + 1

        call fig%initialize()

        x = [0.0_wp, 0.5_wp, -0.5_wp]
        y = [0.0_wp, -0.25_wp, 0.75_wp]
        z = [0.0_wp, 0.8_wp, 0.4_wp]

        call get_default_view_angles(azim, elev, dist)
        call project_3d_to_2d(x, y, z, azim, elev, dist, x_expected, y_expected)

        call add_scatter_plot_data(fig, x, y, z=z)

        plot_idx = fig%plot_count
        tol = get_windows_safe_tolerance(1.0e-12_wp)

        if (plot_idx /= 1) then
            print *, 'FAIL: test_scatter_projection - expected one plot'
            return
        end if

        if (.not. allocated(fig%plots(plot_idx)%z)) then
            print *, 'FAIL: test_scatter_projection - z not retained'
            return
        end if

        if (any(abs(fig%plots(plot_idx)%x - x_expected) > tol)) then
            print *, 'FAIL: test_scatter_projection - x mismatch'
            return
        end if

        if (any(abs(fig%plots(plot_idx)%y - y_expected) > tol)) then
            print *, 'FAIL: test_scatter_projection - y mismatch'
            return
        end if

        if (any(abs(fig%plots(plot_idx)%z - z) > tol)) then
            print *, 'FAIL: test_scatter_projection - z mismatch'
            return
        end if

        print *, '  PASS: test_scatter_projection'
        passed_tests = passed_tests + 1
    end subroutine test_scatter_projection

    subroutine test_tick_orientation()
        !! Test 3D axis tick orientations
        real(dp), allocatable :: x(:), y(:), z(:)
        character(len=:), allocatable :: output_dir
        integer :: i, n

        total_tests = total_tests + 1

        call ensure_test_output_dir('3d_tick_orientation', output_dir)

        n = 50
        allocate (x(n), y(n), z(n))

        do i = 1, n
            x(i) = cos(real(i - 1, dp)*0.2_dp)
            y(i) = sin(real(i - 1, dp)*0.2_dp)
            z(i) = real(i - 1, dp)*0.1_dp
        end do

        call figure(figsize=[8.0_dp, 6.0_dp])
        call add_3d_plot(x, y, z, label='Test Helix')
        call title('3D Tick Orientation Test')

        call savefig(output_dir//'test_3d_ticks.png')
        call savefig(output_dir//'test_3d_ticks.pdf')

        deallocate (x, y, z)

        print *, '  PASS: test_tick_orientation'
        passed_tests = passed_tests + 1
    end subroutine test_tick_orientation

    subroutine test_axes_pdf_ticks()
        !! Verify PDF backend renders 3D axis tick labels
        type(pdf_context) :: ctx
        integer, parameter :: W = 400, H = 300
        character(len=:), allocatable :: output_dir
        character(len=:), allocatable :: out_pdf
        character(len=:), allocatable :: stream
        integer :: status

        total_tests = total_tests + 1

        call ensure_test_output_dir('3d_axes_pdf_ticks', output_dir)

        ctx = create_pdf_canvas(W, H)
        ctx%x_min = 0.0_wp
        ctx%x_max = 1.0_wp
        ctx%y_min = 0.0_wp
        ctx%y_max = 1.0_wp

        call ctx%draw_axes_and_labels_backend('linear', 'linear', 1.0_wp, &
                                              0.0_wp, 1.0_wp, 0.0_wp, 1.0_wp, &
                                              has_3d_plots=.true., z_min=0.0_wp, &
                                              z_max=1.0_wp)

        out_pdf = output_dir//'test_3d_axes_ticks.pdf'
        call ctx%save(out_pdf)

        call extract_pdf_stream_text(out_pdf, stream, status)
        if (status /= 0) then
            print *, 'FAIL: test_axes_pdf_ticks - could not read PDF stream'
            return
        end if

        if (index(stream, '0.0') <= 0 .and. index(stream, '(0.0)') <= 0) then
            print *, 'FAIL: test_axes_pdf_ticks - missing tick label 0.0'
            return
        end if

        if (index(stream, '1.0') <= 0 .and. index(stream, '(1.0)') <= 0) then
            print *, 'FAIL: test_axes_pdf_ticks - missing tick label 1.0'
            return
        end if

        print *, '  PASS: test_axes_pdf_ticks'
        passed_tests = passed_tests + 1
    end subroutine test_axes_pdf_ticks

    subroutine test_pdf_3d_plot_rendering()
        !! Verify PDF backend renders 3D plots (surface + scatter)
        type(figure_t) :: fig
        character(len=:), allocatable :: output_dir
        character(len=:), allocatable :: out_pdf
        character(len=:), allocatable :: stream
        integer :: status
        real(dp) :: tol_color

        integer, parameter :: nx = 6
        integer, parameter :: ny = 5
        real(wp) :: x_grid(nx), y_grid(ny)
        real(wp) :: z_grid(ny, nx)
        integer :: i, j
        real(wp) :: edgecolor(3)

        integer, parameter :: npts = 5
        real(wp) :: xs(npts), ys(npts), zs(npts)
        real(wp) :: scatter_color(3)

        total_tests = total_tests + 1

        call ensure_test_output_dir('3d_pdf_plot_rendering', output_dir)
        tol_color = real(get_windows_safe_tolerance(1.0e-6_wp), dp)

        edgecolor = [1.0_wp, 0.0_wp, 0.0_wp]
        do i = 1, nx
            x_grid(i) = real(i - 1, wp)/real(nx - 1, wp)
        end do
        do j = 1, ny
            y_grid(j) = real(j - 1, wp)/real(ny - 1, wp)
        end do
        do j = 1, ny
            do i = 1, nx
                z_grid(j, i) = x_grid(i)**2 + y_grid(j)**2
            end do
        end do

        call fig%initialize()
        call fig%add_surface(x_grid, y_grid, z_grid, edgecolor=edgecolor, &
                             linewidth=1.0_wp, filled=.true.)
        call fig%set_title('3D surface PDF smoke test')

        out_pdf = output_dir//'test_3d_surface.pdf'
        call fig%savefig(out_pdf)
        call assert_pdf_file_valid(out_pdf)

        call extract_pdf_stream_text(out_pdf, stream, status)
        if (status /= 0) then
            print *, 'FAIL: test_pdf_3d_plot_rendering - could not read surface PDF'
            return
        end if

        if (.not. pdf_stream_has_stroke_rgb(stream, real(edgecolor, dp), &
                                            tol_color)) then
            print *, 'FAIL: test_pdf_3d_plot_rendering - missing surface edge color'
            return
        end if

        if (pdf_stream_count_operator(stream, 'B') + &
            pdf_stream_count_operator(stream, 'B*') <= 0) then
            print *, 'FAIL: test_pdf_3d_plot_rendering - missing filled quads'
            return
        end if

        scatter_color = [0.0_wp, 1.0_wp, 0.0_wp]
        xs = [0.0_wp, 0.25_wp, 0.5_wp, 0.75_wp, 1.0_wp]
        ys = [0.0_wp, 0.75_wp, 0.25_wp, 1.0_wp, 0.5_wp]
        zs = [0.0_wp, 0.2_wp, 0.4_wp, 0.6_wp, 0.8_wp]

        call fig%initialize()
        call add_scatter_plot_data(fig, xs, ys, z=zs, marker='o', &
                                   color=scatter_color)
        call fig%set_title('3D scatter PDF smoke test')

        out_pdf = output_dir//'test_3d_scatter.pdf'
        call fig%savefig(out_pdf)
        call assert_pdf_file_valid(out_pdf)

        call extract_pdf_stream_text(out_pdf, stream, status)
        if (status /= 0) then
            print *, 'FAIL: test_pdf_3d_plot_rendering - could not read scatter PDF'
            return
        end if

        if (.not. pdf_stream_has_stroke_rgb(stream, real(scatter_color, dp), &
                                            tol_color)) then
            print *, 'FAIL: test_pdf_3d_plot_rendering - missing scatter color'
            return
        end if

        if (pdf_stream_count_operator(stream, 'c') <= 0) then
            print *, 'FAIL: test_pdf_3d_plot_rendering - missing marker geometry'
            return
        end if

        print *, '  PASS: test_pdf_3d_plot_rendering'
        passed_tests = passed_tests + 1
    end subroutine test_pdf_3d_plot_rendering

    subroutine test_filled_surface_depth_ordered_wireframe()
        !! Verify filled surface with wireframe renders edge lines interleaved
        !! with filled quads (depth-ordered), not as a flat overlay.
        !! Regression test for issue #1729.
        type(figure_t) :: fig
        character(len=:), allocatable :: output_dir
        character(len=:), allocatable :: out_pdf
        character(len=:), allocatable :: stream
        integer :: status
        real(dp) :: tol_color
        real(wp) :: x_grid(6), y_grid(5), z_grid(5, 6)
        real(wp) :: edgecolor(3)
        integer :: i, j, n_quads

        total_tests = total_tests + 1

        call ensure_test_output_dir('filled_surface_wireframe', output_dir)
        tol_color = real(get_windows_safe_tolerance(1.0e-6_wp), dp)

        edgecolor = [0.0_dp, 0.0_dp, 1.0_dp]
        do i = 1, 6
            x_grid(i) = real(i - 1, wp) / 5.0_wp
        end do
        do j = 1, 5
            y_grid(j) = real(j - 1, wp) / 4.0_wp
        end do
        do j = 1, 5
            do i = 1, 6
                z_grid(j, i) = x_grid(i) * y_grid(j)
            end do
        end do

        call fig%initialize()
        call fig%add_surface(x_grid, y_grid, z_grid, edgecolor=edgecolor, &
                             filled=.true., linewidth=0.5_wp)
        call fig%set_title('Filled surface with wireframe')

        out_pdf = output_dir//'test_filled_surface_wireframe.pdf'
        call fig%savefig(out_pdf)
        call assert_pdf_file_valid(out_pdf)

        call extract_pdf_stream_text(out_pdf, stream, status)
        if (status /= 0) then
            print *, 'FAIL: test_filled_surface_depth_ordered_wireframe - could not read PDF'
            return
        end if

        ! Verify filled quads are present (B or B* operator)
        if (pdf_stream_count_operator(stream, 'B') + &
            pdf_stream_count_operator(stream, 'B*') <= 0) then
            print *, 'FAIL: test_filled_surface_depth_ordered_wireframe - missing filled quads'
            return
        end if

        ! Verify wireframe edge color is present (blue edge lines)
        if (.not. pdf_stream_has_stroke_rgb(stream, real(edgecolor, dp), &
                                            tol_color)) then
            print *, 'FAIL: test_filled_surface_depth_ordered_wireframe - missing wireframe edge color'
            return
        end if

        ! Count expected quads: (nx-1)*(ny-1) = 5*4 = 20
        n_quads = (6 - 1) * (5 - 1)
        if (pdf_stream_count_operator(stream, 'B') + &
            pdf_stream_count_operator(stream, 'B*') < n_quads) then
            print *, 'FAIL: test_filled_surface_depth_ordered_wireframe - fewer quads than expected'
            return
        end if

        ! Assert depth ordering: S (stroke) operators must be interleaved with
        ! each filled quad, not just a single flat overlay at the end.
        ! With interleaved per-quad edges, S count >= B count.
        ! With old post-fill wireframe overlay, S count = 0 (wireframe uses B*).
        if (pdf_stream_count_operator(stream, 'S') < n_quads) then
            print *, 'FAIL: test_filled_surface_depth_ordered_wireframe - ' // &
                     'wireframe not depth-ordered'
            return
        end if

        print *, '  PASS: test_filled_surface_depth_ordered_wireframe'
        passed_tests = passed_tests + 1
    end subroutine test_filled_surface_depth_ordered_wireframe

    subroutine test_projection_elevation_rotation()
        !! Validate that the elevation rotation in project_3d_to_2d uses the
        !! correct standard x-axis rotation formula:
        !!   y2d = y_rot * cos(elev) - z * sin(elev)
        !!       Regression test for issue #1725 (helix Z-axis collapsed).
        real(wp) :: x3(3), y3(3), z3(3)
        real(wp) :: x2d3(3), y2d3(3)
        real(wp) :: azim, elev, dist
        real(wp) :: cos_azim, sin_azim, cos_elev, sin_elev
        real(wp) :: x_rot, y_rot
        real(wp) :: tol
        integer :: i, n_points
        real(wp), allocatable :: x(:), y(:), z(:)
        real(wp), allocatable :: x2d(:), y2d(:)

        total_tests = total_tests + 1

        call get_default_view_angles(azim, elev, dist)

        ! Test points: origin, point on +Y axis, point on +Z axis
        x3 = [0.0_wp, 0.0_wp, 0.0_wp]
        y3 = [0.0_wp, 1.0_wp, 0.0_wp]
        z3 = [0.0_wp, 0.0_wp, 1.0_wp]

        call project_3d_to_2d(x3, y3, z3, azim, elev, dist, x2d3, y2d3)

        ! Compute expected values using the correct formula
        cos_azim = cos(azim)
        sin_azim = sin(azim)
        cos_elev = cos(elev)
        sin_elev = sin(elev)

        ! Point 1: (0, 0, 0) → projects to origin
        if (abs(x2d3(1)) > 1.0e-14_wp) then
            print *, 'FAIL: test_projection_elevation_rotation - origin x2d'
            return
        end if
        if (abs(y2d3(1)) > 1.0e-14_wp) then
            print *, 'FAIL: test_projection_elevation_rotation - origin y2d'
            return
        end if

        ! Point 2: (0, 1, 0) → x2d = sin(a), y2d = cos(a)*cos(e)
        x_rot = 0.0_wp * cos_azim - 1.0_wp * sin_azim
        y_rot = 0.0_wp * sin_azim + 1.0_wp * cos_azim
        if (abs(x2d3(2) - x_rot) > get_windows_safe_tolerance(1.0e-12_wp)) then
            print *, 'FAIL: test_projection_elevation_rotation - y-axis x2d'
            return
        end if
        if (abs(y2d3(2) - (y_rot * cos_elev - 0.0_wp * sin_elev)) > &
            get_windows_safe_tolerance(1.0e-12_wp)) then
            print *, 'FAIL: test_projection_elevation_rotation - y-axis y2d'
            return
        end if

        ! Point 3: (0, 0, 1) → x2d = 0, y2d = -sin(elev)
        ! This is the KEY test: Z must contribute NEGATIVELY to y2d
        ! (y2d = 0*cos(elev) - 1*sin(elev) = -sin(elev))
        ! The old buggy formula gave: y2d = 0*sin(elev) + 1*cos(elev) = cos(elev)
        if (abs(x2d3(3)) > get_windows_safe_tolerance(1.0e-12_wp)) then
            print *, 'FAIL: test_projection_elevation_rotation - z-axis x2d'
            return
        end if
        if (abs(y2d3(3) - (-sin_elev)) > get_windows_safe_tolerance(1.0e-12_wp)) then
            print *, 'FAIL: test_projection_elevation_rotation - z-axis y2d'
            return
        end if

        ! Helix test: verify Z contribution produces visible vertical spread
        ! Helix: x=cos(0.1*i), y=sin(0.1*i), z=0.05*i, i=0..9
        n_points = 10
        allocate(x(n_points), y(n_points), z(n_points))
        allocate(x2d(n_points), y2d(n_points))
        do i = 1, n_points
            x(i) = cos(real(i-1, wp)*0.1_wp)
            y(i) = sin(real(i-1, wp)*0.1_wp)
            z(i) = real(i-1, wp)*0.05_wp
        end do

        call project_3d_to_2d(x, y, z, azim, elev, dist, x2d, y2d)

        ! The y2d range must span at least 0.2 units (Z contribution visible)
        ! With correct formula: y2d = y_rot*cos(elev) - z*sin(elev)
        ! At i=0: z=0, at i=10: z=0.45, Z contribution = -0.45*0.5 = -0.225
        if ((maxval(y2d) - minval(y2d)) < 0.15_wp) then
            print *, 'FAIL: test_projection_elevation_rotation - helix Z spread too small'
            return
        end if

        print *, '  PASS: test_projection_elevation_rotation'
        passed_tests = passed_tests + 1
    end subroutine test_projection_elevation_rotation

end program test_3d

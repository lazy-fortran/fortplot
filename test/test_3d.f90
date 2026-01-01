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

end program test_3d

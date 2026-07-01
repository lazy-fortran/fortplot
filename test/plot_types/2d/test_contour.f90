program test_contour
    !! Comprehensive test suite for contour plot functionality
    !! Consolidates: test_contour_default_levels_rendering, test_contour_empty_levels_rendering,
    !! test_contour_is_not_3d, test_contour_memory_safety_regression, test_contour_refactoring_final,
    !! test_contour_unsorted_levels_rendering
    use iso_fortran_env, only: wp => real64
    use fortplot
    use fortplot_plot_data, only: plot_data_t
    use fortplot_validation, only: validation_result_t, validate_file_exists, &
        validate_file_size, validate_ascii_format, validate_png_format
    use fortplot_contour_regions, only: contour_region_t, extract_contour_regions
    use fortplot_contour_algorithms, only: calculate_marching_squares_config, &
        get_contour_lines
    implicit none

    call test_default_levels()
    call test_empty_levels()
    call test_is_not_3d()
    call test_memory_safety_regression()
    call test_refactoring_combinations()
    call test_unsorted_levels()
    call test_linear_scale_fast_path()
    call test_cell_rejection_optimization()
    call test_levels_sorting()
    call test_radial_filled_regions_geometry()
    call test_saddle_deterministic()

    print *, 'All contour tests PASSED!'

contains

    subroutine test_default_levels()
        !! Verify filled contours render with default levels (no levels provided)
        real(wp), dimension(20) :: x_grid, y_grid
        real(wp), dimension(20,20) :: z_grid
        integer :: i, j
        type(validation_result_t) :: val
        logical :: ok_png, ok_txt

        do i = 1, 20
            x_grid(i) = -3.0_wp + real(i-1, wp) * 6.0_wp / 19.0_wp
            y_grid(i) = -3.0_wp + real(i-1, wp) * 6.0_wp / 19.0_wp
        end do

        do i = 1, 20
            do j = 1, 20
                z_grid(i,j) = exp(-(x_grid(i)**2 + y_grid(j)**2))
            end do
        end do

        call figure(figsize=[8.0_wp, 6.0_wp])
        call title('Default contour levels rendering test')
        call xlabel('x')
        call ylabel('y')

        call add_contour_filled(x_grid, y_grid, z_grid)

        call savefig('build/test/output/test_contour_default_levels.png')
        call savefig('build/test/output/test_contour_default_levels.txt')

        val = validate_file_exists('build/test/output/test_contour_default_levels.png')
        ok_png = val%passed
        if (ok_png) then
            val = validate_file_size('build/test/output/test_contour_default_levels.png', min_size=4000)
            ok_png = val%passed
        end if

        val = validate_file_exists('build/test/output/test_contour_default_levels.txt')
        ok_txt = val%passed

        if (.not. (ok_png .and. ok_txt)) then
            print *, 'FAIL: Default contour levels render'
            error stop 1
        end if
        print *, '  PASS: test_default_levels'
    end subroutine test_default_levels

    subroutine test_empty_levels()
        !! Verify contour routines handle present-but-empty levels arrays
        real(wp), dimension(40) :: x_grid, y_grid
        real(wp), dimension(40, 40) :: z_grid
        real(wp), allocatable :: levels(:)
        integer :: i, j
        logical :: ok_png, ok_txt

        do i = 1, 40
            x_grid(i) = -3.0_wp + real(i - 1, wp) * 6.0_wp / 39.0_wp
            y_grid(i) = -3.0_wp + real(i - 1, wp) * 6.0_wp / 39.0_wp
        end do

        do j = 1, 40
            do i = 1, 40
                z_grid(j, i) = exp(-(x_grid(i)**2 + y_grid(j)**2))
            end do
        end do

        allocate(levels(0))

        block
            type(figure_t) :: fig

            call fig%initialize(640, 480)
            call fig%set_title('OO contour: empty levels')
            call fig%add_contour(x_grid, y_grid, z_grid, levels=levels)
            call fig%savefig('build/test/output/test_contour_empty_levels_oo.png')
            call fig%savefig('build/test/output/test_contour_empty_levels_oo.txt')
        end block

        call validate_outputs('build/test/output/test_contour_empty_levels_oo.png', &
            'build/test/output/test_contour_empty_levels_oo.txt', ok_png, ok_txt)
        if (.not. (ok_png .and. ok_txt)) error stop 1

        call figure(figsize=[6.0_wp, 4.5_wp])
        call title('Stateful contourf: empty levels')
        call add_contour_filled(x_grid, y_grid, z_grid, levels=levels)
        call savefig('build/test/output/test_contour_empty_levels_stateful.png')
        call savefig('build/test/output/test_contour_empty_levels_stateful.txt')

        call validate_outputs('build/test/output/test_contour_empty_levels_stateful.png', &
            'build/test/output/test_contour_empty_levels_stateful.txt', ok_png, ok_txt)
        if (.not. (ok_png .and. ok_txt)) error stop 1

        print *, '  PASS: test_empty_levels'
    end subroutine test_empty_levels

    subroutine validate_outputs(png_path, txt_path, ok_png_out, ok_txt_out)
        character(len=*), intent(in) :: png_path, txt_path
        logical, intent(out) :: ok_png_out, ok_txt_out
        type(validation_result_t) :: val

        val = validate_file_exists(png_path)
        ok_png_out = val%passed
        if (ok_png_out) then
            val = validate_png_format(png_path)
            ok_png_out = val%passed
        end if
        if (ok_png_out) then
            val = validate_file_size(png_path, min_size=4000)
            ok_png_out = val%passed
        end if

        val = validate_file_exists(txt_path)
        ok_txt_out = val%passed
        if (ok_txt_out) then
            val = validate_ascii_format(txt_path)
            ok_txt_out = val%passed
        end if
    end subroutine validate_outputs

    subroutine test_is_not_3d()
        !! Ensure contour/pcolormesh style data does not mark figure as 3D
        type(plot_data_t) :: plot
        real(wp), allocatable :: x(:), y(:), z(:,:)
        integer :: nx, ny, i, j

        nx = 10
        ny = 8
        allocate(x(nx), y(ny), z(ny, nx))

        x = [(real(i-1, wp), i=1,nx)]
        y = [(real(j-1, wp), j=1,ny)]
        z = 0.0_wp

        plot%x_grid = x
        plot%y_grid = y
        plot%z_grid = z

        if (plot%is_3d()) then
            error stop 'Contour-style plot must not be detected as 3D'
        end if
        print *, '  PASS: test_is_not_3d'
    end subroutine test_is_not_3d

    subroutine test_memory_safety_regression()
        !! Test to prevent memory safety regressions in contour plotting (Issue #401)
        real(wp), dimension(10) :: x_grid, y_grid
        real(wp), dimension(10,10) :: z_grid
        integer :: i, j

        do i = 1, 10
            x_grid(i) = (i-1) * 0.5_wp
            y_grid(i) = (i-1) * 0.5_wp
        end do
        do i = 1, 10
            do j = 1, 10
                z_grid(i,j) = sin(x_grid(i)) * cos(y_grid(j))
            end do
        end do

        call figure()
        call contour_filled(x_grid, y_grid, z_grid)

        call figure()
        call contour_filled(x_grid, y_grid, z_grid, colormap='plasma')

        call figure()
        call add_contour_filled(x_grid, y_grid, z_grid)

        call figure()
        call add_contour_filled(x_grid, y_grid, z_grid, colormap='inferno')

        print *, '  PASS: test_memory_safety_regression'
    end subroutine test_memory_safety_regression

    subroutine test_refactoring_combinations()
        !! Test contour function refactoring preserves all functionality (Issue #403)
        real(wp), dimension(10) :: x_grid, y_grid
        real(wp), dimension(10,10) :: z_grid
        real(wp), dimension(5) :: levels
        integer :: i, j

        do i = 1, 10
            x_grid(i) = (i-1) * 0.5_wp
            y_grid(i) = (i-1) * 0.5_wp
        end do
        do i = 1, 10
            do j = 1, 10
                z_grid(i,j) = sin(x_grid(i)) * cos(y_grid(j))
            end do
        end do

        levels = [-0.5_wp, -0.25_wp, 0.0_wp, 0.25_wp, 0.5_wp]

        call contour_filled(x_grid, y_grid, z_grid)
        call contour_filled(x_grid, y_grid, z_grid, levels=levels)
        call contour_filled(x_grid, y_grid, z_grid, colormap='viridis')
        call contour_filled(x_grid, y_grid, z_grid, show_colorbar=.true.)
        call contour_filled(x_grid, y_grid, z_grid, label='Test contour')
        call contour_filled(x_grid, y_grid, z_grid, levels=levels, colormap='plasma')
        call contour_filled(x_grid, y_grid, z_grid, colormap='hot', show_colorbar=.false.)
        call contour_filled(x_grid, y_grid, z_grid, levels=levels, &
                          colormap='coolwarm', show_colorbar=.true., label='Full test')

        call add_contour_filled(x_grid, y_grid, z_grid)
        call add_contour_filled(x_grid, y_grid, z_grid, levels=levels)
        call add_contour_filled(x_grid, y_grid, z_grid, colormap='RdBu')
        call add_contour_filled(x_grid, y_grid, z_grid, show_colorbar=.false.)
        call add_contour_filled(x_grid, y_grid, z_grid, label='Add contour test')
        call add_contour_filled(x_grid, y_grid, z_grid, levels=levels, colormap='seismic')
        call add_contour_filled(x_grid, y_grid, z_grid, colormap='twilight', &
                              show_colorbar=.true., label='Complex test')
        call add_contour_filled(x_grid, y_grid, z_grid, levels=levels, &
                              colormap='jet', show_colorbar=.true., label='Complete test')

        print *, '  PASS: test_refactoring_combinations'
    end subroutine test_refactoring_combinations

    subroutine test_unsorted_levels()
        !! Verify filled contours render correctly with unsorted explicit levels
        real(wp), dimension(50) :: x_grid, y_grid
        real(wp), dimension(50,50) :: z_grid
        integer :: i, j
        type(validation_result_t) :: val
        logical :: ok_png, ok_txt

        do i = 1, 50
            x_grid(i) = -3.0_wp + real(i-1, wp) * 6.0_wp / 49.0_wp
            y_grid(i) = -3.0_wp + real(i-1, wp) * 6.0_wp / 49.0_wp
        end do

        do i = 1, 50
            do j = 1, 50
                z_grid(i,j) = exp(-(x_grid(i)**2 + y_grid(j)**2))
            end do
        end do

        call figure(figsize=[6.0_wp, 4.5_wp])
        call title('Unsorted contour levels rendering test')
        call xlabel('x')
        call ylabel('y')

        call add_contour_filled(x_grid, y_grid, z_grid, &
            levels=[0.7_wp, 0.3_wp, 0.5_wp], colormap='plasma')

        call savefig('build/test/output/test_contour_unsorted_levels.png')
        call savefig('build/test/output/test_contour_unsorted_levels.txt')

        val = validate_file_exists('build/test/output/test_contour_unsorted_levels.png')
        ok_png = val%passed
        if (ok_png) then
            val = validate_file_size('build/test/output/test_contour_unsorted_levels.png', min_size=4000)
            ok_png = val%passed
        end if

        val = validate_file_exists('build/test/output/test_contour_unsorted_levels.txt')
        ok_txt = val%passed

        if (.not. (ok_png .and. ok_txt)) then
            print *, 'FAIL: Unsorted contour levels render'
            error stop 1
        end if
        print *, '  PASS: test_unsorted_levels'
    end subroutine test_unsorted_levels

    subroutine test_linear_scale_fast_path()
        !! Verify filled contours render correctly with linear scales
        !! (the fast path that skips scale transforms, ref #1746).
        real(wp), dimension(30) :: x_grid, y_grid
        real(wp), dimension(30,30) :: z_grid
        integer :: i, j
        type(validation_result_t) :: val
        logical :: ok_png

        do i = 1, 30
            x_grid(i) = -2.0_wp + real(i-1, wp) * 4.0_wp / 29.0_wp
            y_grid(i) = -2.0_wp + real(i-1, wp) * 4.0_wp / 29.0_wp
        end do

        do i = 1, 30
            do j = 1, 30
                z_grid(i,j) = sin(x_grid(i)) * cos(y_grid(j))
            end do
        end do

        call figure(figsize=[6.0_wp, 4.5_wp])
        call set_xscale('linear')
        call set_yscale('linear')
        call title('Linear scale fast-path contour test')
        call xlabel('x')
        call ylabel('y')

        call add_contour_filled(x_grid, y_grid, z_grid, &
            levels=[-0.8_wp, -0.4_wp, 0.0_wp, 0.4_wp, 0.8_wp], &
            colormap='viridis')

        call savefig('build/test/output/test_contour_linear_fast_path.png')

        val = validate_file_exists('build/test/output/test_contour_linear_fast_path.png')
        ok_png = val%passed
        if (ok_png) then
            val = validate_file_size('build/test/output/test_contour_linear_fast_path.png', min_size=4000)
            ok_png = val%passed
        end if

        if (.not. ok_png) then
            print *, 'FAIL: Linear scale fast-path contour render'
            error stop 1
        end if
        print *, '  PASS: test_linear_scale_fast_path'
    end subroutine test_linear_scale_fast_path

    subroutine test_cell_rejection_optimization()
        !! Verify that cells outside the [lo, hi] band are correctly skipped,
        !! producing the same output as without the optimization (ref #1746).
        real(wp), dimension(40) :: x_grid, y_grid
        real(wp), dimension(40,40) :: z_grid
        integer :: i, j
        type(validation_result_t) :: val
        logical :: ok_png, ok_txt

        ! Create a Gaussian with a narrow peak so many cells will be below
        ! the lowest level and get rejected by the optimization.
        do i = 1, 40
            x_grid(i) = -4.0_wp + real(i-1, wp) * 8.0_wp / 39.0_wp
            y_grid(i) = -4.0_wp + real(i-1, wp) * 8.0_wp / 39.0_wp
        end do

        do i = 1, 40
            do j = 1, 40
                z_grid(i,j) = exp(-(x_grid(i)**2 + y_grid(j)**2) * 0.5_wp)
            end do
        end do

        call figure(figsize=[6.0_wp, 4.5_wp])
        call title('Cell-rejection optimization test')
        call xlabel('x')
        call ylabel('y')

        ! Use a high minimum level so many cells are above hi and get skipped.
        call add_contour_filled(x_grid, y_grid, z_grid, &
            levels=[0.3_wp, 0.5_wp, 0.7_wp, 0.9_wp], &
            colormap='hot')

        call savefig('build/test/output/test_contour_cell_rejection.png')
        call savefig('build/test/output/test_contour_cell_rejection.txt')

        val = validate_file_exists('build/test/output/test_contour_cell_rejection.png')
        ok_png = val%passed
        if (ok_png) then
            val = validate_file_size('build/test/output/test_contour_cell_rejection.png', min_size=4000)
            ok_png = val%passed
        end if

        val = validate_file_exists('build/test/output/test_contour_cell_rejection.txt')
        ok_txt = val%passed

        if (.not. (ok_png .and. ok_txt)) then
            print *, 'FAIL: Cell-rejection contour render'
            error stop 1
        end if
        print *, '  PASS: test_cell_rejection_optimization'
    end subroutine test_cell_rejection_optimization

    subroutine test_levels_sorting()
        !! Verify that unsorted levels are sorted before rendering
        !! (introsort replacement for bubble sort, ref #1746).
        real(wp), dimension(20) :: x_grid, y_grid
        real(wp), dimension(20,20) :: z_grid
        real(wp), dimension(6) :: unsorted_levels
        integer :: i, j
        type(validation_result_t) :: val
        logical :: ok_png

        do i = 1, 20
            x_grid(i) = real(i-1, wp) * 2.0_wp / 19.0_wp
            y_grid(i) = real(i-1, wp) * 2.0_wp / 19.0_wp
        end do

        do i = 1, 20
            do j = 1, 20
                z_grid(i,j) = real(i + j - 2, wp) / 38.0_wp
            end do
        end do

        ! Levels in reverse order to test sorting
        unsorted_levels = [0.9_wp, 0.7_wp, 0.5_wp, 0.3_wp, 0.1_wp, 0.0_wp]

        call figure(figsize=[6.0_wp, 4.5_wp])
        call title('Levels sorting test (unsorted input)')
        call xlabel('x')
        call ylabel('y')

        call add_contour_filled(x_grid, y_grid, z_grid, &
            levels=unsorted_levels, colormap='plasma')

        call savefig('build/test/output/test_contour_levels_sorted.png')

        val = validate_file_exists('build/test/output/test_contour_levels_sorted.png')
        ok_png = val%passed
        if (ok_png) then
            val = validate_file_size('build/test/output/test_contour_levels_sorted.png', min_size=4000)
            ok_png = val%passed
        end if

        if (.not. ok_png) then
            print *, 'FAIL: Levels sorting contour render'
            error stop 1
        end if
        print *, '  PASS: test_levels_sorting'
    end subroutine test_levels_sorting

    subroutine test_radial_filled_regions_geometry()
        !! Smooth radial Gaussian: filled regions must be closed rings with no
        !! stray one-cell fragments, level bands must ascend, and the two
        !! interior bands must be annuli (two concentric closed rings each).
        integer, parameter :: n = 41
        real(wp) :: x(n), y(n), z(n, n)
        real(wp) :: levels(3)
        type(contour_region_t), allocatable :: regions(:)
        integer :: i, j, r, b
        integer :: annular_regions

        do i = 1, n
            x(i) = -3.0_wp + real(i - 1, wp)*6.0_wp/real(n - 1, wp)
            y(i) = x(i)
        end do
        do j = 1, n
            do i = 1, n
                z(j, i) = exp(-(x(i)**2 + y(j)**2))
            end do
        end do

        levels = [0.2_wp, 0.5_wp, 0.8_wp]
        regions = extract_contour_regions(x, y, z, levels)

        if (size(regions) /= size(levels) + 1) then
            print *, 'FAIL: region count', size(regions)
            error stop 1
        end if

        do r = 1, size(regions)
            if (regions(r)%level_min >= regions(r)%level_max) then
                print *, 'FAIL: region band not ordered', r
                error stop 1
            end if
            if (r > 1) then
                if (regions(r)%level_min < regions(r - 1)%level_min) then
                    print *, 'FAIL: region order not ascending', r
                    error stop 1
                end if
            end if
        end do

        annular_regions = 0
        do r = 1, size(regions)
            if (.not. allocated(regions(r)%boundaries)) cycle
            do b = 1, size(regions(r)%boundaries)
                if (.not. regions(r)%boundaries(b)%is_closed) then
                    print *, 'FAIL: open boundary in region', r
                    error stop 1
                end if
                if (size(regions(r)%boundaries(b)%x) < 4) then
                    print *, 'FAIL: stray fragment in region', r
                    error stop 1
                end if
            end do
            if (size(regions(r)%boundaries) >= 2) then
                annular_regions = annular_regions + 1
            end if
        end do

        if (annular_regions < 2) then
            print *, 'FAIL: expected two annular interior bands, got', &
                annular_regions
            error stop 1
        end if

        print *, '  PASS: test_radial_filled_regions_geometry'
    end subroutine test_radial_filled_regions_geometry

    subroutine test_saddle_deterministic()
        !! An ambiguous saddle cell must resolve to one deterministic topology,
        !! identical on repeated evaluation (PNG/PDF consistency) and matching
        !! Matplotlib's mean decider.
        real(wp) :: lp1(8), lp2(8)
        integer :: cfg, nl1, nl2, k
        real(wp) :: z1, z2, z3, z4, level

        z1 = 2.0_wp; z2 = -1.0_wp; z3 = 2.0_wp; z4 = -1.0_wp
        level = 0.0_wp

        call calculate_marching_squares_config(z1, z2, z3, z4, level, cfg)
        if (cfg /= 5) then
            print *, 'FAIL: expected saddle config 5, got', cfg
            error stop 1
        end if

        call get_contour_lines(cfg, 0.0_wp, 0.0_wp, 1.0_wp, 0.0_wp, &
                               1.0_wp, 1.0_wp, 0.0_wp, 1.0_wp, &
                               z1, z2, z3, z4, level, lp1, nl1)
        call get_contour_lines(cfg, 0.0_wp, 0.0_wp, 1.0_wp, 0.0_wp, &
                               1.0_wp, 1.0_wp, 0.0_wp, 1.0_wp, &
                               z1, z2, z3, z4, level, lp2, nl2)

        if (nl1 /= 2 .or. nl2 /= 2) then
            print *, 'FAIL: saddle must yield two segments', nl1, nl2
            error stop 1
        end if
        do k = 1, 8
            if (abs(lp1(k) - lp2(k)) > 1.0e-12_wp) then
                print *, 'FAIL: saddle topology not deterministic at', k
                error stop 1
            end if
        end do

        ! center = 0.5 >= level 0.0: high corners connect, low corners isolated.
        ! First segment isolates low corner 2, starting at edge 1-2 crossing
        ! (t = 2/3 along a unit edge) -> (2/3, 0).
        if (abs(lp1(1) - 2.0_wp/3.0_wp) > 1.0e-9_wp) then
            print *, 'FAIL: saddle x does not match mean decider', lp1(1)
            error stop 1
        end if
        if (abs(lp1(2)) > 1.0e-9_wp) then
            print *, 'FAIL: saddle y does not match mean decider', lp1(2)
            error stop 1
        end if

        print *, '  PASS: test_saddle_deterministic'
    end subroutine test_saddle_deterministic

end program test_contour

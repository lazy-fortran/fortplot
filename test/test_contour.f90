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
    implicit none

    call test_default_levels()
    call test_empty_levels()
    call test_is_not_3d()
    call test_memory_safety_regression()
    call test_refactoring_combinations()
    call test_unsorted_levels()

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

        call savefig('test/output/test_contour_default_levels.png')
        call savefig('test/output/test_contour_default_levels.txt')

        val = validate_file_exists('test/output/test_contour_default_levels.png')
        ok_png = val%passed
        if (ok_png) then
            val = validate_file_size('test/output/test_contour_default_levels.png', min_size=4000)
            ok_png = val%passed
        end if

        val = validate_file_exists('test/output/test_contour_default_levels.txt')
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
            call fig%savefig('test/output/test_contour_empty_levels_oo.png')
            call fig%savefig('test/output/test_contour_empty_levels_oo.txt')
        end block

        call validate_outputs('test/output/test_contour_empty_levels_oo.png', &
            'test/output/test_contour_empty_levels_oo.txt', ok_png, ok_txt)
        if (.not. (ok_png .and. ok_txt)) error stop 1

        call figure(figsize=[6.0_wp, 4.5_wp])
        call title('Stateful contourf: empty levels')
        call add_contour_filled(x_grid, y_grid, z_grid, levels=levels)
        call savefig('test/output/test_contour_empty_levels_stateful.png')
        call savefig('test/output/test_contour_empty_levels_stateful.txt')

        call validate_outputs('test/output/test_contour_empty_levels_stateful.png', &
            'test/output/test_contour_empty_levels_stateful.txt', ok_png, ok_txt)
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

        call savefig('test/output/test_contour_unsorted_levels.png')
        call savefig('test/output/test_contour_unsorted_levels.txt')

        val = validate_file_exists('test/output/test_contour_unsorted_levels.png')
        ok_png = val%passed
        if (ok_png) then
            val = validate_file_size('test/output/test_contour_unsorted_levels.png', min_size=4000)
            ok_png = val%passed
        end if

        val = validate_file_exists('test/output/test_contour_unsorted_levels.txt')
        ok_txt = val%passed

        if (.not. (ok_png .and. ok_txt)) then
            print *, 'FAIL: Unsorted contour levels render'
            error stop 1
        end if
        print *, '  PASS: test_unsorted_levels'
    end subroutine test_unsorted_levels

end program test_contour

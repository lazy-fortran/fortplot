program test_cmap_keyword
    !! Test that cmap is the canonical keyword and colormap is a backward-compatible alias
    !! for contour, contour_filled, contourf, pcolormesh, and surface functions.
    !!
    !! Regression test for issue #1670.
    use iso_fortran_env, only: wp => real64
    use fortplot
    use fortplot_validation, only: validation_result_t, validate_file_exists, &
        validate_file_size, validate_png_format
    implicit none

    call test_cmap_contour_filled()
    call test_cmap_pcolormesh()
    call test_cmap_surface()
    call test_colormap_alias_contour_filled()
    call test_colormap_alias_pcolormesh()
    call test_colormap_alias_surface()

    print *, 'All cmap keyword tests PASSED!'

contains

    subroutine test_cmap_contour_filled()
        !! Test that cmap keyword works for contour_filled
        real(wp), dimension(10) :: x_grid, y_grid
        real(wp), dimension(10,10) :: z_grid
        integer :: i, j
        type(validation_result_t) :: val

        do i = 1, 10
            x_grid(i) = real(i, wp)
            y_grid(i) = real(i, wp)
        end do

        do i = 1, 10
            do j = 1, 10
                z_grid(i,j) = real(i + j, wp)
            end do
        end do

        call figure(figsize=[6.0_wp, 6.0_wp])
        call title('cmap keyword test - contour_filled')
        call add_contour_filled(x_grid, y_grid, z_grid, cmap='viridis', &
                                show_colorbar=.true., label='test_cmap')

        call savefig('build/test/output/test_cmap_contour_filled.png')
        val = validate_file_exists('build/test/output/test_cmap_contour_filled.png')
        if (.not. val%passed) error stop 'test_cmap_contour_filled: file not created'
        val = validate_file_size('build/test/output/test_cmap_contour_filled.png', 1000)
        if (.not. val%passed) error stop 'test_cmap_contour_filled: file too small'
        val = validate_png_format('build/test/output/test_cmap_contour_filled.png')
        if (.not. val%passed) error stop 'test_cmap_contour_filled: invalid PNG'
    end subroutine test_cmap_contour_filled

    subroutine test_cmap_pcolormesh()
        !! Test that cmap keyword works for pcolormesh
        real(wp), dimension(11) :: x, y
        real(wp), dimension(10,10) :: z
        integer :: i, j
        type(validation_result_t) :: val

        do i = 1, 11
            x(i) = real(i-1, wp)
        end do
        do j = 1, 11
            y(j) = real(j-1, wp)
        end do

        do i = 1, 10
            do j = 1, 10
                z(i,j) = real(i * j, wp)
            end do
        end do

        call figure(figsize=[6.0_wp, 6.0_wp])
        call title('cmap keyword test - pcolormesh')
        call add_pcolormesh(x, y, z, cmap='plasma', show_colorbar=.true., label='test_cmap')

        call savefig('build/test/output/test_cmap_pcolormesh.png')
        val = validate_file_exists('build/test/output/test_cmap_pcolormesh.png')
        if (.not. val%passed) error stop 'test_cmap_pcolormesh: file not created'
        val = validate_file_size('build/test/output/test_cmap_pcolormesh.png', 1000)
        if (.not. val%passed) error stop 'test_cmap_pcolormesh: file too small'
        val = validate_png_format('build/test/output/test_cmap_pcolormesh.png')
        if (.not. val%passed) error stop 'test_cmap_pcolormesh: invalid PNG'
    end subroutine test_cmap_pcolormesh

    subroutine test_cmap_surface()
        !! Test that cmap keyword works for add_surface
        real(wp), dimension(10) :: x, y
        real(wp), dimension(10,10) :: z
        integer :: i, j
        type(validation_result_t) :: val

        do i = 1, 10
            x(i) = real(i, wp)
            y(i) = real(i, wp)
        end do

        do i = 1, 10
            do j = 1, 10
                z(i,j) = real(i + j, wp)
            end do
        end do

        call figure(figsize=[8.0_wp, 6.0_wp])
        call title('cmap keyword test - surface')
        call add_surface(x, y, z, cmap='viridis', show_colorbar=.true., label='test_cmap')

        call savefig('build/test/output/test_cmap_surface.png')
        val = validate_file_exists('build/test/output/test_cmap_surface.png')
        if (.not. val%passed) error stop 'test_cmap_surface: file not created'
        val = validate_file_size('build/test/output/test_cmap_surface.png', 1000)
        if (.not. val%passed) error stop 'test_cmap_surface: file too small'
    end subroutine test_cmap_surface

    subroutine test_colormap_alias_contour_filled()
        !! Test that colormap keyword still works (backward-compat alias) for contour_filled
        real(wp), dimension(10) :: x_grid, y_grid
        real(wp), dimension(10,10) :: z_grid
        integer :: i, j
        type(validation_result_t) :: val

        do i = 1, 10
            x_grid(i) = real(i, wp)
            y_grid(i) = real(i, wp)
        end do

        do i = 1, 10
            do j = 1, 10
                z_grid(i,j) = real(i + j, wp)
            end do
        end do

        call figure(figsize=[6.0_wp, 6.0_wp])
        call title('colormap alias test - contour_filled')
        call add_contour_filled(x_grid, y_grid, z_grid, colormap='viridis', &
                                show_colorbar=.true., label='test_colormap')

        call savefig('build/test/output/test_colormap_alias_contour_filled.png')
        val = validate_file_exists('build/test/output/test_colormap_alias_contour_filled.png')
        if (.not. val%passed) error stop 'test_colormap_alias_contour_filled: file not created'
        val = validate_file_size('build/test/output/test_colormap_alias_contour_filled.png', 1000)
        if (.not. val%passed) error stop 'test_colormap_alias_contour_filled: file too small'
    end subroutine test_colormap_alias_contour_filled

    subroutine test_colormap_alias_pcolormesh()
        !! Test that colormap keyword still works (backward-compat alias) for pcolormesh
        real(wp), dimension(11) :: x, y
        real(wp), dimension(10,10) :: z
        integer :: i, j
        type(validation_result_t) :: val

        do i = 1, 11
            x(i) = real(i-1, wp)
        end do
        do j = 1, 11
            y(j) = real(j-1, wp)
        end do

        do i = 1, 10
            do j = 1, 10
                z(i,j) = real(i * j, wp)
            end do
        end do

        call figure(figsize=[6.0_wp, 6.0_wp])
        call title('colormap alias test - pcolormesh')
        call add_pcolormesh(x, y, z, colormap='plasma', show_colorbar=.true., label='test_colormap')

        call savefig('build/test/output/test_colormap_alias_pcolormesh.png')
        val = validate_file_exists('build/test/output/test_colormap_alias_pcolormesh.png')
        if (.not. val%passed) error stop 'test_colormap_alias_pcolormesh: file not created'
        val = validate_file_size('build/test/output/test_colormap_alias_pcolormesh.png', 1000)
        if (.not. val%passed) error stop 'test_colormap_alias_pcolormesh: file too small'
    end subroutine test_colormap_alias_pcolormesh

    subroutine test_colormap_alias_surface()
        !! Test that colormap keyword still works (backward-compat alias) for add_surface
        real(wp), dimension(10) :: x, y
        real(wp), dimension(10,10) :: z
        integer :: i, j
        type(validation_result_t) :: val

        do i = 1, 10
            x(i) = real(i, wp)
            y(i) = real(i, wp)
        end do

        do i = 1, 10
            do j = 1, 10
                z(i,j) = real(i + j, wp)
            end do
        end do

        call figure(figsize=[8.0_wp, 6.0_wp])
        call title('colormap alias test - surface')
        call add_surface(x, y, z, colormap='viridis', show_colorbar=.true., label='test_colormap')

        call savefig('build/test/output/test_colormap_alias_surface.png')
        val = validate_file_exists('build/test/output/test_colormap_alias_surface.png')
        if (.not. val%passed) error stop 'test_colormap_alias_surface: file not created'
        val = validate_file_size('build/test/output/test_colormap_alias_surface.png', 1000)
        if (.not. val%passed) error stop 'test_colormap_alias_surface: file too small'
    end subroutine test_colormap_alias_surface

end program test_cmap_keyword

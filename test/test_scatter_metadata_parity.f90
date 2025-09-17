program test_scatter_metadata_parity
    !! Ensure scatter wrappers delegate to core implementation without divergence
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use fortplot, only: figure_t
    use fortplot_plot_data, only: plot_data_t
    use fortplot_scatter_plots, only: add_scatter_plot_data
    implicit none

    type(figure_t) :: fig_public
    type(figure_t) :: fig_core
    type(figure_t) :: fig_3d
    real(dp) :: x(4), y(4), sizes(4), colors(4)
    real(dp) :: rgb(3)
    real(dp) :: zvals(4)
    real(dp), parameter :: tol = 1.0e-12_dp

    x = [0.0_dp, 1.0_dp, 2.0_dp, 3.0_dp]
    y = [1.5_dp, 2.5_dp, 3.5_dp, 4.5_dp]
    sizes = [10.0_dp, 20.0_dp, 15.0_dp, 12.0_dp]
    colors = [0.2_dp, 0.4_dp, 0.6_dp, 0.8_dp]
    rgb = [0.1_dp, 0.3_dp, 0.5_dp]
    zvals = [-1.0_dp, -0.5_dp, 0.5_dp, 1.0_dp]

    call fig_public%initialize()
    call fig_core%initialize()

    call add_scatter_plot_data(fig_public, x, y, s=sizes, c=colors, &
                               label='parity', marker='^', markersize=12.0_dp, &
                               color=rgb, colormap='viridis', vmin=-1.0_dp, &
                               vmax=1.5_dp, show_colorbar=.true.)

    call fig_core%scatter(x, y, s=sizes, c=colors, label='parity', marker='^', &
                          markersize=12.0_dp, color=rgb, colormap='viridis', &
                          vmin=-1.0_dp, vmax=1.5_dp, show_colorbar=.true.)

    if (fig_public%plot_count /= 1) then
        print *, 'FAIL: public wrapper plot count mismatch'
        stop 1
    end if
    if (fig_core%plot_count /= 1) then
        print *, 'FAIL: core scatter plot count mismatch'
        stop 1
    end if

    call assert_metadata_equal(fig_public%plots(1), fig_core%plots(1), tol)

    call fig_3d%initialize()
    call add_scatter_plot_data(fig_3d, x, y, z=zvals)

    if (.not. allocated(fig_3d%plots(1)%z)) then
        print *, 'FAIL: 3D scatter lost z allocation during delegation'
        stop 1
    end if

    if (any(abs(fig_3d%plots(1)%z - zvals) > tol)) then
        print *, 'FAIL: 3D scatter z values diverged'
        stop 1
    end if

    print *, 'PASS: scatter metadata parity preserved across entry points'

contains

    subroutine assert_metadata_equal(lhs, rhs, tol)
        type(plot_data_t), intent(in) :: lhs
        type(plot_data_t), intent(in) :: rhs
        real(dp), intent(in) :: tol

        call check_array(lhs%x, rhs%x, tol, 'x data mismatch')
        call check_array(lhs%y, rhs%y, tol, 'y data mismatch')
        call check_optional_array(lhs%scatter_sizes, rhs%scatter_sizes, tol, &
                                  'scatter size data mismatch')
        call check_optional_array(lhs%scatter_colors, rhs%scatter_colors, tol, &
                                  'scatter color data mismatch')

        if (any(abs(lhs%color - rhs%color) > tol)) then
            call fail('solid color mismatch')
        end if

        if (lhs%scatter_size_default /= rhs%scatter_size_default) then
            call fail('default size mismatch')
        end if

        if (trim(lhs%marker) /= trim(rhs%marker)) then
            call fail('marker mismatch')
        end if

        if (trim(lhs%linestyle) /= trim(rhs%linestyle)) then
            call fail('linestyle mismatch')
        end if

        if (trim(lhs%scatter_colormap) /= trim(rhs%scatter_colormap)) then
            call fail('colormap mismatch')
        end if

        if (lhs%scatter_colorbar .neqv. rhs%scatter_colorbar) then
            call fail('colorbar flag mismatch')
        end if

        if (lhs%scatter_vrange_set .neqv. rhs%scatter_vrange_set) then
            call fail('color range flag mismatch')
        end if

        if (lhs%scatter_vrange_set) then
            if (abs(lhs%scatter_vmin - rhs%scatter_vmin) > tol) then
                call fail('vmin mismatch')
            end if
            if (abs(lhs%scatter_vmax - rhs%scatter_vmax) > tol) then
                call fail('vmax mismatch')
            end if
        end if

        if (allocated(lhs%label) .neqv. allocated(rhs%label)) then
            call fail('label allocation mismatch')
        end if
        if (allocated(lhs%label)) then
            if (trim(lhs%label) /= trim(rhs%label)) then
                call fail('label text mismatch')
            end if
        end if
    end subroutine assert_metadata_equal

    subroutine check_array(lhs, rhs, tol, context)
        real(dp), intent(in) :: lhs(:)
        real(dp), intent(in) :: rhs(:)
        real(dp), intent(in) :: tol
        character(len=*), intent(in) :: context

        if (size(lhs) /= size(rhs)) then
            call fail(context)
        end if
        if (any(abs(lhs - rhs) > tol)) then
            call fail(context)
        end if
    end subroutine check_array

    subroutine check_optional_array(lhs, rhs, tol, context)
        real(dp), intent(in), optional :: lhs(:)
        real(dp), intent(in), optional :: rhs(:)
        real(dp), intent(in) :: tol
        character(len=*), intent(in) :: context

        logical :: lhs_present
        logical :: rhs_present

        lhs_present = present(lhs)
        rhs_present = present(rhs)

        if (lhs_present .neqv. rhs_present) then
            call fail(context)
        end if

        if (lhs_present) then
            call check_array(lhs, rhs, tol, context)
        end if
    end subroutine check_optional_array

    subroutine fail(message)
        character(len=*), intent(in) :: message
        print *, 'FAIL:', trim(message)
        stop 1
    end subroutine fail

end program test_scatter_metadata_parity

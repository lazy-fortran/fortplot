program test_pcolormesh_shading_forwarding
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_core, only: figure_t
    use fortplot_matplotlib, only: add_pcolormesh, figure, pcolormesh
    use fortplot_matplotlib_session, only: get_global_figure
    implicit none

    real(wp), parameter :: x_edges(3) = [0.0_wp, 1.0_wp, 2.0_wp]
    real(wp), parameter :: y_edges(3) = [0.0_wp, 1.0_wp, 2.0_wp]
    real(wp), parameter :: cell_values(2, 2) = reshape( &
        [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp], [2, 2])
    real(wp), parameter :: x_nodes(2) = [0.0_wp, 1.0_wp]
    real(wp), parameter :: y_nodes(2) = [0.0_wp, 1.0_wp]
    real(wp), parameter :: node_values(2, 2) = reshape( &
        [4.0_wp, 3.0_wp, 2.0_wp, 1.0_wp], [2, 2])

    call figure()
    call pcolormesh(x_edges, y_edges, cell_values, shading='flat')
    call assert_latest_shading('flat')

    call figure()
    call add_pcolormesh(x_nodes, y_nodes, node_values, shading='gouraud')
    call assert_latest_shading('gouraud')

    call figure()
    call pcolormesh(x_edges, y_edges, cell_values)
    call assert_latest_shading('flat')

    print *, 'PASS: pcolormesh shading reaches backend plot data'

contains

    subroutine assert_latest_shading(expected)
        character(len=*), intent(in) :: expected
        class(*), pointer :: any_figure

        any_figure => get_global_figure()
        select type (current_figure => any_figure)
        type is (figure_t)
            if (current_figure%plot_count /= 1) then
                error stop 'pcolormesh wrapper did not register exactly one plot'
            end if
            if (trim(current_figure%plots(1)%pcolormesh_data%shading) /= expected) then
                error stop 'pcolormesh shading was not forwarded'
            end if
        class default
            error stop 'global figure does not expose figure_t state'
        end select
    end subroutine assert_latest_shading

end program test_pcolormesh_shading_forwarding

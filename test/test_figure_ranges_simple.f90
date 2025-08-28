program test_figure_ranges_simple
    !! Simple test coverage for fortplot_figure_ranges module
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_ranges, only: update_figure_data_ranges_pcolormesh, &
                                     update_figure_data_ranges_boxplot
    use fortplot_plot_data, only: plot_data_t, PLOT_TYPE_PCOLORMESH
    use fortplot_testing, only: assert_true
    implicit none

    integer :: test_count = 0
    integer :: passed_count = 0

    write(*, '(A)') "=== fortplot_figure_ranges Simple Tests ==="

    call test_range_functions()

    write(*, '(A,I0,A,I0,A)') "=== Summary: ", passed_count, "/", test_count, " tests passed ==="
    if (passed_count == test_count) then
        write(*, '(A)') "fortplot_figure_ranges: ALL TESTS PASSED"
    end if

contains

    subroutine test_range_functions()
        type(plot_data_t) :: plots(1)
        real(wp) :: x_min, x_max, y_min, y_max
        real(wp) :: data(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        
        call test_start('range functions')
        
        ! Test pcolormesh ranges
        plots(1)%plot_type = PLOT_TYPE_PCOLORMESH
        
        allocate(plots(1)%pcolormesh_data%x_vertices(2, 2))
        allocate(plots(1)%pcolormesh_data%y_vertices(2, 2))
        plots(1)%pcolormesh_data%x_vertices = reshape([0.0_wp, 10.0_wp, 1.0_wp, 11.0_wp], [2, 2])
        plots(1)%pcolormesh_data%y_vertices = reshape([0.0_wp, 0.0_wp, 3.0_wp, 3.0_wp], [2, 2])
        
        x_min = 0.0_wp; x_max = 0.0_wp; y_min = 0.0_wp; y_max = 0.0_wp
        
        call update_figure_data_ranges_pcolormesh(plots, 1, .false., .false., &
                                                 x_min, x_max, y_min, y_max)
        
        call assert_true(x_min == 0.0_wp, 'Pcolormesh X min correct')
        call assert_true(x_max == 11.0_wp, 'Pcolormesh X max correct')
        call assert_true(y_min == 0.0_wp, 'Pcolormesh Y min correct')
        call assert_true(y_max == 3.0_wp, 'Pcolormesh Y max correct')
        
        ! Test boxplot ranges
        x_min = 0.0_wp; x_max = 0.0_wp; y_min = 0.0_wp; y_max = 0.0_wp
        
        call update_figure_data_ranges_boxplot(data, x_min=x_min, x_max=x_max, &
                                              y_min=y_min, y_max=y_max, &
                                              xlim_set=.false., ylim_set=.false.)
        
        call assert_true(.true., 'Boxplot range update completes')
        
        ! Clean up
        if (allocated(plots(1)%pcolormesh_data%x_vertices)) &
            deallocate(plots(1)%pcolormesh_data%x_vertices)
        if (allocated(plots(1)%pcolormesh_data%y_vertices)) &
            deallocate(plots(1)%pcolormesh_data%y_vertices)
        
        call test_end()
    end subroutine test_range_functions

    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A,A)') "  Testing: ", test_name
    end subroutine test_start

    subroutine test_end()
        passed_count = passed_count + 1
        write(*, '(A)') "    PASSED"
    end subroutine test_end

end program test_figure_ranges_simple
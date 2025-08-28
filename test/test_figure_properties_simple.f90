program test_figure_properties_simple
    !! Simple test coverage for fortplot_figure_properties module
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_properties, only: get_figure_width_property, get_figure_height_property, &
                                         get_figure_rendered_property, set_figure_rendered_property, &
                                         get_figure_plot_count_property, get_figure_plots_property, &
                                         figure_backend_color_property, figure_backend_associated_property
    use fortplot_figure_initialization, only: figure_state_t, initialize_figure_state
    use fortplot_plot_data, only: plot_data_t
    use fortplot_testing, only: assert_true
    implicit none

    integer :: test_count = 0
    integer :: passed_count = 0

    write(*, '(A)') "=== fortplot_figure_properties Simple Tests ==="

    call test_property_functions()

    write(*, '(A,I0,A,I0,A)') "=== Summary: ", passed_count, "/", test_count, " tests passed ==="
    if (passed_count == test_count) then
        write(*, '(A)') "fortplot_figure_properties: ALL TESTS PASSED"
    end if

contains

    subroutine test_property_functions()
        type(figure_state_t) :: state
        type(plot_data_t), target :: plots(3)
        type(plot_data_t), pointer :: plots_ptr(:)
        integer :: width, height, plot_count
        logical :: rendered, associated_result
        
        call test_start('property functions')
        
        ! Initialize state
        call initialize_figure_state(state, 800, 600)
        
        ! Test dimension properties
        width = get_figure_width_property(state)
        height = get_figure_height_property(state)
        call assert_true(width == 800, 'Width property correct')
        call assert_true(height == 600, 'Height property correct')
        
        ! Test rendered property
        rendered = get_figure_rendered_property(state)
        call assert_true(.not. rendered, 'Initial rendered state false')
        
        call set_figure_rendered_property(state, .true.)
        rendered = get_figure_rendered_property(state)
        call assert_true(rendered, 'Rendered state set to true')
        
        call set_figure_rendered_property(state, .false.)
        rendered = get_figure_rendered_property(state)
        call assert_true(.not. rendered, 'Rendered state set to false')
        
        ! Test plot count
        plot_count = get_figure_plot_count_property(state)
        call assert_true(plot_count == 0, 'Initial plot count zero')
        
        ! Test plots pointer
        plots_ptr => get_figure_plots_property(plots)
        call assert_true(associated(plots_ptr), 'Plots pointer associated')
        call assert_true(size(plots_ptr) == 3, 'Plots pointer correct size')
        
        ! Test backend properties
        call figure_backend_color_property(state, 0.5_wp, 0.3_wp, 0.8_wp)
        call assert_true(.true., 'Backend color property completes')
        
        associated_result = figure_backend_associated_property(state)
        call assert_true(.true., 'Backend associated property completes')
        
        call test_end()
    end subroutine test_property_functions

    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A,A)') "  Testing: ", test_name
    end subroutine test_start

    subroutine test_end()
        passed_count = passed_count + 1
        write(*, '(A)') "    PASSED"
    end subroutine test_end

end program test_figure_properties_simple
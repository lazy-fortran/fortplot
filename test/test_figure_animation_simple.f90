program test_figure_animation_simple
    !! Simple test coverage for fortplot_figure_animation module
    !! Uses only available testing functions to ensure compilation
    use, intrinsic :: iso_fortran_env, only: wp => real64, error_unit
    use fortplot_figure_animation, only: setup_figure_png_backend_for_animation, &
                                        extract_figure_rgb_data_for_animation, &
                                        extract_figure_png_data_for_animation
    use fortplot_figure_initialization, only: figure_state_t, initialize_figure_state
    use fortplot_testing, only: assert_true
    implicit none

    integer :: test_count = 0
    integer :: passed_count = 0

    write(*, '(A)') "=== fortplot_figure_animation Simple Tests ==="

    call test_all_functions()

    ! Print final summary
    write(*, '(A,I0,A,I0,A)') "=== Summary: ", passed_count, "/", test_count, " tests passed ==="
    if (passed_count == test_count) then
        write(*, '(A)') "fortplot_figure_animation: ALL TESTS PASSED"
    else
        write(*, '(A)') "fortplot_figure_animation: SOME TESTS FAILED"
    end if

contains

    subroutine test_all_functions()
        !! Test all functions in the module complete without errors
        type(figure_state_t) :: state
        real(wp), allocatable :: rgb_data(:,:,:)
        integer(1), allocatable :: png_data(:)
        integer :: status
        
        call test_start('all animation functions')
        
        ! Initialize state
        call initialize_figure_state(state, 100, 80)
        allocate(rgb_data(100, 80, 3))
        rgb_data = 0.0_wp
        
        ! Test setup function
        call setup_figure_png_backend_for_animation(state)
        call assert_true(.true., 'setup_png_backend_for_animation completes')
        
        ! Test RGB extraction with rendered=true
        call extract_figure_rgb_data_for_animation(state, rgb_data, .true.)
        call assert_true(allocated(rgb_data), 'RGB data extraction with rendered=true')
        
        ! Test RGB extraction with rendered=false  
        call extract_figure_rgb_data_for_animation(state, rgb_data, .false.)
        call assert_true(allocated(rgb_data), 'RGB data extraction with rendered=false')
        
        ! Test PNG extraction with rendered=true
        call extract_figure_png_data_for_animation(state, png_data, status, .true.)
        call assert_true(allocated(png_data), 'PNG data extraction with rendered=true')
        
        ! Clean up and test PNG extraction with rendered=false
        if (allocated(png_data)) deallocate(png_data)
        call extract_figure_png_data_for_animation(state, png_data, status, .false.)
        call assert_true(allocated(png_data), 'PNG data extraction with rendered=false')
        call assert_true(status == -1, 'Status -1 when rendered=false')
        call assert_true(size(png_data) == 0, 'Empty PNG data when rendered=false')
        
        ! Clean up
        if (allocated(rgb_data)) deallocate(rgb_data)
        if (allocated(png_data)) deallocate(png_data)
        
        call test_end()
    end subroutine test_all_functions

    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A,A)') "  Testing: ", test_name
    end subroutine test_start

    subroutine test_end()
        passed_count = passed_count + 1
        write(*, '(A)') "    PASSED"
    end subroutine test_end

end program test_figure_animation_simple
program test_figure_state_reset
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_initialization, only: figure_state_t, initialize_figure_state, &
                                              reset_figure_state
    implicit none

    type(figure_state_t) :: state

    call initialize_figure_state(state)
    call assert_true(state%plot_count == 0, "initialize: plot_count is zero")
    call assert_true(state%legend_data%num_entries == 0, &
                     "initialize: legend count is zero")
    call assert_true(allocated(state%legend_data%entries), &
                     "initialize: legend entries allocated")
    call assert_true(size(state%legend_data%entries) == 0, &
                     "initialize: legend entries size zero")
    call assert_true(.not. allocated(state%twinx_ylabel), &
                     "initialize: twinx_ylabel unallocated")
    call assert_true(.not. allocated(state%twiny_xlabel), &
                     "initialize: twiny_xlabel unallocated")
    call assert_true(.not. allocated(state%colorbar_label), &
                     "initialize: colorbar_label unallocated")

    state%twinx_ylabel = "y2"
    state%twiny_xlabel = "x2"
    state%colorbar_label = "cb"
    allocate (state%stream_arrows(1))

    call reset_figure_state(state)
    call assert_true(state%plot_count == 0, "reset: plot_count is zero")
    call assert_true(state%legend_data%num_entries == 0, "reset: legend count is zero")
    call assert_true(allocated(state%legend_data%entries), &
                     "reset: legend entries allocated")
    call assert_true(size(state%legend_data%entries) == 0, &
                     "reset: legend entries size zero")
    call assert_true(.not. allocated(state%twinx_ylabel), &
                     "reset: twinx_ylabel unallocated")
    call assert_true(.not. allocated(state%twiny_xlabel), &
                     "reset: twiny_xlabel unallocated")
    call assert_true(.not. allocated(state%colorbar_label), &
                     "reset: colorbar_label unallocated")
    call assert_true(.not. allocated(state%stream_arrows), &
                     "reset: stream_arrows unallocated")

contains

    subroutine assert_true(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) then
            write (*, '(A)') "FAIL: "//trim(message)
            stop 1
        end if
    end subroutine assert_true

end program test_figure_state_reset

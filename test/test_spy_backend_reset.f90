program test_spy_backend_reset
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_spy_backend, only: spy_context_t
    implicit none

    logical :: failed
    type(spy_context_t) :: backend

    failed = .false.

    backend%current_color = [0.5_wp, 0.25_wp, 0.75_wp]
    backend%expected_fill = [1.0_wp, 2.0_wp, 3.0_wp]
    backend%expected_edge = [4.0_wp, 5.0_wp, 6.0_wp]
    backend%fill_calls = 7
    backend%line_calls = 8
    backend%unexpected_calls = 9
    backend%fill_color_ok = .false.
    backend%line_color_ok = .false.

    backend%width = 123
    backend%height = 456
    backend%x_min = -10.0_wp
    backend%x_max = 20.0_wp
    backend%y_min = -30.0_wp
    backend%y_max = 40.0_wp
    backend%has_rendered_arrows = .true.
    backend%uses_vector_arrows = .true.
    backend%has_triangular_arrows = .true.

    call backend%reset()

    call assert_true("reset clears current_color", &
                     all(backend%current_color == [0.0_wp, 0.0_wp, 0.0_wp]), failed)
    call assert_true("reset clears expected_fill", &
                     all(backend%expected_fill == [0.0_wp, 0.0_wp, 0.0_wp]), failed)
    call assert_true("reset clears expected_edge", &
                     all(backend%expected_edge == [0.0_wp, 0.0_wp, 0.0_wp]), failed)

    call assert_true("reset clears fill_calls", backend%fill_calls == 0, failed)
    call assert_true("reset clears line_calls", backend%line_calls == 0, failed)
    call assert_true("reset clears unexpected_calls", backend%unexpected_calls == 0, &
                     failed)
    call assert_true("reset sets fill_color_ok", backend%fill_color_ok, failed)
    call assert_true("reset sets line_color_ok", backend%line_color_ok, failed)

    call assert_true("reset clears width", backend%width == 0, failed)
    call assert_true("reset clears height", backend%height == 0, failed)
    call assert_true("reset sets x_min", backend%x_min == -1.0_wp, failed)
    call assert_true("reset sets x_max", backend%x_max == 1.0_wp, failed)
    call assert_true("reset sets y_min", backend%y_min == -1.0_wp, failed)
    call assert_true("reset sets y_max", backend%y_max == 1.0_wp, failed)
    call assert_true("reset clears has_rendered_arrows", &
                     .not. backend%has_rendered_arrows, failed)
    call assert_true("reset clears uses_vector_arrows", &
                     .not. backend%uses_vector_arrows, failed)
    call assert_true("reset clears has_triangular_arrows", &
                     .not. backend%has_triangular_arrows, failed)

    if (failed) stop 1
    print *, "spy backend reset tests passed"

contains

    subroutine assert_true(name, condition, failed)
        character(len=*), intent(in) :: name
        logical, intent(in) :: condition
        logical, intent(inout) :: failed

        if (.not. condition) then
            failed = .true.
            print *, "FAIL: ", trim(name)
        end if
    end subroutine assert_true

end program test_spy_backend_reset

program test_spy_backend_reset
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_spy_backend, only: spy_context_t
    use fortplot_testing, only: assert_true, test_result_t
    implicit none

    type(spy_context_t) :: backend
    type(test_result_t) :: test_result

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

    call assert_true(all(backend%current_color == [0.0_wp, 0.0_wp, 0.0_wp]), &
                     "reset clears current_color", test_result)
    call assert_true(all(backend%expected_fill == [0.0_wp, 0.0_wp, 0.0_wp]), &
                     "reset clears expected_fill", test_result)
    call assert_true(all(backend%expected_edge == [0.0_wp, 0.0_wp, 0.0_wp]), &
                     "reset clears expected_edge", test_result)

    call assert_true(backend%fill_calls == 0, "reset clears fill_calls", &
                     test_result)
    call assert_true(backend%line_calls == 0, "reset clears line_calls", &
                     test_result)
    call assert_true(backend%unexpected_calls == 0, "reset clears unexpected_calls", &
                     test_result)
    call assert_true(backend%fill_color_ok, "reset sets fill_color_ok", test_result)
    call assert_true(backend%line_color_ok, "reset sets line_color_ok", test_result)

    call assert_true(backend%width == 0, "reset clears width", test_result)
    call assert_true(backend%height == 0, "reset clears height", test_result)
    call assert_true(backend%x_min == -1.0_wp, "reset sets x_min", test_result)
    call assert_true(backend%x_max == 1.0_wp, "reset sets x_max", test_result)
    call assert_true(backend%y_min == -1.0_wp, "reset sets y_min", test_result)
    call assert_true(backend%y_max == 1.0_wp, "reset sets y_max", test_result)
    call assert_true(.not. backend%has_rendered_arrows, &
                     "reset clears has_rendered_arrows", test_result)
    call assert_true(.not. backend%uses_vector_arrows, &
                     "reset clears uses_vector_arrows", test_result)
    call assert_true(.not. backend%has_triangular_arrows, &
                     "reset clears has_triangular_arrows", test_result)

    if (.not. test_result%passed) then
        write(*, '(A,A)') "spy backend reset tests failed: ", trim(test_result%message)
        stop test_result%get_status()
    end if
    print *, "spy backend reset tests passed"

end program test_spy_backend_reset

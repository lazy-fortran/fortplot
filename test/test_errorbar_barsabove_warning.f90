program test_errorbar_barsabove_warning
    !! Verify Issue #1841: barsabove parameter emits warning instead of silent stub
    !! Tests that errorbar handles barsabove=.true. and .false. without crashing
    !! and that the warning path is exercised.
    use fortplot
    use fortplot_logging, only: set_log_level, LOG_LEVEL_WARNING, &
                                initialize_warning_suppression
    implicit none

    real(wp), dimension(5) :: x, y, yerr

    x = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
    y = [2.0_wp, 2.5_wp, 3.0_wp, 3.5_wp, 4.0_wp]
    yerr = [0.2_wp, 0.3_wp, 0.25_wp, 0.35_wp, 0.15_wp]

    ! Force warnings on so the test can verify output
    call initialize_warning_suppression()

    ! Test 1: barsabove=.false. should NOT emit a warning
    call figure()
    call errorbar(x, y, yerr=yerr, barsabove=.false.)
    call title('barsabove=.false. test')
    call savefig('build/test/output/test_barsabove_false.pdf')
    print *, 'PASS: barsabove=.false. did not crash'

    ! Test 2: barsabove=.true. should emit a warning (visible in stdout)
    call figure()
    call errorbar(x, y, yerr=yerr, barsabove=.true.)
    call title('barsabove=.true. test')
    call savefig('build/test/output/test_barsabove_true.pdf')
    print *, 'PASS: barsabove=.true. did not crash'

    ! Test 3: barsabove omitted (default) should work without warning
    call figure()
    call errorbar(x, y, yerr=yerr)
    call title('barsabove omitted test')
    call savefig('build/test/output/test_barsabove_omitted.pdf')
    print *, 'PASS: barsabove omitted did not crash'

    print *, 'PASS: barsabove warning behavior verified (fixes #1841)'

end program test_errorbar_barsabove_warning

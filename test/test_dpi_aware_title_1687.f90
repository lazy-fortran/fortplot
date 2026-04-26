program test_dpi_aware_title_1687
    !! Regression test for issue #1687: PNG backend annotation_demo oversized text
    !!
    !! Verifies that the title font size is DPI-aware so that user-specified
    !! annotation font sizes (in points) render at visually consistent sizes
    !! relative to the title.
    !!
    !! Before fix: title used hardcoded TITLE_FONT_SIZE=20 pixels (DPI-unaware),
    !! while annotation text converted font_size from points to pixels via
    !! font_size*dpi/72. At 100 DPI, 16pt annotation (22.22px) was larger than
    !! the 20px title, making the annotation appear "disproportionately large".
    !!
    !! After fix: title uses TITLE_FONT_SIZE_PT=14pt converted to pixels via
    !! dpi/72, so at 100 DPI the title is ~19.44px and the 16pt annotation
    !! is ~22.22px — a reasonable 16pt > 14pt relationship.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot
    use fortplot_text_layout, only: TITLE_FONT_SIZE_PT
    use fortplot_raster_core, only: pt2px
    implicit none

    integer :: status
    logical :: pass
    real(wp) :: title_px_100, title_px_72, title_px_150
    real(wp) :: ann_px_16pt_100, ann_px_16pt_72
    real(wp) :: tol

    tol = 0.5_wp  ! Tolerance for rounding in pt->px conversion

    print *, "=== DPI-Aware Title Font Size Test (Issue #1687) ==="
    print *, ""

    ! Test 1: TITLE_FONT_SIZE_PT is defined and reasonable
    pass = (TITLE_FONT_SIZE_PT > 10.0_wp .and. TITLE_FONT_SIZE_PT < 20.0_wp)
    print *, "Test 1: TITLE_FONT_SIZE_PT in reasonable range (10-20pt)"
    print *, "  TITLE_FONT_SIZE_PT =", TITLE_FONT_SIZE_PT
    if (.not. pass) then
        print *, "  FAIL: expected 10 < TITLE_FONT_SIZE_PT < 20"
        stop 1
    end if
    print *, "  PASS"
    print *, ""

    ! Test 2: pt2px conversion is correct
    title_px_100 = pt2px(TITLE_FONT_SIZE_PT, 100.0_wp)
    title_px_72 = pt2px(TITLE_FONT_SIZE_PT, 72.0_wp)
    title_px_150 = pt2px(TITLE_FONT_SIZE_PT, 150.0_wp)

    print *, "Test 2: pt2px conversion for title font size"
    print *, "  At 100 DPI: 14pt ->", title_px_100, "px (expect ~19.44)"
    print *, "  At 72 DPI:  14pt ->", title_px_72, "px (expect ~14.00)"
    print *, "  At 150 DPI: 14pt ->", title_px_150, "px (expect ~29.17)"

    pass = .true.
    if (abs(title_px_100 - 19.44_wp) > tol) pass = .false.
    if (abs(title_px_72 - 14.0_wp) > tol) pass = .false.
    if (abs(title_px_150 - 29.17_wp) > tol) pass = .false.

    if (.not. pass) then
        print *, "  FAIL: pt2px conversion incorrect"
        stop 1
    end if
    print *, "  PASS"
    print *, ""

    ! Test 3: 16pt annotation text is larger than 14pt title, but not excessive
    ann_px_16pt_100 = pt2px(16.0_wp, 100.0_wp)
    ann_px_16pt_72 = pt2px(16.0_wp, 72.0_wp)

    print *, "Test 3: Annotation text (16pt) vs title (14pt) size ratio"
    print *, "  At 100 DPI: annotation=", ann_px_16pt_100, "px, title=", title_px_100, &
         "px, ratio=", ann_px_16pt_100/title_px_100
    print *, "  At 72 DPI:  annotation=", ann_px_16pt_72, "px, title=", title_px_72, &
         "px, ratio=", ann_px_16pt_72/title_px_72

    ! 16pt / 14pt = 1.143, should be consistent across DPIs
    pass = .true.
    if (ann_px_16pt_100 <= title_px_100) then
        print *, "  FAIL: 16pt annotation should be larger than 14pt title"
        pass = .false.
    end if
    if (ann_px_16pt_100/title_px_100 > 1.5_wp) then
        print *, "  FAIL: ratio too large (>1.5), annotation disproportionately oversized"
        pass = .false.
    end if

    if (.not. pass) then
        stop 1
    end if
    print *, "  PASS"
    print *, ""

    ! Test 4: End-to-end render test — annotation_demo produces valid output
    print *, "Test 4: End-to-end annotation rendering"

    call figure(figsize=[8.0_wp, 6.0_wp])

    ! Simple plot
    call add_plot([0.0_wp, 1.0_wp], [0.0_wp, 1.0_wp], label="test")
    call title("Test Title")

    ! Annotation with 16pt text at figure coordinates (like the demo)
    call text(0.5_wp, 0.95_wp, "ANNOTATION", &
              coord_type=COORD_FIGURE, font_size=16.0_wp, alignment="center")

    ! Save and verify
    call savefig_with_status("build/test/output/test_dpi_aware_title_1687.png", status)
    if (status /= 0) then
        print *, "  FAIL: savefig returned status", status
        stop 1
    end if
    print *, "  PNG saved successfully"
    print *, "  PASS"
    print *, ""

    print *, "=== All DPI-aware title tests PASSED ==="

end program test_dpi_aware_title_1687
